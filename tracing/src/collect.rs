//! Collects and records trace data.
use core::{
    any::Any,
    fmt,
    sync::atomic::{AtomicBool, AtomicUsize, Ordering},
};

#[cfg(feature = "std")]
use std::{
    cell::{Cell, RefCell, RefMut},
};

use tracing_core::{Dispatch, Metadata, span, LevelFilter, Event};

pub use tracing_core::collect::*;
pub use tracing_core::dispatch::SetGlobalDefaultError;

#[cfg(feature = "std")]
thread_local! {
    static CURRENT_STATE: State = State {
        default: RefCell::new(None),
        can_enter: Cell::new(true),
    };
}

#[cfg(feature = "std")]
static SCOPED_COUNT: AtomicUsize = AtomicUsize::new(0);

/// A collector that delegates to per-thread collectors.
#[cfg(feature = "std")]
#[cfg_attr(docsrs, doc(cfg(feature = "std")))]
#[derive(Debug)]
pub struct DelegatingCollector {
    fallback: Dispatch
}

/// The dispatch state of a thread.
#[cfg(feature = "std")]
struct State {
    /// This thread's current default dispatcher.
    // TODO: should this be a dispatch? it doesn't need to point to a GlobalCollect,
    // merely a LocalCollect
    default: RefCell<Option<Dispatch>>,
    /// Whether or not we can currently begin dispatching a trace event.
    ///
    /// This is set to `false` when functions such as `enter`, `exit`, `event`,
    /// and `new_span` are called on this thread's default dispatcher, to
    /// prevent further trace events triggered inside those functions from
    /// creating an infinite recursion. When we finish handling a dispatch, this
    /// is set back to `true`.
    can_enter: Cell<bool>,
}

/// While this guard is active, additional calls to collector functions on
/// the default dispatcher will not be able to access the dispatch context.
/// Dropping the guard will allow the dispatch context to be re-entered.
#[cfg(feature = "std")]
struct Entered<'a>(&'a State);

/// A guard that resets the current default dispatcher to the prior
/// default dispatcher when dropped.
#[cfg(feature = "std")]
#[cfg_attr(docsrs, doc(cfg(feature = "std")))]
#[derive(Debug)]
pub struct CollectGuard(Option<Dispatch>);

// ===== impl DelegatingCollector =====

#[cfg(feature = "std")]
#[cfg_attr(docsrs, doc(cfg(feature = "std")))]
impl DelegatingCollector {
    /// Constructs a new [`DelegatingCollector`] that uses [`NoCollector`] as a
    /// fallback when no thread-local collector is set. 
    pub fn new() -> Self {
        static NO_COLLECTOR: NoCollector = NoCollector::new();
        Self {
            fallback: Dispatch::from_static(&NO_COLLECTOR)
        }
    }

    /// Constructs a new [`DelegatingCollector`] that uses the given collector
    /// as a fallback when no thread-local collector is set.
    pub fn with_fallback<C>(collector: C) -> Self
    where
        C: Collect + Send + Sync + 'static,
    {
        Self {
            fallback: Dispatch::new(collector)
        }
    }

    /// Use the given collector for the duration of the lifetime of the returned
    /// [`CollectGuard`].
    #[must_use = "Dropping the guard unregisters the dispatcher."]
    pub fn set(dispatch: impl Into<Dispatch>) -> CollectGuard
    {
        // When this guard is dropped, the default dispatcher will be reset to the
        // prior default. Using this ensures that we always reset to the prior
        // dispatcher even if the thread calling this function panics.
        State::set_default(dispatch.into())
    }

    /// Run the given closure, using the given collector during its execution.
    ///
    /// The given `collector` is used when creating a new [`Span`] or
    /// [`Event`], _if no span is currently executing_. If a span is currently
    /// executing, new spans or events are dispatched to the collector that
    /// tagged that span, instead.
    ///
    /// [`Span`]: super::span::Span
    /// [`Collect`]: super::collect::Collect
    /// [`Event`]: tracing_core::Event
    pub fn run<T, C>(dispatch: impl Into<Dispatch>, f: impl FnOnce() -> T) -> T
    where
        C: Collect + Send + Sync + 'static,
    {
        // When this guard is dropped, the default dispatcher will be reset to the
        // prior default. Using this (rather than simply resetting after calling
        // `f`) ensures that we always reset to the prior dispatcher even if `f`
        // panics.
        let _guard = State::set_default(dispatch.into());
        f()
    }

    #[inline(always)]
    fn get_global() -> &'static Dispatch {
        crate::dispatch::get_default(|dispatch|
            match dispatch.downcast_ref::<Self>() {
                Some(delegating) => &delegating.fallback,
                None => dispatch,
            }
        )
    }

    /// Executes a closure with a reference to this thread's current [dispatcher].
    ///
    /// Note that calls to `get_default` should not be nested; if this function is
    /// called while inside of another `get_default`, that closure will be provided
    /// with `Dispatch::none` rather than the previously set dispatcher.
    ///
    /// [dispatcher]: super::dispatch::Dispatch
    #[inline(always)]
    fn get_default<T, F>(&self, mut f: F) -> T
    where
        F: FnOnce(&Dispatch) -> T,
    {
        if SCOPED_COUNT.load(Ordering::Acquire) == 0 {
            // fast path if no scoped dispatcher has been set; just use the global
            // default.
            return f(&self.fallback);
        }
        self.get_default_slow(f)
    }

    #[inline(never)]
    fn get_default_slow<T, F>(&self, mut f: F) -> T
    where
        F: FnOnce(&Dispatch) -> T,
    {
        // While this guard is active, additional calls to collector functions on
        // the default dispatcher will not be able to access the dispatch context.
        // Dropping the guard will allow the dispatch context to be re-entered.
        struct Entered<'a>(&'a Cell<bool>);
        impl<'a> Drop for Entered<'a> {
            #[inline]
            fn drop(&mut self) {
                self.0.set(true);
            }
        }

        CURRENT_STATE
            .try_with(|state| {
                if state.can_enter.replace(false) {
                    let _guard = Entered(&state.can_enter);

                    let mut default = state.default.borrow_mut();
                    let default = default
                        // if the local default for this thread has never been set,
                        // populate it with the global default, so we don't have to
                        // keep getting the global on every `get_default_slow` call.
                        .get_or_insert_with(|| self.fallback.clone());

                    return f(&*default);
                }

                f(&Dispatch::none())
            })
            .unwrap_or_else(|_| f(&Dispatch::none()))
    }
}

impl GlobalCollect for DelegatingCollector
{
    fn register_callsite(&self, metadata: &'static Metadata<'static>) -> Interest {
        self.get_default(|dispatcher| dispatcher.register_callsite(metadata))
    }

    fn enabled(&self, metadata: &Metadata<'_>) -> bool {
        self.get_default(|dispatcher| dispatcher.enabled(metadata))
    }

    fn max_level_hint(&self) -> Option<LevelFilter> {
        self.get_default(|dispatcher| dispatcher.max_level_hint())
    }

    fn new_span(&self, span: &span::Attributes<'_>) -> span::GlobalId {
        self.get_default(|dispatcher| dispatcher.new_span(span))
    }

    fn record(&self, span: &span::GlobalId, values: &span::Record<'_>) {
        self.get_default(|dispatcher| dispatcher.record(span, values))
    }

    fn record_follows_from(&self, span: &span::GlobalId, follows: &span::GlobalId) {
        self.get_default(|dispatcher| dispatcher.record_follows_from(span, follows))
    }

    fn event(&self, event: &Event<'_>) {
        self.get_default(|dispatcher| dispatcher.event(event))
    }

    fn enter(&self, span: &span::GlobalId) {
        self.get_default(|dispatcher| dispatcher.enter(span))
    }

    fn exit(&self, span: &span::GlobalId) {
        self.get_default(|dispatcher| dispatcher.exit(span))
    }

    fn clone_span(&self, span: &span::GlobalId) -> span::GlobalId {
        self.get_default(|dispatcher| dispatcher.clone_span(span))
    }

    fn drop_span(&self, span: span::GlobalId) {
        #[allow(deprecated)]
        self.get_default(|dispatcher| dispatcher.drop_span(span))
    }

    fn try_close(&self, span: span::GlobalId) -> bool {
        self.get_default(|dispatcher| dispatcher.try_close(span))
    }

    fn current_span(&self) -> span::Current {
        self.get_default(|dispatcher| dispatcher.current_span())
    }
}

// ===== impl State =====

#[cfg(feature = "std")]
impl State {
    /// Replaces the current default dispatcher on this thread with the provided
    /// dispatcher.
    ///
    /// Dropping the returned `CollectGuard` will reset the default dispatcher
    /// to the previous value.
    #[inline]
    fn set_default(new_dispatch: Dispatch) -> CollectGuard {
        let prior = CURRENT_STATE
            .try_with(|state| {
                state.can_enter.set(true);
                state
                    .default
                    .replace(Some(new_dispatch))
                    // if the scoped default was not set on this thread, set the
                    // `prior` default to the global default to populate the
                    // scoped default when unsetting *this* default
                    .unwrap_or_else(|| DelegatingCollector::get_global().clone())
            })
            .ok();
        SCOPED_COUNT.fetch_add(1, Ordering::Release);
        CollectGuard(prior)
    }

    #[inline]
    fn enter(&self) -> Option<Entered<'_>> {
        if self.can_enter.replace(false) {
            Some(Entered(self))
        } else {
            None
        }
    }
}

// ===== impl Entered =====

#[cfg(feature = "std")]
impl<'a> Entered<'a> {
    #[inline]
    fn current(&self) -> RefMut<'a, Dispatch> {
        let default = self.0.default.borrow_mut();
        RefMut::map(default, |default| {
            default.get_or_insert_with(|| DelegatingCollector::get_global().clone())
        })
    }
}

#[cfg(feature = "std")]
impl<'a> Drop for Entered<'a> {
    #[inline]
    fn drop(&mut self) {
        self.0.can_enter.set(true);
    }
}

// ===== impl DefaultGuard =====

#[cfg(feature = "std")]
impl Drop for CollectGuard {
    #[inline]
    fn drop(&mut self) {
        SCOPED_COUNT.fetch_sub(1, Ordering::Release);
        if let Some(dispatch) = self.0.take() {
            // Replace the dispatcher and then drop the old one outside
            // of the thread-local context. Dropping the dispatch may
            // lead to the drop of a collector which, in the process,
            // could then also attempt to access the same thread local
            // state -- causing a clash.
            let prev = CURRENT_STATE.try_with(|state| state.default.replace(Some(dispatch)));
            drop(prev)
        }
    }
}
