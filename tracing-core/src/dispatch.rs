//! Dispatches trace events to [`GlobalCollect`]s.
//!
//! The _dispatcher_ is the component of the tracing system which is responsible
//! for forwarding trace data from the instrumentation points that generate it
//! to the collector that collects it.
//!
//! # Using the Trace Dispatcher
//!
//! Every thread in a program using `tracing` has a _default collector_. When
//! events occur, or spans are created, they are dispatched to the thread's
//! current collector.
//!
//! ## Setting the Default Collector
//!
//! By default, the current collector is an empty implementation that does
//! nothing. Trace data provided to this "do nothing" implementation is
//! immediately discarded, and is not available for any purpose.
//!
//! To use another collector implementation, it must be set as the default.
//! There are two methods for doing so: [`with_default`] and
//! [`set`]. `with_default` sets the default collector for the
//! duration of a scope, while `set` sets a default collector
//! for the entire process.
//!
//! To use either of these functions, we must first wrap our collector in a
//! [`Dispatch`], a cloneable, type-erased reference to a collector. For
//! example:
//! ```rust
//! # pub struct FooCollector;
//! # use tracing_core::{
//! #   dispatch, Event, Metadata,
//! #   span::{Attributes, Current, Id, Record}
//! # };
//! # impl tracing_core::GlobalCollect for FooCollector {
//! #   fn new_span(&self, _: &Attributes) -> Id { Id::from_u64(0) }
//! #   fn record(&self, _: &Id, _: &Record) {}
//! #   fn event(&self, _: &Event) {}
//! #   fn record_follows_from(&self, _: &Id, _: &Id) {}
//! #   fn enabled(&self, _: &Metadata) -> bool { false }
//! #   fn enter(&self, _: &Id) {}
//! #   fn exit(&self, _: &Id) {}
//! #   fn current_span(&self) -> Current { Current::unknown() }
//! # }
//! # impl FooCollector { fn new() -> Self { FooCollector } }
//! # #[cfg(feature = "alloc")]
//! use dispatch::Dispatch;
//!
//! # #[cfg(feature = "alloc")]
//! let my_collector = FooCollector::new();
//! # #[cfg(feature = "alloc")]
//! let my_dispatch = Dispatch::new(my_collector);
//! ```
//! Then, we can use [`with_default`] to set our `Dispatch` as the default for
//! the duration of a block:
//! ```rust
//! # pub struct FooCollector;
//! # use tracing_core::{
//! #   dispatch, Event, Metadata,
//! #   span::{Attributes, Current, Id, Record}
//! # };
//! # impl tracing_core::GlobalCollect for FooCollector {
//! #   fn new_span(&self, _: &Attributes) -> Id { Id::from_u64(0) }
//! #   fn record(&self, _: &Id, _: &Record) {}
//! #   fn event(&self, _: &Event) {}
//! #   fn record_follows_from(&self, _: &Id, _: &Id) {}
//! #   fn enabled(&self, _: &Metadata) -> bool { false }
//! #   fn enter(&self, _: &Id) {}
//! #   fn exit(&self, _: &Id) {}
//! #   fn current_span(&self) -> Current { Current::unknown() }
//! # }
//! # impl FooCollector { fn new() -> Self { FooCollector } }
//! # let _my_collector = FooCollector::new();
//! # #[cfg(feature = "std")]
//! # let my_dispatch = dispatch::Dispatch::new(_my_collector);
//! // no default collector
//!
//! # #[cfg(feature = "std")]
//! dispatch::with_default(&my_dispatch, || {
//!     // my_collector is the default
//! });
//!
//! // no default collector again
//! ```
//! It's important to note that `with_default` will not propagate the current
//! thread's default collector to any threads spawned within the `with_default`
//! block. To propagate the default collector to new threads, either use
//! `with_default` from the new thread, or use `set`.
//!
//! As an alternative to `with_default`, we can use [`set`] to
//! set a `Dispatch` as the default for all threads, for the lifetime of the
//! program. For example:
//! ```rust
//! # pub struct FooCollector;
//! # use tracing_core::{
//! #   dispatch, Event, Metadata,
//! #   span::{Attributes, Current, Id, Record}
//! # };
//! # impl tracing_core::GlobalCollect for FooCollector {
//! #   fn new_span(&self, _: &Attributes) -> Id { Id::from_u64(0) }
//! #   fn record(&self, _: &Id, _: &Record) {}
//! #   fn event(&self, _: &Event) {}
//! #   fn record_follows_from(&self, _: &Id, _: &Id) {}
//! #   fn enabled(&self, _: &Metadata) -> bool { false }
//! #   fn enter(&self, _: &Id) {}
//! #   fn exit(&self, _: &Id) {}
//! #   fn current_span(&self) -> Current { Current::unknown() }
//! # }
//! # impl FooCollector { fn new() -> Self { FooCollector } }
//! # #[cfg(feature = "std")]
//! # let my_collector = FooCollector::new();
//! # #[cfg(feature = "std")]
//! # let my_dispatch = dispatch::Dispatch::new(my_collector);
//! // no default collector
//!
//! # #[cfg(feature = "std")]
//! dispatch::set(my_dispatch)
//!     // `set` will return an error if the global default
//!     // collector has already been set.
//!     .expect("global default was already set!");
//!
//! // `my_collector` is now the default
//! ```
//!
//! <div class="example-wrap" style="display:inline-block">
//! <pre class="ignore" style="white-space:normal;font:inherit;">
//!
//! **Note**: the thread-local scoped dispatcher ([`with_default`]) requires the
//! Rust standard library. `no_std` users should use [`set`] instead.
//!
//! </pre></div>
//!
//! ## Accessing the Default Collector
//!
//! A thread's current default collector can be accessed using the
//! [`get_default`] function, which executes a closure with a reference to the
//! currently default `Dispatch`. This is used primarily by `tracing`
//! instrumentation.
use crate::{
    collect::{self, GlobalCollect, NoCollector},
    span, Event, LevelFilter, Metadata, parent::Parent, Collect,
};

use core::{
    any::Any,
    fmt,
    hash::{Hash, Hasher},
    ptr,
    sync::atomic::{AtomicBool, AtomicUsize, Ordering},
};

#[cfg(feature = "std")]
use std::{
    cell::{Cell, RefCell, RefMut},
    error,
    sync::Weak,
};

#[cfg(feature = "alloc")]
use alloc::sync::Arc;

#[cfg(feature = "alloc")]
use core::ops::Deref;

/// `Dispatch` trace data to a [`Collect`].
#[derive(Clone)]
pub struct Dispatch {
    #[cfg(feature = "alloc")]
    collector: Kind<Arc<dyn GlobalCollect + Send + Sync>>,

    #[cfg(not(feature = "alloc"))]
    collector: &'static (dyn GlobalCollect + Send + Sync),
}

#[cfg(feature = "alloc")]
#[derive(Clone)]
enum Kind<T> {
    Global(&'static (dyn GlobalCollect + Send + Sync)),
    Scoped(T),
}

static EXISTS: AtomicBool = AtomicBool::new(false);
static GLOBAL_INIT: AtomicUsize = AtomicUsize::new(UNINITIALIZED);

const UNINITIALIZED: usize = 0;
const INITIALIZING: usize = 1;
const INITIALIZED: usize = 2;

static mut GLOBAL_DISPATCH: Dispatch = Dispatch {
    #[cfg(feature = "alloc")]
    collector: Kind::Global(&NO_COLLECTOR),
    #[cfg(not(feature = "alloc"))]
    collector: &NO_COLLECTOR,
};
static NONE: Dispatch = Dispatch {
    #[cfg(feature = "alloc")]
    collector: Kind::Global(&NO_COLLECTOR),
    #[cfg(not(feature = "alloc"))]
    collector: &NO_COLLECTOR,
};
static NO_COLLECTOR: NoCollector = NoCollector::new();

/// Sets this dispatch as the global default for the duration of the entire program.
/// Will be used as a fallback if no thread-local dispatch has been set in a thread
/// (using `with_default`.)
///
/// Can only be set once; subsequent attempts to set the global default will fail.
/// Returns `Err` if the global default has already been set.
///
///
/// <div class="example-wrap" style="display:inline-block"><pre class="compile_fail" style="white-space:normal;font:inherit;">
/// <strong>Warning</strong>: In general, libraries should <em>not</em> call
/// <code>set()</code>! Doing so will cause conflicts when
/// executables that depend on the library try to set the default collector later.
/// </pre></div>
///
/// [span]: super::span
/// [`Event`]: super::event::Event
pub fn set(dispatcher: Dispatch) -> Result<(), SetGlobalDefaultError> {
    // if `compare_exchange` returns Result::Ok(_), then `new` has been set and
    // `current`—now the prior value—has been returned in the `Ok()` branch.
    if GLOBAL_INIT
        .compare_exchange(
            UNINITIALIZED,
            INITIALIZING,
            Ordering::SeqCst,
            Ordering::SeqCst,
        )
        .is_ok()
    {
        let collector = dispatcher.collector;

        unsafe {
            GLOBAL_DISPATCH = Dispatch { collector };
        }
        GLOBAL_INIT.store(INITIALIZED, Ordering::SeqCst);
        EXISTS.store(true, Ordering::Release);
        Ok(())
    } else {
        Err(SetGlobalDefaultError { _no_construct: () })
    }
}

/// Returns true if a `tracing` dispatcher has ever been set.
///
/// This may be used to completely elide trace points if tracing is not in use
/// at all or has yet to be initialized.
#[doc(hidden)]
#[inline(always)]
pub fn has_been_set() -> bool {
    EXISTS.load(Ordering::Relaxed)
}

/// Returned if setting the global dispatcher fails.
#[derive(Debug)]
pub struct SetGlobalDefaultError {
    _no_construct: (),
}

impl fmt::Display for SetGlobalDefaultError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.pad("a global default trace dispatcher has already been set")
    }
}

#[cfg(feature = "std")]
#[cfg_attr(docsrs, doc(cfg(feature = "std")))]
impl error::Error for SetGlobalDefaultError {}

/// Executes a closure with a reference to the current [dispatcher].
///
/// [dispatcher]: super::dispatcher::Dispatch
#[doc(hidden)]
pub fn get_current<T>(f: impl FnOnce(&Dispatch) -> T) -> Option<T> {
    Some(f(&get_global()))
}

/// Executes a closure with a reference to the current [dispatcher].
///
/// [dispatcher]: super::dispatcher::Dispatch
pub fn get_default<T, F>(mut f: F) -> T
where
    F: FnMut(&'static Dispatch) -> T,
{
    f(get_global())
}

#[inline(always)]
pub(crate) fn get_global() -> &'static Dispatch {
    if GLOBAL_INIT.load(Ordering::Acquire) != INITIALIZED {
        return &NONE;
    }
    unsafe {
        // This is safe given the invariant that setting the global dispatcher
        // also sets `GLOBAL_INIT` to `INITIALIZED`.
        &GLOBAL_DISPATCH
    }
}

impl Dispatch {
    /// Returns a new `Dispatch` that discards events and spans.
    #[inline]
    pub fn none() -> Self {
        Dispatch {
            #[cfg(feature = "alloc")]
            collector: Kind::Global(&NO_COLLECTOR),
            #[cfg(not(feature = "alloc"))]
            collector: &NO_COLLECTOR,
        }
    }

    /// Returns a `Dispatch` that forwards to the given [`GlobalCollect`].
    ///
    /// [`GlobalCollect`]: super::collect::GlobalCollect
    #[cfg(feature = "alloc")]
    #[cfg_attr(docsrs, doc(cfg(any(feature = "std", feature = "alloc"))))]
    pub fn new<C>(collector: C) -> Self
    where
        C: GlobalCollect + Send + Sync + 'static,
    {
        let me = Dispatch {
            collector: Kind::Scoped(Arc::new(collector)),
        };
        crate::callsite::register_dispatch(&me);
        me
    }

    /// Returns a `Dispatch` that forwards to the given static [collector].
    ///
    /// Unlike [`Dispatch::new`], this function is always available on all
    /// platforms, even when the `std` or `alloc` features are disabled.
    ///
    /// In order to use `from_static`, the `Collector` itself must be stored in
    /// a static. For example:
    ///
    /// ```rust
    /// struct MyCollector {
    ///    // ...
    /// }
    ///
    /// # use tracing_core::{span::{Id, Attributes, Current, Record}, Event, Metadata};
    /// impl tracing_core::GlobalCollect for MyCollector {
    ///     // ...
    /// #   fn new_span(&self, _: &Attributes) -> Id { Id::from_u64(0) }
    /// #   fn record(&self, _: &Id, _: &Record) {}
    /// #   fn event(&self, _: &Event) {}
    /// #   fn record_follows_from(&self, _: &Id, _: &Id) {}
    /// #   fn enabled(&self, _: &Metadata) -> bool { false }
    /// #   fn enter(&self, _: &Id) {}
    /// #   fn exit(&self, _: &Id) {}
    /// #   fn current_span(&self) -> Current { Current::unknown() }
    /// }
    ///
    /// static COLLECTOR: MyCollector = MyCollector {
    ///     // ...
    /// };
    ///
    /// fn main() {
    ///     use tracing_core::dispatch::{self, Dispatch};
    ///
    ///     let dispatch = Dispatch::from_static(&COLLECTOR);
    ///
    ///     dispatch::set(dispatch)
    ///         .expect("no global default collector should have been set previously!");
    /// }
    /// ```
    ///
    /// Constructing the collector in a static initializer may make some forms
    /// of runtime configuration more challenging. If this is the case, users
    /// with access to `liballoc` or the Rust standard library are encouraged to
    /// use [`Dispatch::new`] rather than `from_static`. `no_std` users who
    /// cannot allocate or do not have access to `liballoc` may want to consider
    /// the [`once_cell`] crate, or another library which allows lazy
    /// initialization of statics.
    ///
    /// [collector]: super::collect::GlobalCollect
    /// [`once_cell`]: https://crates.io/crates/once_cell
    pub fn from_static(collector: &'static (dyn GlobalCollect + Send + Sync)) -> Self {
        #[cfg(feature = "alloc")]
        let me = Self {
            collector: Kind::Global(collector),
        };
        #[cfg(not(feature = "alloc"))]
        let me = Self { collector };
        crate::callsite::register_dispatch(&me);
        me
    }

    #[inline(always)]
    #[cfg(feature = "alloc")]
    fn collector(&self) -> &(dyn GlobalCollect + Send + Sync) {
        match self.collector {
            Kind::Scoped(ref s) => Arc::deref(s),
            Kind::Global(s) => s,
        }
    }

    #[inline(always)]
    #[cfg(not(feature = "alloc"))]
    fn collector(&self) -> &(dyn GlobalCollect + Send + Sync) {
        self.collector
    }

    /// Registers a new callsite with this collector, returning whether or not
    /// the collector is interested in being notified about the callsite.
    ///
    /// This calls the [`register_callsite`] function on the [`GlobalCollect`]
    /// that this `Dispatch` forwards to.
    ///
    /// [`GlobalCollect`]: super::collect::GlobalCollect
    /// [`register_callsite`]: super::collect::GlobalCollect::register_callsite
    #[inline]
    pub fn register_callsite(&self, metadata: &'static Metadata<'static>) -> collect::Interest {
        self.collector().register_callsite(metadata)
    }

    /// Returns the highest [verbosity level][level] that this [collector] will
    /// enable, or `None`, if the collector does not implement level-based
    /// filtering or chooses not to implement this method.
    ///
    /// This calls the [`max_level_hint`] function on the [`GlobalCollect`]
    /// that this `Dispatch` forwards to.
    ///
    /// [level]: super::Level
    /// [collector]: super::collect::GlobalCollect
    /// [`GlobalCollect`]: super::collect::GlobalCollect
    /// [`register_callsite`]: super::collect::GlobalCollect::max_level_hint
    #[inline]
    pub fn max_level_hint(&self) -> Option<LevelFilter> {
        self.collector().max_level_hint()
    }

    /// Record the construction of a new span, returning a new [ID] for the
    /// span being constructed.
    ///
    /// This calls the [`new_span`] function on the [`GlobalCollect`] that this
    /// `Dispatch` forwards to.
    ///
    /// [ID]: super::span::GlobalId
    /// [`GlobalCollect`]: super::collect::GlobalCollect
    /// [`new_span`]: super::collect::GlobalCollect::new_span
    #[inline]
    pub fn new_span(&self, span: &span::Attributes<'_>) -> span::GlobalId {
        self.collector().new_span(span)
    }

    /// Record a set of values on a span.
    ///
    /// This calls the [`record`] function on the [`GlobalCollect`] that this
    /// `Dispatch` forwards to.
    ///
    /// [`GlobalCollect`]: super::collect::GlobalCollect
    /// [`record`]: super::collect::GlobalCollect::record
    #[inline]
    pub fn record(&self, span: &span::GlobalId, values: &span::Record<'_>) {
        self.collector().record(span, values)
    }

    /// Adds an indication that `span` follows from the span with the id
    /// `follows`.
    ///
    /// This calls the [`record_follows_from`] function on the [`GlobalCollect`]
    /// that this `Dispatch` forwards to.
    ///
    /// [`GlobalCollect`]: super::collect::GlobalCollect
    /// [`record_follows_from`]: super::collect::GlobalCollect::record_follows_from
    #[inline]
    pub fn record_follows_from(&self, span: &span::GlobalId, follows: &span::GlobalId) {
        self.collector().record_follows_from(span, follows)
    }

    /// Returns true if a span with the specified [metadata] would be
    /// recorded.
    ///
    /// This calls the [`enabled`] function on the [`GlobalCollect`] that this
    /// `Dispatch` forwards to.
    ///
    /// [metadata]: super::metadata::Metadata
    /// [`GlobalCollect`]: super::collect::GlobalCollect
    /// [`enabled`]: super::collect::GlobalCollect::enabled
    #[inline]
    pub fn enabled(&self, metadata: &Metadata<'_>) -> bool {
        self.collector().enabled(metadata)
    }

    /// Records that an [`Event`] has occurred.
    ///
    /// This calls the [`event`] function on the [`GlobalCollect`] that this
    /// `Dispatch` forwards to.
    ///
    /// [`Event`]: super::event::Event
    /// [`GlobalCollect`]: super::collect::GlobalCollect
    /// [`event`]: super::collect::GlobalCollect::event
    #[inline]
    pub fn event(&self, event: &Event<'_>) {
        self.collector().event(event)
    }

    /// Records that a span has been can_enter.
    ///
    /// This calls the [`enter`] function on the [`GlobalCollect`] that this
    /// `Dispatch` forwards to.
    ///
    /// [`GlobalCollect`]: super::collect::GlobalCollect
    /// [`enter`]: super::collect::GlobalCollect::enter
    pub fn enter(&self, span: &span::GlobalId) {
        self.collector().enter(span);
    }

    /// Records that a span has been exited.
    ///
    /// This calls the [`exit`] function on the [`GlobalCollect`] that this
    /// `Dispatch` forwards to.
    ///
    /// [`GlobalCollect`]: super::collect::GlobalCollect
    /// [`exit`]: super::collect::GlobalCollect::exit
    pub fn exit(&self, span: &span::GlobalId) {
        self.collector().exit(span);
    }

    /// Notifies the [collector] that a [span ID] has been cloned.
    ///
    /// This function must only be called with span IDs that were returned by
    /// this `Dispatch`'s [`new_span`] function. The `tracing` crate upholds
    /// this guarantee and any other libraries implementing instrumentation APIs
    /// must as well.
    ///
    /// This calls the [`clone_span`] function on the [`GlobalCollect`] that this
    /// `Dispatch` forwards to.
    ///
    /// [span ID]: super::span::GlobalId
    /// [collector]: super::collect::GlobalCollect
    /// [`clone_span`]: super::collect::GlobalCollect::clone_span
    /// [`new_span`]: super::collect::GlobalCollect::new_span
    #[inline]
    pub fn clone_span(&self, id: &span::GlobalId) -> span::GlobalId {
        self.collector().clone_span(id)
    }

    /// Notifies the collector that a [span ID] has been dropped.
    ///
    /// This function must only be called with span IDs that were returned by
    /// this `Dispatch`'s [`new_span`] function. The `tracing` crate upholds
    /// this guarantee and any other libraries implementing instrumentation APIs
    /// must as well.
    ///
    /// This calls the [`drop_span`] function on the [`GlobalCollect`] that this
    ///  `Dispatch` forwards to.
    ///
    /// <div class="example-wrap" style="display:inline-block"><pre class="compile_fail" style="white-space:normal;font:inherit;">
    ///
    /// **Deprecated**: The [`try_close`] method is functionally identical, but returns `true` if the span is now closed.
    /// It should be used instead of this method.
    ///
    /// </pre></div>
    ///
    /// [span ID]: super::span::GlobalId
    /// [`GlobalCollect`]: super::collect::GlobalCollect
    /// [`drop_span`]: super::collect::GlobalCollect::drop_span
    /// [`new_span`]: super::collect::GlobalCollect::new_span
    /// [`try_close`]: Self::try_close
    #[inline]
    #[deprecated(since = "0.1.2", note = "use `Dispatch::try_close` instead")]
    pub fn drop_span(&self, id: span::GlobalId) {
        #[allow(deprecated)]
        self.collector().drop_span(id);
    }

    /// Notifies the collector that a [span ID] has been dropped, and returns
    /// `true` if there are now 0 IDs referring to that span.
    ///
    /// This function must only be called with span IDs that were returned by
    /// this `Dispatch`'s [`new_span`] function. The `tracing` crate upholds
    /// this guarantee and any other libraries implementing instrumentation APIs
    /// must as well.
    ///
    /// This calls the [`try_close`] function on the [`GlobalCollect`] trait
    /// that this `Dispatch` forwards to.
    ///
    /// [span ID]: super::span::GlobalId
    /// [`GlobalCollect`]: super::collect::GlobalCollect
    /// [`try_close`]: super::collect::GlobalCollect::try_close
    /// [`new_span`]: super::collect::GlobalCollect::new_span
    pub fn try_close(&self, id: span::GlobalId) -> bool {
        self.collector().try_close(id)
    }

    /// Returns a type representing this collector's view of the current span.
    ///
    /// This calls the [`current`] function on the [`GlobalCollect`] that this
    /// `Dispatch` forwards to.
    ///
    /// [`GlobalCollect`]: super::collect::GlobalCollect
    /// [`current`]: super::collect::GlobalCollect::current_span
    #[inline]
    pub fn current_span(&self) -> span::Current {
        self.collector().current_span()
    }

    /// Returns `true` if this `Dispatch` forwards to a collector of type
    /// `T`.
    #[inline]
    pub fn is<T: Any>(&self) -> bool {
        <dyn GlobalCollect>::is::<T>(&*self.collector())
    }

    /// Returns some reference to the [`GlobalCollect`] this `Dispatch` forwards to
    /// if it is of type `T`, or `None` if it isn't.
    ///
    /// [`GlobalCollect`]: super::collect::GlobalCollect
    #[inline]
    pub fn downcast_ref<T: Any>(&self) -> Option<&T> {
        <dyn GlobalCollect>::downcast_ref(&*self.collector())
    }
}

impl Default for Dispatch {
    /// Returns the current default dispatcher
    fn default() -> Self {
        get_default(|default| default.clone())
    }
}

impl<C: GlobalCollect + Send + Sync> PartialEq<C> for Dispatch {
    fn eq(&self, other: &C) -> bool {
        ptr::eq(self.collector(), other)
    }
}

impl fmt::Debug for Dispatch {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self.collector {
            #[cfg(feature = "alloc")]
            Kind::Global(collector) => f
                .debug_tuple("Dispatch::Global")
                .field(&format_args!("{:p}", collector))
                .finish(),

            #[cfg(feature = "alloc")]
            Kind::Scoped(collector) => f
                .debug_tuple("Dispatch::Scoped")
                .field(&format_args!("{:p}", collector))
                .finish(),

            #[cfg(not(feature = "alloc"))]
            collector => f
                .debug_tuple("Dispatch::Global")
                .field(&format_args!("{:p}", collector))
                .finish(),
        }
    }
}

impl PartialEq for Dispatch {
    fn eq(&self, other: &Self) -> bool {
        ptr::eq(&self.collector, &other.collector as *const _)
    }
}

impl Eq for Dispatch {}

impl Hash for Dispatch {
    fn hash<H: Hasher>(&self, state: &mut H) {
        ptr::hash(&self.collector, state)
    }
}

#[cfg(feature = "std")]
impl<C> From<C> for Dispatch
where
    C: GlobalCollect + Send + Sync + 'static,
{
    #[inline]
    fn from(collector: C) -> Self {
        Dispatch::new(collector)
    }
}

#[cfg(test)]
mod test {

    use super::*;
    use crate::{
        callsite::Callsite,
        collect::Interest,
        metadata::{Kind, Level, Metadata},
    };

    #[test]
    fn dispatch_is() {
        let dispatcher = Dispatch::from_static(&NO_COLLECTOR);
        assert!(dispatcher.is::<NoCollector>());
    }

    #[test]
    fn dispatch_downcasts() {
        let dispatcher = Dispatch::from_static(&NO_COLLECTOR);
        assert!(dispatcher.downcast_ref::<NoCollector>().is_some());
    }

    struct TestCallsite;
    static TEST_CALLSITE: TestCallsite = TestCallsite;
    static TEST_META: Metadata<'static> = metadata! {
        name: "test",
        target: module_path!(),
        level: Level::DEBUG,
        fields: &[],
        callsite: &TEST_CALLSITE,
        kind: Kind::EVENT
    };

    impl Callsite for TestCallsite {
        fn set_interest(&self, _: Interest) {}
        fn metadata(&self) -> &Metadata<'_> {
            &TEST_META
        }
    }

    #[test]
    #[cfg(feature = "std")]
    fn events_dont_infinite_loop() {
        // This test ensures that an event triggered within a collector
        // won't cause an infinite loop of events.
        struct TestCollector;
        impl Collect for TestCollector {
            fn enabled(&self, _: &Metadata<'_>) -> bool {
                true
            }

            fn new_span(&self, _: &span::Attributes<'_>) -> span::LocalId {
                span::LocalId::from_u64(0xAAAA)
            }

            fn record(&self, _: &span::LocalId, _: &span::Record<'_>) {}

            fn record_follows_from(&self, _: &span::LocalId, _: &span::LocalId) {}

            fn event(&self, _: &Event<'_>) {
                static EVENTS: AtomicUsize = AtomicUsize::new(0);
                assert_eq!(
                    EVENTS.fetch_add(1, Ordering::Relaxed),
                    0,
                    "event method called twice!"
                );
                Event::dispatch(&TEST_META, &TEST_META.fields().value_set(&[]))
            }

            fn enter(&self, _: &span::LocalId) {}

            fn exit(&self, _: &span::LocalId) {}

            fn current_span(&self) -> span::Current {
                span::Current::unknown()
            }
        }

        with_default(&Dispatch::new(TestCollector), || {
            Event::dispatch(&TEST_META, &TEST_META.fields().value_set(&[]))
        })
    }

    #[test]
    #[cfg(feature = "std")]
    fn spans_dont_infinite_loop() {
        // This test ensures that a span created within a collector
        // won't cause an infinite loop of new spans.

        fn mk_span() {
            get_default(|current| {
                current.new_span(&span::Attributes::new(
                    &TEST_META,
                    &TEST_META.fields().value_set(&[]),
                ))
            });
        }

        struct TestCollector;
        impl GlobalCollect for TestCollector {
            fn enabled(&self, _: &Metadata<'_>) -> bool {
                true
            }

            fn new_span(&self, _: &span::Attributes<'_>) -> span::GlobalId {
                static NEW_SPANS: AtomicUsize = AtomicUsize::new(0);
                assert_eq!(
                    NEW_SPANS.fetch_add(1, Ordering::Relaxed),
                    0,
                    "new_span method called twice!"
                );
                mk_span();
                span::GlobalId::from_u64(0xAAAA)
            }

            fn record(&self, _: &span::GlobalId, _: &span::Record<'_>) {}

            fn record_follows_from(&self, _: &span::GlobalId, _: &span::GlobalId) {}

            fn event(&self, _: &Event<'_>) {}

            fn enter(&self, _: &span::GlobalId) {}

            fn exit(&self, _: &span::GlobalId) {}

            fn current_span(&self) -> span::Current {
                span::Current::unknown()
            }
        }

        with_default(&Dispatch::new(TestCollector), mk_span)
    }

    #[test]
    fn default_no_collector() {
        let default_dispatcher = Dispatch::default();
        assert!(default_dispatcher.is::<NoCollector>());
    }
}
