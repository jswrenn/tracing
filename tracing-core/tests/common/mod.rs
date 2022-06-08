use tracing_core::{collect::Collect, metadata::Metadata, span, Event};

pub struct TestCollectorA;
impl Collect for TestCollectorA {
    fn enabled(&self, _: &Metadata<'_>) -> bool {
        true
    }
    fn new_span(&self, _: &span::Attributes<'_>) -> span::LocalId {
        span::LocalId::from_u64(1)
    }
    fn record(&self, _: &span::LocalId, _: &span::Record<'_>) {}
    fn record_follows_from(&self, _: &span::LocalId, _: &span::LocalId) {}
    fn event(&self, _: &Event<'_>) {}
    fn enter(&self, _: &span::LocalId) {}
    fn exit(&self, _: &span::LocalId) {}
    fn current_span(&self) -> span::Current {
        span::Current::unknown()
    }
}
pub struct TestCollectorB;
impl Collect for TestCollectorB {
    fn enabled(&self, _: &Metadata<'_>) -> bool {
        true
    }
    fn new_span(&self, _: &span::Attributes<'_>) -> span::LocalId {
        span::LocalId::from_u64(1)
    }
    fn record(&self, _: &span::LocalId, _: &span::Record<'_>) {}
    fn record_follows_from(&self, _: &span::LocalId, _: &span::LocalId) {}
    fn event(&self, _: &Event<'_>) {}
    fn enter(&self, _: &span::LocalId) {}
    fn exit(&self, _: &span::LocalId) {}
    fn current_span(&self) -> span::Current {
        span::Current::unknown()
    }
}
