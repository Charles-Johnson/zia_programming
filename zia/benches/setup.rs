#[macro_use]
extern crate criterion;
extern crate zia;

use criterion::Criterion;
use zia::{Context, ContextMaker};

fn bench_setup(c: &mut Criterion) {
    c.bench_function("setup", |b| b.iter(|| Context::new()));
}

criterion_group!(benches, bench_setup);
criterion_main!(benches);
