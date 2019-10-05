#[macro_use]
extern crate criterion;
extern crate zia;

use criterion::{BenchmarkId, Criterion};
use zia::{Context, ContextMaker};

fn let_reduction(context: &mut Context, size: u8) {
    let mut i = 0u8;
    let mut j = 0u8;
    while j < size {
        if let '(' | ')' | ' ' | '\n' | '\r' = i as char {
            ();
        } else if let '(' | ')' | ' ' | '\n' | '\r' = (i + 1) as char {
            ();
        } else {
            context.execute(&format!("let {} -> {}", i as char, (i + 1) as char));
            j += 1;
        }
        i += 1;
    }
}

fn bench_let_reduction(c: &mut Criterion) {
    let mut let_group = c.benchmark_group("let_reduction");
    let_group.sample_size(40);
    for size in [1, 2, 4, 8, 16, 32, 64].iter() {
        let mut context = Context::new();
        let_group.bench_with_input(BenchmarkId::from_parameter(size), size, |b, &size| {
            b.iter(|| let_reduction(&mut context, size))
        });
    }
    let_group.finish();
}

criterion_group!(benches, bench_let_reduction);
criterion_main!(benches);
