#[test]
fn test() {
    let t = trybuild::TestCases::new();
    t.pass("tests/ui/declare-type-union.rs");
    t.pass("tests/ui/display.rs");
    t.pass("tests/ui/eq.rs");
    t.pass("tests/ui/iterator.rs");
    t.pass("tests/ui/iterator-type-union-item.rs");
    t.pass("tests/ui/iterator-default-item.rs");
    t.pass("tests/ui/hrt-bound.rs");
    // t.pass("tests/ui/impl-non-type-union.rs");
    // t.pass("tests/ui/partial-eq-type-union.rs");
    // t.compile_fail("tests/ui/compile-fail-3.rs");
}
