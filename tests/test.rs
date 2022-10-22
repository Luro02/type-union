#[test]
fn test() {
    let t = trybuild::TestCases::new();
    t.pass("tests/ui/declare-type-union.rs");
    // t.compile_fail("tests/ui/compile-fail-3.rs");
}
