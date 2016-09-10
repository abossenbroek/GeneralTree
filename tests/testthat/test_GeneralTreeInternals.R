context("Test GeneralTreeInternals to C++ to R interface")

test_that("XPtr is correctly passed", {
    init <- GeneralTree:::initialize_tree(0, 0)
    passed <- GeneralTree:::pass_gti_xptr(init)

    expect_true(cmp(init, passed));
})
