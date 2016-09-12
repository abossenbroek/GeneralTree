context("Test GeneralTreeInternals to C++ to R interface")

test_that("XPtr is correctly passed", {
    init <- GeneralTree:::initialize_tree("a", 0)
    passed <- GeneralTree:::pass_gti_xptr(init)

    expect_true(cmp(init, passed));
})

test_that("Stored integer value is correcty found", {
    init <- GeneralTree:::initialize_tree("a", 0)
    init <- GeneralTree:::add_node(init, "a", "b", 1)
    init <- GeneralTree:::add_node(init, "a", "c", 2)

    expect_true(GeneralTree:::get_value(init, "c") == 2);
    expect_true(GeneralTree:::get_value(init, "b") == 1);
})

test_that("Stored list value is correcty found", {
    init <- GeneralTree:::initialize_tree("a", list(0))
    init <- GeneralTree:::add_node(init, "a", "b", list(1))
    init <- GeneralTree:::add_node(init, "a", "c", list(2))

    expect_equal(GeneralTree:::get_value(init, "c"), list(2));
    expect_equal(GeneralTree:::get_value(init, "b"), list(1));
})

test_that("Child keys are correclty returned", {
    init <- GeneralTree:::initialize_tree("a", list(0))
    init <- GeneralTree:::add_node(init, "a", "b", list(1))
    init <- GeneralTree:::add_node(init, "a", "c", list(2))

    expect_equal(GeneralTree:::get_childeren_keys(init, "a"), list("b", "c"));
})
