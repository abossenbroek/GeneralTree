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

    expect_equal(GeneralTree:::get_children_keys(init, "a"), list("b", "c"));
})

test_that("Child keys are correclty returned with mixed keys", {
    init <- GeneralTree:::initialize_tree("a", list(0))
    init <- GeneralTree:::add_node(init, "a", 0, list(1))
    init <- GeneralTree:::add_node(init, "a", "b", list(2))
    init <- GeneralTree:::add_node(init, "a", 1.1, list(2))

    expect_equal(GeneralTree:::get_children_keys(init, "a"), list(0, "b", 1.1));
})

test_that("Child keys are correclty returned with mixed keys", {
    init <- GeneralTree:::initialize_tree("a", list(0))
    init <- GeneralTree:::add_node(init, "a", 0, list(1))
    init <- GeneralTree:::add_node(init, "a", "b", list(2))
    init <- GeneralTree:::add_node(init, "a", 1.1, list(2))

    expect_equal(GeneralTree:::get_siblings_keys(init, 0), list("b", 1.1));
})

test_that("Child keys are correclty returned with mixed keys", {
    node_values <- list(list("a"), new.env(), 0.1, "les cochons volant sont éblouissant")
    init <- GeneralTree:::initialize_tree("a", node_values[[1]])
    init <- GeneralTree:::add_node(init, "a", 0, node_values[[2]])
    init <- GeneralTree:::add_node(init, "a", "b", node_values[[3]])
    init <- GeneralTree:::add_node(init, "a", 1.1, node_values[[4]])

    expect_equal(GeneralTree:::get_children_values(init, "a"), node_values[-1])
})

test_that("Child keys are correclty returned with mixed keys", {
    node_values <- list(list("a"), new.env(), 0.1, "les cochons volant sont éblouissant")
    init <- GeneralTree:::initialize_tree("a", node_values[[1]])
    init <- GeneralTree:::add_node(init, "a", 0, node_values[[2]])
    init <- GeneralTree:::add_node(init, "a", "b", node_values[[3]])
    init <- GeneralTree:::add_node(init, "a", 1.1, node_values[[4]])

    expect_equal(GeneralTree:::get_siblings_values(init, 1.1),
                 node_values[-c(1, 4)])
})

