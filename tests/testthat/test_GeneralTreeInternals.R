context("Test GeneralTreeInternals to C++ to R interface")

test_that("XPtr is correctly passed", {
    init <- GeneralTree:::initialize_tree("a", 0)
    passed <- GeneralTree:::pass_gti_xptr(init)

    expect_true(cmp_gti(init, passed));
})

test_that("Stored integer value is correcty found", {
    init <- GeneralTree:::initialize_tree("a", 0)
    init <- GeneralTree:::add_node(init, "a", "b", 1)
    init <- GeneralTree:::add_node(init, "a", "c", 2)

    expect_true(GeneralTree:::get_data(init, "c") == 2);
    expect_true(GeneralTree:::get_data(init, "b") == 1);
})

test_that("Stored list value is correcty found", {
    init <- GeneralTree:::initialize_tree("a", list(0))
    init <- GeneralTree:::add_node(init, "a", "b", list(1))
    init <- GeneralTree:::add_node(init, "a", "c", list(2))

    expect_equal(GeneralTree:::get_data(init, "c"), list(2));
    expect_equal(GeneralTree:::get_data(init, "b"), list(1));
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

 test_that("Siblings keys are correclty returned with mixed keys", {
     init <- GeneralTree:::initialize_tree("a", list(0))
     init <- GeneralTree:::add_node(init, "a", 0, list(1))
     init <- GeneralTree:::add_node(init, "a", "b", list(2))
     init <- GeneralTree:::add_node(init, "a", 1.1, list(2))

     expect_equal(GeneralTree:::get_siblings_keys(init, 0), list("b", 1.1));
 })

test_that("Child data are correclty returned with mixed keys", {
    node_values <- list(list("a"), new.env(), 0.1, "les cochons volant sont éblouissant")
    init <- GeneralTree:::initialize_tree("a", node_values[[1]])
    init <- GeneralTree:::add_node(init, "a", 0, node_values[[2]])
    init <- GeneralTree:::add_node(init, "a", "b", node_values[[3]])
    init <- GeneralTree:::add_node(init, "a", 1.1, node_values[[4]])

    expect_equal(GeneralTree:::get_children_data(init, "a"), node_values[-1])
})

test_that("Siblings data are correclty returned with mixed keys", {
    node_values <- list(list("a"), new.env(), 0.1, "les cochons volant sont éblouissant")
    init <- GeneralTree:::initialize_tree("a", node_values[[1]])
    init <- GeneralTree:::add_node(init, "a", 0, node_values[[2]])
    init <- GeneralTree:::add_node(init, "a", "b", node_values[[3]])
    init <- GeneralTree:::add_node(init, "a", 1.1, node_values[[4]])

    expect_equal(GeneralTree:::get_siblings_data(init, "b"), node_values[-c(1, 3)])
})


test_that("Leafs keys and data are correclty returned with mixed keys", {
    node_values <- list(list("a"), new.env(), 0.1, "les cochons volant sont éblouissant",
                        "blizzard")
    init <- GeneralTree:::initialize_tree("a", node_values[[1]])
    init <- GeneralTree:::add_node(init, "a", 0, node_values[[2]])
    init <- GeneralTree:::add_node(init, "a", "b", node_values[[3]])
    init <- GeneralTree:::add_node(init, "a", 1.1, node_values[[4]])

    expect_equal(GeneralTree:::get_leafs_data(init, "a"), node_values[-c(1, 5)])
    expect_equal(GeneralTree:::get_leafs_keys(init, "a"), list(0, "b", 1.1))


    init <- GeneralTree:::add_node(init, "b", 1.2, node_values[[5]])

    expect_equal(GeneralTree:::get_leafs_data(init, "a"), node_values[c(2, 5, 4)])
})

test_that("Tree is correctly copied", {
    node_values <- list(list("a"), new.env(), 0.1, "les cochons volant sont éblouissant")
    init <- GeneralTree:::initialize_tree("a", node_values[[1]])
    init <- GeneralTree:::add_node(init, "a", 0, node_values[[2]])
    init <- GeneralTree:::add_node(init, "a", "b", node_values[[3]])
    init <- GeneralTree:::add_node(init, "a", 1.1, node_values[[4]])

    init_copy <- GeneralTree:::copy(init);

    expect_true(GeneralTree:::cmp_gti(init, init_copy))
})

test_that("Tree is correctly (de/se)rialized", {
    node_values <- list(list("a"), new.env(), 0.1, "les cochons volant sont éblouissant")
    init <- GeneralTree:::initialize_tree("a", node_values[[1]])
    init <- GeneralTree:::add_node(init, "a", 0, node_values[[2]])
    init <- GeneralTree:::add_node(init, "a", "b", node_values[[3]])
    init <- GeneralTree:::add_node(init, "a", 1.1, node_values[[4]])

    object <- GeneralTree:::serialize(init)
    copy <- GeneralTree:::deserialize_tree(object)

    expect_true(GeneralTree:::cmp_gti(init, copy))
})

test_that("Relative insertion works as expected", {
    init <- GeneralTree:::initialize_tree(letters[1], letters[1])
    init <- GeneralTree:::add_child(init, letters[2], letters[2])
    init <- GeneralTree:::add_sibling(init, letters[3], letters[3])
    init <- GeneralTree:::add_child(init, letters[4], letters[4])
    init <- GeneralTree:::add_sibling(init, letters[5], letters[5])
    init <- GeneralTree:::travel_up(init)
    init <- GeneralTree:::add_sibling(init, letters[6], letters[6])

    expect_equal(unlist(GeneralTree:::get_branch_keys(init, letters[1])), letters[1 : 6])
})

