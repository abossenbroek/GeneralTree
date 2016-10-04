context("Test GeneralTreeInternals to C++ to R interface")

test_that("XPtr is correctly passed", {
    init <- initialize_tree("a", 0)
    passed <- pass_gti_xptr(init)

    expect_true(cmp_gti(init, passed));
})

test_that("Stored integer value is correcty found", {
    init <- initialize_tree("a", 0)
    init <- add_node(init, "a", "b", 1)
    init <- add_node(init, "a", "c", 2)

    expect_true(get_data(init, "c") == 2);
    expect_true(get_data(init, "b") == 1);
})

test_that("Stored list value is correcty found", {
    init <- initialize_tree("a", list(0))
    init <- add_node(init, "a", "b", list(1))
    init <- add_node(init, "a", "c", list(2))

    expect_equal(get_data(init, "c"), list(2));
    expect_equal(get_data(init, "b"), list(1));
})

test_that("Tree is correctly copied", {
    node_values <- list(list("a"), new.env(), 0.1, "les cochons volant sont éblouissant")
    init <- initialize_tree("a", node_values[[1]])
    init <- add_node(init, "a", 0, node_values[[2]])
    init <- add_node(init, "a", "b", node_values[[3]])
    init <- add_node(init, "a", 1.1, node_values[[4]])

    init_copy <- copy(init, find_uid(init, "a"))

    expect_true(cmp_gti(init, init_copy))
})

test_that("Tree is correctly (de/se)rialized", {
    node_values <- list(list("a"), new.env(), 0.1, "les cochons volant sont éblouissant")
    init <- initialize_tree("a", node_values[[1]])
    init <- add_node(init, "a", 0, node_values[[2]])
    init <- add_node(init, "a", "b", node_values[[3]])
    init <- add_node(init, "a", 1.1, node_values[[4]])

    object <- serialize(init)
    copy <- deserialize_tree(object)

    expect_true(cmp_gti(init, copy))
})

test_that("Relative insertion works as expected", {
    init <- initialize_tree(letters[1], letters[1])
    init <- add_child(init, letters[2], letters[2])
    init <- add_sibling(init, letters[3], letters[3])
    init <- add_child(init, letters[4], letters[4])
    init <- add_sibling(init, letters[5], letters[5])
    init <- travel_up(init)
    init <- add_sibling(init, letters[6], letters[6])

    res <- sapply(get_branch(init, letters[1]), function(x) x$key)

    expect_equal(res, letters[1 : 6])
})

test_that("Branch apply works", {
    init <- initialize_tree(letters[1], letters[1])
    init <- add_child(init, letters[2], letters[2])
    init <- add_sibling(init, letters[3], letters[3])
    init <- add_child(init, letters[4], letters[4])
    init <- add_sibling(init, letters[5], letters[5])
    init <- travel_up(init)
    init <- add_sibling(init, letters[6], letters[6])
    init <- travel_up(init)

    res <- apply_on_branch(init, function(a){ a$key })
    res2 <- apply_on_branch_at_ref(init, letters[1],
                                                 function(a) {
                                                   a$key
                                                 })

    parents_above <- apply_on_branch_at_ref(init, letters[1],
                                           function(a) {
                                             a$parents_above
                                           })
    last_sibling <- apply_on_branch_at_ref(init, letters[1],
                                                 function(a) {
                                                   a$is_last_sibling
                                                 })



    expect_equal(unlist(res), letters[1 : 6])
    expect_equal(res, res2)
    expect_equal(c(0, 1, 1, 2, 2, 1), unlist(parents_above))
    expect_equal(c(F, F, F, F, T, T), unlist(last_sibling))
})

