context("Adding nodes")
test_that("creating an empty tree works", {
  require(GeneralTree)

  root <- GeneralTree$new(0, "parent1")

  expect_is(root, "GeneralTree")
  expect_identical(root$left_child, NULL)
  expect_identical(root$sibling, NULL)
})

test_that("add child to empty tree works", {
 require(GeneralTree)

 root <- GeneralTree$new(0, "parent1")
 child1 <- root$add_node(0, 1, "child1")

 expect_is(child1, "GeneralTree")
 expect_identical(root$left_child, child1)
 expect_identical(child1$root, root)
})

test_that("adding a child with an non existing parent results in failure", {
  root <- GeneralTree$new(0, "parent1")
  expect_error(root$add_node(2, 1, "child1"))
})

test_that("have_siblign action functions is working as expected", {
  nodes <- list()

  nodes[["root"]] <- GeneralTree$new(0, "parent1")

  expect_identical(nodes[["root"]]$have_siblings, FALSE)
  expect_identical(nodes[["root"]]$have_child, FALSE)

  nodes[["child1"]] <- nodes[["root"]]$add_node(0, 1, "child1")

  expect_identical(nodes[["root"]]$have_child, TRUE)
})

