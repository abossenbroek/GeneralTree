context("Cloning trees")

test_that("Cloning an singleton tree works", {
  tree <- GeneralTree$new(0, "parent1")
  copied_tree <- deep_clone(tree)

  expect_identical(copied_tree$id, tree$id)
  expect_identical(copied_tree$data, tree$data)
  expect_false(identical(copied_tree, tree))
})


test_that("Cloning a tree with one child", {
  tree <- GeneralTree$new(0, "parent1")
  tree$addNode(0, 1, "child1")

  copied_tree <- deep_clone(tree)

  expect_equal(copied_tree$searchNode(1)$parent, tree$searchNode(1)$parent)

  expect_false(identical(copied_tree, tree))
})


test_that("Cloning a more complex tree", {
  tree <- GeneralTree$new(0, "parent1")
  tree$addNode(0, 1, "child1")

  copied_tree <- deep_clone(tree)

  expect_equal(copied_tree$searchNode(1)$parent, tree$searchNode(1)$parent)

  expect_false(identical(copied_tree, tree))
})


test_that("Cloning a branch works", {

  tree <- GeneralTree$new(0, "parent1")
  tree$addNode(0, "a", "child.a")
  tree$addNode(0, "b", "child.b")
  tree$addNode("b", "c", "child.b.c")
  tree$addNode("b", "d", "child.b.d")
  tree$addNode("c", "e", "child.c.e")
  tree$addNode("c", "f", "child.c.e")

  branch_copy <- deep_clone(tree$searchNode("b"))

  expect_equal(tree$searchData("b"), branch_copy$data)
})

test_that("Assignment operator works", {

  tree <- GeneralTree$new(0, "parent1")
  tree$addNode(0, "a", "child.a")
  tree$addNode(0, "b", "child.b")
  tree$addNode("b", "c", "child.b.c")
  tree$addNode("b", "d", "child.b.d")
  tree$addNode("c", "e", "child.c.e")
  tree$addNode("c", "f", "child.c.e")

  branch_copy <- tree$searchNode("b")
  branch_copy_assignment = tree$searchNode("b")

  expect_equal(tree$searchData("b"), branch_copy$data)
  expect_equal(tree$searchData("b"), branch_copy_assignment$data)
})

test_that("Assignment operator works by calling as function", {

  tree <- GeneralTree$new(0, "parent1")
  tree$addNode(0, "a", "child.a")
  tree$addNode(0, "b", "child.b")
  tree$addNode("b", "c", "child.b.c")
  tree$addNode("b", "d", "child.b.d")
  tree$addNode("c", "e", "child.c.e")
  tree$addNode("c", "f", "child.c.e")

  branch_copy <- NULL
  branch_copy <- `<-.GeneralTree`(branch_copy, tree$searchNode("b"))
  branch_copy_assignment <- `=.GeneralTree`(branch_copy_assignment,
                                             tree$searchNode("b"))

  expect_equal(tree$searchData("b"), branch_copy$data)
  expect_equal(tree$searchData("b"), branch_copy_assignment$data)
})


