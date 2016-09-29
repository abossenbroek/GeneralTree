context("Adding nodes")
test_that("creating an empty tree works", {
  tree <- GeneralTree$new(0, "parent1")

  expect_is(tree, "GeneralTree")
  expect_identical(tree$left_child, NULL)
  expect_identical(tree$sibling, NULL)
})

test_that("add child to empty tree works", {
 tree <- GeneralTree$new(0, "parent1")
 child1 <- tree$addNode(0, 1, "child1")

 expect_is(child1, "GeneralTree")
 expect_true(child1$root == tree)
})

test_that("add child with non existing parent gives correct error", {
 root <- GeneralTree$new(0, "parent1")
 expect_error(root$addNode(1, 1, "child1"), "^.*not find parent in tree.*$")
})


test_that("adding a child with an non existing parent results in failure", {
  root <- GeneralTree$new(0, "parent1")
  expect_error(root$addNode(2, 1, "child1"))
})

test_that("add multiple childeren", {
  tree <- GeneralTree$new(0, "parent1")
  tree$addNode(0, 1, "child1")
  tree$addNode(0, 2, "child2")

  tree$addNode(2, 3, "child2.3")

  tree$searchNode(3)$data

  expect_identical(tree$searchNode(3)$data, "child2.3")
})

test_that("add multiple childeren as part of child", {
  tree <- GeneralTree$new(0, "parent1")
  tree$addNode(0, 1, "child1")
  tree$addNode(0, 2, "child2")
  tree$addNode(2, 3, "child2.3")
  tree$addNode(2, 4, "child2.4")
  tree$addNode(3, 5, "child3.5")

  expect_identical(tree$searchNode(5)$data, "child3.5")
  expect_identical(tree$searchNode(5)$data, "child3.5")
})

test_that("add multiple childeren as part of child with char id", {

  tree <- GeneralTree$new(0, "parent1")
  tree$addNode(0, "a", "child.a")
  tree$addNode(0, "b", "child.b")

  tree$addNode("b", "c", "child.b.c")

  tree$addNode("b", "d", "child.b.d")


  tree$addNode("c", "e", "child.c.e")
  tree$addNode("c", "f", "child.c.e")

  expect_identical(tree$searchNode("e")$data, "child.c.e")
})

test_that("child by using addChild on search result.", {
  tree <- GeneralTree$new(0, "parent1")
  tree$addChild("a", "child.a")
  tree$addChild("b", "child.b")
  tree$searchNode("a")$addChild("c", "child.b.c")
  tree$searchNode("b")$addChild("d", "child.b.d")

  tree$addNode("c", "e", "child.c.e")
  tree$addNode("c", "f", "child.c.f")

  expect_identical(tree$searchNode("e")$data, "child.c.e")
})

test_that("test relative insertion", {

  tree <- GeneralTree$new(0, "parent1")
  expect_identical(unlist(tree$getBranchKeys()), 0)
  tree$searchNode(0)$addChild("a", "child.a")
  expect_identical(unlist(tree$getBranchKeys()), c(0, "a"))
  tree$searchNode(0)$addChild("b", "child.b")
  expect_identical(unlist(tree$getBranchKeys()), c(0, "a", "b"))
  tree$searchNode("b")$addChild("c", "child.b.c")
  expect_identical(unlist(tree$getBranchKeys()), c(0, "a", "b", "c"))
  tree$searchNode("b")$addChild("d", "child.b.d")
  expect_identical(unlist(tree$getBranchKeys()), c(0, "a", "b", "c", "d"))
  tree$addNode("c", "e", "child.c.e")
  expect_identical(unlist(tree$getBranchKeys()), c(0, "a", "b", "c", "e", "d"))
  tree$addNode("c", "f", "child.c.e")
  expect_identical(unlist(tree$getBranchKeys()), c(0, "a", "b", "c", "e",
                                                   "f", "d"))
  tree$searchNode("b")$addSibling("g", "child.g")
  expect_identical(unlist(tree$getBranchKeys()), c(0, "a", "b", "c", "e",
                                                   "f", "d", "g"))

  expect_identical(tree$searchNode("g")$parent$key, 0)
})


