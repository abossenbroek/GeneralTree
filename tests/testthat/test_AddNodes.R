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
 expect_identical(tree$left_child, child1)
 expect_identical(child1$root, tree)
})

test_that("add child with non existing parent gives correct error", {
 root <- GeneralTree$new(0, "parent1")
 expect_error(root$addNode(1, 1, "child1"), "^.*id 1$")
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

test_that("multiple childeren will yield error", {
  tree <- GeneralTree$new(0, "parent1")
  tree$addNode(0, 1, "child.0.1")

  expect_warning(tree$setLeftChild(GeneralTree$new(1, "child.0.1")))
})

test_that("child by using addChild on search result.", {

  tree <- GeneralTree$new(0, "parent1")
  tree$searchNode(0)$addChild("a", "child.a")
  tree$searchNode(0)$addChild("b", "child.b")
  tree$searchNode("b")$addChild("c", "child.b.c")
  tree$searchNode("b")$addChild("d", "child.b.d")
  tree$addNode("c", "e", "child.c.e")
  tree$addNode("c", "f", "child.c.e")

  expect_identical(tree$searchNode("e")$data, "child.c.e")
})

test_that("child by using addChild on search result.", {

  tree <- GeneralTree$new(0, "parent1")
  tree$searchNode(0)$addChild("a", "child.a")
  tree$searchNode(0)$addChild("b", "child.b")
  tree$searchNode("b")$addChild("c", "child.b.c")
  tree$searchNode("b")$addChild("d", "child.b.d")
  tree$addNode("c", "e", "child.c.e")
  tree$addNode("c", "f", "child.c.e")
  tree$searchNode("b")$addSibling("g", "child.g")

  expect_identical(tree$searchNode("g")$parent$id, 0)
})


