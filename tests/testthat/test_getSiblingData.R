context("Test sibling data")

test_that("Verify correct sibling data is returned", {

  tree <- GeneralTree$new(0, "parent1")
  tree$addNode(0, "a", "child.a")
  tree$addNode(0, "b", "child.b")
  tree$addNode("b", "c", "child.b.c")
  tree$addNode("b", "d", "child.b.d")
  tree$addNode("c", "e", "child.c.e")
  tree$addNode("c", "f", "child.c.f")
  tree$addNode("c", "g", "child.c.g")

  expect_identical(tree$searchNode("e")$getSiblingData(),
                   list("child.c.f", "child.c.g"))
  expect_identical(tree$searchNode("e")$getSiblingId(),
                   list("f", "g"))
  expect_identical(tree$searchNode("c")$getSiblingData(),
                   list("child.b.d"))
  expect_identical(tree$searchNode("c")$getSiblingId(),
                   list("d"))
})
