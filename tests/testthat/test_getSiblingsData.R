context("Test sibling data")

tree <- GeneralTree$new(0, "parent1")
tree$addNode(0, "a", "child.a")
tree$addNode(0, "b", "child.b")
tree$addNode("b", "c", "child.b.c")
tree$addNode("b", "d", "child.b.d")
tree$addNode("c", "e", "child.c.e")
tree$addNode("c", "f", "child.c.f")
tree$addNode("c", "g", "child.c.g")

test_that("Verify correct sibling data is returned with relative getter", {
  expect_identical(tree$searchNode("e")$getSiblingsInfo("data"),
                   list("child.c.f", "child.c.g"))
  expect_identical(tree$searchNode("e")$getSiblingsInfo("key"),
                   list("f", "g"))
  expect_identical(tree$searchNode("c")$getSiblingsInfo(what = "data"),
                   list("child.b.d"))
  expect_identical(tree$searchNode("c")$getSiblingsInfo(what = "key"),
                   list("d"))
})

test_that("Verify correct sibling data is returned with absolute getter", {
  expect_identical(tree$getSiblingsInfoByKey("e", "data"),
                   list("child.c.f", "child.c.g"))
  expect_identical(tree$getSiblingsInfoByKey("e", "key"),
                   list("f", "g"))
  expect_identical(tree$getSiblingsInfoByKey("c", what = "data"),
                   list("child.b.d"))
  expect_identical(tree$getSiblingsInfoByKey("c", what = "key"),
                   list("d"))
})

test_that("Verify deprecated functions work but yield warnings", {
  expect_warning(data <- tree$searchNode("e")$getSiblingData())
  expect_warning(keys <- tree$searchNode("e")$getSiblingId())
  expect_identical(data, list("child.c.f", "child.c.g"))
  expect_identical(keys, list("f", "g"))
})
