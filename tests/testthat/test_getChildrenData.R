context("Test children data")

tree <- GeneralTree$new(0, "parent1")
tree$addNode(0, "a", "child.a")
tree$addNode(0, "b", "child.b")
tree$addNode("b", "c", "child.b.c")
tree$addNode("b", "d", "child.b.d")
tree$addNode("c", "e", "child.c.e")
tree$addNode("c", "f", "child.c.f")
tree$addNode("c", "g", "child.c.g")


test_that("Verify correct child data is returned with relative getter", {
  expect_identical(tree$searchNode("b")$getChildrenData(),
                   list("child.b.c", "child.b.d"))
  expect_identical(tree$searchNode("c")$getChildrenData(),
                   list("child.c.e", "child.c.f", "child.c.g"))
  expect_identical(tree$searchNode("c")$getChildrenKeys(),
                   list("e", "f", "g"))
  expect_identical(tree$searchNode("b")$getChildrenKeys(TRUE),
                   list("c", "e", "f", "g", "d"))
  expect_identical(tree$searchNode("b")$getChildrenKeys(recursive = TRUE),
                   list("c", "e", "f", "g", "d"))
  expect_identical(tree$searchNode("b")$getChildrenData(TRUE),
                   list("child.b.c", "child.c.e", "child.c.f",
                        "child.c.g", "child.b.d"))
  expect_identical(tree$searchNode("b")$getChildrenData(recursive = TRUE),
                   list("child.b.c", "child.c.e", "child.c.f",
                        "child.c.g", "child.b.d"))
  expect_identical(tree$searchNode("g")$getChildrenDataByKey(recursive = TRUE),
                   list())
})

test_that("Verify correct child data is returned with absolute getter", {
  expect_identical(tree$getChildrenDataByKey(key = "b"),
                   list("child.b.c", "child.b.d"))
  expect_identical(tree$getChildrenDataByKey(key = "c"),
                   list("child.c.e", "child.c.f", "child.c.g"))
  expect_identical(tree$getChildrenKeysByKey(key = "c"),
                   list("e", "f", "g"))
  expect_identical(tree$getChildrenKeysByKey("b", TRUE),
                   list("c", "e", "f", "g", "d"))
  expect_identical(tree$getChildrenKeysByKey(key = "b", recursive = TRUE),
                   list("c", "e", "f", "g", "d"))
  expect_identical(tree$getChildrenDataByKey("b", TRUE),
                   list("child.b.c", "child.c.e", "child.c.f",
                        "child.c.g", "child.b.d"))
  expect_identical(tree$getChildrenDataByKey(key = "b", recursive = TRUE),
                   list("child.b.c", "child.c.e", "child.c.f",
                        "child.c.g", "child.b.d"))
  expect_identical(tree$getChildrenDataByKey(key = "g", recursive = TRUE),
                   list())
})

test_that("Verify deprecated functions work but yield warnings", {
  expect_warning(data <- tree$searchNode("b")$getChildData())
  expect_warning(keys <- tree$searchNode("c")$getChildId())
  expect_identical(data, list("child.b.c", "child.b.d"))
  expect_identical(keys, list("e", "f", "g"))
})

test_that("Verify correct error is returned", {
  expect_error(tree$getChildrenDataByKey(key = 10),
                   ".*not find key in tree.$")
})


