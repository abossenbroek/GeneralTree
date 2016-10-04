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
  expect_identical(tree$searchNode("b")$getChildrenInfo("data"),
                   list("child.b.c", "child.b.d"))
  expect_identical(tree$searchNode("c")$getChildrenInfo("data"),
                   list("child.c.e", "child.c.f", "child.c.g"))
  expect_identical(tree$searchNode("c")$getChildrenInfo("key"),
                   list("e", "f", "g"))
  expect_identical(tree$searchNode("b")$getChildrenInfo("key", TRUE),
                   list("c", "e", "f", "g", "d"))
  expect_identical(tree$searchNode("b")$getChildrenInfo(what = "key",
                                                        recursive = TRUE),
                   list("c", "e", "f", "g", "d"))
  expect_identical(tree$searchNode("b")$getChildrenInfo("data", TRUE),
                   list("child.b.c", "child.c.e", "child.c.f",
                        "child.c.g", "child.b.d"))
  expect_identical(tree$searchNode("b")$getChildrenInfo(recursive = TRUE,
                                                        what = "data"),
                   list("child.b.c", "child.c.e", "child.c.f",
                        "child.c.g", "child.b.d"))
  expect_identical(tree$searchNode("g")$getChildrenInfoByKey(what = "key", 
                                                             recursive = TRUE),
                   list())
})

test_that("Verify correct child data is returned with absolute getter", {
  expect_identical(tree$getChildrenInfoByKey(key = "b", what = "data"),
                   list("child.b.c", "child.b.d"))
  expect_identical(tree$getChildrenInfoByKey(key = "c", "data"),
                   list("child.c.e", "child.c.f", "child.c.g"))
  expect_identical(tree$getChildrenInfoByKey(key = "c", "key"),
                   list("e", "f", "g"))
  expect_identical(tree$getChildrenInfoByKey("b", what = "key", recursive = TRUE),
                   list("c", "e", "f", "g", "d"))
  expect_identical(tree$getChildrenInfoByKey(key = "b", recursive = TRUE,
                                             "key"),
                   list("c", "e", "f", "g", "d"))
  expect_identical(tree$getChildrenInfoByKey("b", recursive = TRUE, what = "data"),
                   list("child.b.c", "child.c.e", "child.c.f",
                        "child.c.g", "child.b.d"))
  expect_identical(tree$getChildrenInfoByKey(key = "b", recursive = TRUE,
                                             "data"),
                   list("child.b.c", "child.c.e", "child.c.f",
                        "child.c.g", "child.b.d"))
  expect_identical(tree$getChildrenInfoByKey(key = "g", recursive = TRUE,
                                             "data"),
                   list())
})

test_that("Verify deprecated functions work but yield warnings", {
  expect_warning(data <- tree$searchNode("b")$getChildData())
  expect_warning(keys <- tree$searchNode("c")$getChildId())
  expect_identical(data, list("child.b.c", "child.b.d"))
  expect_identical(keys, list("e", "f", "g"))
})

test_that("Verify correct error is returned", {
  expect_error(tree$getChildrenInfoByKey(key = 10, what = "key"),
                   ".*not find key in tree.$")
})


