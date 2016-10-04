context("Test leafs information getters")

tree <- GeneralTree$new(0, "parent1")
tree$addNode(0, "a", "child.a")
tree$addNode(0, "b", "child.b")
tree$addNode("b", "c", "child.b.c")
tree$addNode("b", "d", "child.b.d")
tree$addNode("c", "e", "child.c.e")
tree$addNode("c", "f", "child.c.f")
tree$addNode("c", "g", "child.c.g")

test_that("Verify correct leafs data is returned with relative getter", {
  expect_identical(tree$searchNode("e")$getLeafsInfo(what = "data"),
                   list("child.c.e"))
  expect_identical(tree$searchNode("e")$getLeafsInfo(what = "key"),
                   list("e"))
  expect_identical(tree$searchNode("b")$getLeafsInfo("data"),
                   list("child.c.e", "child.c.f", "child.c.g", "child.b.d"))
  expect_identical(tree$searchNode("b")$getLeafsInfo("key"),
                   list("e", "f", "g", "d"))
})

test_that("Verify correct leafs data is returned with absolute getter", {
  expect_identical(tree$getLeafsInfoByKey("e", what = "data"), list("child.c.e"))
  expect_identical(tree$getLeafsInfoByKey("e", what = "key"), list("e"))
  expect_identical(tree$getLeafsInfoByKey("b", "data"),
                   list("child.c.e", "child.c.f", "child.c.g", "child.b.d"))
  expect_identical(tree$getLeafsInfoByKey("b", "key"),
                   list("e", "f", "g", "d"))
})

