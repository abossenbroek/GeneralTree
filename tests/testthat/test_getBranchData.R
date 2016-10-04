context("Test branch data")

tree <- GeneralTree$new(0, "parent1")
tree$addNode(0, "a", "child.a")
tree$addNode(0, "b", "child.b")
tree$addNode("b", "c", "child.b.c")
tree$addNode("b", "d", "child.b.d")
tree$addNode("c", "e", "child.c.e")
tree$addNode("c", "f", "child.c.f")
tree$addNode("c", "g", "child.c.g")

test_that("Verify correct branch data is returned with relative getter", {
  expect_identical(tree$searchNode("e")$getBranchInfo(what= "data"),
                   list("child.c.e"))
  expect_identical(tree$searchNode("b")$getBranchInfo(what = "data"),
                   list("child.b", "child.b.c", "child.c.e", "child.c.f",
                        "child.c.g", "child.b.d"))
  expect_identical(tree$searchNode("e")$getBranchInfo("key"),
                   list("e"))
  expect_identical(tree$searchNode("b")$getBranchInfo(what = "key"),
                   list("b", "c", "e", "f", "g", "d"))
})

test_that("Verify correct branch data is returned with absolute getter", {
  expect_identical(tree$getBranchInfoByKey("e", what = "data"),
                   list("child.c.e"))
  expect_identical(tree$getBranchInfoByKey("b", what = "data"),
                   list("child.b", "child.b.c", "child.c.e", "child.c.f",
                        "child.c.g", "child.b.d"))
  expect_identical(tree$getBranchInfoByKey("e", "key"),
                   list("e"))
  expect_identical(tree$getBranchInfoByKey("b", "key"),
                   list("b", "c", "e", "f", "g", "d"))
})
