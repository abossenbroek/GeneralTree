context("Searching tree")
test_that("search tree with only a single child", {
  nodes <- list()

  nodes[["root"]] <- GeneralTree$new(0, "parent1")
  nodes[["child1"]] <- nodes[["root"]]$addNode(0, 1, "child1")
  search_result <- nodes[["child1"]]$searchNode(1)

  expect_identical(search_result, nodes[["child1"]])

  search_result <- nodes[["child1"]]$searchNode(0)

  expect_identical(search_result, nodes[["root"]])
})

