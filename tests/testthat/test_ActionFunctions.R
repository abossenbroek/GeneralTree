context("Test action functions")

test_that("have_siblings is working as expected", {
  tree <- GeneralTree$new(0, "parent1")

  expect_identical(tree$have_siblings, FALSE)

  tree$addNode(0, 1, "child.1")
  tree$addNode(0, 2, "child.2")
  tree$addNode(0, 3, "child.3")

  expect_identical(tree$searchNode(2)$have_siblings, TRUE)
})

test_that("is_last_sibling is working as expected", {
  tree <- GeneralTree$new(0, "parent1")

  expect_identical(tree$have_siblings, FALSE)

  tree$addNode(0, 1, "child.1")
  tree$addNode(0, 2, "child.2")
  tree$addNode(0, 3, "child.3")

  expect_identical(tree$searchNode(2)$is_last_sibling, FALSE)
  expect_identical(tree$searchNode(3)$is_last_sibling, TRUE)
})

test_that("setData is working as expected", {
  tree <- GeneralTree$new(0, "parent1")

  tree$addNode(0, 1, "child.1")
  tree$addNode(0, 2, "child.2")
  tree$addNode(0, 3, "child.3")

  tree$searchNode(2)$setData("new data for child 2")

  expect_identical(tree$searchNode(2)$data, "new data for child 2")
})

test_that("search is working as expected", {
  tree <- GeneralTree$new(0, "parent1")

  tree$addNode(0, 1, "child.1")
  tree$addNode(0, 2, "child.2")
  tree$addNode(0, 3, "child.3")

  expect_identical(tree$searchData(2), "child.2")
})

test_that(" is working as expected", {
  tree <- GeneralTree$new(0, "parent1")

  tree$addNode(0, 1, "child.1")

  expect_identical(tree$searchNode(1)$is_last_sibling, FALSE)
})


