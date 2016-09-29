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

test_that("is_last_sibling is working as expected", {
  tree <- GeneralTree$new(0, "parent1")

  tree$addNode(0, 1, "child.1")

  expect_identical(tree$searchNode(1)$is_last_sibling, TRUE)
})

test_that("branchToList is working as expected", {
  tree <- GeneralTree$new(1, "parent1")
  tree$addNode(1, 2, "child.1.2")
  tree$addNode(2, 3, "child.2.3")
  tree$addNode(3, 4, "child.3.4")
  tree$addNode(3, 5, "child.3.5")
  tree$addNode(1, 7, "child.1.7")
  tree$addNode(1, 8, "child.1.8")
  tree$addNode(8, 9, "child.8.9")
  tree$addNode(9, 10, "child.9.10")
  tree$addNode(9, 11, "child.9.11")
  tree$addNode(9, 12, "child.9.12")
  tree$addNode(12, 13, "child.12.13")
  tree$addNode(8, 14, "child.8.14")
  tree$addNode(2, 6, "child.2.6")

  expect_equal(unlist(tree$getBranchKeys()), 1 : 14)

  res <- tree$searchNode(8)$getBranchKeys()
  expect_equal(unlist(res), 8 : 14)
})
