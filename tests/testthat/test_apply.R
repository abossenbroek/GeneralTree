context("Test apply functions")

tree <- GeneralTree$new(letters[1], letters[1])
tree$addNode(letters[1], letters[2], letters[2])
tree$addNode(letters[1], letters[3], letters[3])
tree$addNode(letters[1], letters[5], letters[5])
tree$addNode(letters[3], letters[4], letters[4])
tree$addNode(letters[1], letters[6], letters[6])

test_that("Apply on branch returns the correct keys", {
  res <- tree$applyOnBranch(function(n) n$key )

  expect_equal(unlist(res), letters[1:6])
})

test_that("Apply on branch returns the correct data", {
  res <- tree$applyOnBranch(function(n) n$data )

  expect_equal(unlist(res), letters[1:6])
})

test_that("Apply on branch returns the correct keys", {
  res <- tree$applyOnBranchByKey(letters[3], function(n) n$key )

  expect_equal(unlist(res), letters[3:4])
})

test_that("Apply on branch returns the correct data", {
  res <- tree$applyOnBranchByKey(letters[3], function(n) n$data )

  expect_equal(unlist(res), letters[3:4])
})

