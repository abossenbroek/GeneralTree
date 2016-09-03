context("Test tree walker")

test_that("test that the tree_walker walks depth first", {
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


  require(iterators)
  require(foreach)
  itx <- iter(tree, by = "id")
  numbers_in_tree <- foreach(i = itx, .combine = c) %do% c(i)
  expect_identical(numbers_in_tree, as.numeric(1 : 14))

  itx <- iter(tree, by = "data")
  data_in_tree <- foreach(i = itx, .combine = c) %do% c(i)

  expect_equal(length(data_in_tree), 14)
})


test_that("test that the tree_walker permits methods part of tree", {
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


  require(iterators)
  require(foreach)
  itx <- iter(tree, by = "is_root")
  numbers_in_tree <- foreach(i = itx, .combine = c) %do% c(i)
  # Only the first node should be root.
  expect_identical(numbers_in_tree, c(TRUE, rep(FALSE, 13)))
})

test_that("test that the tree_walker with while loop", {

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

  i <- tree$iterator()

  ids <- c()
  while (!is.null(i)) {
    ids <- c(ids, i$id)
    i <- tryCatch(i$nextElem(), error = function(e) {
       NULL
    })
  }

  expect_identical(ids, as.numeric(1 : 14))
})

test_that("test that the tree_walker with while loop", {

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

  i <- tree$iterator()

  ids <- c()
  while (!is.null(i)) {
    ids <- c(ids, i$id)
    i <- tryCatch(nextElem(i), error = function(e) {
      NULL
    })
  }

  expect_identical(ids, as.numeric(1 : 14))
})

test_that("test that the tree_walker works when iterator is created from child node", {

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

  i <- tree$searchNode(10)$iterator()

  ids <- c()
  while (!is.null(i)) {
    ids <- c(ids, i$id)
    i <- tryCatch(i$nextElem(), error = function(e) {
       NULL
    })
  }

  expect_identical(ids, as.numeric(1 : 14))
})

test_that("iteration works correctly with checkFunction returning FALSE", {

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

  require(iterators)
  require(foreach)
  itx <- iter(tree, by = "id", checkFunction = function(...) FALSE)

  ids <- NULL
  repeat {
    id <- tryCatch(nextElem(itx), error = function(e) NULL)
    if (is.null(id))
        break
    ids <- c(ids, id)
  }

  expect_identical(ids, as.numeric(1 : 14))
})

test_that("iteration works correctly with recycle on", {

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

  require(iterators)
  require(foreach)
  itx <- iter(tree, by = "id", recycle = TRUE)

  ids <- NULL
  for (i in 1 : 28) {
    id <- tryCatch(nextElem(itx), error = function(e) NULL)
#    if (is.null(id))
#        break
    ids <- c(ids, id)
  }

  expect_identical(ids, rep(as.numeric(1 : 14), 2))
})


