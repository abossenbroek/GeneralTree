context("Test benchmark functions")
test_that("Test for appropriate errors and warnings", {
  expect_error(benchmarkGeneralTree(depth = "a"))
  expect_error(benchmarkGeneralTree(depth = 0))
  expect_error(benchmarkGeneralTree(fraction_to_search = "a"))
  expect_error(benchmarkGeneralTree(fraction_to_search = 0.001))
  expect_error(benchmarkGeneralTree(times = "a"))
  expect_error(benchmarkGeneralTree(times = -1))
})

test_that("Correct tests are run", {
  res <- benchmarkGeneralTree(times = 1, fraction_to_search = .1,
                              number_of_splits = 2)

  expect_equal(colnames(res), c("mean", "median"))
  expect_equal(rownames(res), c("2-create", "2-search", "2-native-iter",
                                "2-foreach-iter", "2-casting-df",
                                "2-casting-gt-df"))
})
