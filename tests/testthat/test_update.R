context("Test update functions")

tree <- GeneralTree$new(letters[1], letters[1])
tree$addNode(letters[1], letters[2], letters[2])
tree$addNode(letters[2], letters[3], letters[3])
tree$addNode(letters[2], letters[4], letters[4])
tree$addNode(letters[4], letters[5], letters[5])
tree$addNode(letters[5], letters[6], letters[6])


test_that("absolute update key is working as expected", {
  expect_identical(tree$updateKeyByKey(letters[1], letters[26]), letters[1])
  expect_identical(tree$updateDataByKey(letters[26], letters[26]), letters[1])
})

test_that("relative update key is working as expected", {
  expect_identical(tree$searchNode(letters[4])$updateKey(letters[25]),
                   letters[4])
  expect_identical(tree$searchNode(letters[25])$updateData(letters[25]),
                   letters[4])
})

test_that("delete of updated node works", {
  tree$searchNode(letters[25])$delete()

  expect_error(tree$searchNode(letters[25]))
})

test_that("expect error when deleting an updated node as it is the root node", {
  expect_error(tree$searchNode(letters[26])$delete())
})
