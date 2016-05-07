context('Test parent')

test_that('Parent is correct in empty tree', {
  root <- GeneralTree$new(0, 'parent1')

  expect_identical(root$parent, NULL)
})

test_that('Parent is correct in single child case', {
  root <- GeneralTree$new(0, 'parent1')
  child1 <- root$addNode(0, 1, 'child1')

  expect_identical(child1$parent$data, 'parent1')
})

test_that('Parent stays consistent with multiple children', {
  tree <- GeneralTree$new(0, 'parent1')
  tree$addNode(0, 1, 'child1')
  tree$addNode(0, 2, 'child2')

  tree$addNode(2, 3, 'child2.3')

  expect_identical(tree$searchNode(1)$parent$data, 'parent1')
  expect_identical(tree$searchNode(3)$parent$data, 'child2')
})

test_that('add multiple childeren as part of child', {
  tree <- GeneralTree$new(0, 'parent1')
  tree$addNode(0, 1, 'child1')
  tree$addNode(0, 2, 'child2')
  tree$addNode(2, 3, 'child2.3')
  tree$addNode(2, 4, 'child2.4')
  tree$addNode(3, 5, 'child3.5')

  expect_identical(tree$searchNode(5)$parent$data, 'child2.3')
  expect_identical(tree$searchNode(3)$parent$data, 'child2')
})
