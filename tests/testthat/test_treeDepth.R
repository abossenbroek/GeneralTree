context('Test tree_depth')

test_that('tree_depth reports the correct depth when creating a tree', {
  tree <- GeneralTree$new(0, 'parent1')

  tree$addNode(0, 1, 'child.1')
  tree$addNode(0, 2, 'child.2')
  tree$addNode(0, 3, 'child.3')
  expect_identical(tree$tree_depth, 2)
})

test_that('treeDepth reports the correct depth when creating a tree with two levels deep', {
  tree <- GeneralTree$new(0, 'parent1')

  tree$addNode(0, 1, 'child.1')
  tree$addNode(0, 2, 'child.2')
  tree$addNode(0, 3, 'child.3')
  tree$addNode(1, 4, 'child.1.4')
  expect_identical(tree$tree_depth, 3)
})


test_that('treeDepth reports the correct depth when deleting a child', {
  tree <- GeneralTree$new(0, 'parent1')

  tree$addNode(0, 1, 'child.1')
  tree$addNode(0, 2, 'child.2')
  tree$addNode(0, 3, 'child.3')
  tree$addNode(1, 4, 'child.1.4')
  tree$deleteId(4)

  expect_identical(tree$tree_depth, 2)
})

test_that('treeDepth reports the correct depth when deleting a child', {
  tree <- GeneralTree$new(0, 'parent1')

  tree$addNode(0, 1, 'child.1')
  tree$addNode(0, 2, 'child.2')
  tree$addNode(0, 3, 'child.3')
  tree$addNode(1, 4, 'child.1.4')
  tree$deleteId(4)
  tree$addNode(1, 4, 'child.1.4')
  tree$addNode(4, 5, 'child.4.5')
  tree$addNode(4, 6, 'child.4.6')
  tree$addNode(5, 7, 'child.5.7')
  tree$addNode(6, 8, 'child.6.8')
  expect_identical(tree$tree_depth, 5)
})

test_that('treeDepth reports the correct depth when adding nodes in any order', {
  tree <- GeneralTree$new(1, 'parent1')

  tree$addNode(1, 2, 'child.1.2')
  tree$addNode(2, 3, 'child.2.3')

  expect_identical(tree$tree_depth, 3)

  tree$addNode(3, 4, 'child.3.4')
  tree$addNode(1, 8, 'child.1.8')
  tree$addNode(8, 9, 'child.8.9')
  tree$addNode(9, 12, 'child.9.12')

  expect_identical(tree$tree_depth, 4)

  tree$addNode(8, 14, 'child.8.14')

  expect_identical(tree$tree_depth, 4)


  tree$addNode(12, 13, 'child.12.13')

  expect_identical(tree$tree_depth, 5)
})
