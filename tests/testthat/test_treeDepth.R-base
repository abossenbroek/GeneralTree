context('Test tree_depth')

test_that('tree_depth reports the correct depth when creating a tree', {
  tree <- GeneralTree$new(0, 'parent1')

  tree$add_node(0, 1, 'child.1')
  tree$add_node(0, 2, 'child.2')
  tree$add_node(0, 3, 'child.3')
  expect_identical(tree$tree_depth, 2)
})

test_that('treeDepth reports the correct depth when creating a tree with two levels deep', {
  nodes <- list()

  tree <- GeneralTree$new(0, 'parent1')

  tree$add_node(0, 1, 'child.1')
  tree$add_node(0, 2, 'child.2')
  tree$add_node(0, 3, 'child.3')
  tree$add_node(1, 4, 'child.1.4')
  expect_identical(tree$tree_depth, 3)
})


test_that('treeDepth reports the correct depth when deleting a child', {
  nodes <- list()

  tree <- GeneralTree$new(0, 'parent1')

  tree$add_node(0, 1, 'child.1')
  tree$add_node(0, 2, 'child.2')
  tree$add_node(0, 3, 'child.3')
  tree$add_node(1, 4, 'child.1.4')
  tree$deleteId(4)

  expect_identical(tree$tree_depth, 2)
})

test_that('treeDepth reports the correct depth when deleting a child', {
  nodes <- list()

  tree <- GeneralTree$new(0, 'parent1')

  tree$add_node(0, 1, 'child.1')
  tree$add_node(0, 2, 'child.2')
  tree$add_node(0, 3, 'child.3')
  tree$add_node(1, 4, 'child.1.4')
  tree$deleteId(4)
  tree$add_node(1, 4, 'child.1.4')
  tree$add_node(4, 5, 'child.4.5')
  tree$add_node(4, 6, 'child.4.6')
  tree$add_node(5, 7, 'child.5.7')
  tree$add_node(6, 8, 'child.6.8')

  expect_identical(tree$tree_depth, 5)
})
