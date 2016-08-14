context('Printing tree')
test_that('printing a tree with only a single node', {
  tree <- GeneralTree$new(1, 'parent1')

  expect_identical(tree$toString(), '1')
})


test_that('printing a tree with only a single child node', {
  tree <- GeneralTree$new(1, 'parent1')
  tree$addNode(1, 2, 'child1.2')

  expect_identical(tree$toString(), '1 --> 2')
})

test_that('printing a tree with only a single child node with characters', {
  tree <- GeneralTree$new('root', 'parent1')
  tree$addNode('root', 'child1', 'child1.2')

  expect_identical(tree$toString(), 'root --> child1')
})

test_that('printing a tree with three childeren with characters as id, just one level deep', {
  tree <- GeneralTree$new('root', 'parent1')
  tree$addNode('root', 'child1', 'child1.1')
  tree$addNode('root', 'child2', 'child1.2')
  tree$addNode('root', 'child3', 'child1.3')

  expect_identical(tree$toString(), 'root --> child1\n     |-> child2\n     \\-> child3')
})

