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

test_that('printing a tree with three childeren with characters as id, just two level deep', {
  tree <- GeneralTree$new('root', 'parent1')
  tree$addNode('root', 'child1', 'child1.1')
  tree$addNode('root', 'child2', 'child1.2')
  tree$addNode('root', 'child3', 'child1.3')
  tree$addNode('child1', 'child4', 'child1.4')
  tree$addNode('child1', 'child5', 'child1.5')
  tree$addNode('child1', 'child6', 'child1.6')

  expect_identical(tree$toString(), 'root --> child1 --> child4\n     |          |-> child5\n     |          \\-> child6\n     |-> child2\n     \\-> child3')
})

test_that('printing a tree with multiple levels and different length ids', {
  tree <- GeneralTree$new('root', 'parent1')
  tree$addNode('root', 'child1', 'child1.1')
  tree$addNode('root', 'child2', 'child1.2')
  tree$addNode('root', 'child3', 'child1.3')
  tree$addNode('child2', 'child4', 'child1.4')
  tree$addNode('child2', 'child5', 'child1.5')
  tree$addNode('child2', '6', 'child1.6')
  tree$addNode('child5', 'leaf7', 'child5.7')
  tree$addNode('child5', 'child8', 'child5.8')
  tree$addNode('child5', 'child9', 'child5.9')
  tree$addNode('child1', 'ten', 'child1.ten')
  tree$addNode('child1', 'eleven', 'child1.eleven')
  tree$addNode('child1', 'twelve', 'child1.twelve')



  expect_identical(tree$toString(), 'root --> child1 --> ten\n     |          |-> eleven\n     |          \\-> twelve\n     |-> child2 --> child4\n     |          |-> child5 --> leaf7\n     |          |          |-> child8\n     |          |          \\-> child9\n     |          \\-> 6\n     \\-> child3')
})

