context('Printing tree')
test_that('printing a tree with only a single node', {
  tree <- GeneralTree$new(1, 'parent1')

  expect_identical(tree$toString(what = 'id'), '1')
})


test_that('printing a tree with only a single child node', {
  tree <- GeneralTree$new(1, 'parent1')
  tree$addNode(1, 2, 'child1.2')

  expect_identical(tree$toString(what = 'id'), '1 --> 2')
})

test_that('printing a tree with only a single child node with characters', {
  tree <- GeneralTree$new('root', 'parent1')
  tree$addNode('root', 'child1', 'child1.2')

  expect_identical(tree$toString(what = 'id'), 'root --> child1')
})

test_that('printing a tree with three childeren with characters as id, just one level deep', {
  tree <- GeneralTree$new('root', 'parent1')
  tree$addNode('root', 'child1', 'child1.1')
  tree$addNode('root', 'child2', 'child1.2')
  tree$addNode('root', 'child3', 'child1.3')

  expect_identical(tree$toString(what = 'id'), 'root --> child1\n     |-> child2\n     \\-> child3')
})

test_that('printing a tree with three childeren with characters as id, just two level deep', {
  tree <- GeneralTree$new('root', 'parent1')
  tree$addNode('root', 'child1', 'child1.1')
  tree$addNode('root', 'child2', 'child1.2')
  tree$addNode('root', 'child3', 'child1.3')
  tree$addNode('child1', 'child4', 'child1.4')
  tree$addNode('child1', 'child5', 'child1.5')
  tree$addNode('child1', 'child6', 'child1.6')

  expect_identical(tree$toString(what = 'id'), 'root --> child1 --> child4\n     |          |-> child5\n     |          \\-> child6\n     |-> child2\n     \\-> child3')
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



  expect_identical(tree$toString(what = 'id'), 'root --> child1 --> ten\n     |          |-> eleven\n     |          \\-> twelve\n     |-> child2 --> child4\n     |          |-> child5 --> leaf7\n     |          |          |-> child8\n     |          |          \\-> child9\n     |          \\-> 6\n     \\-> child3')
})

test_that('printing a tree with multiple levels and different length data', {
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



  expect_identical(tree$toString(what = 'data'), 'parent1 --> child1.1 --> child1.ten\n        |            |-> child1.eleven\n        |            \\-> child1.twelve\n        |-> child1.2 --> child1.4\n        |            |-> child1.5 --> child5.7\n        |            |            |-> child5.8\n        |            |            \\-> child5.9\n        |            \\-> child1.6\n        \\-> child1.3')
})


test_that('printing a tree with multiple levels and different length data, printing id and data', {
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



  expect_identical(tree$toString(what = c('id', 'data')), 'root : parent1 --> child1 : child1.1 --> ten : child1.ten\n               |                     |-> eleven : child1.eleven\n               |                     \\-> twelve : child1.twelve\n               |-> child2 : child1.2 --> child4 : child1.4\n               |                     |-> child5 : child1.5 --> leaf7 : child5.7\n               |                     |                     |-> child8 : child5.8\n               |                     |                     \\-> child9 : child5.9\n               |                     \\-> 6 : child1.6\n               \\-> child3 : child1.3')
})

test_that('Print tree correctly when last child has many leafs.', {
  tree <- GeneralTree$new(0, 'parent1')
  tree$addNode(0, 1, 'child.1')
  tree$addNode(0, 2, 'child.2')
  tree$addNode(0, 3, 'child.3')
  tree$addNode(3, 4, 'child.3.4')
  tree$addNode(3, 5, 'child.3.5')
  tree$addNode(3, 6, 'child.3.6')
  tree$addNode(3, 7, 'child.3.7')
  tree$addNode(7, 8, 'child.7.8')
  tree$addNode(7, 9, 'child.7.9')
  tree$addNode(7, 10, 'child.7.10')

  expect_identical(tree$toString(), "0 : parent1 --> 1 : child.1\n            |-> 2 : child.2\n            \\-> 3 : child.3 --> 4 : child.3.4\n                            |-> 5 : child.3.5\n                            |-> 6 : child.3.6\n                            \\-> 7 : child.3.7 --> 8 : child.7.8\n                                              |-> 9 : child.7.9\n                                              \\-> 10 : child.7.10")
})
