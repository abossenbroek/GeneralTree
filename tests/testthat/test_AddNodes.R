context('Adding nodes')
test_that('creating an empty tree works', {
  root <- GeneralTree$new(0, 'parent1')

  expect_is(root, 'GeneralTree')
  expect_identical(root$left_child, NULL)
  expect_identical(root$sibling, NULL)
})

test_that('add child to empty tree works', {
 root <- GeneralTree$new(0, 'parent1')
 child1 <- root$addNode(0, 1, 'child1')

 expect_is(child1, 'GeneralTree')
 expect_identical(root$left_child, child1)
 expect_identical(child1$root, root)
})

test_that('adding a child with an non existing parent results in failure', {
  root <- GeneralTree$new(0, 'parent1')
  expect_error(root$addNode(2, 1, 'child1'))
})

test_that('add multiple childeren', {
  nodes <- list()

  nodes[['root']] <- GeneralTree$new(0, 'parent1')
  nodes[['child1']] <- nodes[['root']]$addNode(0, 1, 'child1')
  nodes[['child2']] <- nodes[['root']]$addNode(0, 2, 'child2')

  nodes[['child2.3']] <- nodes[['root']]$addNode(2, 3, 'child2.3')

  nodes[['root']]$searchNode(3)$data

  expect_identical(nodes[['root']]$searchNode(3)$data, 'child2.3')
})

test_that('add multiple childeren as part of child', {
  nodes <- list()

  nodes[['root']] <- GeneralTree$new(0, 'parent1')
  nodes[['child1']] <- nodes[['root']]$addNode(0, 1, 'child1')
  nodes[['child2']] <- nodes[['root']]$addNode(0, 2, 'child2')

  nodes[['child2.3']] <- nodes[['root']]$addNode(2, 3, 'child2.3')

  nodes[['child2.4']] <- nodes[['root']]$addNode(2, 4, 'child2.4')


  nodes[['child3.5']] <- nodes[['child2.4']]$addNode(3, 5, 'child3.5')

  expect_identical(nodes[['child2.4']]$searchNode(5)$data, 'child3.5')
  expect_identical(nodes[['child1']]$searchNode(5)$data, 'child3.5')
})

test_that('add multiple childeren as part of child with char id', {
  nodes <- list()

  nodes[['root']] <- GeneralTree$new(0, 'parent1')
  nodes[['child.a']] <- nodes[['root']]$addNode(0, 'a', 'child.a')
  nodes[['child.b']] <- nodes[['root']]$addNode(0, 'b', 'child.b')

  nodes[['child.b.c']] <- nodes[['root']]$addNode('b', 'c', 'child.b.c')

  nodes[['child.b.d']] <- nodes[['root']]$addNode('b', 'd', 'child.b.d')


  nodes[['child.c.e']] <- nodes[['child.b.d']]$addNode('c', 'e', 'child.c.e')
  nodes[['child.c.f']] <- nodes[['child.b.d']]$addNode('c', 'f', 'child.c.e')

  expect_identical(nodes[['child.b.d']]$searchNode('e')$data, 'child.c.e')
  expect_identical(nodes[['child.a']]$searchNode('e')$data, 'child.c.e')
})

test_that('multiple childeren will yield error', {
  nodes <- list()
  nodes[['root']] <- GeneralTree$new(0, 'parent1')
  nodes[['child.0.1']] <- nodes[['root']]$addNode(0, 1, 'child.0.1')

  expect_warning(nodes[['root']]$set_left_child(GeneralTree$new(1, 'child.0.1')))
})

