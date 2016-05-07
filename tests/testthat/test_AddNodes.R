context('Adding nodes')
test_that('creating an empty tree works', {
  root <- GeneralTree$new(0, 'parent1')

  expect_is(root, 'GeneralTree')
  expect_identical(root$left_child, NULL)
  expect_identical(root$sibling, NULL)
})

test_that('add child to empty tree works', {
 root <- GeneralTree$new(0, 'parent1')
 child1 <- root$add_node(0, 1, 'child1')

 expect_is(child1, 'GeneralTree')
 expect_identical(root$left_child, child1)
 expect_identical(child1$root, root)
})

test_that('adding a child with an non existing parent results in failure', {
  root <- GeneralTree$new(0, 'parent1')
  expect_error(root$add_node(2, 1, 'child1'))
})

test_that('add multiple childeren', {
  nodes <- list()

  nodes[['root']] <- GeneralTree$new(0, 'parent1')
  nodes[['child1']] <- nodes[['root']]$add_node(0, 1, 'child1')
  nodes[['child2']] <- nodes[['root']]$add_node(0, 2, 'child2')

  nodes[['child2.3']] <- nodes[['root']]$add_node(2, 3, 'child2.3')

  nodes[['root']]$search_node(3)$data

  expect_identical(nodes[['root']]$search_node(3)$data, 'child2.3')
})

test_that('add multiple childeren as part of child', {
  nodes <- list()

  nodes[['root']] <- GeneralTree$new(0, 'parent1')
  nodes[['child1']] <- nodes[['root']]$add_node(0, 1, 'child1')
  nodes[['child2']] <- nodes[['root']]$add_node(0, 2, 'child2')

  nodes[['child2.3']] <- nodes[['root']]$add_node(2, 3, 'child2.3')

  nodes[['child2.4']] <- nodes[['root']]$add_node(2, 4, 'child2.4')


  nodes[['child3.5']] <- nodes[['child2.4']]$add_node(3, 5, 'child3.5')

  expect_identical(nodes[['child2.4']]$search_node(5)$data, 'child3.5')
  expect_identical(nodes[['child1']]$search_node(5)$data, 'child3.5')
})

test_that('add multiple childeren as part of child with char id', {
  nodes <- list()

  nodes[['root']] <- GeneralTree$new(0, 'parent1')
  nodes[['child.a']] <- nodes[['root']]$add_node(0, 'a', 'child.a')
  nodes[['child.b']] <- nodes[['root']]$add_node(0, 'b', 'child.b')

  nodes[['child.b.c']] <- nodes[['root']]$add_node('b', 'c', 'child.b.c')

  nodes[['child.b.d']] <- nodes[['root']]$add_node('b', 'd', 'child.b.d')


  nodes[['child.c.e']] <- nodes[['child.b.d']]$add_node('c', 'e', 'child.c.e')
  nodes[['child.c.f']] <- nodes[['child.b.d']]$add_node('c', 'f', 'child.c.e')

  expect_identical(nodes[['child.b.d']]$search_node('e')$data, 'child.c.e')
  expect_identical(nodes[['child.a']]$search_node('e')$data, 'child.c.e')
})

test_that('multiple childeren will yield error', {
  nodes <- list()
  nodes[['root']] <- GeneralTree$new(0, 'parent1')
  nodes[['child.0.1']] <- nodes[['root']]$add_node(0, 1, 'child.0.1')

  expect_error(nodes[['root']]$set_left_child(GeneralTree$new(1, 'child.0.1')))
})

