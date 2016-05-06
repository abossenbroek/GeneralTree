context('Test parent')

test_that('Parent is correct in empty tree', {
  root <- GeneralTree$new(0, 'parent1')

  expect_identical(root$parent, NULL)
})

test_that('Parent is correct in single child case', {
  root <- GeneralTree$new(0, 'parent1')
  child1 <- root$add_node(0, 1, 'child1')

  expect_identical(child1$parent$data, 'parent1')
})

test_that('Parent stays consistent with multiple children', {
  nodes <- list()

  nodes[['root']] <- GeneralTree$new(0, 'parent1')
  nodes[['child1']] <- nodes[['root']]$add_node(0, 1, 'child1')
  nodes[['child2']] <- nodes[['root']]$add_node(0, 2, 'child2')

  nodes[['child2.3']] <- nodes[['root']]$add_node(2, 3, 'child2.3')

  nodes[['root']]$search_id(3)$data

  expect_identical(nodes[['child1']]$search_id(1)$parent$data, 'parent1')
  expect_identical(nodes[['root']]$search_id(3)$parent$data, 'child2')
})

test_that('add multiple childeren as part of child', {
  nodes <- list()

  nodes[['root']] <- GeneralTree$new(0, 'parent1')
  nodes[['child1']] <- nodes[['root']]$add_node(0, 1, 'child1')
  nodes[['child2']] <- nodes[['root']]$add_node(0, 2, 'child2')

  nodes[['child2.3']] <- nodes[['root']]$add_node(2, 3, 'child2.3')

  nodes[['child2.4']] <- nodes[['root']]$add_node(2, 4, 'child2.4')


  nodes[['child3.5']] <- nodes[['child2.4']]$add_node(3, 5, 'child3.5')

  expect_identical(nodes[['child2.4']]$search_id(5)$parent$data, 'child2.3')
  expect_identical(nodes[['child1']]$search_id(3)$parent$data, 'child2')
})
