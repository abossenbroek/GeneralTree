context('Test delete')

test_that('delete node that is sibling', {
  tree <- GeneralTree$new(0, 'parent1')

  expect_identical(tree$have_siblings, FALSE)

  tree$add_node(0, 1, 'child.1')
  tree$add_node(0, 2, 'child.2')
  tree$add_node(0, 3, 'child.3')
  expect_identical(tree$search_node(3)$data, 'child.3')
  tree$deleteId(3)

  expect_identical(tree$search_node(3), NULL)
})

test_that('delete node that is left child', {
  tree <- GeneralTree$new(0, 'parent1')

  expect_identical(tree$have_siblings, FALSE)

  tree$add_node(0, 1, 'child.1')
  tree$add_node(0, 2, 'child.2')
  tree$add_node(0, 3, 'child.3')
  expect_identical(tree$search_node(1)$data, 'child.1')
  tree$deleteId(1)

  expect_identical(tree$search_node(1), NULL)
  expect_identical(tree$search_node(3)$data, 'child.3')
})

test_that('Delete a child without siblings', {
  tree <- GeneralTree$new(0, 'parent1')

  tree$add_node(0, 1, 'child.1')
  tree$add_node(0, 2, 'child.2')
  tree$add_node(0, 3, 'child.3')
  tree$add_node(1, 4, 'child.1.4')
  tree$deleteId(4)

  expect_identical(tree$search_node(4), NULL)
})

test_that('delete node that is three levels deep and sibling', {
  tree <- GeneralTree$new(0, 'parent1')

  expect_identical(tree$have_siblings, FALSE)

  tree$add_node(0, 1, 'child.1')
  tree$add_node(0, 2, 'child.2')
  tree$add_node(0, 3, 'child.3')
  tree$add_node(3, 4, 'child.3.4')
  tree$add_node(3, 5, 'child.3.5')
  tree$add_node(3, 6, 'child.3.6')
  tree$add_node(3, 7, 'child.3.7')
  tree$add_node(7, 8, 'child.7.8')
  tree$add_node(7, 9, 'child.7.9')

  expect_identical(tree$search_node(9)$data, 'child.7.9')
  tree$deleteId(9)
  expect_identical(tree$search_node(9), NULL)
})

test_that('delete node that is three levels deep and left child', {
  tree <- GeneralTree$new(0, 'parent1')

  expect_identical(tree$have_siblings, FALSE)

  tree$add_node(0, 1, 'child.1')
  tree$add_node(0, 2, 'child.2')
  tree$add_node(0, 3, 'child.3')
  tree$add_node(3, 4, 'child.3.4')
  tree$add_node(3, 5, 'child.3.5')
  tree$add_node(3, 6, 'child.3.6')
  tree$add_node(3, 7, 'child.3.7')
  tree$add_node(7, 8, 'child.7.8')
  tree$add_node(7, 9, 'child.7.9')
  tree$add_node(7, 10, 'child.7.10')

  expect_identical(tree$search_node(8)$data, 'child.7.8')
  tree$deleteId(8)
  expect_identical(tree$search_node(8), NULL)
  expect_identical(tree$search_node(9)$data, 'child.7.9')
  expect_identical(tree$search_node(10)$data, 'child.7.10')
})
