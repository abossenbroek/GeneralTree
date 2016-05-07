context('Test action functions')

test_that('have_siblign is working as expected', {
  nodes <- list()

  tree <- GeneralTree$new(0, 'parent1')

  expect_identical(tree$have_siblings, FALSE)

  tree$add_node(0, 1, 'child.1')
  tree$add_node(0, 2, 'child.2')
  tree$add_node(0, 3, 'child.3')

  expect_identical(tree$search_node(2)$have_siblings, TRUE)
})
