context('Test action functions')

test_that('have_siblings is working as expected', {
  tree <- GeneralTree$new(0, 'parent1')

  expect_identical(tree$have_siblings, FALSE)

  tree$addNode(0, 1, 'child.1')
  tree$addNode(0, 2, 'child.2')
  tree$addNode(0, 3, 'child.3')

  expect_identical(tree$searchNode(2)$have_siblings, TRUE)
})

test_that('is_last_sibling is working as expected', {
  tree <- GeneralTree$new(0, 'parent1')

  expect_identical(tree$have_siblings, FALSE)

  tree$addNode(0, 1, 'child.1')
  tree$addNode(0, 2, 'child.2')
  tree$addNode(0, 3, 'child.3')

  expect_identical(tree$searchNode(2)$is_last_sibling, FALSE)
  expect_identical(tree$searchNode(3)$is_last_sibling, TRUE)
})
