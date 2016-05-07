context("Test child data")

test_that("Verify correct child data is returned", {

  tree <- GeneralTree$new(0, 'parent1')
  tree$add_node(0, 'a', 'child.a')
  tree$add_node(0, 'b', 'child.b')
  tree$add_node('b', 'c', 'child.b.c')
  tree$add_node('b', 'd', 'child.b.d')
  tree$add_node('c', 'e', 'child.c.e')
  tree$add_node('c', 'f', 'child.c.f')
  tree$add_node('c', 'g', 'child.c.g')

  expect_identical(tree$search_node('b')$getChildData(),
                   list('child.b.c', 'child.b.d'))
  expect_identical(tree$search_node('c')$getChildData(),
                   list('child.c.e', 'child.c.f', 'child.c.g'))
  expect_identical(tree$search_node('c')$getChildId(),
                   list('e', 'f', 'g'))
})
