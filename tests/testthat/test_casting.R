context("Test casting functions")
test_that("Cast singelton tree to data frame", {
  tree <- GeneralTree$new(1, "parent1")

  result <- data.frame(id = 1, data = "parent1", parent = NA)

  expect_identical(as.data.frame(tree), result)
})

test_that("Cast more complex tree", {
  tree <- GeneralTree$new("root", "parent1")
  tree$addNode("root", "child1", "child1.1")
  tree$addNode("root", "child2", "child1.2")
  tree$addNode("root", "child3", "child1.3")
  tree$addNode("child2", "child4", "child1.4")
  tree$addNode("child2", "child5", "child1.5")
  tree$addNode("child2", "6", "child1.6")
  tree$addNode("child5", "leaf7", "child5.7")
  tree$addNode("child5", "child8", "child5.8")
  tree$addNode("child5", "child9", "child5.9")
  tree$addNode("child1", "ten", "child1.ten")
  tree$addNode("child1", "eleven", "child1.eleven")
  tree$addNode("child1", "twelve", "child1.twelve")

  id <- c("root", "child1", "ten", "eleven", "twelve", "child2", "child4",
          "child5", "leaf7", "child8", "child9", "6", "child3")

  data <- c("parent1", "child1.1", "child1.ten", "child1.eleven",
            "child1.twelve", "child1.2", "child1.4", "child1.5", "child5.7",
            "child5.8", "child5.9", "child1.6", "child1.3")

  parent <- c(NA, "root", "child1", "child1", "child1", "root", "child2", "child2",
              "child5", "child5", "child5", "child2", "root")


  result <- data.frame(id = id, data = data, parent = parent)

  expect_identical(as.data.frame(tree), result)
})

test_that("Cast singelton tree from data frame to tree", {
  tree <- GeneralTree$new("root", "parent1")

  test_tree <- data.frame(id = "root", data = "parent1", parent = NA)


  expect_identical(as.GeneralTree(test_tree)$toString(what = c('id', 'data')),
                   tree$toString(what = c('id', 'data')))
})
