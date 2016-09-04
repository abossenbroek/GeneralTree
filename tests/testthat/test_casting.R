context("Test casting functions")
test_that("Cast singelton tree to data frame", {
  tree <- GeneralTree$new(1, "parent1")

  result <- data.frame(id = 1, data = "parent1", parent = NA, stringsAsFactors = FALSE)

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


  result <- data.frame(id = id, data = data, parent = parent, stringsAsFactors = FALSE)

  expect_identical(as.data.frame(tree), result)
})

test_that("Cast singelton tree from data frame to tree", {
  tree <- GeneralTree$new("root", "parent1")

  test_tree <- data.frame(id = "root", data = "parent1", parent = NA, stringsAsFactors = FALSE)


  expect_identical(as.GeneralTree(test_tree)$toString(what = c("id", "data")),
                   tree$toString(what = c("id", "data")))
})

test_that("Cast more complex tree from data frame to tree", {
  tree <- GeneralTree$new("root", "parent1")
  tree$addNode("root", "child1", "data1.1")
  tree$addNode("root", "child2", "data1.2")

  test_tree <- data.frame(
    id = c("root", "child1", "child2"),
    data = c("parent1", "data1.1", "data1.2"),
    parent = c(NA, "root", "root"), stringsAsFactors = FALSE)


  expect_identical(as.GeneralTree(test_tree)$toString(what = c("id", "data")),
                   tree$toString(what = c("id", "data")))
})

test_that("Cast more complex tree from data frame to tree", {
  tree <- GeneralTree$new("root", "parent1")
  tree$addNode("root", "child3", "data1.3")
  tree$addNode("child3", "child1", "data3.1")
  tree$addNode("root", "child2", "data1.2")

  test_tree <- data.frame(
    id = c("root", "child1", "child2", "child3"),
    data = c("parent1", "data3.1", "data1.2", "data1.3"),
    parent = c(NA, "child3", "root", "root"), stringsAsFactors = FALSE)


  expect_identical(as.GeneralTree(test_tree)$toString(what = c("id", "data")),
                   tree$toString(what = c("id", "data")))
})

test_that("Cast data frame with non standard column names to tree", {
  tree <- GeneralTree$new("root", "parent1")
  tree$addNode("root", "child3", "data1.3")
  tree$addNode("child3", "child1", "data3.1")
  tree$addNode("root", "child2", "data1.2")

  test_tree_df <- data.frame(
    ID = c("root", "child1", "child2", "child3"),
    DATA = c("parent1", "data3.1", "data1.2", "data1.3"),
    PARENT = c(NA, "child3", "root", "root"), stringsAsFactors = FALSE)

  test_tree <- as.GeneralTree(test_tree_df, id = "ID", data = "DATA", parent = "PARENT")

  expect_identical(test_tree$toString(what = c("id", "data")),
                   tree$toString(what = c("id", "data")))
})


test_that("Test for appropriate errors and warnings", {
  test_tree_df <- data.frame(
    ID = c("root", "child1", "child2", "child3"),
    DATA = c("parent1", "data3.1", "data1.2", "data1.3"),
    PARENT = c(NA, "child3", "root", "root"), stringsAsFactors = FALSE)

  expect_error(as.GeneralTree(data = "DATA", parent = "PARENT"))
  expect_error(as.GeneralTree(test_tree_df, id = "ID", parent = "PARENT"))
  expect_error(as.GeneralTree(test_tree_df, id = "ID", data = "DATA"))

  test_tree_df <- data.frame(
    ID = c("root", "child1", "child2", "child3"),
    DATA = c("parent1", "data3.1", "data1.2", "data1.3"),
    PARENT = c("child10", "child3", "root", "root"), stringsAsFactors = FALSE)

  expect_error(as.GeneralTree(test_tree_df, id = "ID", data = "DATA", parent = "PARENT"))

  test_tree_df <- data.frame(
    ID = c("root", "child1", "child2", "child3"),
    DATA = c("parent1", "data3.1", "data1.2", "data1.3"),
    PARENT = c(NA, "child3", "root", "child4"), stringsAsFactors = FALSE)

  expect_error(as.GeneralTree(test_tree_df, id = "ID", data = "DATA", parent = "PARENT"))

  test_tree_df <- data.frame(
    ID = c("root", "child1", "child2", "child3"),
    DATA = c("parent1", "data3.1", "data1.2", "data1.3"),
    PARENT = c(NA, "child3", "root", NA), stringsAsFactors = FALSE)

  expect_error(as.GeneralTree(test_tree_df, id = "ID", data = "DATA", parent = "PARENT"))

  test_tree_df <- data.frame(
    ID = c("root", "child1"),
    DATA = c("parent1", "data3.1"),
    PARENT = c(NA, "child3"), stringsAsFactors = FALSE)

  expect_error(as.GeneralTree(test_tree_df, id = "ID", data = "DATA", parent = "PARENT"))


  test_tree_df <- data.frame(
    ID = c("root"),
    DATA = c("parent1"),
    PARENT = c(NA), stringsAsFactors = TRUE)

  expect_warning(as.GeneralTree(test_tree_df, id = "ID", data = "DATA", parent = "PARENT"))
})

test_that("For casting parsed source to GeneralTree", {
   p <- parse(text = "test_that(\"test that the tree_walker with while loop\", {

             tree <- GeneralTree$new(1, \"parent1\")
             tree$addNode(1, 2, \"child.1.2\")
             tree$addNode(2, 3, \"child.2.3\")
             })
             ",
           keep.source = TRUE)

  result <- "0 : BaseEnvironment --> 92 :  --> 3 :  --> 1 : test_that
                              |-> 2 : (
                              |-> 6 :  --> 4 : \"test that the tree_walker with while loop\"
                              |-> 5 : ,
                              |-> 88 :  --> 9 : {
                              |          |-> 33 :  --> 14 :  --> 12 : tree
                              |          |          |-> 13 : <-
                              |          |          \\-> 31 :  --> 19 :  --> 17 :  --> 15 : GeneralTree
                              |          |                      |          |-> 16 : $
                              |          |                      |          \\-> 18 : new
                              |          |                      |-> 20 : (
                              |          |                      |-> 22 :  --> 21 : 1
                              |          |                      |-> 23 : ,
                              |          |                      |-> 28 :  --> 26 : \"parent1\"
                              |          |                      \\-> 27 : )
                              |          |-> 57 :  --> 40 :  --> 38 :  --> 36 : tree
                              |          |          |          |-> 37 : $
                              |          |          |          \\-> 39 : addNode
                              |          |          |-> 41 : (
                              |          |          |-> 43 :  --> 42 : 1
                              |          |          |-> 44 : ,
                              |          |          |-> 48 :  --> 47 : 2
                              |          |          |-> 49 : ,
                              |          |          |-> 54 :  --> 52 : \"child.1.2\"
                              |          |          \\-> 53 : )
                              |          |-> 82 :  --> 65 :  --> 63 :  --> 61 : tree
                              |          |          |          |-> 62 : $
                              |          |          |          \\-> 64 : addNode
                              |          |          |-> 66 : (
                              |          |          |-> 68 :  --> 67 : 2
                              |          |          |-> 69 : ,
                              |          |          |-> 73 :  --> 72 : 3
                              |          |          |-> 74 : ,
                              |          |          |-> 79 :  --> 77 : \"child.2.3\"
                              |          |          \\-> 78 : )
                              |          \\-> 86 : }
                              \\-> 89 : )"
  expect_identical(as.GeneralTree(p)$toString(), result)
})

test_that("Casting parsed source to GeneralTree gives right error", {
     p <- parse(text = "test_that(\"test that the tree_walker with while loop\", {

             tree <- GeneralTree$new(1, \"parent1\")
             tree$addNode(1, 2, \"child.1.2\")
             tree$addNode(2, 3, \"child.2.3\")
             })
             ",
           keep.source = TRUE)

  expect_match(as.GeneralTree(p, what = "token")$toString(what = "data"),
               "STR_CONST")
  expect_match(as.GeneralTree(p, what = "text")$toString(what = "data"),
               "tree_walker")
  expect_match(as.GeneralTree(p, what = c("token", "text"))$toString(what =
                                                                     "data"),
               "SYMBOL_FUNCTION_CALL: test_that")
  expect_error(as.GeneralTree(p, what = "some"))
})
