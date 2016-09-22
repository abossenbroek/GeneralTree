// [[Rcpp::plugins(cpp11)]]
#include "config.h"

#ifdef HAVE_TESTTHAT_H

#include <testthat.h>
#include <Rcpp.h>


#include "tree_types.h"
#include "GeneralTreeInternal.h"

context("TreeInternal can be created") {
    String root_id_string = "a";
    SEXP root_id = wrap(root_id_string);
    // Create a gti
    TreeInternal ti(root_id, root_id);
    // Retrieve the unique id.
    uid root_uid = ti.find_uid(root_id);
    // Verify whether it is the correct uid.
  test_that("root node can be found in tree") {
    expect_true(root_uid == (ti.get_uid() - 1));
  }
}

context("TreeNode attributes work as expected") {
  String child_id_string = "child";
  String child2_id_string = "child2";
  String root_id_string = "root";
  SEXP root_id = wrap(root_id_string);
  SEXP child_id = wrap(child_id_string);
  SEXP child2_id = wrap(child2_id_string);
  // Create a gti.
  TreeInternal gti(root_id, root_id);

  tree_node_sp root = gti.find_node(root_id);

  test_that("has_siblings yields correct values") {
    expect_false(root->has_siblings());
  }
  test_that("has_left_child yields correct values") {
    expect_false(root->has_left_child());
  }
  test_that("get_parent yields exception") {
    expect_error(root->get_parent());
  }

  gti.add_node(root_id, child_id, child_id);

  test_that("has_siblings yields correct values") {
    expect_false(root->has_siblings());
  }
  test_that("has_left_child yields correct values") {
    expect_true(root->has_left_child());
  }


  gti.add_node(root_id, child2_id, child2_id);

  tree_node_sp child1 = gti.find_node(child_id);

  test_that("has_siblings yields correct values") {
    expect_true(child1->has_siblings());
  }
  test_that("has_left_child yields correct values") {
    expect_false(child1->has_left_child());
  }
}


context("TreeInternal can add children directly under each other") {
  test_that("we can add a child to the tree directly under the root") {
    String child_id_string = "child";
    String root_id_string = "root";
    SEXP root_id = wrap(root_id_string);
    SEXP child_id = wrap(child_id_string);
    // Create a gti.
    TreeInternal gti(root_id, root_id);
    // Add child node.
    gti.add_node(root_id, child_id, child_id);
    uid child_uid = gti.find_uid(child_id);
    // Verify whether it is the correct uid.
    expect_true(child_uid == (gti.get_uid() - 1));
  }

  test_that("verify whether right properties are returned on root and single child") {
    String child_id_string = "child";
    String root_id_string = "root";
    SEXP root_id = wrap(root_id_string);
    SEXP child_id = wrap(child_id_string);
    // Create a gti.
    TreeInternal gti(root_id, root_id);
    // Add child node.
    gti.add_node(root_id, child_id, child_id);
    uid root_uid = gti.find_uid(root_id);
    uid child_uid = gti.find_uid(child_id);
    // Verify whether all the getters return the proper result of the tree.
    expect_false(gti.has_siblings(child_id));
    expect_true(gti.has_child(root_id));
    expect_true(gti.get_parent(child_id)->get_data() == root_id);
    expect_false(gti.has_siblings(child_id));
  }

  test_that("we can add a child to the child uner the root") {
    String child_id_string = "child.level1";
    String child2_id_string = "child.level2";
    String root_id_string = "root";
    SEXP root_id = wrap(root_id_string);
    SEXP child_id = wrap(child_id_string);
    SEXP child2_id = wrap(child2_id_string);
    // Create a gti.
    TreeInternal gti(root_id, root_id);
    // Add child node.
    gti.add_node(root_id, child_id, child_id);
    gti.add_node(child_id, child2_id, child2_id);

    uid root_uid = gti.find_uid(root_id);
    uid child_uid = gti.find_uid(child_id);
    uid child2_uid = gti.find_uid(child2_id);

    // Verify whether all the getters return the proper result of the tree.
    expect_false(gti.has_siblings(child_id));
    expect_true(gti.has_child(root_id));
    expect_true(gti.has_child(child_id));
    expect_false(gti.has_siblings(child2_id));
    expect_true(gti.get_parent(child2_id)->get_data() == child_id);
  }
}

#endif
