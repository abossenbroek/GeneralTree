#include <testthat.h>
#include <Rcpp.h>

#include "GeneralTreeInternal.h"

context("GeneralTreeInternal can be created") {
  test_that("root node can be found in tree") {
    String root_id_string = "a";
    SEXP root_id = wrap(root_id_string);
    // Create a gti
    GeneralTreeInternal gti(root_id, root_id);
    // Retrieve the unique id.
    int root_uid = gti.find_uid_given_id(root_id);
    // Verify whether it is the correct uid.
    expect_true(root_uid == (gti.uid_counter - 1));
  }
}

context("GeneralTreeInternal can add childeren directly under each other") {
  test_that("we can add a child to the tree directly under the root") {
    String child_id_string = "child";
    String root_id_string = "root";
    SEXP root_id = wrap(root_id_string);
    SEXP child_id = wrap(child_id_string);
    // Create a gti.
    GeneralTreeInternal gti(root_id, root_id);
    // Add child node.
    gti.add_node(root_id, child_id, child_id);
    int child_uid = gti.find_uid_given_id(child_id);
    // Verify whether it is the correct uid.
    expect_true(child_uid == (gti.uid_counter - 1));
  }

  test_that("verify whether right properties are returned on root and single child") {
    String child_id_string = "child";
    String root_id_string = "root";
    SEXP root_id = wrap(root_id_string);
    SEXP child_id = wrap(child_id_string);
    // Create a gti.
    GeneralTreeInternal gti(root_id, root_id);
    // Add child node.
    gti.add_node(root_id, child_id, child_id);
    int root_uid = gti.find_uid_given_id(root_id);
    int child_uid = gti.find_uid_given_id(child_id);
    // Verify whether all the getters return the proper result of the tree.
    expect_false(gti.has_siblings(child_uid));
    expect_true(gti.has_child(root_uid));
    expect_true(gti.get_parent(child_uid) == root_uid);
    expect_false(gti.has_siblings(child_uid));
  }

  test_that("we can add a child to the child uner the root") {
    String child_id_string = "child.level1";
    String child2_id_string = "child.level2";
    String root_id_string = "root";
    SEXP root_id = wrap(root_id_string);
    SEXP child_id = wrap(child_id_string);
    SEXP child2_id = wrap(child2_id_string);
    // Create a gti.
    GeneralTreeInternal gti(root_id, root_id);
    // Add child node.
    gti.add_node(root_id, child_id, child_id);
    gti.add_node(child_id, child2_id, child2_id);

    int root_uid = gti.find_uid_given_id(root_id);
    int child_uid = gti.find_uid_given_id(child_id);
    int child2_uid = gti.find_uid_given_id(child2_id);

    // Verify whether all the getters return the proper result of the tree.
    expect_false(gti.has_siblings(child_uid));
    expect_true(gti.has_child(root_uid));
    expect_true(gti.has_child(child_uid));
    expect_false(gti.has_siblings(child2_uid));
    expect_true(gti.get_parent(child2_uid) == child_uid);
    expect_true(gti.get_parent(gti.get_parent(child2_uid)) == root_uid);
  }}


context("GeneralTreeInternal can add siblings") {
  test_that("we can add a sibling to the child under the root") {
    String child_id_string = "child";
    String sibling_id_string = "sibling";
    String root_id_string = "root";
    SEXP root_id = wrap(root_id_string);
    SEXP child_id = wrap(child_id_string);
    SEXP sibling_id = wrap(sibling_id_string);
    // Create a gti.
    GeneralTreeInternal gti(root_id, root_id);
    // Add child node.
    gti.add_node(root_id, child_id, child_id);
    gti.add_node(root_id, sibling_id, sibling_id);

    int root_uid = gti.find_uid_given_id(root_id);
    int child_uid = gti.find_uid_given_id(child_id);
    int sibling_uid = gti.find_uid_given_id(sibling_id);

    // Verify whether all the getters return the proper result of the tree.
    expect_true(gti.has_siblings(child_uid));
    expect_true(gti.has_siblings(sibling_uid));
    expect_true(gti.has_child(root_uid));
    expect_true(gti.get_parent(sibling_uid) == root_uid);
  }

  test_that("we can add several siblings to the child under the root") {
    String child_id_string = "child";
    String root_id_string = "root";
    String sibling_id_string = "sibling";
    String sibling2_id_string = "sibling2";
    SEXP root_id = wrap(root_id_string);
    SEXP child_id = wrap(child_id_string);
    SEXP sibling_id = wrap(sibling_id_string);
    SEXP sibling2_id = wrap(sibling2_id_string);
    // Create a gti.
    GeneralTreeInternal gti(root_id, root_id);
    // Add child node.
    gti.add_node(root_id, child_id, child_id);
    gti.add_node(root_id, sibling_id, sibling_id);
    gti.add_node(root_id, sibling2_id, sibling2_id);

    int root_uid = gti.find_uid_given_id(root_id);
    int child_uid = gti.find_uid_given_id(child_id);
    int sibling_uid = gti.find_uid_given_id(sibling_id);
    int sibling2_uid = gti.find_uid_given_id(sibling2_id);

    // Verify whether all the getters return the proper result of the tree.
    expect_true(gti.has_siblings(child_uid));
    expect_true(gti.has_siblings(sibling_uid));
    expect_true(gti.has_siblings(sibling2_uid));
    expect_false(gti.has_child(child_uid));
    expect_false(gti.has_child(sibling_uid));
    expect_false(gti.has_child(sibling2_uid));
    expect_true(gti.has_child(root_uid));
    expect_true(gti.get_parent(sibling2_uid) == root_uid);
  }

}

context("GeneralTreeInternal correct exceptions are returned") {
  test_that("correct exceptions") {
    String child_id_string = "child";
    String sibling_id_string = "sibling";
    String root_id_string = "root";
    SEXP root_id = wrap(root_id_string);
    SEXP child_id = wrap(child_id_string);
    SEXP sibling_id = wrap(sibling_id_string);
    // Create a gti.
    GeneralTreeInternal gti(root_id, root_id);
    // Add child node.
    gti.add_node(root_id, child_id, child_id);

    expect_error(gti.find_uid_given_id(sibling_id));
    expect_error(gti.find_child_given_uid(gti.uid_counter));
    expect_error(gti.get_parent(gti.uid_counter));
  }
}
