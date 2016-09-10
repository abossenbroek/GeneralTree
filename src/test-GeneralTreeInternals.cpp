#include <testthat.h>
#include <Rcpp.h>

#include "GeneralTreeInternal.h"

context("GeneralTreeInternal C++ works") {
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
    expect_false(gti.has_siblings(root_uid));
  }

  test_that("we can add a sibling to the child uner the root") {
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
    expect_true(gti.has_child(root_uid));
    expect_true(gti.get_parent(sibling_uid) == root_uid);
    expect_true(gti.has_siblings(child_uid));
  }

}
