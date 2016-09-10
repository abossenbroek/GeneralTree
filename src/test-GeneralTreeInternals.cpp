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
    int root_uid = gti.find_uid(root_id);
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
    int child_uid = gti.find_uid(child_id);
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
    int root_uid = gti.find_uid(root_id);
    int child_uid = gti.find_uid(child_id);
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

    int root_uid = gti.find_uid(root_id);
    int child_uid = gti.find_uid(child_id);
    int child2_uid = gti.find_uid(child2_id);

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

    int root_uid = gti.find_uid(root_id);
    int child_uid = gti.find_uid(child_id);
    int sibling_uid = gti.find_uid(sibling_id);

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

    int root_uid = gti.find_uid(root_id);
    int child_uid = gti.find_uid(child_id);
    int sibling_uid = gti.find_uid(sibling_id);
    int sibling2_uid = gti.find_uid(sibling2_id);

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

  test_that("we can add several siblings and childeren") {
    String child_id_string = "child";
    String child2_id_string = "child2";
    String child3_id_string = "child3";
    String root_id_string = "root";
    String sibling_id_string = "sibling";
    String sibling2_id_string = "sibling2";
    String sibling3_id_string = "sibling3";
    SEXP root_id = wrap(root_id_string);
    SEXP child_id = wrap(child_id_string);
    SEXP child2_id = wrap(child2_id_string);
    SEXP child3_id = wrap(child3_id_string);
    SEXP sibling_id = wrap(sibling_id_string);
    SEXP sibling2_id = wrap(sibling2_id_string);
    SEXP sibling3_id = wrap(sibling3_id_string);
    // Create a gti.
    GeneralTreeInternal gti(root_id, root_id);
    // Add child node.
    gti.add_node(root_id, child_id, child_id);
    // Add fist sibling.
    gti.add_node(root_id, sibling_id, sibling_id);
    // Add child to first sibling.
    gti.add_node(sibling_id, child2_id, child2_id);
    // Add sibling to the last child.
    gti.add_node(sibling_id, sibling2_id, sibling2_id);
    // Add child to last sibling.
    gti.add_node(sibling2_id, child3_id, child3_id);
    gti.add_node(sibling2_id, sibling3_id, sibling3_id);

    int root_uid = gti.find_uid(root_id);
    int child_uid = gti.find_uid(child_id);
    int child2_uid = gti.find_uid(child2_id);
    int child3_uid = gti.find_uid(child3_id);
    int sibling_uid = gti.find_uid(sibling_id);
    int sibling2_uid = gti.find_uid(sibling2_id);
    int sibling3_uid = gti.find_uid(sibling3_id);

    // Verify whether all the getters return the proper result of the tree.
    expect_true(gti.get_parent(child_uid) == root_uid);
    expect_true(gti.get_parent(child3_uid) == sibling2_uid);
    expect_true(gti.get_parent(sibling2_uid) == sibling_uid);
    expect_true(gti.get_parent(sibling3_uid) == sibling2_uid);
  }


}


context("GeneralTreeInternal returns correct values") {
  test_that("we can add several siblings and childeren") {
    String child_id_string = "child";
    String child2_id_string = "child2";
    String child3_id_string = "child3";
    String root_id_string = "root";
    String sibling_id_string = "sibling";
    String sibling2_id_string = "sibling2";
    String sibling3_id_string = "sibling3";
    SEXP root_id = wrap(root_id_string);
    SEXP child_id = wrap(child_id_string);
    SEXP child2_id = wrap(child2_id_string);
    SEXP child3_id = wrap(child3_id_string);
    SEXP sibling_id = wrap(sibling_id_string);
    SEXP sibling2_id = wrap(sibling2_id_string);
    SEXP sibling3_id = wrap(sibling3_id_string);
    // Create a gti.
    GeneralTreeInternal gti(root_id, root_id);
    // Add child node.
    gti.add_node(root_id, child_id, child_id);
    // Add fist sibling.
    gti.add_node(root_id, sibling_id, sibling_id);
    // Add child to first sibling.
    gti.add_node(sibling_id, child2_id, child2_id);
    // Add sibling to the last child.
    gti.add_node(sibling_id, sibling2_id, sibling2_id);
    // Add child to last sibling.
    gti.add_node(sibling2_id, child3_id, child3_id);
    gti.add_node(sibling2_id, sibling3_id, sibling3_id);

    // Verify whether all the getters return the proper result of the tree.
    expect_true(gti.get_value(root_id) == root_id);
    expect_true(gti.get_value(sibling3_id) == sibling3_id);
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

    expect_error(gti.find_uid(sibling_id));
    expect_error(gti.find_child(gti.uid_counter));
    expect_error(gti.get_parent(gti.uid_counter));
  }
}
