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
  String root_id_string = "root";
  SEXP root_id = wrap(root_id_string);
  SEXP child_id = wrap(child_id_string);
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
}


#endif
