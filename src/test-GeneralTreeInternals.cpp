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
    String root_id_string = "root";
    String child_id_string = "child";
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
}
