#include <testthat.h>
#include <Rcpp.h>

#include "GeneralTreeInternal.h"

context("GeneralTreeInternal C++ works") {
  test_that("root node can be found in tree.") {
    String root_id_string = "a";
    SEXP root_id = wrap(root_id_string);
    List lst;
    // Create a gti
    GeneralTreeInternal gti(root_id, root_id);
    // Retrieve the unique id.
    int root_uid = gti.find_uid_given_id(root_id);
    // Verify whether it is the correct uid.
    expect_true(root_uid == (gti.uid_counter - 1));
  }
}
