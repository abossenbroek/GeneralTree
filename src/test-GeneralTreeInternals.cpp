// [[Rcpp::plugins(cpp11)]]
#include "config.h"

#ifdef HAVE_TESTTHAT_H

#include <testthat.h>
#include <Rcpp.h>

#include "tree_types.h"
#include "GeneralTreeInternal.h"

context("GeneralTreeInternal can be created") {
    String root_id_string = "a";
    SEXP root_id = wrap(root_id_string);
    // Create a gti
    GeneralTreeInternal gti(root_id, root_id);
    // Retrieve the unique id.
    int root_uid = gti.find_uid(root_id);
    // Verify whether it is the correct uid.
  test_that("root node can be found in tree") {
    expect_true(root_uid == (gti.uid_counter - 1));
  }
}



context("GeneralTreeInternal can add children directly under each other") {
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
  }
}

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

  test_that("we can add several siblings and children") {
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
  test_that("we can add several siblings and children") {
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

context("GeneralTreeInternal get_children_uid and get_siblings_uid work") {
  test_that("we get the right uids") {
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
    uid_vec first_level = {child_uid, sibling_uid};
    uid_vec siblings_of_child = {sibling_uid};
    uid_vec siblings_of_sibling = {child_uid};


    // Verify whether all the getters return the proper result of the tree.
    expect_true(gti.get_siblings_uid(root_uid)->size() == 0);
    expect_true(gti.get_siblings_uid(child_uid)->size() ==
        gti.get_siblings_uid(sibling_uid)->size());
    expect_true((*gti.get_children_uid(root_uid)) == first_level);
    expect_true((*gti.get_siblings_uid(child_uid)) == siblings_of_child);
    expect_true((*gti.get_siblings_uid(sibling_uid)) == siblings_of_sibling);
  }

  test_that("we get the right keys") {
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
    std::vector<tree_key> first_level = {child_id_string, sibling_id_string};
    std::vector<tree_key> siblings_of_child = {sibling_id_string};
    std::vector<tree_key> siblings_of_sibling = {child_id_string};


    // Verify whether all the getters return the proper result of the tree.
    expect_true(gti.get_siblings_keys(root_uid)->size() == 0);
    expect_true(gti.get_siblings_keys(child_uid)->size() ==
        gti.get_siblings_keys(sibling_uid)->size());
    expect_true((*gti.get_children_keys(root_uid)) == first_level);
    expect_true((*gti.get_siblings_keys(child_uid)) == siblings_of_child);
    expect_true((*gti.get_siblings_keys(sibling_uid)) == siblings_of_sibling);
  }

  test_that("we get the right keys") {
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
    std::vector<SEXP> first_level = {child_id, sibling_id};
    std::vector<SEXP> siblings_of_child = {sibling_id};
    std::vector<SEXP> siblings_of_sibling = {child_id};

    // Verify whether all the getters return the proper result of the tree.
    expect_true(gti.get_siblings_values(root_uid)->size() == 0);
    expect_true(gti.get_siblings_values(child_uid)->size() ==
        gti.get_siblings_values(sibling_uid)->size());
    expect_true((*gti.get_children_values(root_uid)) == first_level);
    expect_true((*gti.get_siblings_values(child_uid)) == siblings_of_child);
    expect_true((*gti.get_siblings_values(sibling_uid)) == siblings_of_sibling);
  }
}

context("GeneralTreeInternal support various key types") {
  test_that("root node can be initiated with integer") {
    int root_id_string = 0;
    SEXP root_id = wrap(root_id_string);
    // Create a gti
    GeneralTreeInternal gti(root_id, root_id);
    // Retrieve the unique id.
    int root_uid = gti.find_uid(root_id);
    // Verify whether it is the correct uid.
    expect_true(root_uid == (gti.uid_counter - 1));
  }

  test_that("root node can be initiated with float") {
    double root_id_string = 0.0;
    SEXP root_id = wrap(root_id_string);
    // Create a gti
    GeneralTreeInternal gti(root_id, root_id);
    // Retrieve the unique id.
    int root_uid = gti.find_uid(root_id);
    // Verify whether it is the correct uid.
    expect_true(root_uid == (gti.uid_counter - 1));
  }

  test_that("we get the right keys using integer") {
    int child_id_int = 0;
    int child2_id_int = 1;
    int child3_id_int = 2;
    int root_id_int = 3;
    int sibling_id_int = 4;
    int sibling2_id_int = 5;
    int sibling3_id_int = 6;
    SEXP root_id = wrap(root_id_int);
    SEXP child_id = wrap(child_id_int);
    SEXP child2_id = wrap(child2_id_int);
    SEXP child3_id = wrap(child3_id_int);
    SEXP sibling_id = wrap(sibling_id_int);
    SEXP sibling2_id = wrap(sibling2_id_int);
    SEXP sibling3_id = wrap(sibling3_id_int);
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
    uid_vec first_level = {child_id_int, sibling_id_int};
    uid_vec siblings_of_child = {sibling_id_int};
    uid_vec siblings_of_sibling = {child_id_int};


    // Verify whether all the getters return the proper result of the tree.
    expect_true(gti.get_siblings_uid(root_uid)->size() == 0);
    expect_true(gti.get_siblings_uid(child_uid)->size() ==
        gti.get_siblings_uid(sibling_uid)->size());

    std::vector<uid> res;
    key_int_visitor* v = new key_int_visitor();

    /*  convert the tree key to integer */
    key_vec first_level_found(*gti.get_children_keys(root_uid));
    res.reserve(first_level_found.size());

    transform(first_level_found.begin(), first_level_found.end(),
        back_inserter(res),
        [&](tree_key& k){ return boost::apply_visitor(*v, k); } );

    /*  test behaviour */
    expect_true(res == first_level);

    /*  convert the tree key to integer */
    key_vec siblings_of_child_level_found(*gti.get_siblings_keys(child_uid));
    res.clear();
    res.reserve(siblings_of_child_level_found.size());

    transform(siblings_of_child_level_found.begin(), siblings_of_child_level_found.end(),
        back_inserter(res),
        [&](tree_key& k){ return boost::apply_visitor(*v, k); } );

    /*  test behaviour */
    expect_true(res == siblings_of_child);


    /*  convert the tree key to integer */
    key_vec siblings_of_sibling_level_found(*gti.get_siblings_keys(sibling_uid));
    res.clear();
    res.reserve(siblings_of_sibling_level_found.size());

    transform(siblings_of_sibling_level_found.begin(),
        siblings_of_sibling_level_found.end(),
        back_inserter(res),
        [&](tree_key& k){ return boost::apply_visitor(*v, k); } );

    /*  test behaviour */
    expect_true(res == siblings_of_sibling);
  }
}

context("GeneralTreeInternal branch information is correctly reported") {
  test_that("number of children is correctly reported") {
    SEXP values[] = {
      NumericVector::create(0),
      NumericVector::create(1),
      NumericVector::create(2),
      NumericVector::create(3),
      NumericVector::create(4),
      NumericVector::create(5),
      NumericVector::create(6),
      NumericVector::create(7)
    };
    // Create a gti.
    GeneralTreeInternal gti(values[0], values[0]);
    // Add child node.
    gti.add_node(values[0], values[1], values[1]);
    gti.add_node(values[0], values[2], values[2]);
    gti.add_node(values[0], values[3], values[3]);
    gti.add_node(values[0], values[4], values[4]);
    gti.add_node(values[0], values[5], values[5]);
    gti.add_node(values[0], values[6], values[6]);
    gti.add_node(values[0], values[7], values[7]);

    int root_uid = gti.find_uid(values[0]);

    // Verify whether all the getters return the proper result of the tree.
    expect_true(gti.count_child_nodes(root_uid) == 7);
  }

  test_that("recursive works for number of children") {
    SEXP values[] = {
      NumericVector::create(0),
      NumericVector::create(1),
      NumericVector::create(2),
      NumericVector::create(3),
      NumericVector::create(4),
      NumericVector::create(5),
      NumericVector::create(6),
      NumericVector::create(7)
    };

    // Create a gti.
    GeneralTreeInternal gti(values[0], values[0]);
    // Add child node.
    gti.add_node(values[0], values[1], values[1]);
    gti.add_node(values[1], values[2], values[2]);
    gti.add_node(values[2], values[3], values[3]);
    gti.add_node(values[2], values[4], values[4]);
    gti.add_node(values[2], values[5], values[5]);
    gti.add_node(values[2], values[6], values[6]);
    gti.add_node(values[2], values[7], values[7]);

    int root_uid = gti.find_uid(values[0]);
    int child1_uid = gti.find_uid(values[1]);
    int child2_uid = gti.find_uid(values[2]);

    // Verify whether all the getters return the proper result of the tree.
    expect_true(gti.count_child_nodes(root_uid) == 1);
    expect_true(gti.count_child_nodes(child1_uid) == 1);
    expect_true(gti.count_child_nodes(child2_uid) == 5);
    expect_true(gti.count_child_nodes(child2_uid) == 5);
    expect_true(gti.count_child_nodes(root_uid, true) == 7);
    expect_false(gti.count_child_nodes(child2_uid, true) == 7);
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
    expect_error(gti.get_lchild(gti.uid_counter));
    expect_error(gti.get_parent(gti.uid_counter));
  }
}

#endif //HAVE_TESTTHAT_H
