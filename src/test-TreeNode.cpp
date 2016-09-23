// [[Rcpp::plugins(cpp11)]]
#include "config.h"

#ifdef HAVE_TESTTHAT_H

#include <testthat.h>
#include <Rcpp.h>

#include <memory>

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

context("TreeInternal returns correct values") {
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
    TreeInternal gti(root_id, root_id);
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
    expect_true(gti.get_data(root_id) == root_id);
    expect_true(gti.get_data(sibling3_id) == sibling3_id);
  }
}

context("TreeInternal get_childeren works correctly") {
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
    TreeInternal gti(values[0], values[0]);
    // Add child node.
    gti.add_node(values[0], values[1], values[1]);
    gti.add_node(values[1], values[2], values[2]);
    gti.add_node(values[2], values[3], values[3]);
    gti.add_node(values[2], values[4], values[4]);
    gti.add_node(values[2], values[5], values[5]);
    gti.add_node(values[5], values[6], values[6]);
    gti.add_node(values[5], values[7], values[7]);

    tree_node_sp_vec added_nodes;
    added_nodes.reserve(8);

    for (auto val : values) {
      added_nodes.push_back(gti.find_node(val));
    }

    tree_node_sp_vec level_one_not_recursive = {
      added_nodes[1]
    };

    tree_node_sp_vec level_one_recursive = {
      added_nodes[1],
      added_nodes[2],
      added_nodes[3],
      added_nodes[4],
      added_nodes[5],
      added_nodes[6],
      added_nodes[7]
    };

    test_that("recursive works when retrieving children list first level") {
      expect_true(*gti.get_children(values[0]) == level_one_not_recursive);
      expect_true(*gti.get_children(values[0], true) == level_one_recursive);
    }

    test_that("const recursive works when retrieving children list first level") {
      const TreeInternal* gti_const = const_cast<const TreeInternal*>(&gti);
      std::shared_ptr<const tree_node_sp_vec> result_non_recursive = gti_const->get_children(values[0]);
      std::shared_ptr<const tree_node_sp_vec> result_recursive = gti_const->get_children(values[0], true);
      expect_true(*result_non_recursive == level_one_not_recursive);
      expect_true(*result_recursive == level_one_recursive);
    }

    tree_node_sp_vec level_two_not_recursive = {
      added_nodes[2]
    };

    tree_node_sp_vec level_two_recursive = {
      added_nodes[2],
      added_nodes[3],
      added_nodes[4],
      added_nodes[5],
      added_nodes[6],
      added_nodes[7]
    };

    test_that("recursive works when retrieving children list second level") {
      expect_true(*gti.get_children(values[1]) == level_two_not_recursive);
      expect_true(*gti.get_children(values[1], true) == level_two_recursive);
    }

}

#endif
