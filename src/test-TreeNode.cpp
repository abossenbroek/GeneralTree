// [[Rcpp::plugins(cpp11)]]
#include "config.h"

#ifdef HAVE_TESTTHAT_H

#include <testthat.h>

#include <Rcpp.h>

#include <memory>
#include <iostream>

#include "tree_types.h"
#include "GeneralTreeInternal.h"

context("GeneralTreeInternal can be created") {
    String root_id_string = "a";
    SEXP root_id = wrap(root_id_string);
    // Create a gti
    GeneralTreeInternal ti(root_id, root_id);
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
  GeneralTreeInternal gti(root_id, root_id);

  tree_node_sp root = gti.find_node(root_id);
  tree_node_c_sp root_c = gti.find_node(root_id);

  test_that("have_siblings yields correct values") {
    expect_false(root->have_siblings());
  }
  test_that("have_left_child yields correct values") {
    expect_false(root->have_left_child());
  }
  test_that("get_parent yields exception") {
    expect_error(root->get_parent());
  }
  test_that("get_tree_siblings yields exception") {
    expect_error(root->get_tree_siblings());
  }
  test_that("const get_tree_siblings yields exception") {
    expect_error(root_c->get_tree_siblings());
  }
  test_that("find_uid yields exception") {
    expect_error(gti.find_uid(child_id));
  }
  test_that("get_root gives correct node") {
    expect_true(gti.get_root() == root);
  }

  gti.add_node(root_id, child_id, child_id);

  test_that("have_siblings yields correct values") {
    expect_false(root->have_siblings());
  }
  test_that("have_left_child yields correct values") {
    expect_true(root->have_left_child());
  }


  gti.add_node(root_id, child2_id, child2_id);

  tree_node_sp child1 = gti.find_node(child_id);

  test_that("have_siblings yields correct values") {
    expect_true(child1->have_siblings());
  }
  test_that("have_left_child yields correct values") {
    expect_false(child1->have_left_child());
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
    GeneralTreeInternal gti(root_id, root_id);
    // Add child node.
    gti.add_node(root_id, child_id, child_id);
    uid root_uid = gti.find_uid(root_id);
    uid child_uid = gti.find_uid(child_id);
    // Verify whether all the getters return the proper result of the tree.
    expect_false(gti.have_siblings(child_id));
    expect_true(gti.has_child(root_id));
    expect_true(gti.get_parent(child_id)->get_data() == root_id);
    expect_false(gti.have_siblings(child_id));
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

    uid root_uid = gti.find_uid(root_id);
    uid child_uid = gti.find_uid(child_id);
    uid child2_uid = gti.find_uid(child2_id);

    // Verify whether all the getters return the proper result of the tree.
    expect_false(gti.have_siblings(child_id));
    expect_true(gti.has_child(root_id));
    expect_true(gti.has_child(child_id));
    expect_false(gti.have_siblings(child2_id));
    expect_true(gti.get_parent(child2_id)->get_data() == child_id);
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
    expect_true(gti.get_data(root_id) == root_id);
    expect_true(gti.get_data(sibling3_id) == sibling3_id);
  }
}

context("GeneralTreeInternal get_branch works correctly") {
    SEXP values[] = {
      NumericVector::create(0),
      NumericVector::create(1),
      NumericVector::create(2),
      NumericVector::create(3),
      NumericVector::create(4),
      NumericVector::create(5),
      NumericVector::create(6),
      NumericVector::create(7),
      NumericVector::create(8),
      NumericVector::create(9)
    };


    // 0
    // \ 1
    //   \ 2
    //     - 3
    //     - 5
    //     | - 6
    //     | | - 8
    //     | | - 9
    //     | \ 7
    //     |
    //     \ 4
    //  Create the tree above.
    GeneralTreeInternal gti(values[0], values[0]);
    // Add child node.
    gti.add_node(values[0], values[1], values[1]);
    gti.add_node(values[1], values[2], values[2]);
    gti.add_node(values[2], values[3], values[3]);
    gti.add_node(values[2], values[5], values[5]);
    gti.add_node(values[5], values[6], values[6]);
    gti.add_node(values[5], values[7], values[7]);
    gti.add_node(values[6], values[8], values[8]);
    gti.add_node(values[6], values[9], values[9]);
    gti.add_node(values[2], values[4], values[4]);

    tree_node_sp_vec added_nodes;
    added_nodes.reserve(10);

    for (auto val : values) {
      added_nodes.push_back(gti.find_node(val));
    }

    tree_node_sp_vec root_branch = {
      added_nodes[0],
      added_nodes[1],
      added_nodes[2],
      added_nodes[3],
      added_nodes[5],
      added_nodes[6],
      added_nodes[8],
      added_nodes[9],
      added_nodes[7],
      added_nodes[4]
    };

    tree_node_c_sp_vec root_branch_c = {
      std::const_pointer_cast<const TreeNode>(added_nodes[0]),
      std::const_pointer_cast<const TreeNode>(added_nodes[1]),
      std::const_pointer_cast<const TreeNode>(added_nodes[2]),
      std::const_pointer_cast<const TreeNode>(added_nodes[3]),
      std::const_pointer_cast<const TreeNode>(added_nodes[5]),
      std::const_pointer_cast<const TreeNode>(added_nodes[6]),
      std::const_pointer_cast<const TreeNode>(added_nodes[8]),
      std::const_pointer_cast<const TreeNode>(added_nodes[9]),
      std::const_pointer_cast<const TreeNode>(added_nodes[7]),
      std::const_pointer_cast<const TreeNode>(added_nodes[4])
    };



    test_that("recursive works when retrieving children list first level") {
      expect_true(*gti.get_branch(values[0]) == root_branch);
    }

    test_that("const recursive works when retrieving children list first level") {
      const GeneralTreeInternal* gti_const = const_cast<const GeneralTreeInternal*>(&gti);
      tree_node_c_sp_vec_sp result = gti_const->get_branch(values[0]);
      expect_true(*result == root_branch_c);
    }

    tree_node_sp_vec branch_five = {
      added_nodes[5],
      added_nodes[6],
      added_nodes[8],
      added_nodes[9],
      added_nodes[7]
    };

    test_that("recursive works when retrieving children list second level") {
      expect_true(*gti.get_branch(values[5]) == branch_five);
    }
}

context("GeneralTreeInternal get_childeren works correctly") {
    SEXP values[] = {
      NumericVector::create(0),
      NumericVector::create(1),
      NumericVector::create(2),
      NumericVector::create(3),
      NumericVector::create(4),
      NumericVector::create(5),
      NumericVector::create(6),
      NumericVector::create(7),
      NumericVector::create(8),
      NumericVector::create(9)
    };


    // 0
    // \ 1
    //   \ 2
    //     - 3
    //     - 5
    //     | - 6
    //     | | - 8
    //     | | - 9
    //     | \ 7
    //     |
    //     \ 4
    //  Create the tree above.
    GeneralTreeInternal gti(values[0], values[0]);
    // Add child node.
    gti.add_node(values[0], values[1], values[1]);
    gti.add_node(values[1], values[2], values[2]);
    gti.add_node(values[2], values[3], values[3]);
    gti.add_node(values[2], values[5], values[5]);
    gti.add_node(values[5], values[6], values[6]);
    gti.add_node(values[5], values[7], values[7]);
    gti.add_node(values[6], values[8], values[8]);
    gti.add_node(values[6], values[9], values[9]);
    gti.add_node(values[2], values[4], values[4]);

    tree_node_sp_vec added_nodes;
    added_nodes.reserve(10);

    for (auto val : values) {
      added_nodes.push_back(gti.find_node(val));
    }

    tree_node_sp_vec level_one_not_recursive = {
      added_nodes[1]
    };

    tree_node_c_sp_vec level_one_not_recursive_c = {
      std::const_pointer_cast<const TreeNode>(added_nodes[1])
    };


    tree_node_sp_vec level_one_recursive = {
      added_nodes[1],
      added_nodes[2],
      added_nodes[3],
      added_nodes[5],
      added_nodes[6],
      added_nodes[8],
      added_nodes[9],
      added_nodes[7],
      added_nodes[4]
    };

    tree_node_c_sp_vec level_one_recursive_c = {
      std::const_pointer_cast<const TreeNode>(added_nodes[1]),
      std::const_pointer_cast<const TreeNode>(added_nodes[2]),
      std::const_pointer_cast<const TreeNode>(added_nodes[3]),
      std::const_pointer_cast<const TreeNode>(added_nodes[5]),
      std::const_pointer_cast<const TreeNode>(added_nodes[6]),
      std::const_pointer_cast<const TreeNode>(added_nodes[8]),
      std::const_pointer_cast<const TreeNode>(added_nodes[9]),
      std::const_pointer_cast<const TreeNode>(added_nodes[7]),
      std::const_pointer_cast<const TreeNode>(added_nodes[4])
    };



    test_that("recursive works when retrieving children list first level") {
      expect_true(*gti.get_children(values[0]) == level_one_not_recursive);
      expect_true(*gti.get_children(values[0], true) == level_one_recursive);
    }

    test_that("const recursive works when retrieving children list first level") {
      const GeneralTreeInternal* gti_const = const_cast<const GeneralTreeInternal*>(&gti);
      tree_node_c_sp_vec_sp result_non_recursive = gti_const->get_children(values[0]);
      tree_node_c_sp_vec_sp result_recursive = gti_const->get_children(values[0], true);
      expect_true(*result_non_recursive == level_one_not_recursive_c);
      expect_true(*result_recursive == level_one_recursive_c);
    }

    tree_node_sp_vec level_two_not_recursive = {
      added_nodes[2]
    };

    tree_node_sp_vec level_two_recursive = {
      added_nodes[2],
      added_nodes[3],
      added_nodes[5],
      added_nodes[6],
      added_nodes[8],
      added_nodes[9],
      added_nodes[7],
      added_nodes[4]
    };

    test_that("recursive works when retrieving children list second level") {
      expect_true(*gti.get_children(values[1]) == level_two_not_recursive);
      expect_true(*gti.get_children(values[1], true) == level_two_recursive);
    }
}

context("GeneralTreeInternal get_childeren_keys works correctly") {
    SEXP values[] = {
      NumericVector::create(0),
      NumericVector::create(1),
      NumericVector::create(2),
      NumericVector::create(3),
      NumericVector::create(4),
      NumericVector::create(5),
      NumericVector::create(6),
      NumericVector::create(7),
      NumericVector::create(8),
      NumericVector::create(9)
    };

    // 0
    // \ 1
    //   \ 2
    //     - 3
    //     - 5
    //     | - 6
    //     | | - 8
    //     | | - 9
    //     | \ 7
    //     |
    //     \ 4
    //  Create the tree above.
    GeneralTreeInternal gti(values[0], values[0]);
    // Add child node.
    gti.add_node(values[0], values[1], values[1]);
    gti.add_node(values[1], values[2], values[2]);
    gti.add_node(values[2], values[3], values[3]);
    gti.add_node(values[2], values[5], values[5]);
    gti.add_node(values[5], values[6], values[6]);
    gti.add_node(values[5], values[7], values[7]);
    gti.add_node(values[6], values[8], values[8]);
    gti.add_node(values[6], values[9], values[9]);
    gti.add_node(values[2], values[4], values[4]);

    SEXP_vec level_one_not_recursive = {
      values[1]
    };

    SEXP_vec level_one_recursive = {
      values[1],
      values[2],
      values[3],
      values[5],
      values[6],
      values[8],
      values[9],
      values[7],
      values[4]
    };

    test_that("recursive works when retrieving children keys first level") {
      expect_true(*gti.get_children_keys(values[0]) == level_one_not_recursive);
      expect_true(*gti.get_children_keys(values[0], true) == level_one_recursive);
    }

    SEXP_vec level_two_not_recursive = {
      values[2]
    };

    SEXP_vec level_two_recursive = {
      values[2],
      values[3],
      values[5],
      values[6],
      values[8],
      values[9],
      values[7],
      values[4]
    };

    test_that("recursive works when retrieving children keys second level") {
      expect_true(*gti.get_children_keys(values[1]) == level_two_not_recursive);
      expect_true(*gti.get_children_keys(values[1], true) == level_two_recursive);
    }
}

context("GeneralTreeInternal get_childeren_data works correctly") {
    SEXP values[] = {
      NumericVector::create(0),
      NumericVector::create(1),
      NumericVector::create(2),
      NumericVector::create(3),
      NumericVector::create(4),
      NumericVector::create(5),
      NumericVector::create(6),
      NumericVector::create(7),
      NumericVector::create(8),
      NumericVector::create(9)
    };

    // 0
    // \ 1
    //   \ 2
    //     - 3
    //     - 5
    //     | - 6
    //     | | - 8
    //     | | - 9
    //     | \ 7
    //     |
    //     \ 4
    //  Create the tree above.
    GeneralTreeInternal gti(values[0], values[0]);
    // Add child node.
    gti.add_node(values[0], values[1], values[1]);
    gti.add_node(values[1], values[2], values[2]);
    gti.add_node(values[2], values[3], values[3]);
    gti.add_node(values[2], values[5], values[5]);
    gti.add_node(values[5], values[6], values[6]);
    gti.add_node(values[5], values[7], values[7]);
    gti.add_node(values[6], values[8], values[8]);
    gti.add_node(values[6], values[9], values[9]);
    gti.add_node(values[2], values[4], values[4]);

    SEXP_vec level_one_not_recursive = {
      values[1]
    };

    SEXP_vec level_one_recursive = {
      values[1],
      values[2],
      values[3],
      values[5],
      values[6],
      values[8],
      values[9],
      values[7],
      values[4]
    };

    test_that("recursive works when retrieving children data first level") {
      expect_true(*gti.get_children_data(values[0]) == level_one_not_recursive);
      expect_true(*gti.get_children_data(values[0], true) == level_one_recursive);
    }

    SEXP_vec level_two_not_recursive = {
      values[2]
    };

    SEXP_vec level_two_recursive = {
      values[2],
      values[3],
      values[5],
      values[6],
      values[8],
      values[9],
      values[7],
      values[4]
    };

    test_that("recursive works when retrieving children data second level") {
      expect_true(*gti.get_children_data(values[1]) == level_two_not_recursive);
      expect_true(*gti.get_children_data(values[1], true) == level_two_recursive);
    }
}

context("GeneralTreeInternal get_siblings works correctly") {
    SEXP values[] = {
      NumericVector::create(0),
      NumericVector::create(1),
      NumericVector::create(2),
      NumericVector::create(3),
      NumericVector::create(4),
      NumericVector::create(5),
      NumericVector::create(6),
      NumericVector::create(7),
      NumericVector::create(8),
      NumericVector::create(9)
    };


    // 0
    // \ 1
    //   \ 2
    //     - 3
    //     - 5
    //     | - 6
    //     | | - 8
    //     | | - 9
    //     | \ 7
    //     |
    //     \ 4
    //  Create the tree above.
    GeneralTreeInternal gti(values[0], values[0]);
    // Add child node.
    gti.add_node(values[0], values[1], values[1]);
    gti.add_node(values[1], values[2], values[2]);
    gti.add_node(values[2], values[3], values[3]);
    gti.add_node(values[2], values[5], values[5]);
    gti.add_node(values[5], values[6], values[6]);
    gti.add_node(values[5], values[7], values[7]);
    gti.add_node(values[6], values[8], values[8]);
    gti.add_node(values[6], values[9], values[9]);
    gti.add_node(values[2], values[4], values[4]);

    tree_node_sp_vec added_nodes;
    added_nodes.reserve(10);

    for (auto val : values) {
      added_nodes.push_back(gti.find_node(val));
    }

    tree_node_sp_vec siblings_three = {
      added_nodes[5],
      added_nodes[4]
    };

    tree_node_c_sp_vec siblings_three_c = {
      std::const_pointer_cast<const TreeNode>(added_nodes[5]),
      std::const_pointer_cast<const TreeNode>(added_nodes[4])
    };


    test_that("get_siblings works on first run") {
      expect_true(*gti.get_siblings(values[3]) == siblings_three);
    }
    test_that("get_siblings works on second run, ensuring nothing is deleted") {
      expect_true(*gti.get_siblings(values[3]) == siblings_three);
    }


    test_that("const get_siblings works") {
      const GeneralTreeInternal* gti_const = const_cast<const GeneralTreeInternal*>(&gti);
      tree_node_c_sp_vec_sp results = gti_const->get_siblings(values[3]);
      expect_true(*results == siblings_three_c);
    }
}

context("GeneralTreeInternal get_siblings_keys works correctly") {
    SEXP values[] = {
      NumericVector::create(0),
      NumericVector::create(1),
      NumericVector::create(2),
      NumericVector::create(3),
      NumericVector::create(4),
      NumericVector::create(5),
      NumericVector::create(6),
      NumericVector::create(7),
      NumericVector::create(8),
      NumericVector::create(9)
    };

    // 0
    // \ 1
    //   \ 2
    //     - 3
    //     - 5
    //     | - 6
    //     | | - 8
    //     | | - 9
    //     | \ 7
    //     |
    //     \ 4
    //  Create the tree above.
    GeneralTreeInternal gti(values[0], values[0]);
    // Add child node.
    gti.add_node(values[0], values[1], values[1]);
    gti.add_node(values[1], values[2], values[2]);
    gti.add_node(values[2], values[3], values[3]);
    gti.add_node(values[2], values[5], values[5]);
    gti.add_node(values[5], values[6], values[6]);
    gti.add_node(values[5], values[7], values[7]);
    gti.add_node(values[6], values[8], values[8]);
    gti.add_node(values[6], values[9], values[9]);
    gti.add_node(values[2], values[4], values[4]);

    SEXP_vec siblings_three = {
      values[5],
      values[4]
    };

    SEXP_vec siblings_five = {
      values[3],
      values[4]
    };

    test_that("get_siblings_keys works correctly on left child") {
      expect_true(*gti.get_siblings_keys(values[3]) == siblings_three);
    }
    test_that("get_siblings_keys works correctly on sibling") {
      expect_true(*gti.get_siblings_keys(values[5]) == siblings_five);
    }
}


context("GeneralTreeInternal get_siblings_data works correctly") {
    SEXP values[] = {
      NumericVector::create(0),
      NumericVector::create(1),
      NumericVector::create(2),
      NumericVector::create(3),
      NumericVector::create(4),
      NumericVector::create(5),
      NumericVector::create(6),
      NumericVector::create(7),
      NumericVector::create(8),
      NumericVector::create(9)
    };

    // 0
    // \ 1
    //   \ 2
    //     - 3
    //     - 5
    //     | - 6
    //     | | - 8
    //     | | - 9
    //     | \ 7
    //     |
    //     \ 4
    //  Create the tree above.
    GeneralTreeInternal gti(values[0], values[0]);
    // Add child node.
    gti.add_node(values[0], values[1], values[1]);
    gti.add_node(values[1], values[2], values[2]);
    gti.add_node(values[2], values[3], values[3]);
    gti.add_node(values[2], values[5], values[5]);
    gti.add_node(values[5], values[6], values[6]);
    gti.add_node(values[5], values[7], values[7]);
    gti.add_node(values[6], values[8], values[8]);
    gti.add_node(values[6], values[9], values[9]);
    gti.add_node(values[2], values[4], values[4]);

    SEXP_vec siblings_three = {
      values[5],
      values[4]
    };

    SEXP_vec siblings_five = {
      values[3],
      values[4]
    };

    test_that("get_siblings_data works correctly on left child") {
      expect_true(*gti.get_siblings_data(values[3]) == siblings_three);
    }
    test_that("get_siblings_data works correctly on sibling") {
      expect_true(*gti.get_siblings_data(values[5]) == siblings_five);
    }
}

context("GeneralTreeInternal get_leafs works correctly") {
    SEXP values[] = {
      NumericVector::create(0),
      NumericVector::create(1),
      NumericVector::create(2),
      NumericVector::create(3),
      NumericVector::create(4),
      NumericVector::create(5),
      NumericVector::create(6),
      NumericVector::create(7),
      NumericVector::create(8),
      NumericVector::create(9)
    };


    // 0
    // \ 1
    //   \ 2
    //     - 3
    //     - 5
    //     | - 6
    //     | | - 8
    //     | | - 9
    //     | \ 7
    //     |
    //     \ 4
    //  Create the tree above.
    GeneralTreeInternal gti(values[0], values[0]);
    // Add child node.
    gti.add_node(values[0], values[1], values[1]);
    gti.add_node(values[1], values[2], values[2]);
    gti.add_node(values[2], values[3], values[3]);
    gti.add_node(values[2], values[5], values[5]);
    gti.add_node(values[5], values[6], values[6]);
    gti.add_node(values[5], values[7], values[7]);
    gti.add_node(values[6], values[8], values[8]);
    gti.add_node(values[6], values[9], values[9]);
    gti.add_node(values[2], values[4], values[4]);

    tree_node_sp_vec added_nodes;
    added_nodes.reserve(10);

    for (auto val : values) {
      added_nodes.push_back(gti.find_node(val));
    }

    tree_node_sp_vec leafs = {
      added_nodes[3],
      added_nodes[8],
      added_nodes[9],
      added_nodes[7],
      added_nodes[4]
    };

    tree_node_c_sp_vec leafs_c = {
      std::const_pointer_cast<const TreeNode>(added_nodes[3]),
      std::const_pointer_cast<const TreeNode>(added_nodes[8]),
      std::const_pointer_cast<const TreeNode>(added_nodes[9]),
      std::const_pointer_cast<const TreeNode>(added_nodes[7]),
      std::const_pointer_cast<const TreeNode>(added_nodes[4])
    };


    test_that("get_leafs work") {
      expect_true(*gti.get_leafs(values[0]) == leafs);
    }

    test_that("const get_leafs works") {
      const GeneralTreeInternal* gti_const = const_cast<const GeneralTreeInternal*>(&gti);
      tree_node_c_sp_vec_sp results = gti_const->get_leafs(values[0]);
      expect_true(*results == leafs_c);
    }
}

context("GeneralTreeInternal get_leafs_keys and get_leafs_data work") {
    SEXP values[] = {
      NumericVector::create(0),
      NumericVector::create(1),
      NumericVector::create(2),
      NumericVector::create(3),
      NumericVector::create(4),
      NumericVector::create(5),
      NumericVector::create(6),
      NumericVector::create(7),
      NumericVector::create(8),
      NumericVector::create(9)
    };

    // 0
    // \ 1
    //   \ 2
    //     - 3
    //     - 5
    //     | - 6
    //     | | - 8
    //     | | - 9
    //     | \ 7
    //     |
    //     \ 4
    //  Create the tree above.
    GeneralTreeInternal gti(values[0], values[0]);
    // Add child node.
    gti.add_node(values[0], values[1], values[1]);
    gti.add_node(values[1], values[2], values[2]);
    gti.add_node(values[2], values[3], values[3]);
    gti.add_node(values[2], values[5], values[5]);
    gti.add_node(values[5], values[6], values[6]);
    gti.add_node(values[5], values[7], values[7]);
    gti.add_node(values[6], values[8], values[8]);
    gti.add_node(values[6], values[9], values[9]);
    gti.add_node(values[2], values[4], values[4]);

    SEXP_vec leafs = {
      values[8],
      values[9],
      values[7]
    };

    test_that("get_leafs_keys works") {
      expect_true(*gti.get_leafs_keys(values[5]) == leafs);
    }

    test_that("get_leafs_data works") {
      expect_true(*gti.get_leafs_data(values[5]) == leafs);
    }
}

context("GeneralTreeInternal get_branch_keys and get_branch_data work") {
    SEXP values[] = {
      NumericVector::create(0),
      NumericVector::create(1),
      NumericVector::create(2),
      NumericVector::create(3),
      NumericVector::create(4),
      NumericVector::create(5),
      NumericVector::create(6),
      NumericVector::create(7),
      NumericVector::create(8),
      NumericVector::create(9)
    };

    // 0
    // \ 1
    //   \ 2
    //     - 3
    //     - 5
    //     | - 6
    //     | | - 8
    //     | | - 9
    //     | \ 7
    //     |
    //     \ 4
    //  Create the tree above.
    GeneralTreeInternal gti(values[0], values[0]);
    // Add child node.
    gti.add_node(values[0], values[1], values[1]);
    gti.add_node(values[1], values[2], values[2]);
    gti.add_node(values[2], values[3], values[3]);
    gti.add_node(values[2], values[5], values[5]);
    gti.add_node(values[5], values[6], values[6]);
    gti.add_node(values[5], values[7], values[7]);
    gti.add_node(values[6], values[8], values[8]);
    gti.add_node(values[6], values[9], values[9]);
    gti.add_node(values[2], values[4], values[4]);

    SEXP_vec branch = {
      values[5],
      values[6],
      values[8],
      values[9],
      values[7]
    };

    test_that("get_branch_keys works") {
      expect_true(*gti.get_branch_keys(values[5]) == branch);
    }

    test_that("get_branch_data works") {
      expect_true(*gti.get_branch_data(values[5]) == branch);
    }
}

context("GeneralTreeInternal copy works correctly") {
    SEXP values[] = {
      NumericVector::create(0),
      NumericVector::create(1),
      NumericVector::create(2),
      NumericVector::create(3),
      NumericVector::create(4),
      NumericVector::create(5),
      NumericVector::create(6),
      NumericVector::create(7),
      NumericVector::create(8),
      NumericVector::create(9)
    };

    // 0
    // \ 1
    //   \ 2
    //     - 3
    //     - 5
    //     | - 6
    //     | | - 8
    //     | | - 9
    //     | \ 7
    //     |
    //     \ 4
    //  Create the tree above.
    GeneralTreeInternal gti(values[0], values[0]);
    // Add child node.
    gti.add_node(values[0], values[1], values[1]);
    gti.add_node(values[1], values[2], values[2]);
    gti.add_node(values[2], values[3], values[3]);
    gti.add_node(values[2], values[5], values[5]);
    gti.add_node(values[5], values[6], values[6]);
    gti.add_node(values[5], values[7], values[7]);
    gti.add_node(values[6], values[8], values[8]);
    gti.add_node(values[6], values[9], values[9]);
    gti.add_node(values[2], values[4], values[4]);

    GeneralTreeInternal gti_copy(gti);

    test_that("Verify whether roots are identical") {
      expect_true(gti.get_root()->get_key() ==
          gti.get_root()->get_key());
      expect_true(gti.get_root()->get_data() ==
          gti.get_root()->get_data());
    }
    test_that("Verify whether keys in tree are identical") {
      expect_true(*gti.get_children_keys(values[0], true) ==
          *gti.get_children_keys(values[0], true));
    }
    test_that("Verify whether data in tree are identical") {
      expect_true(*gti.get_children_data(values[0], true) ==
          *gti.get_children_data(values[0], true));
    }
}

context("Comparison works") {
  SEXP values[] = {
      NumericVector::create(0),
      NumericVector::create(1),
      NumericVector::create(2),
      NumericVector::create(3),
      NumericVector::create(4),
      NumericVector::create(5),
      NumericVector::create(6),
      NumericVector::create(7),
      NumericVector::create(8),
      NumericVector::create(9),
      NumericVector::create(10)
    };

    // 0
    // \ 1
    //   \ 2
    //     - 3
    //     - 5
    //     | - 6
    //     | | - 8
    //     | | - 9
    //     | \ 7
    //     |
    //     \ 4
    //  Create the tree above.
    GeneralTreeInternal gti(values[0], values[0]);
    // Add child node.
    gti.add_node(values[0], values[1], values[1]);
    gti.add_node(values[1], values[2], values[2]);
    gti.add_node(values[2], values[3], values[3]);
    gti.add_node(values[2], values[5], values[5]);
    gti.add_node(values[5], values[6], values[6]);
    gti.add_node(values[5], values[7], values[7]);
    gti.add_node(values[6], values[8], values[8]);
    gti.add_node(values[6], values[9], values[9]);
    gti.add_node(values[2], values[4], values[4]);

    GeneralTreeInternal gti_copy(gti, gti.find_uid(values[0]));

    test_that("Comparison returns true on copies") {
      expect_true(gti == gti_copy);
    }

    gti_copy.add_node(values[0], values[10], values[10]);

    test_that("Comparison returns false on unequal length") {
      expect_false(gti == gti_copy);
    }

    GeneralTreeInternal gti_diff(values[0], values[0]);
    // Add child node.
    gti_diff.add_node(values[0], values[1], values[1]);
    gti_diff.add_node(values[1], values[2], values[2]);
    gti_diff.add_node(values[2], values[3], values[3]);
    gti_diff.add_node(values[2], values[5], values[5]);
    gti_diff.add_node(values[5], values[6], values[6]);
    gti_diff.add_node(values[0], values[7], values[7]);
    gti_diff.add_node(values[6], values[8], values[8]);
    gti_diff.add_node(values[6], values[9], values[9]);
    gti_diff.add_node(values[2], values[4], values[4]);

    test_that("Comparison returns false on unequal structure") {
      expect_false(gti == gti_diff);
    }

    GeneralTreeInternal gti_diff_root(values[10], values[10]);
    // Add child node.
    gti_diff_root.add_node(values[10], values[1], values[1]);
    gti_diff_root.add_node(values[1], values[2], values[2]);
    gti_diff_root.add_node(values[2], values[3], values[3]);
    gti_diff_root.add_node(values[2], values[5], values[5]);
    gti_diff_root.add_node(values[5], values[6], values[6]);
    gti_diff_root.add_node(values[5], values[7], values[7]);
    gti_diff_root.add_node(values[6], values[8], values[8]);
    gti_diff_root.add_node(values[6], values[9], values[9]);
    gti_diff_root.add_node(values[2], values[4], values[4]);

    test_that("Comparison returns false on unequal root") {
      expect_false(gti == gti_diff_root);
    }
}

context("add_child, add_sibling and travel_up work") {
  SEXP values[] = {
    NumericVector::create(0),
    NumericVector::create(1),
    NumericVector::create(2),
    NumericVector::create(3),
    NumericVector::create(4),
    NumericVector::create(5),
    NumericVector::create(6),
    NumericVector::create(7),
    NumericVector::create(8),
    NumericVector::create(9),
    NumericVector::create(10)
  };

  // 0
  // \ 1
  //   \ 2
  //     - 3
  //     - 5
  //     | - 6
  //     | | - 8
  //     | | - 9
  //     | \ 7
  //     |
  //     \ 4
  //  Create the tree above.
  GeneralTreeInternal gti(values[0], values[0]);
  // Add child node.
  gti.add_node(values[0], values[1], values[1]);
  gti.add_node(values[1], values[2], values[2]);
  gti.add_node(values[2], values[3], values[3]);
  gti.add_node(values[2], values[5], values[5]);
  gti.add_node(values[5], values[6], values[6]);
  gti.add_node(values[5], values[7], values[7]);
  gti.add_node(values[6], values[8], values[8]);
  gti.add_node(values[6], values[9], values[9]);
  gti.add_node(values[2], values[4], values[4]);

  test_that("Both trees should be equal") {
    GeneralTreeInternal gti_copy(values[0], values[0]);

    gti_copy.add_child(values[1], values[1]);
    gti_copy.add_child(values[2], values[2]);
    gti_copy.add_child(values[3], values[3]);
    gti_copy.add_sibling(values[5], values[5]);
    gti_copy.add_child(values[6], values[6]);
    gti_copy.add_child(values[8], values[8]);
    gti_copy.add_sibling(values[9], values[9]);
    gti_copy.travel_up();
    gti_copy.add_sibling(values[7], values[7]);
    gti_copy.travel_up();
    gti_copy.add_sibling(values[4], values[4]);

    expect_true(gti == gti_copy);
  }
}

context("Tree depth is correctly reported") {
  SEXP values[] = {
      NumericVector::create(0),
      NumericVector::create(1),
      NumericVector::create(2),
      NumericVector::create(3),
      NumericVector::create(4),
      NumericVector::create(5),
      NumericVector::create(6),
      NumericVector::create(7),
      NumericVector::create(8),
      NumericVector::create(9),
      NumericVector::create(10)
    };

    // 0
    // \ 1
    //   \ 2
    //     - 3
    //     - 5
    //     | - 6
    //     | | - 8
    //     | | - 9
    //     | \ 7
    //     |
    //     \ 4
    //  Create the tree above.
    GeneralTreeInternal gti(values[0], values[0]);

    // Add child node.
    gti.add_node(values[0], values[1], values[1]);

    test_that("Depth is correctly computed for simple tree") {
      expect_true(gti.tree_depth() == 1);
    }

    gti.add_node(values[1], values[2], values[2]);
    gti.add_node(values[2], values[3], values[3]);
    gti.add_node(values[2], values[5], values[5]);
    gti.add_node(values[5], values[6], values[6]);
    gti.add_node(values[5], values[7], values[7]);
    gti.add_node(values[6], values[8], values[8]);
    gti.add_node(values[6], values[9], values[9]);
    gti.add_node(values[2], values[4], values[4]);

    test_that("Depth is correctly computed for simple tree") {
      expect_true(gti.tree_depth() == 5);
    }
}

context("(De)serialization works") {
  SEXP values[] = {
      NumericVector::create(0),
      NumericVector::create(1),
      NumericVector::create(2),
      NumericVector::create(3),
      NumericVector::create(4),
      NumericVector::create(5),
      NumericVector::create(6),
      NumericVector::create(7),
      NumericVector::create(8),
      NumericVector::create(9),
      NumericVector::create(10)
    };

    // 0
    // \ 1
    //   \ 2
    //     - 3
    //     - 5
    //     | - 6
    //     | | - 8
    //     | | - 9
    //     | \ 7
    //     |
    //     \ 4
    //  Create the tree above.
    GeneralTreeInternal gti(values[0], values[0]);
    // Add child node.
    gti.add_node(values[0], values[1], values[1]);
    gti.add_node(values[1], values[2], values[2]);
    gti.add_node(values[2], values[3], values[3]);
    gti.add_node(values[2], values[5], values[5]);
    gti.add_node(values[5], values[6], values[6]);
    gti.add_node(values[5], values[7], values[7]);
    gti.add_node(values[6], values[8], values[8]);
    gti.add_node(values[6], values[9], values[9]);
    gti.add_node(values[2], values[4], values[4]);

    SEXP gti_exp = wrap(gti);
    GeneralTreeInternal gti_copy(gti_exp);

    test_that("Result of serialization and deserialization are equal") {
      expect_true(gti == gti_copy);
    }
}


context("Update works") {
  SEXP values[] = {
    NumericVector::create(0),
    NumericVector::create(1),
    NumericVector::create(2),
    NumericVector::create(3),
    NumericVector::create(4),
    NumericVector::create(5),
    NumericVector::create(6),
    NumericVector::create(7),
    NumericVector::create(8),
    NumericVector::create(9),
    NumericVector::create(10)
  };

  // 0
  // \ 1
  //   \ 2
  //     - 3
  //     - 5
  //     | - 6
  //     | | - 8
  //     | | - 9
  //     | \ 7
  //     |
  //     \ 4
  //  Create the tree above.

  GeneralTreeInternal gti(values[0], values[0]);
  // Add child node.
  gti.add_node(values[0], values[1], values[1]);
  gti.add_node(values[1], values[2], values[2]);
  gti.add_node(values[2], values[3], values[3]);
  gti.add_node(values[2], values[5], values[5]);
  gti.add_node(values[5], values[6], values[6]);
  gti.add_node(values[5], values[7], values[7]);
  gti.add_node(values[6], values[8], values[8]);
  gti.add_node(values[6], values[9], values[9]);
  gti.add_node(values[2], values[4], values[4]);

  expect_true(gti.update_key(values[0], values[10])
      == values[0]);
  expect_true(gti.find_node(values[10])->get_data() == values[0]);

  expect_true(gti.update_data(values[10], values[10])
      == values[0]);
  expect_true(gti.find_node(values[10])->get_data() == values[10]);

  expect_true(gti.find_node(values[2])->get_data() == values[2]);
}


context("set_data and set_key works") {
  SEXP values[] = {
    NumericVector::create(0),
    NumericVector::create(1),
    NumericVector::create(2),
    NumericVector::create(3),
    NumericVector::create(4),
    NumericVector::create(5),
    NumericVector::create(6),
    NumericVector::create(7),
    NumericVector::create(8),
    NumericVector::create(9),
    NumericVector::create(10)
  };

  // 0
  // \ 1
  //   \ 2
  //     - 3
  //     - 5
  //     | - 6
  //     | | - 8
  //     | | - 9
  //     | \ 7
  //     |
  //     \ 4
  //  Create the tree above.
  GeneralTreeInternal gti(values[0], values[0]);
  // Add child node.
  gti.add_child(values[1], values[1]);
  gti.add_child(values[2], values[2]);
  gti.add_child(values[3], values[3]);
  gti.add_sibling(values[5], values[5]);
  gti.add_child(values[6], values[6]);
  gti.add_child(values[8], values[8]);
  gti.add_sibling(values[9], values[9]);
  gti.travel_up();
  gti.add_sibling(values[7], values[7]);
  gti.travel_up();
  gti.add_sibling(values[4], values[4]);
  gti.set_key(values[10]);

  expect_true(gti.find_node(values[10])->get_data() == values[4]);

  gti.set_data(values[10]);

  expect_true(gti.find_node(values[10])->get_data() == values[10]);
}

context("is_last_sibling works") {
  SEXP values[] = {
    NumericVector::create(0),
    NumericVector::create(1),
    NumericVector::create(2),
    NumericVector::create(3),
    NumericVector::create(4),
    NumericVector::create(5),
    NumericVector::create(6),
    NumericVector::create(7),
    NumericVector::create(8),
    NumericVector::create(9),
    NumericVector::create(10)
  };

  // 0
  // \ 1
  //   \ 2
  //     - 3
  //     - 5
  //     | - 6
  //     | | - 8
  //     | | - 9
  //     | \ 7
  //     |
  //     \ 4
  //  Create the tree above.
  GeneralTreeInternal gti(values[0], values[0]);
  // Add child node.
  gti.add_child(values[1], values[1]);
  gti.add_child(values[2], values[2]);
  gti.add_child(values[3], values[3]);
  gti.add_sibling(values[5], values[5]);
  gti.add_child(values[6], values[6]);
  gti.add_child(values[8], values[8]);
  gti.add_sibling(values[9], values[9]);
  gti.travel_up();
  gti.add_sibling(values[7], values[7]);
  gti.travel_up();
  gti.add_sibling(values[4], values[4]);

  expect_true(gti.is_last_sibling(values[4]));
}
#endif
