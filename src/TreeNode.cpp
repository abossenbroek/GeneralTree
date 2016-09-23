#include "TreeNode.h"
#include <Rcpp.h>

void
TreeNode::add_child(const std::shared_ptr<TreeNode>& new_child) {
  new_child->set_parent(shared_from_this());

  if (has_left_child()) {
    left_child->add_sibling(new_child);
  } else {
    left_child = new_child;
  }
}

tree_node_sp_vec_sp
TreeNode::get_children(bool recursive) {
  /* Will be used to reserve the size of the final array. */
  size_t num_children = 0;
  /* Create a vector to store the results */
  tree_node_sp_vec_sp children(new tree_node_sp_vec());

  /* If the curent node has a child we add it to the list. */
  if (has_left_child()) {
    children->push_back(left_child);

    /* If the child has siblings we add those to the list too. */
    if (left_child->has_siblings()) {
      tree_node_sp_vec* siblings = left_child->get_siblings();
      num_children = 1 + siblings->size();

      children->reserve(num_children);
      children->insert(end(*children), begin(*siblings), end(*siblings));
    }

    /* Finally, if we were called recursively we need to call the function on
     * all child nodes. */
    if (recursive) {
      for (auto c : *children) {
        /* If c has children we call the function and add the results. */
        if (c->has_left_child()) {
          tree_node_sp_vec_sp sub_children = c->get_children(recursive);
          num_children += sub_children->size();
          children->reserve(num_children);
          children->insert(end(*children), begin(*sub_children), end(*sub_children));
        }
      }
    }
  }

  return children;
}
