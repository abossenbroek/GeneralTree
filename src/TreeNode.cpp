#include "TreeNode.h"
#include <Rcpp.h>

void TreeNode::add_child(const std::shared_ptr<TreeNode>& new_child) {
  new_child->set_parent(shared_from_this());

  if (has_left_child()) {
    left_child->add_sibling(new_child);
  } else {
    left_child = new_child;
  }
}

