#ifndef _ACCESS_FUNCTORS_H_
#define _ACCESS_FUNCTORS_H_
#pragma once

#include <string>

#include "tree_types.h"
#include "TreeNode.h"

struct AccessFunctor {
  virtual tree_node_c_sp_vec_sp tree_accessor(const TreeNode& tn) const = 0;
};

struct AccessChildrenFunctor : public AccessFunctor {
  private:
    bool recursive;

  public:
    AccessChildrenFunctor(bool recursive_) : recursive(recursive_) {}

    tree_node_c_sp_vec_sp tree_accessor(const TreeNode& tn) const {
      return tn.get_children(recursive);
    }
};

struct AccessSiblingsFunctor : public AccessFunctor {
  tree_node_c_sp_vec_sp tree_accessor(const TreeNode& tn) const {
    return tn.get_tree_siblings();
  }
};

struct AccessBranchFunctor : public AccessFunctor {
  tree_node_c_sp_vec_sp tree_accessor(const TreeNode& tn) const {
    return tn.get_branch();
  }
};

struct AccessLeafsFunctor : public AccessFunctor {
  tree_node_c_sp_vec_sp tree_accessor(const TreeNode& tn) const {
    return tn.get_leafs();
  }
};

#endif /* _ACCESS_FUNCTORS_H */
