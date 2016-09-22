// [[Rcpp::depends(BH)]]
// [[Rcpp::plugins(cpp11)]]
#ifndef _TREENODE_H_
#define _TREENODE_H_

#include "config.h"

#include <Rcpp.h>

#include <map>
#include <vector>

#include <memory>

#define INVALID_UID -1


#include "tree_types.h"

using namespace Rcpp;

class TreeNode : public std::enable_shared_from_this< TreeNode > {
private:
  uid my_uid;
  tree_key key;
  SEXP data;
  std::shared_ptr<TreeNode> left_child;
  std::vector<std::shared_ptr<TreeNode> > siblings;
  std::shared_ptr<TreeNode> parent;

public:
  TreeNode(uid& my_uid_, tree_key& key_, SEXP& data_,
      std::shared_ptr<TreeNode> const& left_child_, std::shared_ptr<TreeNode> const& parent_) :
    my_uid(my_uid_), key(key_), data(data_), left_child(left_child_),
    parent(parent_)
  {}

  TreeNode(tree_key const& key_, SEXP const& data_) :
    my_uid(INVALID_UID), key(key_), data(data_)
  {}

  TreeNode(uid const& my_uid_, tree_key const& key_, SEXP const& data_) :
    my_uid(my_uid_), key(key_), data(data_)
  {}

  TreeNode() :
    my_uid(INVALID_UID)
  {}

  virtual ~TreeNode()
  {
  }

  tree_key get_key() {
    return key;
  }

  SEXP get_data() {
    return data;
  }

  void set_uid(uid const& new_uid) {
    my_uid = new_uid;
  }

  bool has_left_child() {
    return left_child.get() != nullptr;
  }

  bool has_siblings() {
    return siblings.size() != 0;
  }

  void add_sibling(const std::shared_ptr<TreeNode>& new_sibling) {
    siblings.push_back(new_sibling);
  }

  void set_parent(const std::shared_ptr<TreeNode>& parent_) {
    parent = parent_;
  }

  std::shared_ptr<TreeNode> get_parent() {
    if (parent.get() == nullptr) {
      throw std::runtime_error("Node does not have a parent.");
    }
    return parent;
  }

  void add_child(const std::shared_ptr<TreeNode>& new_child);
};

typedef std::shared_ptr<TreeNode> tree_node_sp;
typedef std::vector<tree_node_sp> tree_node_sp_vec;



#endif /* _TREENODE_H_ */