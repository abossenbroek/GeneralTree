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

class TreeNode;

typedef std::shared_ptr<TreeNode> tree_node_sp;
typedef std::vector<tree_node_sp> tree_node_sp_vec;
typedef std::shared_ptr<tree_node_sp_vec> tree_node_sp_vec_sp;

typedef std::shared_ptr<const TreeNode> tree_node_c_sp;
typedef std::vector<tree_node_c_sp> tree_node_c_sp_vec;
typedef std::shared_ptr<tree_node_c_sp_vec> tree_node_c_sp_vec_sp;

class TreeNode : public std::enable_shared_from_this< TreeNode > {
private:
  uid my_uid;
  SEXP key;
  SEXP data;
  std::shared_ptr<TreeNode> left_child;
  std::vector<std::shared_ptr<TreeNode> > siblings;
  std::shared_ptr<TreeNode> parent;

public:
  TreeNode(const uid& my_uid_, const SEXP& key_, const SEXP& data_,
      std::shared_ptr<TreeNode> const& left_child_, std::shared_ptr<TreeNode> const& parent_) :
    my_uid(my_uid_), key(key_), data(data_), left_child(left_child_),
    parent(parent_)
  {}

  TreeNode(SEXP const& key_, SEXP const& data_) :
    my_uid(INVALID_UID), key(key_), data(data_)
  {}

  TreeNode(uid const& my_uid_, SEXP const& key_, SEXP const& data_) :
    my_uid(my_uid_), key(key_), data(data_)
  {}

  TreeNode() :
    my_uid(INVALID_UID)
  {}

  virtual ~TreeNode()
  {
  }

  SEXP get_key() const {
    return key;
  }

  SEXP get_data() const {
    return data;
  }

  uid get_uid() const {
    return my_uid;
  }

  std::vector<std::shared_ptr<TreeNode> >* get_siblings() {
    return &siblings;
  }

  friend bool operator== (const TreeNode& lhs, const TreeNode& rhs)
  {
    return lhs.get_key() == rhs.get_key() && lhs.get_uid() == rhs.get_uid() &&
      lhs.get_data() == rhs.get_data();
  }

  friend bool operator!= (const TreeNode& lhs, const TreeNode& rhs) {
    return !(lhs == rhs);
  }

  std::shared_ptr<TreeNode> get_parent() const {
    if (parent.get() == nullptr) {
      throw std::runtime_error("Node does not have a parent.");
    }
    return parent;
  }

  std::shared_ptr<TreeNode> get_left_child() const {
    return left_child;
  }

  void set_uid(uid const& new_uid) {
    my_uid = new_uid;
  }

  bool has_left_child() const {
    return left_child.get() != nullptr;
  }

  bool has_siblings() const {
    return siblings.size() != 0;
  }

  void add_sibling(const std::shared_ptr<TreeNode>& new_sibling) {
    siblings.push_back(new_sibling);
  }

  void set_parent(const std::shared_ptr<TreeNode>& parent_) {
    parent = parent_;
  }

  void add_child(const std::shared_ptr<TreeNode>& new_child);

  tree_node_sp_vec_sp get_children(bool recursive = false);
  tree_node_c_sp_vec_sp get_children(bool recursive = false) const;
};

#endif /* _TREENODE_H_ */
