// [[Rcpp::depends(BH)]]
// [[Rcpp::plugins(cpp11)]]
#ifndef _TREENODE_H_
#define _TREENODE_H_

#include "config.h"

#include <RcppCommon.h>

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

  TreeNode(SEXP tn);

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

  std::vector<std::shared_ptr<TreeNode> >* get_siblings() const {
    return const_cast<std::vector<std::shared_ptr<TreeNode> >*>(&siblings);
  }

  const uid get_parent_uid() const {
    if (!has_parent())
      return INVALID_UID;

    return parent->get_uid();
  }

  friend bool operator== (const TreeNode& lhs, const TreeNode& rhs)
  {
    bool result = true;

    if (lhs.has_left_child() && rhs.has_left_child()) {
      result = result && *lhs.get_left_child() == *rhs.get_left_child();
    } else if (!lhs.has_left_child() != !rhs.has_left_child()) {
      /* Either lhs or rhs does not have a child so result is false. */
      return false;
    }

    if (lhs.has_siblings() && rhs.has_siblings()) {
      /* Verify whether the size of both are equal. */
      if (lhs.get_siblings()->size() != rhs.get_siblings()->size())
        return false;

      /* Compare all the siblings. */
      for (int i = 0; i < lhs.get_siblings()->size(); ++i)
        result = result && *lhs.get_siblings()->at(i) ==
          *rhs.get_siblings()->at(i);
    } else if (!lhs.has_siblings() != !rhs.has_siblings()) {
      /* Either lhs or rhs does not have siblings so result is false. */
      return false;
    }

    return result && lhs.get_key() == rhs.get_key() && lhs.get_data() == rhs.get_data();
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

  bool has_parent() const {
    return parent.get() != nullptr;
  }

  void set_parent(const std::shared_ptr<TreeNode>& parent_) {
    parent = parent_;
  }

  operator SEXP() const;

  void add_child(const std::shared_ptr<TreeNode>& new_child);

  tree_node_sp_vec_sp get_children(bool recursive = false);
  tree_node_c_sp_vec_sp get_children(bool recursive = false) const;

  tree_node_sp_vec_sp get_tree_siblings();
  tree_node_c_sp_vec_sp get_tree_siblings() const;
};

#endif /* _TREENODE_H_ */
