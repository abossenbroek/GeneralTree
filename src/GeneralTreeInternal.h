// [[Rcpp::depends(BH)]]
// [[Rcpp::plugins(cpp11)]]
#ifndef _GENERALTREEINTERNALS_H_
#define _GENERALTREEINTERNALS_H_

#include <RcppCommon.h>

#include <vector>
#include <map>
#include <string>
#include <utility>
#include <memory>

#if defined(__clang__)
# pragma clang diagnostic push
# pragma clang diagnostic ignored "-Wredeclared-class-member"
#endif

#include <boost/bimap.hpp>
#include <boost/bimap/multiset_of.hpp>

#if defined(__clang__)
# pragma clang diagnostic pop
#endif

#include "tree_types.h"
#include "key_visitor.h"

#include "TreeNode.h"

struct ListFunctor {
  virtual SEXP Process(const TreeNode& tn) const = 0;
};

struct GetDataFunctor : public ListFunctor {
  SEXP Process(const TreeNode& tn) const {
    return tn.get_data();
  }
};

struct GetKeyFunctor : public ListFunctor {
  SEXP Process(const TreeNode& tn) const {
    return tn.get_key();
  }
};

class GeneralTreeInternal {
private:
  uid_id_bimap uid_to_key;
  tree_node_sp_vec nodes;
  tree_node_sp root;
  uid internal_storage_insert(tree_node_sp& new_node);
  void internal_storage_update(const uid& current_uid, const SEXP& new_key);
  void internal_storage_delete(const uid& to_delete);

  tree_node_sp last_ref_node;

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

  SEXP_vec_sp access_tree_node_vec(const SEXP& node_id, const AccessFunctor& af,
      const ListFunctor& lf) const;

  SEXP_vec_sp access_tree_node_vec(const AccessFunctor& af, const ListFunctor& lf) const;


public:
  GeneralTreeInternal(const SEXP& root_id, const SEXP& root_data);
  GeneralTreeInternal(const GeneralTreeInternal& to_clone);

  GeneralTreeInternal();

  GeneralTreeInternal(SEXP gti);

  virtual ~GeneralTreeInternal()
  {}

  uid add_node(const SEXP& parent, const SEXP& child_key, const SEXP& child_data);
  uid add_node(const uid& parent_uid, const SEXP& child_key, const SEXP& child_data);
  uid add_child(const SEXP& child_key, const SEXP& child_data);
  uid add_sibling(const SEXP& sibling_key, const SEXP& sibling_data);

  uid find_uid(const SEXP& id) const;
  tree_node_sp find_node(const SEXP& id) const;
  tree_node_sp find_node(const uid& uid_) const;
  //TODO: change name of method.
  uid get_uid() const;
  SEXP get_data(const SEXP& id) const;
  const SEXP get_data() const;
  bool has_child(const SEXP& id) const;
  bool have_siblings(const SEXP& id) const;
  const tree_node_sp get_parent(const SEXP& id) const;
  const tree_node_sp get_parent() const;

  const tree_node_sp get_ref() const;
  void change_ref(const uid& new_uid);

  std::shared_ptr<tree_node_sp_vec> get_children(const SEXP& parent_id, bool recursive = false);
  tree_node_c_sp_vec_sp get_children(const SEXP& parent_id, bool recursive = false) const;

  std::shared_ptr<tree_node_sp_vec> get_siblings(const SEXP& node_id);
  tree_node_c_sp_vec_sp get_siblings(const SEXP& node_id) const;

  std::shared_ptr<tree_node_sp_vec> get_branch(const SEXP& node_id);
  tree_node_c_sp_vec_sp get_branch(const SEXP& node_id) const;

  std::shared_ptr<tree_node_sp_vec> get_leafs(const SEXP& node_id);
  tree_node_c_sp_vec_sp get_leafs(const SEXP& node_id) const;

  SEXP_vec_sp get_children_keys(const SEXP& parent_id, bool recursive = false) const
  {
    return access_tree_node_vec(parent_id, AccessChildrenFunctor(recursive),
        GetKeyFunctor());
  }

  SEXP_vec_sp get_children_data(const SEXP& parent_id, bool recursive = false) const
  {
    return access_tree_node_vec(parent_id, AccessChildrenFunctor(recursive),
        GetDataFunctor());
  }

  SEXP_vec_sp get_children_keys(bool recursive = false) const
  {
    return access_tree_node_vec(AccessChildrenFunctor(recursive),
        GetKeyFunctor());
  }

  SEXP_vec_sp get_children_data(bool recursive = false) const
  {
    return access_tree_node_vec(AccessChildrenFunctor(recursive),
        GetDataFunctor());
  }

  SEXP_vec_sp get_siblings_keys(const SEXP& node_id) const
  {
    return access_tree_node_vec(node_id, AccessSiblingsFunctor(),
        GetKeyFunctor());
  }

  SEXP_vec_sp get_siblings_data(const SEXP& node_id) const
  {
    return access_tree_node_vec(node_id, AccessSiblingsFunctor(),
        GetDataFunctor());
  }

  SEXP_vec_sp get_siblings_keys() const
  {
    return access_tree_node_vec(AccessSiblingsFunctor(), GetKeyFunctor());
  }

  SEXP_vec_sp get_siblings_data() const
  {
    return access_tree_node_vec(AccessSiblingsFunctor(), GetDataFunctor());
  }

  SEXP_vec_sp get_branch_keys(const SEXP& node_id) const
  {
    return access_tree_node_vec(node_id, AccessBranchFunctor(),
        GetKeyFunctor());
  }

  SEXP_vec_sp get_branch_data(const SEXP& node_id) const
  {
    return access_tree_node_vec(node_id, AccessBranchFunctor(),
        GetDataFunctor());
  }

  SEXP_vec_sp get_branch_keys() const
  {
    return access_tree_node_vec(AccessBranchFunctor(), GetKeyFunctor());
  }

  SEXP_vec_sp get_branch_data() const
  {
    return access_tree_node_vec(AccessBranchFunctor(), GetDataFunctor());
  }

  SEXP_vec_sp get_leafs_keys(const SEXP& node_id) const
  {
    return access_tree_node_vec(node_id, AccessLeafsFunctor(),
        GetKeyFunctor());
  }

  SEXP_vec_sp get_leafs_data(const SEXP& node_id) const
  {
    return access_tree_node_vec(node_id, AccessLeafsFunctor(),
        GetDataFunctor());
  }

  SEXP_vec_sp get_leafs_keys() const
  {
    return access_tree_node_vec(AccessLeafsFunctor(), GetKeyFunctor());
  }

  SEXP_vec_sp get_leafs_data() const
  {
    return access_tree_node_vec(AccessLeafsFunctor(), GetDataFunctor());
  }

  void set_key(const SEXP& new_key);
  void set_data(const SEXP& new_data);

  uid travel_up();

  operator SEXP() const;

  const uid delete_node(const SEXP& node_id);
  const uid delete_node();

  tree_node_sp get_root() const {
    return root;
  }

  const bool have_siblings() const {
    return last_ref_node->have_tree_siblings();
  }

  tree_node_sp_vec* get_nodes() const {
    return const_cast<tree_node_sp_vec*>(&nodes);
  }

  const unsigned int tree_depth() const {
    return root->tree_depth();
  }

  const unsigned int tree_depth_at_ref() const {
    return last_ref_node->tree_depth();
  }

  const bool is_last_sibling(const SEXP& id) const;
  const bool is_last_sibling() const;
  const bool is_last_sibling(const tree_node_sp& tn) const;

  friend bool operator== (const GeneralTreeInternal& lhs,
      const GeneralTreeInternal& rhs)
  {
    bool result = true;

    if (*lhs.get_root() != *rhs.get_root())
      return false;


    tree_node_c_sp_vec_sp lhs_tree =
      std::const_pointer_cast<const TreeNode>(lhs.get_root())->get_children(true);
    tree_node_c_sp_vec_sp rhs_tree =
      std::const_pointer_cast<const TreeNode>(rhs.get_root())->get_children(true);

    if (lhs_tree->size() != rhs_tree->size())
      return false;

    for (int i = 0; i < lhs_tree->size(); ++i)
      result = result && *lhs_tree->at(i) == *rhs_tree->at(i);

    return result;
  }

  friend bool operator!= (const GeneralTreeInternal& lhs, const
      GeneralTreeInternal& rhs) {
    return !(lhs == rhs);
  }
};

typedef Rcpp::XPtr<GeneralTreeInternal> gti_xptr;

#endif // _GENERALTREEINTERNALS_H_
