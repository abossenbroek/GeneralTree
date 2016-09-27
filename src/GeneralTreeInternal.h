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

#include <boost/bimap.hpp>
#include <boost/bimap/multiset_of.hpp>

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
  uid insert_node(tree_node_sp& new_node);
  tree_node_sp last_added_node;

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
  uid get_uid() const;
  SEXP get_data(const SEXP& id) const;
  bool has_child(const SEXP& id) const;
  bool has_siblings(const SEXP& id) const;
  tree_node_sp get_parent(const SEXP& id) const;

  std::shared_ptr<tree_node_sp_vec> get_children(const SEXP& parent_id, bool recursive = false);
  tree_node_c_sp_vec_sp get_children(const SEXP& parent_id, bool recursive = false) const;

  SEXP_vec_sp get_children_keys(const SEXP& parent_id, bool recursive = false) const;
  SEXP_vec_sp get_children_data(const SEXP& parent_id, bool recursive = false) const;

  std::shared_ptr<tree_node_sp_vec> get_siblings(const SEXP& node_id);
  tree_node_c_sp_vec_sp get_siblings(const SEXP& node_id) const;

  SEXP_vec_sp get_siblings_keys(const SEXP& node_id) const;
  SEXP_vec_sp get_siblings_data(const SEXP& node_id) const;

  std::shared_ptr<tree_node_sp_vec> get_branch(const SEXP& node_id);
  tree_node_c_sp_vec_sp get_branch(const SEXP& node_id) const;

  SEXP_vec_sp get_branch_keys(const SEXP& node_id) const;
  SEXP_vec_sp get_branch_data(const SEXP& node_id) const;

  std::shared_ptr<tree_node_sp_vec> get_leafs(const SEXP& node_id);
  tree_node_c_sp_vec_sp get_leafs(const SEXP& node_id) const;

  SEXP_vec_sp get_leafs_keys(const SEXP& node_id) const;
  SEXP_vec_sp get_leafs_data(const SEXP& node_id) const;

  uid travel_up();

  operator SEXP() const;

  tree_node_sp get_root() const {
    return root;
  }

  tree_node_sp_vec* get_nodes() const {
    return const_cast<tree_node_sp_vec*>(&nodes);
  }

  const unsigned int tree_depth() const {
    return root->tree_depth();
  }

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
