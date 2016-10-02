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
#include "SEXPListFunctor.h"

#include "TreeNode.h"

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

  SEXP_vec_sp access_tree_node_vec(const SEXP& node_key,
      const AccessFunctor& af, const SEXPListFunctor& lf) const;

  SEXP_vec_sp access_tree_node_vec(const AccessFunctor& af, const
      SEXPListFunctor& lf) const;


public:
  GeneralTreeInternal(const SEXP& root_key, const SEXP& root_data);
  GeneralTreeInternal(GeneralTreeInternal& to_clone, const uid& new_root_uid);

  GeneralTreeInternal();

  GeneralTreeInternal(SEXP gti);

  virtual ~GeneralTreeInternal()
  {}

  uid add_node(const SEXP& parent, const SEXP& child_key, const SEXP& child_data);
  uid add_node(const uid& parent_uid, const SEXP& child_key, const SEXP& child_data);
  uid add_child(const SEXP& child_key, const SEXP& child_data);
  uid add_sibling(const SEXP& sibling_key, const SEXP& sibling_data);

  uid find_uid(const SEXP& key) const;
  tree_node_sp find_node(const SEXP& key) const;
  tree_node_sp find_node(const uid& uid_) const;
  //TODO: change name of method.
  uid get_uid() const;
  SEXP get_data(const SEXP& key) const;
  const SEXP get_data() const;
  bool has_child(const SEXP& key) const;
  bool have_siblings(const SEXP& key) const;
  const tree_node_sp get_parent(const SEXP& key) const;
  const tree_node_sp get_parent() const;
  SEXP update_key(const SEXP& old_key, const SEXP& new_key);
  SEXP update_key(const uid& uid_, const SEXP& new_key);
  SEXP update_key(const SEXP& new_key);

  SEXP update_data(const SEXP& key, const SEXP& new_data);
  SEXP update_data(const uid& uid_, const SEXP& new_data);
  SEXP update_data(const SEXP& new_data);

  const tree_node_sp get_ref() const;
  void change_ref(const uid& new_uid);

  std::shared_ptr<tree_node_sp_vec> get_children(const SEXP& parent_key, bool recursive = false);
  tree_node_c_sp_vec_sp get_children(const SEXP& parent_key, bool recursive = false) const;

  std::shared_ptr<tree_node_sp_vec> get_siblings(const SEXP& node_key);
  tree_node_c_sp_vec_sp get_siblings(const SEXP& node_key) const;

  std::shared_ptr<tree_node_sp_vec> get_branch(const SEXP& node_key);
  tree_node_c_sp_vec_sp get_branch(const SEXP& node_key) const;

  std::shared_ptr<tree_node_sp_vec> get_leafs(const SEXP& node_key);
  tree_node_c_sp_vec_sp get_leafs(const SEXP& node_key) const;

  void clean_internal_storage();

  SEXP_vec_sp get_children_keys(const SEXP& parent_key, bool recursive = false)
    const;
  SEXP_vec_sp get_children_data(const SEXP& parent_key, bool recursive = false)
    const;
  SEXP_vec_sp get_children_keys(bool recursive = false) const;
  SEXP_vec_sp get_children_data(bool recursive = false) const;
  SEXP_vec_sp get_siblings_keys(const SEXP& node_key) const;
  SEXP_vec_sp get_siblings_data(const SEXP& node_key) const;
  SEXP_vec_sp get_siblings_keys() const;
  SEXP_vec_sp get_siblings_data() const;
  SEXP_vec_sp get_branch_keys(const SEXP& node_key) const;
  SEXP_vec_sp get_branch_data(const SEXP& node_key) const;
  SEXP_vec_sp get_branch_uids() const;
  SEXP_vec_sp get_branch_keys() const;
  SEXP_vec_sp get_branch_data() const;
  SEXP_vec_sp get_leafs_keys(const SEXP& node_key) const;
  SEXP_vec_sp get_leafs_data(const SEXP& node_key) const;
  SEXP_vec_sp get_leafs_keys() const;
  SEXP_vec_sp get_leafs_data() const;

  SEXP_vec_sp apply_branch(const Function& f) const;

  void set_key(const SEXP& new_key);
  void set_data(const SEXP& new_data);

  uid travel_up();

  operator SEXP() const;

  const uid delete_node(const SEXP& node_key);
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

  const unsigned int tree_depth() const;

  const unsigned int tree_depth_at_ref() const {
    return last_ref_node->tree_depth();
  }

  const bool is_last_sibling(const SEXP& key) const;
  const bool is_last_sibling() const;
  const bool is_last_sibling(const tree_node_sp& tn) const;

  friend bool operator== (const GeneralTreeInternal& lhs,
      const GeneralTreeInternal& rhs);
  friend bool operator!= (const GeneralTreeInternal& lhs, const
      GeneralTreeInternal& rhs);
};

typedef Rcpp::XPtr<GeneralTreeInternal> gti_xptr;

#endif // _GENERALTREEINTERNALS_H_
