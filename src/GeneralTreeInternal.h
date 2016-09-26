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

class GeneralTreeInternal {
private:
  uid_id_bimap uid_to_key;
  tree_node_sp_vec nodes;
  tree_node_sp root;
  uid insert_node(tree_node_sp& new_node);
  SEXP_vec_sp get_info_from_children(const SEXP& parent_id, bool recursive,
      bool get_key) const;
  SEXP_vec_sp get_info_from_siblings(const SEXP& node_id, bool get_key) const;

public:
  GeneralTreeInternal(const SEXP& root_id, const SEXP& root_data);
  GeneralTreeInternal(const GeneralTreeInternal& to_clone);

  GeneralTreeInternal();
  virtual ~GeneralTreeInternal()
  {}

  uid add_node(const SEXP& parent, const SEXP& child_key, const SEXP& child_data);
  uid add_node(const uid& parent_uid, const SEXP& child_key, const SEXP& child_data);

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

  operator SEXP() const;

  tree_node_sp get_root() const {
    return root;
  }

  tree_node_sp_vec* get_nodes() const {
    return const_cast<tree_node_sp_vec*>(&nodes);
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

  //  /* Convert a R structure to a bimap tree mapping. */
//  template <> uid_id_bimap as(SEXP t_m_exp) {
//    List t_m = as<List>(t_m_exp);
//    std::vector<int> left_vector = t_m["left"];
//    std::vector<tree_key> right_vector = t_m["right"];
//    std::vector<int>::iterator lit;
//    std::vector<tree_key>::iterator rit;
//
//    uid_id_bimap result;
//
//    for (lit = left_vector.begin(),
//         rit = right_vector.begin();
//         lit != left_vector.end();
//         ++lit, ++rit) {
//      result.insert(uid_id_pair(*lit, *rit));
//    }
//
//    return(result);
//  }
//
//  template <> SEXP wrap(const uid_to_uid_map& mapping) {
//    std::vector<int> left_vector;
//    std::vector<int> right_vector;
//
//    for (uid_to_uid_map::const_iterator id_iter = mapping.begin();
//         id_iter != mapping.end(); ++id_iter) {
//      left_vector.push_back(id_iter->first);
//      right_vector.push_back(id_iter->second);
//    }
//
//    return List::create(Named("left") = wrap(left_vector), Named("right") = wrap(right_vector));
//  }
//
//  /* Convert a R structure to a map. */
//  template <> uid_to_uid_map as(SEXP u_u_exp) {
//    List u_u_m = as<List>(u_u_exp);
//    std::vector<int> left_vector = u_u_m["left"];
//    std::vector<int> right_vector = u_u_m["right"];
//    std::vector<int>::iterator lit;
//    std::vector<int>::iterator rit;
//
//    uid_to_uid_map result;
//
//    for (lit = left_vector.begin();
//         lit != left_vector.end();
//         ++lit, ++rit) {
//      result.insert(uid_uid_pair(*lit, *rit));
//    }
//
//    return(result);
//  }
//
//  template <> SEXP wrap(const uid_to_uids_map& mapping) {
//    std::vector<int> left_vector;
//    std::vector<std::vector<int> > right_vector;
//
//    List lst = List::create();
//
//    for (uid_to_uids_map::const_iterator id_iter = mapping.begin();
//         id_iter != mapping.end(); ++id_iter) {
//      left_vector.push_back(id_iter->first);
//      right_vector.push_back(id_iter->second);
//    }
//
//    return List::create(Named("left") = wrap(left_vector), Named("right") = wrap(right_vector));
//  }
//
//  /* Convert a R structure to a map. */
//  template <> uid_to_uids_map as(SEXP u_u_exp) {
//    List u_u_m = as<List>(u_u_exp);
//    std::vector<int> left_vector = u_u_m["left"];
//    std::vector<uid_vec> right_vector = u_u_m["right"];
//    std::vector<int>::iterator lit;
//    std::vector<uid_vec>::iterator rit;
//
//    uid_to_uids_map result;
//
//    for (lit = left_vector.begin();
//         lit != left_vector.end();
//         ++lit, ++rit) {
//      result.insert(uid_uids_pair(*lit, *rit));
//    }
//
//    return(result);
//  }
//
//
//  template <> SEXP wrap(const GeneralTreeInternal& gti) {
//    List lst = List::create();
//    lst["uid_counter"] = wrap(gti.uid_counter);
//    lst["uid_to_id"] = wrap(gti.uid_to_id);
//    lst["uid_to_data"] = wrap(gti.uid_to_data);
//    lst["uid_to_child"] = wrap(gti.uid_to_child);
//    lst["uid_to_parent"] = wrap(gti.uid_to_parent);
//    lst["uid_to_siblings"] = wrap(gti.uid_to_siblings);
//
//    return lst;
//  }
//
//  template <> GeneralTreeInternal as(SEXP gti_exp) {
//    List lst = as<List>(gti_exp);
//
//    GeneralTreeInternal gti;
//    gti.uid_counter = lst["uid_counter"];
//    gti.uid_to_id = as<uid_id_bimap>(lst["uid_to_id"]);
//    gti.uid_to_data = as<uid_SEXP_bimap>(lst["uid_to_data"]);
//    gti.uid_to_child = as<uid_to_uid_map>(lst["uid_to_child"]);
//    gti.uid_to_parent = as<uid_to_uid_map>(lst["uid_to_parent"]);
//    gti.uid_to_siblings = as<uid_to_uids_map>(lst["uid_to_siblings"]);
//
//    return gti;
//  }
//
//}

typedef Rcpp::XPtr<GeneralTreeInternal> gti_xptr;


#endif // _GENERALTREEINTERNALS_H_
