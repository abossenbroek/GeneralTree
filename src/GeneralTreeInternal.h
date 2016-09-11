#ifndef _GENERALTREEINTERNALS_H_
#define _GENERALTREEINTERNALS_H_

#include <Rcpp.h>

#include <map>
#include <vector>

#include <boost/bimap.hpp>
#include <boost/bimap/multiset_of.hpp>

#include <boost/shared_ptr.hpp>

#include <map>
#include <string>
#include <utility>


// nocov start

using namespace Rcpp;

typedef boost::bimap<int, boost::bimaps::multiset_of<std::string> > uid_id_bimap;
typedef uid_id_bimap::value_type uid_id_pair;

typedef std::pair<int, int> uid_uid_pair;
typedef std::map<int, int> uid_to_uid_map;

typedef std::pair<int, SEXP> uid_SEXP_pair;
typedef std::map<int, SEXP> uid_to_SEXP_map;

typedef int uid;

typedef std::vector<uid> uids_vector;

typedef std::map<int, uids_vector> uid_to_uids_map;
typedef std::pair<int, std::vector<int> > uid_uids_pair;

class GeneralTreeInternal;

class GeneralTreeInternal {
public:
  uint uid_counter;
  uid_id_bimap uid_to_id;
  uid_to_SEXP_map uid_to_data;
  uid_to_uid_map uid_to_child;
  uid_to_uid_map uid_to_parent;
  uid_to_uids_map uid_to_siblings;

  GeneralTreeInternal(SEXP root_id, SEXP root_data);
  GeneralTreeInternal();

  void add_node(SEXP parent, SEXP child, SEXP data);

  int find_uid(SEXP id);
  std::string find_key(uid node_uid);
  uid get_lchild(uid parent_uid);
  bool has_child(uid parent_uid);
  bool has_siblings(uid node_uid);
  void add_sibling(uid origin_uid, uid sibling_uid);
  void set_parent(uid parent_uid, uid child_uid);
  uid get_parent(uid child_uid);
  bool has_parent(uid child_uid);
  SEXP get_value(SEXP key);
  bool is_id_in_tree(SEXP id);
  boost::shared_ptr<uids_vector> get_childeren_uid(uid parent_uid);
  boost::shared_ptr<uids_vector> get_siblings_uid(uid node_uid);
  boost::shared_ptr<std::vector<std::string> > get_childeren_keys(uid
      parent_uid);
  boost::shared_ptr<std::vector<std::string> > get_siblings_keys(uid node_uid);


  bool cmp(const GeneralTreeInternal& gti);
};

namespace Rcpp {
//  /* Convert a bimap tree mapping to a R structure. */
//  template <> SEXP wrap(const uid_SEXP_bimap& mapping) {
//    std::vector<int> left_vector;
//    std::vector<SEXP> right_vector;
//
//    for (uid_SEXP_bimap::left_const_iterator id_iter = mapping.left.begin(),
//         iend = mapping.left.end();
//         id_iter != iend; ++id_iter) {
//      left_vector.push_back(id_iter->first);
//      right_vector.push_back(id_iter->second);
//    }
//
//    return List::create(Named("left") = wrap(left_vector), Named("right") = wrap(right_vector));
//  }
//
//  /* Convert a R structure to a bimap tree mapping. */
//  template <> uid_SEXP_bimap as(SEXP t_m_exp) {
//    List t_m = as<List>(t_m_exp);
//    std::vector<int> left_vector = t_m["left"];
//    std::vector<SEXP> right_vector = t_m["right"];
//    std::vector<int>::iterator lit;
//    std::vector<SEXP>::iterator rit;
//
//    uid_SEXP_bimap result;
//
//    for (lit = left_vector.begin(),
//         rit = right_vector.begin();
//         lit != left_vector.end();
//         ++lit, ++rit) {
//      result.insert(uid_SEXP_pair(*lit, *rit));
//    }
//
//    return(result);
//  }
//
//  /* Convert a bimap tree mapping to a R structure. */
//  template <> SEXP wrap(const uid_id_bimap& mapping) {
//    std::vector<int> left_vector;
//    std::vector<std::string> right_vector;
//
//    for (uid_id_bimap::left_const_iterator id_iter = mapping.left.begin(),
//         iend = mapping.left.end();
//         id_iter != iend; ++id_iter) {
//      left_vector.push_back(id_iter->first);
//      right_vector.push_back(id_iter->second);
//    }
//
//    return List::create(Named("left") = wrap(left_vector), Named("right") = wrap(right_vector));
//  }
//
//  /* Convert a R structure to a bimap tree mapping. */
//  template <> uid_id_bimap as(SEXP t_m_exp) {
//    List t_m = as<List>(t_m_exp);
//    std::vector<int> left_vector = t_m["left"];
//    std::vector<std::string> right_vector = t_m["right"];
//    std::vector<int>::iterator lit;
//    std::vector<std::string>::iterator rit;
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
//    std::vector<uids_vector> right_vector = u_u_m["right"];
//    std::vector<int>::iterator lit;
//    std::vector<uids_vector>::iterator rit;
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
}

typedef Rcpp::XPtr<GeneralTreeInternal> gti_xptr;


// nocov end

#endif // _GENERALTREEINTERNALS_H_
