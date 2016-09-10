#ifndef _GENERALTREEINTERNALS_H_
#define _GENERALTREEINTERNALS_H_

#include <Rcpp.h>

#include <map>
#include <vector>

// nocov start

using namespace Rcpp;

typedef boost::bimap<int, boost::bimaps::multiset_of<SEXP> > uid_SEXP_bimap;
typedef boost::bimap<int, boost::bimaps::multiset_of<std::string> > uid_id_bimap;
typedef uid_SEXP_bimap::value_type uid_SEXP_pair;
typedef uid_id_bimap::value_type uid_id_pair;

typedef std::pair<int, int> uid_uid_pair;
typedef std::map<int, int> uid_to_uid_map;

typedef std::vector<int> uids_list;

typedef std::map<int, uids_list> uid_to_uids_map;
typedef std::pair<int, std::vector<int> > uid_uids_pair;

class GeneralTreeInternal {
public:
  uint uid_counter;
  uid_id_bimap uid_to_id;
  uid_SEXP_bimap uid_to_data;
  uid_to_uid_map uid_to_child;
  uid_to_uid_map uid_to_parent;
  uid_to_uids_map uid_to_siblings;

  GeneralTreeInternal(SEXP root_id, SEXP root_data);
  GeneralTreeInternal();

  void add_node(SEXP parent, SEXP child, SEXP data);

  int find_uid_given_id(SEXP id);
  int find_child_given_uid(int uid);
  bool has_child(int uid);
  bool has_siblings(int uid);
  void add_sibling(int origin_uid, int sibling_uid);
  void set_parent(int parent_uid, int child_uid);
  int get_parent(int child_uid);
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
//    std::vector<uids_list> right_vector = u_u_m["right"];
//    std::vector<int>::iterator lit;
//    std::vector<uids_list>::iterator rit;
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
