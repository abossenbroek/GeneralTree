// [[Rcpp::depends(BH)]]
// [[Rcpp::plugins(cpp11)]]

#ifndef _TREE_TYPES_H
#define _TREE_TYPES_H

#include <Rcpp.h>
#include <boost/variant.hpp>
#include <boost/bimap.hpp>
#include <boost/bimap/multiset_of.hpp>

#include <boost/shared_ptr.hpp>

//typedef std::string tree_key;
typedef int uid;
typedef boost::variant<double, int, std::string> tree_key;

typedef boost::bimap<int, boost::bimaps::multiset_of<tree_key> > uid_id_bimap;
typedef uid_id_bimap::value_type uid_id_pair;

typedef std::pair<int, int> uid_uid_pair;
typedef std::map<int, int> uid_to_uid_map;

typedef std::pair<uid, SEXP> uid_SEXP_pair;
typedef std::map<uid, SEXP> uid_to_SEXP_map;

typedef std::vector<uid> uid_vec;

typedef std::map<int, uid_vec> uid_to_uids_map;
typedef std::pair<int, std::vector<int> > uid_uids_pair;

typedef std::vector<SEXP> SEXP_vec;
typedef std::vector<tree_key> key_vec;

typedef std::vector<const SEXP> SEXP_c_vec;
typedef std::shared_ptr<SEXP_c_vec> SEXP_c_vec_sp;
typedef std::shared_ptr<SEXP_vec> SEXP_vec_sp;

typedef std::shared_ptr<std::vector<SEXP> > shared_ptr_SEXP_vec;
typedef std::shared_ptr<key_vec> shared_ptr_key_vec;

typedef std::shared_ptr<uid_vec> shared_ptr_uid_vec;

#endif // _TREE_TYPES_H
