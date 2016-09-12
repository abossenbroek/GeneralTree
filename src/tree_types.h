// [[Rcpp::depends(BH)]]
// [[Rcpp::plugins(cpp11)]]

#ifndef _TREE_TYPES_H
#define _TREE_TYPES_H

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

typedef std::vector<uid> uids_vector;

typedef std::map<int, uids_vector> uid_to_uids_map;
typedef std::pair<int, std::vector<int> > uid_uids_pair;

typedef std::shared_ptr<std::vector<SEXP> > shared_ptr_SEXP_vec;
typedef std::shared_ptr<std::vector<tree_key> > shared_ptr_key_vec;

#endif // _TREE_TYPES_H
