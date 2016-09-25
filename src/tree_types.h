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

typedef std::vector<SEXP> SEXP_vec;
typedef std::vector<tree_key> key_vec;

typedef std::shared_ptr<SEXP_vec> SEXP_vec_sp;
typedef std::shared_ptr<tree_key> tree_key_sp;


#endif // _TREE_TYPES_H
