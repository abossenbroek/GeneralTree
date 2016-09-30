// [[Rcpp::depends(BH)]]
// [[Rcpp::plugins(cpp11)]]

#ifndef _TREE_TYPES_H
#define _TREE_TYPES_H

#include <RcppCommon.h>
#include <boost/variant.hpp>

#if defined(__clang__)
# pragma clang diagnostic push
# pragma clang diagnostic ignored "-Wredeclared-class-member"
#endif

#include <boost/mpl/int.hpp>
#include <boost/mpl/long.hpp>
#include <boost/mpl/equal_to.hpp>
#include <boost/mpl/assert.hpp>

#include <boost/bimap/bimap.hpp>
#include <boost/bimap/unordered_set_of.hpp>
#include <boost/bimap/list_of.hpp>
#include <boost/bimap/multiset_of.hpp>

#if defined(__clang__)
# pragma clang diagnostic pop
#endif

namespace bb = boost::bimaps;

#include <memory>

//typedef std::string tree_key;
typedef int uid;
typedef boost::variant<double, int, std::string> tree_key;

typedef bb::bimap<bb::set_of< uid >, bb::multiset_of<tree_key> > uid_id_bimap;
typedef uid_id_bimap::value_type uid_id_pair;

typedef std::vector<SEXP> SEXP_vec;
typedef std::vector<tree_key> key_vec;

typedef std::vector<uid> uid_vec;

typedef std::shared_ptr<SEXP_vec> SEXP_vec_sp;
typedef std::shared_ptr<tree_key> tree_key_sp;

#endif // _TREE_TYPES_H
