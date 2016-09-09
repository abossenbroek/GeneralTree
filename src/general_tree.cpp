// [[Rcpp::depends(BH)]]
#include <Rcpp.h>
#include <boost/bimap.hpp>

typedef boost::bimap<int, SEXP> tree_mapping;
typedef tree_mapping::value_type tree_map;


typedef struct _GeneralTree {
    tree_mapping uid_to_id;
    tree_mapping uid_to_data;
    std::map <int, int> uid_to_child;
    std::map <int, int> uid_to_parent;
    std::map <int, std::vector<int> > uid_to_siblings;
} GeneralTree;

namespace Rcpp {
  /* Convert a bimap tree mapping to a R structure. */
  template <> SEXP wrap(const tree_mapping& mapping) {
    std::vector<int> left_vector;
    std::vector<SEXP> right_vector;

    for (tree_mapping::left_const_iterator id_iter = mapping.left.begin(),
         iend = mapping.left.end();
         id_iter != iend; ++id_iter) {
      left_vector.push_back(id_iter->first);
      right_vector.push_back(id_iter->second);
    }

    return List::create(Named("left") = wrap(left_vector), Named("right") = wrap(right_vector));
  }

  /* Convert a R structure to a bimap tree mapping. */
  template <> tree_mapping as(SEXP t_m_exp) {
    List t_m = as<List>(t_m_exp);
    std::vector<int> left_vector = t_m["left"];
    std::vector<SEXP> right_vector = t_m["right"];
    std::vector<int>::iterator lit;
    std::vector<SEXP>::iterator rit;

    tree_mapping result;

    for (lit = left_vector.begin(),
         rit = right_vector.begin();
         lit != left_vector.end();
         ++lit, ++rit) {
      result.insert(tree_map(*lit, *rit));
    }

    return(result);
  }



}



#define NUMBER_OF_INTERNAL_OBJECTS 5


using namespace Rcpp;

// [[Rcpp::export]]
void
add_child(List tree_data, SEXP parent_id, SEXP id, SEXP data)
{
  /*
  std::map <int, SEXP> map_id = tree_data[0];
  std::map <int, SEXP> map_data = tree_data[1];
  std::map <int, int> map_left_child = tree_data[2];
  std::map <int, IntegerVector> map_sib = tree_data[3];
  int unique_id = tree_data[4];
*/
  //TODO: detect whether the node already exists. Only required if specified.

}

// [[Rcpp::export]]
  List
initialize_tree(SEXP id, SEXP data)
{
  tree_mapping uid_to_id;
  std::map <int, SEXP> map_data;
  std::map <int, int> map_left_child;
  std::map <int, IntegerVector> map_sib;
  int unique_id = 0;
  unique_id++;

  uid_to_id.insert(tree_map(unique_id, id));
  //map_data.insert(std::pair<int, SEXP> (unique_id, data));

  return List::create(uid_to_id, map_data, map_left_child, map_sib, unique_id) ;
}


