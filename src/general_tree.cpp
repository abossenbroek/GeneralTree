// [[Rcpp::depends(BH)]]
#include <Rcpp.h>
#include <boost/bimap.hpp>

#include <map>
#include <utility>

typedef boost::bimap<int, SEXP> uid_SEXP_bimap;
typedef uid_SEXP_bimap::value_type uid_SEXP_pair;

typedef std::pair<int, int> uid_uid_pair;
typedef std::map<int, int> uid_to_uid_map;

typedef std::map<int, std::vector<int> > uid_to_uids_map;


class GeneralTreeInternals {
public:
  uint uid_counter;
  uid_SEXP_bimap uid_to_id;
  uid_SEXP_bimap uid_to_data;
  uid_to_uid_map uid_to_child;
  uid_to_uid_map uid_to_parent;
  uid_to_uids_map uid_to_siblings;

  GeneralTreeInternals(SEXP root_id, SEXP root_data);

  void add_node(SEXP parent, SEXP child, SEXP data);

  int find_uid_given_id(SEXP id);
  int find_uid_given_data(SEXP data);
  int find_child_given_uid(int uid);
  bool has_child(int uid);
  bool has_siblings(int uid);
};


namespace Rcpp {
  /* Convert a bimap tree mapping to a R structure. */
  template <> SEXP wrap(const uid_SEXP_bimap& mapping) {
    std::vector<int> left_vector;
    std::vector<SEXP> right_vector;

    for (uid_SEXP_bimap::left_const_iterator id_iter = mapping.left.begin(),
         iend = mapping.left.end();
         id_iter != iend; ++id_iter) {
      left_vector.push_back(id_iter->first);
      right_vector.push_back(id_iter->second);
    }

    return List::create(Named("left") = wrap(left_vector), Named("right") = wrap(right_vector));
  }

  /* Convert a R structure to a bimap tree mapping. */
  template <> uid_SEXP_bimap as(SEXP t_m_exp) {
    List t_m = as<List>(t_m_exp);
    std::vector<int> left_vector = t_m["left"];
    std::vector<SEXP> right_vector = t_m["right"];
    std::vector<int>::iterator lit;
    std::vector<SEXP>::iterator rit;

    uid_SEXP_bimap result;

    for (lit = left_vector.begin(),
         rit = right_vector.begin();
         lit != left_vector.end();
         ++lit, ++rit) {
      result.insert(uid_SEXP_pair(*lit, *rit));
    }

    return(result);
  }

  template <> SEXP wrap(const GeneralTreeInternals& gti) {
    List lst = List::create();
    lst["uid_counter"] = wrap(gti.uid_counter);
    lst["uid_to_id"] = wrap(gti.uid_to_id);
    lst["uid_to_data"] = wrap(gti.uid_to_data);
    lst["uid_to_child"] = wrap(gti.uid_to_child);
    lst["uid_to_parent"] = wrap(gti.uid_to_parent);
    lst["uid_to_siblings"] = wrap(gti.uid_to_siblings);

    return lst;
  }

}



#define NUMBER_OF_INTERNAL_OBJECTS 5


using namespace Rcpp;

// [[Rcpp::export]]
void
add_child(List gti, SEXP parent_id, SEXP id, SEXP data)
{
}

// [[Rcpp::export]]
List
initialize_tree(SEXP id, SEXP data)
{
  GeneralTreeInternals gti(id, data);

  return wrap(gti);
}

GeneralTreeInternals::GeneralTreeInternals(SEXP root_id, SEXP root_data)
{
  this->uid_counter = 0;

  this->uid_to_id.insert(uid_SEXP_pair(this->uid_counter, root_id));
  this->uid_to_data.insert(uid_SEXP_pair(this->uid_counter, root_data));

  this->uid_counter++;
}

void
GeneralTreeInternals::add_node(SEXP parent_id, SEXP child_id, SEXP data)
{
  // Resolve the uid
  int parent_uid = this->find_uid_given_id(parent_id);
  this->uid_to_id.insert(uid_SEXP_pair(this->uid_counter, child_id));
  this->uid_to_data.insert(uid_SEXP_pair(this->uid_counter, data));
  this->uid_to_parent.insert(uid_uid_pair(this->uid_counter, parent_uid));
  this->uid_counter++;

  // Check whether the parent has a child.
  if (this->has_child(parent_uid)) {
  }



}


int
GeneralTreeInternals::find_uid_given_id(SEXP id)
{
  uid_SEXP_bimap::right_const_iterator id_iter =
    this->uid_to_id.right.find(id);

  if (id_iter == this->uid_to_id.right.end()) {
    throw std::invalid_argument("Could not find id in tree.");
  }

  return id_iter->second;
}

int
GeneralTreeInternals::find_uid_given_data(SEXP data)
{
  uid_SEXP_bimap::right_const_iterator data_iter =
    this->uid_to_data.right.find(data);

  if (data_iter == this->uid_to_id.right.end()) {
    throw std::invalid_argument("Could not find data in tree.");
  }

  return data_iter->second;
}

int
GeneralTreeInternals::find_child_given_uid(int uid)
{
  uid_to_uid_map::iterator child_iter =
    this->uid_to_parent.find(uid);

  if (child_iter == this->uid_to_parent.end()) {
    throw std::invalid_argument("Could not find child in tree.");
  }

  return child_iter->second;
}

bool
GeneralTreeInternals::has_child(int uid) {
  uid_to_uid_map::iterator child_iter =
    this->uid_to_parent.find(uid);

  return child_iter != this->uid_to_parent.end();
}

bool
GeneralTreeInternals::has_siblings(int uid) {
  uid_to_uids_map::iterator sib_iter =
    this->uid_to_siblings.find(uid);

  return sib_iter != this->uid_to_siblings.end();
}
