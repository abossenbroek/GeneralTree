// [[Rcpp::depends(BH)]]
#include <Rcpp.h>
#include <boost/bimap.hpp>
#include <boost/bimap/multiset_of.hpp>

#include <boost/any.hpp>

#include <map>
#include <utility>
#include <string>

#include "general_tree.h"

using namespace Rcpp;

// nocov start
// [[Rcpp::export]]
List
add_node(List gti_param, SEXP parent_id, SEXP id, SEXP data)
{
  GeneralTreeInternals gti = as<GeneralTreeInternals>(gti_param);
  gti.add_node(parent_id, id, data);

  return wrap(gti);
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

  this->uid_to_id.insert(uid_id_pair(this->uid_counter, as<std::string>(root_id)));
  this->uid_to_data.insert(uid_SEXP_pair(this->uid_counter, root_data));

  this->uid_counter++;
}

GeneralTreeInternals::GeneralTreeInternals()
{
}

void
GeneralTreeInternals::add_node(SEXP parent_id, SEXP child_id, SEXP data)
{
  // Resolve the uid
  int parent_uid = 0;
  try {
    parent_uid = this->find_uid_given_id(parent_id);
  } catch (std::exception &ex) {
    forward_exception_to_r(ex);
  } catch (...) {
    ::Rf_error("c++ exception (unknown reason)");
  }
  this->uid_to_id.insert(uid_id_pair(this->uid_counter, as<std::string>(child_id)));
  this->uid_to_data.insert(uid_SEXP_pair(this->uid_counter, data));
  this->uid_to_parent.insert(uid_uid_pair(this->uid_counter, parent_uid));

  // Check whether the parent has a child.
  if (this->has_child(parent_uid)) {
    // Find the left child of this parent.
    int left_child = this->find_child_given_uid(parent_uid);
    // Add the new node to the existing child.
    this->add_sibling(left_child, this->uid_counter);
  } else {
    // Set the parent.
    this->uid_to_parent.insert(uid_uid_pair(this->uid_counter, parent_uid));
    // Set the child.
    this->uid_to_child.insert(uid_uid_pair(parent_uid, this->uid_counter));
  }

  // Finally increase the counter.
  this->uid_counter++;
}

int
GeneralTreeInternals::find_uid_given_id(SEXP id)
{
  uid_id_bimap::right_const_iterator id_iter =
    this->uid_to_id.right.find(as<std::string>(id));

  if (id_iter == this->uid_to_id.right.end()) {
    throw std::invalid_argument("Could not find id in tree.");
  }

  return id_iter->second;
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
    this->uid_to_child.find(uid);

  return child_iter != this->uid_to_child.end();
}

bool
GeneralTreeInternals::has_siblings(int uid) {
  uid_to_uids_map::iterator sib_iter =
    this->uid_to_siblings.find(uid);

  return sib_iter != this->uid_to_siblings.end();
}

void
GeneralTreeInternals::add_sibling(int origin_uid, int sibling_uid)
{
  uid_to_uids_map::iterator origin_iter =
    this->uid_to_siblings.find(origin_uid);

  if (origin_iter == this->uid_to_siblings.end()) {
    origin_iter = (this->uid_to_siblings.insert(uid_uids_pair(origin_uid,
          std::vector<int>()))).first;
  }

  origin_iter->second.push_back(sibling_uid);
  int origin_parent_id = this->get_parent(origin_uid);
  this->set_parent(origin_parent_id, sibling_uid);
}


void
GeneralTreeInternals::set_parent(int parent_uid, int child_uid)
{
  uid_to_uid_map::iterator par_iter =
    this->uid_to_parent.find(child_uid);

  // Verify whether we should create a new setting or update the reference.
  if (par_iter == this->uid_to_parent.end()) {
    this->uid_to_parent.insert(uid_uid_pair(child_uid, parent_uid));
  } else {
    par_iter->second = parent_uid;
  }
}

int
GeneralTreeInternals::get_parent(int child_uid)
{
  uid_to_uid_map::iterator par_iter =
    this->uid_to_parent.find(child_uid);

  // Verify whether we should create a new setting or update the reference.
  if (par_iter == this->uid_to_parent.end()) {
    throw std::invalid_argument("Could not find id with this parent id.");
  }

  return par_iter->second;
}


// nocov end
