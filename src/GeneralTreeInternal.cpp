// [[Rcpp::depends(BH)]]
#include <Rcpp.h>

#include "string"

#include "GeneralTreeInternal.h"


using namespace Rcpp;
using namespace std;

// [[Rcpp::export]]
SEXP
initialize_tree(SEXP id, SEXP data)
{
  GeneralTreeInternal* gti = new GeneralTreeInternal(id, data);
  gti_xptr p(gti, true);

  return p;
}

// [[Rcpp::export]]
SEXP
pass_gti_xptr(SEXP gti)
{
  gti_xptr p(gti);

  return p;
}

// [[Rcpp::export]]
LogicalVector
cmp(SEXP gti_lhs, SEXP gti_rhs)
{
  gti_xptr lhs(gti_lhs);
  gti_xptr rhs(gti_rhs);

  return lhs->cmp(*(GeneralTreeInternal*)rhs);
}

GeneralTreeInternal::GeneralTreeInternal(SEXP root_id, SEXP root_data)
{
  this->uid_counter = 0;
  this->uid_to_id.insert(uid_id_pair(this->uid_counter, as<string>(root_id)));
  this->uid_to_data.insert(uid_SEXP_pair(this->uid_counter, root_data));

  this->uid_counter++;
}

GeneralTreeInternal::GeneralTreeInternal()
{
}

void
GeneralTreeInternal::add_node(SEXP parent_id, SEXP child_id, SEXP data)
{
  // Resolve the uid
  int parent_uid = 0;
  try {
    parent_uid = this->find_uid(parent_id);
  } catch (std::exception &ex) {
    forward_exception_to_r(ex);
  } catch (...) {
    ::Rf_error("c++ exception (unknown reason)");
  }
  this->uid_to_id.insert(uid_id_pair(this->uid_counter, as<string>(child_id)));
  this->uid_to_data.insert(uid_SEXP_pair(this->uid_counter, data));
  this->uid_to_parent.insert(uid_uid_pair(this->uid_counter, parent_uid));

  // Check whether the parent has a child.
  if (this->has_child(parent_uid)) {
    // Find the left child of this parent.
    int left_child = this->find_child(parent_uid);
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
GeneralTreeInternal::find_uid(SEXP id)
{
  uid_id_bimap::right_const_iterator id_iter =
    this->uid_to_id.right.find(as<string>(id));

  if (id_iter == this->uid_to_id.right.end()) {
    throw std::invalid_argument("Could not find id in tree.");
  }

  return id_iter->second;
}

int
GeneralTreeInternal::find_child(int parent_uid)
{
  uid_to_uid_map::iterator child_iter =
    this->uid_to_child.find(parent_uid);

  if (child_iter == this->uid_to_child.end()) {
    throw std::invalid_argument("Could not find child in tree.");
  }

  return child_iter->second;
}

bool
GeneralTreeInternal::has_child(int parent_uid) {

  uid_to_uid_map::iterator child_iter =
    this->uid_to_child.find(parent_uid);

  return child_iter != this->uid_to_child.end();
}

bool
GeneralTreeInternal::has_siblings(int uid) {

  // First try to find whether this node is present in uid_to_siblings.
  uid_to_uids_map::iterator sib_iter =
    this->uid_to_siblings.find(uid);

  if (sib_iter != this->uid_to_siblings.end()) {
    return true;
  }


  if (has_parent(uid)) {
    int parent_uid = get_parent(uid);
    int child = find_child(parent_uid);

    // If the child has a different uid than the one we got passed we can safely
    // conclude that this node has a sibling.
    return child != uid;
  }

  return false;
}

void
GeneralTreeInternal::add_sibling(int origin_uid, int sibling_uid)
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
GeneralTreeInternal::set_parent(int parent_uid, int child_uid)
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
GeneralTreeInternal::get_parent(int child_uid)
{
  uid_to_uid_map::iterator par_iter =
    this->uid_to_parent.find(child_uid);

  // Verify whether we should create a new setting or update the reference.
  if (par_iter == this->uid_to_parent.end()) {
    throw std::invalid_argument("Could not find id with this parent id.");
  }

  return par_iter->second;
}

bool
GeneralTreeInternal::has_parent(int child_uid)
{
  uid_to_uid_map::iterator par_iter =
    this->uid_to_parent.find(child_uid);

  return par_iter != this->uid_to_parent.end();
}

bool
GeneralTreeInternal::cmp(const GeneralTreeInternal& gti)
{
  bool status = true;

  status = status && (this->uid_counter == gti.uid_counter);

  // TODO: finish comparison implementation.
  return status;
}
