// This file was generated by Rcpp::compileAttributes
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

// initialize_tree
SEXP initialize_tree(SEXP id, SEXP data);
RcppExport SEXP GeneralTree_initialize_tree(SEXP idSEXP, SEXP dataSEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< SEXP >::type id(idSEXP);
    Rcpp::traits::input_parameter< SEXP >::type data(dataSEXP);
    __result = Rcpp::wrap(initialize_tree(id, data));
    return __result;
END_RCPP
}
// deserialize_tree
SEXP deserialize_tree(SEXP tree);
RcppExport SEXP GeneralTree_deserialize_tree(SEXP treeSEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< SEXP >::type tree(treeSEXP);
    __result = Rcpp::wrap(deserialize_tree(tree));
    return __result;
END_RCPP
}
// pass_gti_xptr
SEXP pass_gti_xptr(SEXP gti);
RcppExport SEXP GeneralTree_pass_gti_xptr(SEXP gtiSEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< SEXP >::type gti(gtiSEXP);
    __result = Rcpp::wrap(pass_gti_xptr(gti));
    return __result;
END_RCPP
}
// cmp_gti
LogicalVector cmp_gti(SEXP gti_lhs, SEXP gti_rhs);
RcppExport SEXP GeneralTree_cmp_gti(SEXP gti_lhsSEXP, SEXP gti_rhsSEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< SEXP >::type gti_lhs(gti_lhsSEXP);
    Rcpp::traits::input_parameter< SEXP >::type gti_rhs(gti_rhsSEXP);
    __result = Rcpp::wrap(cmp_gti(gti_lhs, gti_rhs));
    return __result;
END_RCPP
}
// add_node
SEXP add_node(SEXP gti_sexp, SEXP parent_id, SEXP id, SEXP data);
RcppExport SEXP GeneralTree_add_node(SEXP gti_sexpSEXP, SEXP parent_idSEXP, SEXP idSEXP, SEXP dataSEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< SEXP >::type gti_sexp(gti_sexpSEXP);
    Rcpp::traits::input_parameter< SEXP >::type parent_id(parent_idSEXP);
    Rcpp::traits::input_parameter< SEXP >::type id(idSEXP);
    Rcpp::traits::input_parameter< SEXP >::type data(dataSEXP);
    __result = Rcpp::wrap(add_node(gti_sexp, parent_id, id, data));
    return __result;
END_RCPP
}
// delete_node
SEXP delete_node(SEXP gti_sexp, SEXP to_delete);
RcppExport SEXP GeneralTree_delete_node(SEXP gti_sexpSEXP, SEXP to_deleteSEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< SEXP >::type gti_sexp(gti_sexpSEXP);
    Rcpp::traits::input_parameter< SEXP >::type to_delete(to_deleteSEXP);
    __result = Rcpp::wrap(delete_node(gti_sexp, to_delete));
    return __result;
END_RCPP
}
// delete_node_at_ref
SEXP delete_node_at_ref(SEXP gti_sexp);
RcppExport SEXP GeneralTree_delete_node_at_ref(SEXP gti_sexpSEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< SEXP >::type gti_sexp(gti_sexpSEXP);
    __result = Rcpp::wrap(delete_node_at_ref(gti_sexp));
    return __result;
END_RCPP
}
// add_child
SEXP add_child(SEXP gti_sexp, SEXP id, SEXP data);
RcppExport SEXP GeneralTree_add_child(SEXP gti_sexpSEXP, SEXP idSEXP, SEXP dataSEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< SEXP >::type gti_sexp(gti_sexpSEXP);
    Rcpp::traits::input_parameter< SEXP >::type id(idSEXP);
    Rcpp::traits::input_parameter< SEXP >::type data(dataSEXP);
    __result = Rcpp::wrap(add_child(gti_sexp, id, data));
    return __result;
END_RCPP
}
// add_sibling
SEXP add_sibling(SEXP gti_sexp, SEXP id, SEXP data);
RcppExport SEXP GeneralTree_add_sibling(SEXP gti_sexpSEXP, SEXP idSEXP, SEXP dataSEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< SEXP >::type gti_sexp(gti_sexpSEXP);
    Rcpp::traits::input_parameter< SEXP >::type id(idSEXP);
    Rcpp::traits::input_parameter< SEXP >::type data(dataSEXP);
    __result = Rcpp::wrap(add_sibling(gti_sexp, id, data));
    return __result;
END_RCPP
}
// travel_up
SEXP travel_up(SEXP gti_sexp);
RcppExport SEXP GeneralTree_travel_up(SEXP gti_sexpSEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< SEXP >::type gti_sexp(gti_sexpSEXP);
    __result = Rcpp::wrap(travel_up(gti_sexp));
    return __result;
END_RCPP
}
// get_data
SEXP get_data(SEXP gti_sexp, SEXP key);
RcppExport SEXP GeneralTree_get_data(SEXP gti_sexpSEXP, SEXP keySEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< SEXP >::type gti_sexp(gti_sexpSEXP);
    Rcpp::traits::input_parameter< SEXP >::type key(keySEXP);
    __result = Rcpp::wrap(get_data(gti_sexp, key));
    return __result;
END_RCPP
}
// set_key
SEXP set_key(SEXP gti_sexp, SEXP new_key);
RcppExport SEXP GeneralTree_set_key(SEXP gti_sexpSEXP, SEXP new_keySEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< SEXP >::type gti_sexp(gti_sexpSEXP);
    Rcpp::traits::input_parameter< SEXP >::type new_key(new_keySEXP);
    __result = Rcpp::wrap(set_key(gti_sexp, new_key));
    return __result;
END_RCPP
}
// set_data
SEXP set_data(SEXP gti_sexp, SEXP new_data);
RcppExport SEXP GeneralTree_set_data(SEXP gti_sexpSEXP, SEXP new_dataSEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< SEXP >::type gti_sexp(gti_sexpSEXP);
    Rcpp::traits::input_parameter< SEXP >::type new_data(new_dataSEXP);
    __result = Rcpp::wrap(set_data(gti_sexp, new_data));
    return __result;
END_RCPP
}
// copy
SEXP copy(SEXP gti_sexp);
RcppExport SEXP GeneralTree_copy(SEXP gti_sexpSEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< SEXP >::type gti_sexp(gti_sexpSEXP);
    __result = Rcpp::wrap(copy(gti_sexp));
    return __result;
END_RCPP
}
// serialize
SEXP serialize(SEXP gti_sexp);
RcppExport SEXP GeneralTree_serialize(SEXP gti_sexpSEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< SEXP >::type gti_sexp(gti_sexpSEXP);
    __result = Rcpp::wrap(serialize(gti_sexp));
    return __result;
END_RCPP
}
// get_children_keys
std::vector<SEXP> get_children_keys(SEXP gti_sexp, SEXP parent_id, bool recursive);
RcppExport SEXP GeneralTree_get_children_keys(SEXP gti_sexpSEXP, SEXP parent_idSEXP, SEXP recursiveSEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< SEXP >::type gti_sexp(gti_sexpSEXP);
    Rcpp::traits::input_parameter< SEXP >::type parent_id(parent_idSEXP);
    Rcpp::traits::input_parameter< bool >::type recursive(recursiveSEXP);
    __result = Rcpp::wrap(get_children_keys(gti_sexp, parent_id, recursive));
    return __result;
END_RCPP
}
// get_children_data
std::vector<SEXP> get_children_data(SEXP gti_sexp, SEXP parent_id, bool recursive);
RcppExport SEXP GeneralTree_get_children_data(SEXP gti_sexpSEXP, SEXP parent_idSEXP, SEXP recursiveSEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< SEXP >::type gti_sexp(gti_sexpSEXP);
    Rcpp::traits::input_parameter< SEXP >::type parent_id(parent_idSEXP);
    Rcpp::traits::input_parameter< bool >::type recursive(recursiveSEXP);
    __result = Rcpp::wrap(get_children_data(gti_sexp, parent_id, recursive));
    return __result;
END_RCPP
}
// get_siblings_keys
std::vector<SEXP> get_siblings_keys(SEXP gti_sexp, SEXP node_id);
RcppExport SEXP GeneralTree_get_siblings_keys(SEXP gti_sexpSEXP, SEXP node_idSEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< SEXP >::type gti_sexp(gti_sexpSEXP);
    Rcpp::traits::input_parameter< SEXP >::type node_id(node_idSEXP);
    __result = Rcpp::wrap(get_siblings_keys(gti_sexp, node_id));
    return __result;
END_RCPP
}
// get_siblings_data
std::vector<SEXP> get_siblings_data(SEXP gti_sexp, SEXP node_id);
RcppExport SEXP GeneralTree_get_siblings_data(SEXP gti_sexpSEXP, SEXP node_idSEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< SEXP >::type gti_sexp(gti_sexpSEXP);
    Rcpp::traits::input_parameter< SEXP >::type node_id(node_idSEXP);
    __result = Rcpp::wrap(get_siblings_data(gti_sexp, node_id));
    return __result;
END_RCPP
}
// get_branch_data
std::vector<SEXP> get_branch_data(SEXP gti_sexp, SEXP node_id);
RcppExport SEXP GeneralTree_get_branch_data(SEXP gti_sexpSEXP, SEXP node_idSEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< SEXP >::type gti_sexp(gti_sexpSEXP);
    Rcpp::traits::input_parameter< SEXP >::type node_id(node_idSEXP);
    __result = Rcpp::wrap(get_branch_data(gti_sexp, node_id));
    return __result;
END_RCPP
}
// get_branch_keys
std::vector<SEXP> get_branch_keys(SEXP gti_sexp, SEXP node_id);
RcppExport SEXP GeneralTree_get_branch_keys(SEXP gti_sexpSEXP, SEXP node_idSEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< SEXP >::type gti_sexp(gti_sexpSEXP);
    Rcpp::traits::input_parameter< SEXP >::type node_id(node_idSEXP);
    __result = Rcpp::wrap(get_branch_keys(gti_sexp, node_id));
    return __result;
END_RCPP
}
// get_leafs_data
std::vector<SEXP> get_leafs_data(SEXP gti_sexp, SEXP node_id);
RcppExport SEXP GeneralTree_get_leafs_data(SEXP gti_sexpSEXP, SEXP node_idSEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< SEXP >::type gti_sexp(gti_sexpSEXP);
    Rcpp::traits::input_parameter< SEXP >::type node_id(node_idSEXP);
    __result = Rcpp::wrap(get_leafs_data(gti_sexp, node_id));
    return __result;
END_RCPP
}
// get_leafs_keys
std::vector<SEXP> get_leafs_keys(SEXP gti_sexp, SEXP node_id);
RcppExport SEXP GeneralTree_get_leafs_keys(SEXP gti_sexpSEXP, SEXP node_idSEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< SEXP >::type gti_sexp(gti_sexpSEXP);
    Rcpp::traits::input_parameter< SEXP >::type node_id(node_idSEXP);
    __result = Rcpp::wrap(get_leafs_keys(gti_sexp, node_id));
    return __result;
END_RCPP
}
// get_children_keys_at_ref
std::vector<SEXP> get_children_keys_at_ref(SEXP gti_sexp, bool recursive);
RcppExport SEXP GeneralTree_get_children_keys_at_ref(SEXP gti_sexpSEXP, SEXP recursiveSEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< SEXP >::type gti_sexp(gti_sexpSEXP);
    Rcpp::traits::input_parameter< bool >::type recursive(recursiveSEXP);
    __result = Rcpp::wrap(get_children_keys_at_ref(gti_sexp, recursive));
    return __result;
END_RCPP
}
// get_children_data_at_ref
std::vector<SEXP> get_children_data_at_ref(SEXP gti_sexp, bool recursive);
RcppExport SEXP GeneralTree_get_children_data_at_ref(SEXP gti_sexpSEXP, SEXP recursiveSEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< SEXP >::type gti_sexp(gti_sexpSEXP);
    Rcpp::traits::input_parameter< bool >::type recursive(recursiveSEXP);
    __result = Rcpp::wrap(get_children_data_at_ref(gti_sexp, recursive));
    return __result;
END_RCPP
}
// get_siblings_keys_at_ref
std::vector<SEXP> get_siblings_keys_at_ref(SEXP gti_sexp);
RcppExport SEXP GeneralTree_get_siblings_keys_at_ref(SEXP gti_sexpSEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< SEXP >::type gti_sexp(gti_sexpSEXP);
    __result = Rcpp::wrap(get_siblings_keys_at_ref(gti_sexp));
    return __result;
END_RCPP
}
// get_siblings_data_at_ref
std::vector<SEXP> get_siblings_data_at_ref(SEXP gti_sexp);
RcppExport SEXP GeneralTree_get_siblings_data_at_ref(SEXP gti_sexpSEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< SEXP >::type gti_sexp(gti_sexpSEXP);
    __result = Rcpp::wrap(get_siblings_data_at_ref(gti_sexp));
    return __result;
END_RCPP
}
// get_branch_data_at_ref
std::vector<SEXP> get_branch_data_at_ref(SEXP gti_sexp);
RcppExport SEXP GeneralTree_get_branch_data_at_ref(SEXP gti_sexpSEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< SEXP >::type gti_sexp(gti_sexpSEXP);
    __result = Rcpp::wrap(get_branch_data_at_ref(gti_sexp));
    return __result;
END_RCPP
}
// get_branch_keys_at_ref
std::vector<SEXP> get_branch_keys_at_ref(SEXP gti_sexp);
RcppExport SEXP GeneralTree_get_branch_keys_at_ref(SEXP gti_sexpSEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< SEXP >::type gti_sexp(gti_sexpSEXP);
    __result = Rcpp::wrap(get_branch_keys_at_ref(gti_sexp));
    return __result;
END_RCPP
}
// get_leafs_data_at_ref
std::vector<SEXP> get_leafs_data_at_ref(SEXP gti_sexp);
RcppExport SEXP GeneralTree_get_leafs_data_at_ref(SEXP gti_sexpSEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< SEXP >::type gti_sexp(gti_sexpSEXP);
    __result = Rcpp::wrap(get_leafs_data_at_ref(gti_sexp));
    return __result;
END_RCPP
}
// get_leafs_keys_at_ref
std::vector<SEXP> get_leafs_keys_at_ref(SEXP gti_sexp);
RcppExport SEXP GeneralTree_get_leafs_keys_at_ref(SEXP gti_sexpSEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< SEXP >::type gti_sexp(gti_sexpSEXP);
    __result = Rcpp::wrap(get_leafs_keys_at_ref(gti_sexp));
    return __result;
END_RCPP
}
// get_tree_depth_at_ref
int get_tree_depth_at_ref(SEXP gti_sexp);
RcppExport SEXP GeneralTree_get_tree_depth_at_ref(SEXP gti_sexpSEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< SEXP >::type gti_sexp(gti_sexpSEXP);
    __result = Rcpp::wrap(get_tree_depth_at_ref(gti_sexp));
    return __result;
END_RCPP
}
// get_tree_depth
int get_tree_depth(SEXP gti_sexp);
RcppExport SEXP GeneralTree_get_tree_depth(SEXP gti_sexpSEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< SEXP >::type gti_sexp(gti_sexpSEXP);
    __result = Rcpp::wrap(get_tree_depth(gti_sexp));
    return __result;
END_RCPP
}
// have_siblings_at_ref
bool have_siblings_at_ref(SEXP gti_sexp);
RcppExport SEXP GeneralTree_have_siblings_at_ref(SEXP gti_sexpSEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< SEXP >::type gti_sexp(gti_sexpSEXP);
    __result = Rcpp::wrap(have_siblings_at_ref(gti_sexp));
    return __result;
END_RCPP
}
// change_ref
SEXP change_ref(SEXP gti_sexp, int key);
RcppExport SEXP GeneralTree_change_ref(SEXP gti_sexpSEXP, SEXP keySEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< SEXP >::type gti_sexp(gti_sexpSEXP);
    Rcpp::traits::input_parameter< int >::type key(keySEXP);
    __result = Rcpp::wrap(change_ref(gti_sexp, key));
    return __result;
END_RCPP
}
// find_uid
int find_uid(SEXP gti_sexp, SEXP key);
RcppExport SEXP GeneralTree_find_uid(SEXP gti_sexpSEXP, SEXP keySEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< SEXP >::type gti_sexp(gti_sexpSEXP);
    Rcpp::traits::input_parameter< SEXP >::type key(keySEXP);
    __result = Rcpp::wrap(find_uid(gti_sexp, key));
    return __result;
END_RCPP
}
// is_last_sibling_at_ref
bool is_last_sibling_at_ref(SEXP gti_sexp);
RcppExport SEXP GeneralTree_is_last_sibling_at_ref(SEXP gti_sexpSEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< SEXP >::type gti_sexp(gti_sexpSEXP);
    __result = Rcpp::wrap(is_last_sibling_at_ref(gti_sexp));
    return __result;
END_RCPP
}
// is_last_sibling
bool is_last_sibling(SEXP gti_sexp, SEXP key);
RcppExport SEXP GeneralTree_is_last_sibling(SEXP gti_sexpSEXP, SEXP keySEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< SEXP >::type gti_sexp(gti_sexpSEXP);
    Rcpp::traits::input_parameter< SEXP >::type key(keySEXP);
    __result = Rcpp::wrap(is_last_sibling(gti_sexp, key));
    return __result;
END_RCPP
}
// get_data_at_ref
SEXP get_data_at_ref(SEXP gti_sexp);
RcppExport SEXP GeneralTree_get_data_at_ref(SEXP gti_sexpSEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< SEXP >::type gti_sexp(gti_sexpSEXP);
    __result = Rcpp::wrap(get_data_at_ref(gti_sexp));
    return __result;
END_RCPP
}
// get_root
SEXP get_root(SEXP gti_sexp);
RcppExport SEXP GeneralTree_get_root(SEXP gti_sexpSEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< SEXP >::type gti_sexp(gti_sexpSEXP);
    __result = Rcpp::wrap(get_root(gti_sexp));
    return __result;
END_RCPP
}
// get_parent_at_ref
SEXP get_parent_at_ref(SEXP gti_sexp);
RcppExport SEXP GeneralTree_get_parent_at_ref(SEXP gti_sexpSEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< SEXP >::type gti_sexp(gti_sexpSEXP);
    __result = Rcpp::wrap(get_parent_at_ref(gti_sexp));
    return __result;
END_RCPP
}
// get_ref
SEXP get_ref(SEXP gti_sexp);
RcppExport SEXP GeneralTree_get_ref(SEXP gti_sexpSEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< SEXP >::type gti_sexp(gti_sexpSEXP);
    __result = Rcpp::wrap(get_ref(gti_sexp));
    return __result;
END_RCPP
}
