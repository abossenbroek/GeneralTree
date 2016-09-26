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
// cmp
LogicalVector cmp(SEXP gti_lhs, SEXP gti_rhs);
RcppExport SEXP GeneralTree_cmp(SEXP gti_lhsSEXP, SEXP gti_rhsSEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< SEXP >::type gti_lhs(gti_lhsSEXP);
    Rcpp::traits::input_parameter< SEXP >::type gti_rhs(gti_rhsSEXP);
    __result = Rcpp::wrap(cmp(gti_lhs, gti_rhs));
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
