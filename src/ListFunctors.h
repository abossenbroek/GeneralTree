#ifndef _LISTFUNCTORS_H_
#define _LISTFUNCTORS_H_
#pragma once

#include <string>

#include <RcppCommon.h>

using namespace Rcpp;

#include "TreeNode.h"

template<typename T>
struct ListFunctor {
  virtual T Process(const TreeNode& tn) const = 0;
};

struct SEXPGetDataFunctor : public ListFunctor<SEXP> {
  SEXP Process(const TreeNode& tn) const;
};

struct SEXPGetKeyFunctor : public ListFunctor<SEXP> {
  SEXP Process(const TreeNode& tn) const;
};

struct SEXPGetUIDFunctor : public ListFunctor<SEXP> {
  SEXP Process(const TreeNode& tn) const;
};

class SEXPApplyFunctor : public ListFunctor<SEXP> {
private:
  Function f;

public:
  SEXPApplyFunctor(const Function& f_) : f(f_) {}

  SEXP Process(const TreeNode& tn) const;
};

#endif /* _LISTFUNCTOR_H_ */
