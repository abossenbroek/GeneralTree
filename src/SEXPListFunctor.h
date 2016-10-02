#ifndef _SEXPLISTFUNCTOR_H_
#define _SEXPLISTFUNCTOR_H_
#pragma once

#include <RcppCommon.h>

using namespace Rcpp;

#include "TreeNode.h"
struct SEXPListFunctor {
  virtual SEXP Process(const TreeNode& tn) const = 0;
};

struct GetDataFunctor : public SEXPListFunctor {
  SEXP Process(const TreeNode& tn) const;
};

struct GetKeyFunctor : public SEXPListFunctor {
  SEXP Process(const TreeNode& tn) const;
};

struct GetUIDFunctor : public SEXPListFunctor {
  SEXP Process(const TreeNode& tn) const;
};

class ApplyFunctor : public SEXPListFunctor {
private:
  Function f;

public:
  ApplyFunctor(const Function& f_) : f(f_) {}

  SEXP Process(const TreeNode& tn) const;
};


#endif
