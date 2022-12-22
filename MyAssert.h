#pragma once
#include <llvm/Support/raw_ostream.h>

template <class... T> void DumpValues(T *... t) {
  for (auto v : {t...}) {
    v->dump();
  }
}
#ifndef NDEBUG
#define MyAssert(cond, ...)                                                    \
  {                                                                            \
    if (!(cond)) {                                                               \
      DumpValues(__VA_ARGS__);                                                 \
      llvm::errs() << "Assertion Failed at " << __FILE__ << ":" << __LINE__    \
                   << ' ' << __FUNCTION__ << "\n";                             \
      exit(-1);                                                                \
    }                                                                          \
  }
#else
#define MyAssert(cond, t...)
#endif