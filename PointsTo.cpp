#include "PointsTo.h"

// Placement(Load, Alloca) -> Value
void addRetValues(PointsToSet &returnPts, llvm::Value *ret_value,
                         PointsToInfo *dfval) {
  if (llvm::isa<llvm::Function>(ret_value)) {
    returnPts.insert(ret_value);
  } else if (llvm::isa<llvm::LoadInst>(ret_value) ||
             llvm::isa<llvm::AllocaInst>(ret_value)) {
    // Example 1
    // %retval = alloca void (...)*, align 8
    // ...
    // store void (...)* @plus, void (...)** %retval, align 8, !dbg !19
    // %1 = load void (...)*, void (...)** %retval, align 8, !dbg !23
    // ret void (...)* %1, !dbg !23

    // Example 2
    // %p2.addr = alloca void (...)*, align 8
    // %0 = load void (...)*, void (...)** %p2.addr, align 8, !dbg !17
    // ret void (...)* %0, !dbg !18
    //
    // %p2.addr -> {minus}
    // %0 -> {%p2.addr}
    for (auto *value : (*dfval)[ret_value]) {
      addRetValues(returnPts, value, dfval);
    }
  } else {
    llvm::errs() << "TODO! addRetValues\n";
    ret_value->dump();
    exit(-1);
  }
}