#include "PointsTo.h"
#include "MyAssert.h"

OperandType GetOperandType(llvm::Value *v) {
  if (llvm::isa<llvm::AllocaInst>(v) || llvm::isa<llvm::CallInst>(v)) {
    return OperandType::StackMemAddr;
  } else if (llvm::isa<llvm::LoadInst>(v) || llvm::isa<llvm::GetElementPtrInst>(v) || llvm::isa<llvm::BitCastInst>(v)) {
    return OperandType::Register;
  } else if (llvm::isa<llvm::Function>(v) || llvm::isa<llvm::ConstantPointerNull>(v)) {
    return OperandType::FnAddr;
  } else {
    llvm::errs() << "invalid operand\n";
    v->dump();
    exit(-1);
  }
}

// Placement(Load, Alloca) -> Value
void addRetValues(MaybeSet &returnPts, llvm::Value *ret_value,
                  PointsToInfo *dfval) {
    returnPts.merge(dfval->GetMaybeRetValue(ret_value));
  
  return;
  if (llvm::isa<llvm::Function>(ret_value)) {
    returnPts.insert(ret_value);
  } else {
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
    for (auto *value : dfval->GetMaybeSet(ret_value)) {
      addRetValues(returnPts, value, dfval);
    }
  }
}
