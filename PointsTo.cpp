#include "PointsTo.h"
#include "MyAssert.h"

Operand::OperandType Operand::GetOperandType() const {
  if (isHeapAddr()) {
    return OperandType::HeapMemAddr;
  } else if (llvm::isa<llvm::AllocaInst>(v)) {
    return OperandType::StackMemAddr;
  } else if (llvm::isa<llvm::LoadInst>(v) || llvm::isa<llvm::GetElementPtrInst>(v) || llvm::isa<llvm::BitCastInst>(v) ||
             llvm::isa<llvm::CallInst>(v)) {
    return OperandType::Register;
  } else if (llvm::isa<llvm::Function>(v) || llvm::isa<llvm::ConstantPointerNull>(v)) {
    return OperandType::FnAddr;
  } else {
    llvm::errs() << "invalid operand\n";
    v->dump();
    exit(-1);
  }
}
