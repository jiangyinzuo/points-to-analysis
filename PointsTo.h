#pragma once

#include <cstdlib>
#include <llvm/IR/Function.h>
#include <llvm/IR/IntrinsicInst.h>
#include <llvm/IR/Module.h>
#include <llvm/Pass.h>
#include <llvm/Support/raw_ostream.h>

#include "Dataflow.h"
#include "MyAssert.h"
#include <memory>
#include <unordered_map>
#include <unordered_set>

// -------------------------------------------------------------------------------------------------
// operand type    |  mem addr              |  register                      |   fn addr
// -------------------------------------------------------------------------------------------------
// MaybeSet pts[v] |  memory                |  mem addr, register, fn addr   |   /
// -------------------------------------------------------------------------------------------------
// llvm::Value     |  AllocaInst, CallInst  |  LoadInst, GetElementPtrInst,  |   Function,
//                 |                        |  BitCastInst                   |   ConstantPointerNull
// -------------------------------------------------------------------------------------------------
using MaybeSet = std::unordered_set<llvm::Value *>;
enum class OperandType {
  MemAddr,
  Register,
  FnAddr
};

OperandType GetOperandType(llvm::Value *v);

// parammap
// pamam -> arg
using ParamMap = std::unordered_map<llvm::Argument *, llvm::Value *>;

struct PointsToInfo;

void addRetValues(MaybeSet &returnPts, llvm::Value *ret_value, PointsToInfo *dfval);
struct PointsToInfo {
private:
  using PointsToSet = std::unordered_map<llvm::Value *, MaybeSet>;
  PointsToSet pointsToSets;
  PointsToInfo *father;

public:
  PointsToInfo() : father(nullptr) {}
  explicit PointsToInfo(PointsToInfo *father) : father(father) {}
  PointsToInfo(const PointsToInfo &other) {
    father = other.father;
    pointsToSets = other.pointsToSets;
  }

  bool operator==(const PointsToInfo &other) const {
    MyAssert(father == other.father);
    return pointsToSets == other.pointsToSets;
  }

  decltype(pointsToSets.cbegin()) begin() const {
    return pointsToSets.cbegin();
  }

  decltype(pointsToSets.cend()) end() const { return pointsToSets.cend(); }

  auto find(llvm::Value *v) const -> decltype(pointsToSets.find(v)) {
    MyAssert(!llvm::isa<llvm::Function>(v), v);
    auto it = pointsToSets.find(v);
    if (it == pointsToSets.end() && father != nullptr) {
      auto father_it = father->find(v);
      if (father_it == father->end()) {
        return it;
      } else {
        return father_it;
      }
    } else {
      return it;
    }
  }

  MaybeSet GetMaybeAddr(llvm::Value *v) {
    switch (GetOperandType(v)) {
    case OperandType::FnAddr:
      MyAssert(false, v);
    case OperandType::MemAddr:
      return {v};
    case OperandType::Register:
      return GetMaybeSet(v);
    }
    return {};
  }

  MaybeSet LoadMaybeValuesFromAddr(llvm::Value *operand) {
    MaybeSet maybe_set;
    switch (GetOperandType(operand)) {
    case OperandType::FnAddr:
      maybe_set = {operand};
      break;
    case OperandType::MemAddr:
      maybe_set = GetMaybeSet(operand);
      break;
    case OperandType::Register:
      MaybeSet maybe_addrs = GetMaybeSet(operand);
      for (auto *maybe_addr : maybe_addrs) {
        maybe_set.merge(GetMaybeSet(maybe_addr));
      }
    }
    return maybe_set;
  }

  void StoreMaybeValues(llvm::Value *tar, MaybeSet maybe_values) {
    switch (GetOperandType(tar)) {
    case OperandType::FnAddr:
      MyAssert(false, tar);
    case OperandType::MemAddr:
      PutMaybeSet(tar, maybe_values);
      break;
    case OperandType::Register:
      auto maybe_tar_addrs = GetMaybeSet(tar);
      if (maybe_tar_addrs.size() > 1) {
        for (auto maybe_tar_addr : maybe_tar_addrs) {
          // Targets may be assigned, maybe not. So use MergeMaybeSet
          // see test28.c
          MergeMaybeSet(maybe_tar_addr, maybe_values);
        }
      } else if (maybe_tar_addrs.size() == 1) {
        // see test08.c
        for (auto maybe_tar_addr : maybe_tar_addrs) {
          PutMaybeSet(maybe_tar_addr, maybe_values);
        }
      } else {
        tar->dump();
        llvm::errs() << "T: Maybe Any!\n";
        exit(-1);
      }
    }
  }

  void Store(llvm::Value *src, llvm::Value *tar) {
    if (llvm::isa<llvm::ConstantPointerNull>(src)) {
      return;
    } else {
      MaybeSet maybe_values;
      switch (GetOperandType(src)) {
      case OperandType::FnAddr:
      case OperandType::MemAddr:
        maybe_values = {src};
        break;
      case OperandType::Register:
        maybe_values = GetMaybeSet(src);
      }

      StoreMaybeValues(tar, maybe_values);
    }
  }

  void PutMaybeSet(llvm::Value *v, MaybeSet maybe_set) {
    MyAssert(!llvm::isa<llvm::Function>(v), v);
    auto *p = const_cast<PointsToInfo *>(findPointsToInfo(this, v));
    if (p == nullptr) {
      MyAssert(pointsToSets[v].empty());
      pointsToSets[v] = maybe_set;
    } else {
      p->pointsToSets[v] = maybe_set;
    }
  }

  MaybeSet GetMaybeSet(llvm::Value *v) const {
    MyAssert(!llvm::isa<llvm::Function>(v), v);
    const auto *p = findPointsToInfo(this, v);
    if (p == nullptr) {
      return {};
    }
    return p->find(v)->second;
  }

  void MergeMaybeSet(llvm::Value *v, const MaybeSet &other) {
    MyAssert(!llvm::isa<llvm::Function>(v), v);
    auto *p = const_cast<PointsToInfo *>(findPointsToInfo(this, v));
    if (p == nullptr) {
      MyAssert(pointsToSets[v].empty());
      for (auto other_p : other) {
        pointsToSets[v].insert(other_p);
      }
    } else {
      for (auto other_p : other) {
        p->pointsToSets[v].insert(other_p);
      }
    }
  }

  void Merge(const PointsToInfo &other) {
    MyAssert(father == other.father);
    for (auto &[v, ptrs] : other.pointsToSets) {
      if (pointsToSets[v].empty()) {
        pointsToSets[v] = {}; // add empty MaybeSet (test29.c)
      }
      for (auto p : ptrs) {
        pointsToSets[v].insert(p);
      }
    }
  }

private:
  static const PointsToInfo *findPointsToInfo(const PointsToInfo *cur,
                                              llvm::Value *v) {
    while (cur != nullptr &&
           cur->pointsToSets.find(v) == cur->pointsToSets.end()) {
      cur = cur->father;
    }
    return cur;
  }
};

inline llvm::raw_ostream &operator<<(llvm::raw_ostream &out,
                                     const PointsToInfo &info) {
  out << "\n";
  for (const auto [value, pts] : info) {
    out << "var: ";
    value->print(out);
    out << "\npts: ";
    switch (GetOperandType(value)) {
    case OperandType::FnAddr:
      llvm::errs() << "invalid operand type 'fn addr'\n";
      exit(-1);
    case OperandType::MemAddr:
      out << "M ";
      break;
    case OperandType::Register:;
      out << "R ";
    }
    out << "size: " << pts.size() << " {";
    for (auto v : pts) {
      if (llvm::isa<llvm::Function>(v)) {
        out << " " << v->getName();
      } else {
        v->print(out);
      }
    }
    out << " }\n";
  }
  return out;
}

using CallOutputMap = std::map<unsigned int, std::unordered_set<std::string>>;
class PointsToVisitor : public DataflowVisitor<struct PointsToInfo> {
private:
  ParamMap paramMap;
  MaybeSet returnPts;
  CallOutputMap &callOutput;

public:
  PointsToVisitor(CallOutputMap &callOutput) : paramMap(), callOutput(callOutput) {}

  void addCallOutput(unsigned line, llvm::Value *called_operand,
                     PointsToInfo *dfval,
                     std::unordered_set<llvm::Function *> &callset) {
    if (llvm::Function *f = llvm::dyn_cast<llvm::Function>(called_operand)) {
      callOutput[line].insert(f->getName());
      callset.insert(f);
    } else {
      if (auto callptrs = dfval->find(called_operand);
          callptrs != dfval->end()) {
        for (auto it = callptrs->second.begin(); it != callptrs->second.end(); ++it) {
          if (llvm::Function *f = llvm::dyn_cast<llvm::Function>(*it)) {
            callOutput[line].insert(f->getName());
            callset.insert(f);
          } else {
            (*it)->dump();
            addCallOutput(line, *it, dfval, callset);
          }
        }
      }
    }
  }

  // %2 = bitcast %struct.fsptr* %1 to i8*, !dbg !115
  // %3 = bitcast %struct.fsptr* %s_fptr to i8*, !dbg !115
  // call void @llvm.memcpy.p0i8.p0i8.i64(i8* align 8 %2, i8* align 8 %3, i64 8, i1 false), !dbg !115
  void evalIntrinsicMemcpy(llvm::IntrinsicInst *intrinstic_inst, PointsToInfo *dfval) {
    llvm::Value *dest = intrinstic_inst->getArgOperand(0);
    llvm::Value *src = intrinstic_inst->getArgOperand(1);

    MaybeSet maybe_values = dfval->LoadMaybeValuesFromAddr(src);
    dfval->StoreMaybeValues(dest, maybe_values);
  }

  void evalCallInst(llvm::CallInst *callInst, PointsToInfo *dfval) {
    llvm::errs() << "[Call BEGIN]\n";

    // add callOutput
    unsigned line = callInst->getDebugLoc().getLine();
    llvm::Value *called_operand = callInst->getCalledOperand();
    std::unordered_set<llvm::Function *> callset;
    addCallOutput(line, called_operand, dfval, callset);

    for (auto *f : callset) {
      f->dump();
      PointsToVisitor visitor(callOutput);

      // add Param -> Arg
      int i = 0;
      for (auto &arg : f->args()) {
        llvm::Value *param = callInst->getArgOperand(i);
        if (!param->getType()->isIntegerTy() &&
            !param->getType()->isFloatingPointTy()) {
          visitor.paramMap[&arg] = param;
        }
        ++i;
      }
      if (!f->empty()) {
        PointsToInfo initval(dfval);
        DataflowResult<PointsToInfo>::Type result;
        compForwardDataflow(f, &visitor, &result, initval);
        llvm::errs() << "[Call END]\n";
        dfval->MergeMaybeSet(callInst, visitor.returnPts); // test18.c
      } else if (f->getReturnType()->isPointerTy()) {      // malloc
        dfval->PutMaybeSet(callInst, {});
      }
    }
  }

  void merge(PointsToInfo *dest, const PointsToInfo &src) override {
    dest->Merge(src);
  }

  void compDFVal(llvm::Instruction *inst, PointsToInfo *dfval) override {
    if (llvm::isa<llvm::DbgInfoIntrinsic>(inst)) {
      return;
    }
    llvm::errs() << (*dfval);
    llvm::errs() << "--------------\n";
    inst->dump();
    if (llvm::AllocaInst *alloca = llvm::dyn_cast<llvm::AllocaInst>(inst)) {
      auto atype = alloca->getAllocatedType();
      if (atype->isPointerTy() || atype->isAggregateType()) {
        dfval->PutMaybeSet(inst, {});
      }
    } else if (llvm::CallInst *call_inst = llvm::dyn_cast<llvm::CallInst>(inst); call_inst) {
      if (llvm::IntrinsicInst *intrinsic_inst = llvm::dyn_cast<llvm::IntrinsicInst>(inst)) {
        if (intrinsic_inst->getIntrinsicID() == llvm::Intrinsic::memcpy) {
          evalIntrinsicMemcpy(intrinsic_inst, dfval);
        }
      } else {
        evalCallInst(call_inst, dfval);
      }
    } else if (llvm::ReturnInst *ret_inst =
                   llvm::dyn_cast<llvm::ReturnInst>(inst)) {
      llvm::Value *ret_value = ret_inst->getReturnValue();
      if (ret_value == nullptr || (!ret_value->getType()->isFunctionTy() &&
                                   !ret_value->getType()->isPointerTy())) {
        return;
      }
      addRetValues(returnPts, ret_value, dfval);
    } else if (llvm::isa<llvm::LoadInst>(inst)) {
      if (!inst->getType()->isPointerTy() && !inst->getType()->isFunctionTy()) {
        return;
      }
      llvm::Value *pointer_operand = inst->getOperand(0);
      MaybeSet maybe_set = dfval->LoadMaybeValuesFromAddr(pointer_operand);
      dfval->PutMaybeSet(inst, maybe_set);
    } else if (llvm::isa<llvm::BitCastInst>(inst) ||
               llvm::isa<llvm::GetElementPtrInst>(inst)) {
      llvm::Value *pointer_operand = inst->getOperand(0);
      MaybeSet maybe_addr = dfval->GetMaybeAddr(pointer_operand);
      dfval->PutMaybeSet(inst, maybe_addr);
    } else if (llvm::StoreInst *store_inst =
                   llvm::dyn_cast<llvm::StoreInst>(inst)) {
      llvm::Value *value_operand = store_inst->getValueOperand();
      auto value_operand_type = value_operand->getType();
      if (!value_operand_type->isPointerTy() &&
          !value_operand_type->isFunctionTy()) {
        return;
      }
      if (llvm::Argument *arg = llvm::dyn_cast<llvm::Argument>(value_operand)) {
        value_operand = paramMap[arg];
        MyAssert(value_operand != nullptr, store_inst);
      }
      dfval->Store(value_operand, store_inst->getPointerOperand());
    }
  }
};

class PointsTo : public llvm::ModulePass {
public:
  static char ID; // Pass identification, replacement for typeid

  explicit PointsTo() : ModulePass(ID) {}
  bool runOnModule(llvm::Module &M) override {
    DataflowResult<PointsToInfo>::Type result;
    PointsToInfo initval(nullptr);
    CallOutputMap callOutput;
    PointsToVisitor visitor(callOutput);
    for (auto it = M.rbegin(); it != M.rend(); ++it) {
      auto &f = *it;
      f.dump();
      if (!f.isIntrinsic() && !f.empty()) {
        for (llvm::Argument &arg : f.args()) {
          if (arg.getType() != nullptr && arg.getType()->isPointerTy()) {
            continue;
          }
        }
        compForwardDataflow(&f, &visitor, &result, initval);
        break;
      }
    }

    // Print output
    for (auto &[line, callset] : callOutput) {
      llvm::errs() << line << " : " << callset.size() << " ";
      assert(!callset.empty());
      for (auto it = callset.begin(); it != callset.end(); ++it) {
        if (it != callset.begin()) {
          llvm::errs() << ", ";
        }
        llvm::errs() << *it;
      }
      llvm::errs() << '\n';
    }
    return false;
  }
};