#pragma once

#include <cstdlib>
#include <llvm/IR/Function.h>
#include <llvm/IR/IntrinsicInst.h>
#include <llvm/IR/Module.h>
#include <llvm/Pass.h>
#include <llvm/Support/raw_ostream.h>

#include "Dataflow.h"
#include "MyAssert.h"
#include <unordered_map>
#include <unordered_set>
using MaybeSet = std::unordered_set<llvm::Value *>;

struct PointsToInfo;

void addRetValues(MaybeSet &returnPts, llvm::Value *ret_value,
                  PointsToInfo *dfval);
struct PointsToInfo {
private:
  using VarMaybeSets = std::unordered_map<llvm::Value *, MaybeSet>;
  VarMaybeSets pointsToSets;

public:
  bool operator==(const PointsToInfo &info) const {
    return pointsToSets == info.pointsToSets;
  }
  void UpdateExistingKey(PointsToInfo &other) {
    for (auto &[v, pts] : pointsToSets) {
      if (auto it = other.find(v); it != other.end()) {
        pointsToSets[v].clear();
        for (auto *value : it->second) {
          addRetValues(pointsToSets[v], value, &other);
        }
      }
    }
  }

  decltype(pointsToSets.cbegin()) begin() const {
    return pointsToSets.cbegin();
  }

  decltype(pointsToSets.cend()) end() const { return pointsToSets.cend(); }

  auto find(llvm::Value *v) const -> decltype(pointsToSets.find(v)) {
    MyAssert(!llvm::isa<llvm::Function>(v), v);
    if (llvm::GetElementPtrInst *inst =
            llvm::dyn_cast<llvm::GetElementPtrInst>(v)) {
      auto f = pointsToSets.find(v);
      MyAssert(f->second.size() <= 1, v);
      return pointsToSets.find(*f->second.begin());
    }
    return pointsToSets.find(v);
  }

  void Store(llvm::Value *src, llvm::Value *tar) {
    MaybeSet maybe_values;
    if (llvm::isa<llvm::Function>(src) || llvm::isa<llvm::AllocaInst>(src) ||
        llvm::isa<llvm::GetElementPtrInst>(src)) {
      // src is a pointer
      maybe_values = {src};
    } else {
      // src is a register
      maybe_values = (*this)[src];
    }

    if (llvm::isa<llvm::LoadInst>(tar)) {
      auto maybe_targets = (*this)[tar];
      for (auto maybe_target : maybe_targets) {
        (*this)[maybe_target] = maybe_values;
      }
    } else {
      (*this)[tar] = maybe_values;
    }
  }

  auto operator[](llvm::Value *v) -> decltype(pointsToSets[v]) {
    MyAssert(!llvm::isa<llvm::Function>(v), v);

    if (llvm::GetElementPtrInst *inst =
            llvm::dyn_cast<llvm::GetElementPtrInst>(v)) {
      MyAssert(pointsToSets[v].size() <= 1, v);
      return pointsToSets[*pointsToSets[v].begin()];
    }
    return pointsToSets[v];
  }
};

inline llvm::raw_ostream &operator<<(llvm::raw_ostream &out,
                                     const PointsToInfo &info) {
  out << "\n";
  for (const auto [value, pts] : info) {
    out << "var: ";
    value->print(out);
    out << "\npts: " << pts.size() << "\n";
    for (auto v : pts) {
      out << "  " << v->getName() << "\n";
    }
  }
  return out;
}

using CallOutputMap = std::map<unsigned int, std::unordered_set<std::string>>;
class PointsToVisitor : public DataflowVisitor<struct PointsToInfo> {
private:
  MaybeSet returnPts;
  CallOutputMap &callOutput;

public:
  PointsToVisitor(CallOutputMap &callOutput) : callOutput(callOutput) {}

  void addCallOutput(unsigned line, llvm::Value *called_operand,
                     PointsToInfo *dfval,
                     std::unordered_set<llvm::Function *> &callset) {
    if (llvm::Function *f = llvm::dyn_cast<llvm::Function>(called_operand)) {
      callOutput[line].insert(f->getName());
      callset.insert(f);
    } else {
      auto callptrs = (*dfval)[called_operand];
      for (auto it = callptrs.begin(); it != callptrs.end(); ++it) {
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

  void evalCallInst(llvm::CallInst *callInst, PointsToInfo *dfval) {
    llvm::errs() << "[EVAL CallInst]\n";
    callInst->dump();

    // add callOutput
    unsigned line = callInst->getDebugLoc().getLine();
    llvm::Value *called_operand = callInst->getCalledOperand();
    std::unordered_set<llvm::Function *> callset;
    addCallOutput(line, called_operand, dfval, callset);

    for (auto *f : callset) {
      f->dump();
      DataflowResult<PointsToInfo>::Type result;
      PointsToVisitor visitor(callOutput);
      auto &initval = *dfval;

      // add Param -> Arg
      int i = 0;
      for (auto &arg : f->args()) {
        llvm::Value *param = callInst->getArgOperand(i);
        if (!param->getType()->isIntegerTy() &&
            !param->getType()->isFloatingPointTy()) {
          initval.Store(param, &arg);
        }
        ++i;
      }
      if (!f->empty()) {
        compForwardDataflow(f, &visitor, &result, initval);
        (*dfval)[callInst] = visitor.returnPts;
      }
    }
  }

  void merge(PointsToInfo *dest, const PointsToInfo &src) override {
    for (auto [variable, pts] : src) {
      (*dest)[variable].merge(pts);
    }
  }

  void compDFVal(llvm::Instruction *inst, PointsToInfo *dfval) override {
    if (llvm::isa<llvm::DbgInfoIntrinsic>(inst)) {
      return;
    }
    llvm::errs() << "++++++++++++++\n";
    llvm::errs() << (*dfval);
    inst->dump();
    llvm::errs() << "--------------\n";
    if (llvm::CallInst *call_inst = llvm::dyn_cast<llvm::CallInst>(inst);
        call_inst && !llvm::isa<llvm::IntrinsicInst>(inst)) {
      evalCallInst(call_inst, dfval);
      return;
    }

    if (llvm::ReturnInst *ret_inst = llvm::dyn_cast<llvm::ReturnInst>(inst)) {
      llvm::Value *ret_value = ret_inst->getReturnValue();
      if (ret_value == nullptr || (!ret_value->getType()->isFunctionTy() &&
                                   !ret_value->getType()->isPointerTy())) {
        return;
      }
      addRetValues(returnPts, ret_value, dfval);
      return;
    }

    if (llvm::isa<llvm::LoadInst>(inst)) {
      if (!inst->getType()->isPointerTy() && !inst->getType()->isFunctionTy()) {
        return;
      }
      auto pointer_operand = inst->getOperand(0);
      (*dfval)[inst] = (*dfval)[pointer_operand];
      return;
    }

    if (llvm::isa<llvm::BitCastInst>(inst)) {
      (*dfval)[inst] = {inst->getOperand(0)};
      return;
    }

    if (llvm::GetElementPtrInst *getelement =
            llvm::dyn_cast<llvm::GetElementPtrInst>(inst)) {
      llvm::Value *pointer_operand = getelement->getPointerOperand();
      (*dfval).Store(pointer_operand, getelement);
    }

    if (llvm::StoreInst *store_inst = llvm::dyn_cast<llvm::StoreInst>(inst)) {
      llvm::Value *value_operand = store_inst->getValueOperand();
      auto value_operand_type = value_operand->getType();
      if (!value_operand_type->isPointerTy() &&
          !value_operand_type->isFunctionTy()) {
        return;
      }

      dfval->Store(value_operand, store_inst->getPointerOperand());
      return;
    }
  }
};

class PointsTo : public llvm::ModulePass {
public:
  static char ID; // Pass identification, replacement for typeid

  explicit PointsTo() : ModulePass(ID) {}
  bool runOnModule(llvm::Module &M) override {
    DataflowResult<PointsToInfo>::Type result;
    PointsToInfo initval;
    CallOutputMap callOutput;
    PointsToVisitor visitor(callOutput);
    for (auto it = M.rbegin(); it != M.rend(); ++it) {
      auto &f = *it;
      f.dump();
      if (!f.isIntrinsic()) {
        compForwardDataflow(&f, &visitor, &result, initval);
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