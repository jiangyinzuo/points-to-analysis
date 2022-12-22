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
using MaybeSet = std::unordered_set<llvm::Value *>;

struct PointsToInfo;

void addRetValues(MaybeSet &returnPts, llvm::Value *ret_value,
                  PointsToInfo *dfval);
struct PointsToInfo {
private:
  using StackFrame =
      std::unordered_map<llvm::Value *, std::shared_ptr<MaybeSet>>;
  StackFrame pointsToSets;
  PointsToInfo *father;

public:
  PointsToInfo() : father(nullptr) {}
  explicit PointsToInfo(PointsToInfo *father) : father(father) {}
  PointsToInfo(const PointsToInfo &other) {
    father = other.father;
    for (auto &[v, maybe_set_ptr] : other) {
      MaybeSet copy = *maybe_set_ptr;
      pointsToSets[v] = std::make_shared<MaybeSet>(copy);
    }
  }

  bool operator==(const PointsToInfo &other) const {
    MyAssert(father == other.father);
    for (auto it : pointsToSets) {
      MyAssert(it.second != nullptr);
    }
    for (auto it : other.pointsToSets) {
      MyAssert(it.second != nullptr);
    }
    if (pointsToSets.size() != other.pointsToSets.size()) {
      return false;
    }
    for (auto [v, pts] : pointsToSets) {
      if (auto it = other.pointsToSets.find(v); it != other.pointsToSets.end()) {
        MaybeSet &s1 = *pts;
        MaybeSet &s2 = *it->second;
        if (s1 != s2) {
          return false;
        }
      } else {
        return false;
      }
    }
    return true;
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

  void PointTo(llvm::Value *pointer, llvm::Value *pointee) {
    const auto *pinfo = findPointsToInfo(this, pointee);
    MyAssert(pinfo != nullptr);
    decltype(pinfo->pointsToSets.end()) it;
    if (llvm::isa<llvm::LoadInst>(pointee)) { // register
      MaybeSet maybe_set = GetMaybeSet(pointee);
      MyAssert(maybe_set.size() <= 1, pointer, pointee); // TODO:

      const auto *maybe_set_pinfo = findPointsToInfo(this, *maybe_set.begin());
      MyAssert(maybe_set_pinfo != nullptr, pointer, pointee);
      it = maybe_set_pinfo->pointsToSets.find(*maybe_set.begin());
    } else {
      it = pinfo->pointsToSets.find(pointee);
    }
    MyAssert(it != pinfo->pointsToSets.end(), pointer, pointee);
    pointsToSets[pointer] = it->second;
  }
  void Load(llvm::Value *tar, llvm::Value *ptr) {
    if (llvm::isa<llvm::LoadInst>(ptr)) { // register, swap_inline.c
      MaybeSet maybe_set = GetMaybeSet(ptr);
      PutMaybeSet(tar, {});
      for (llvm::Value *v : maybe_set) {
        MergeMaybeSet(tar, GetMaybeSet(v));
      }
    } else {
      PutMaybeSet(tar, GetMaybeSet(ptr));
    }
  }

  void Store(llvm::Value *src, llvm::Value *tar) {
    if (llvm::isa<llvm::ConstantPointerNull>(src)) {
      return;
    }
    MaybeSet maybe_values;
    if (llvm::isa<llvm::Function>(src) || llvm::isa<llvm::BitCastInst>(src) ||
        llvm::isa<llvm::AllocaInst>(src) ||
        llvm::isa<llvm::GetElementPtrInst>(src)) {
      maybe_values = {src};
    } else {
      maybe_values = GetMaybeSet(src);
    }

    if (llvm::isa<llvm::LoadInst>(tar)) {
      auto maybe_targets = GetMaybeSet(tar);
      for (auto maybe_target : maybe_targets) {
        PutMaybeSet(maybe_target, maybe_values);
      }
    } else {
      PutMaybeSet(tar, maybe_values);
    }
  }

  void PutMaybeSet(llvm::Value *v, MaybeSet maybe_set) {
    MyAssert(!llvm::isa<llvm::Function>(v), v);
    auto *p = const_cast<PointsToInfo *>(findPointsToInfo(this, v));
    if (p == nullptr) {
      pointsToSets[v] = std::make_shared<MaybeSet>(maybe_set);
    } else {
      *p->pointsToSets[v] = maybe_set;
    }
  }

  MaybeSet GetMaybeSet(llvm::Value *v) const {
    MyAssert(!llvm::isa<llvm::Function>(v), v);
    const auto *p = findPointsToInfo(this, v);
    if (p == nullptr) {
      return {};
    }
    MaybeSet result = *p->find(v)->second;
    return result;
  }

  void MergeMaybeSet(llvm::Value *v, const MaybeSet &other) {
    MyAssert(!llvm::isa<llvm::Function>(v), v);
    auto *p = const_cast<PointsToInfo *>(findPointsToInfo(this, v));
    if (p == nullptr) {
      pointsToSets[v] = std::make_shared<MaybeSet>(other);
    } else {
      MaybeSet *s = p->pointsToSets[v].get();
      for (auto v : other) {
        s->insert(v);
      }
    }
  }
  void Merge(const PointsToInfo &other) {
    MyAssert(father == other.father);
    for (auto &[v, pts] : other.pointsToSets) {
      MyAssert(pts != nullptr);
      auto point_to_set = pointsToSets.find(v);
      if (point_to_set == pointsToSets.end()) {
        MaybeSet copy = *pts;
        pointsToSets[v] = std::make_shared<MaybeSet>(copy);
      } else {
        point_to_set->second->merge(*pts);
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
    out << "\npts: " << pts.get() << ", refcnt: " << pts.use_count() - 1
        << ", size: " << pts->size() << " {";
    for (auto v : *pts) {
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
      if (auto callptrs = dfval->find(called_operand);
          callptrs != dfval->end()) {
        for (auto it = callptrs->second->begin(); it != callptrs->second->end();
             ++it) {
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

  void evalCallInst(llvm::CallInst *callInst, PointsToInfo *dfval) {
    llvm::errs() << "[EVAL CallInst]\n";

    // add callOutput
    unsigned line = callInst->getDebugLoc().getLine();
    llvm::Value *called_operand = callInst->getCalledOperand();
    std::unordered_set<llvm::Function *> callset;
    addCallOutput(line, called_operand, dfval, callset);

    for (auto *f : callset) {
      f->dump();
      DataflowResult<PointsToInfo>::Type result;
      PointsToVisitor visitor(callOutput);
      PointsToInfo initval(dfval);

      // add Param -> Arg
      int i = 0;
      for (auto &arg : f->args()) {
        llvm::Value *param = callInst->getArgOperand(i);
        if (!param->getType()->isIntegerTy() &&
            !param->getType()->isFloatingPointTy()) {
          initval.PutMaybeSet(&arg, {});
          initval.Store(param, &arg);
        }
        ++i;
      }
      if (!f->empty()) {
        llvm::errs() << "[initval]\n"<< initval << "~~~~~~~~\n";
        compForwardDataflow(f, &visitor, &result, initval);
        llvm::errs() << "Call END\n";
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
    llvm::errs() << "++++++++++++++\n";
    llvm::errs() << (*dfval);
    inst->dump();
    llvm::errs() << "--------------\n";
    if (llvm::AllocaInst *alloca = llvm::dyn_cast<llvm::AllocaInst>(inst)) {
      auto atype = alloca->getAllocatedType();
      if (atype->isPointerTy() || atype->isAggregateType()) {
        dfval->PutMaybeSet(inst, {});
      }
    } else if (llvm::CallInst *call_inst = llvm::dyn_cast<llvm::CallInst>(inst);
               call_inst && !llvm::isa<llvm::IntrinsicInst>(inst)) {
      evalCallInst(call_inst, dfval);
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
      dfval->Load(inst, pointer_operand);
    } else if (llvm::isa<llvm::BitCastInst>(inst) ||
               llvm::isa<llvm::GetElementPtrInst>(inst)) {
      llvm::Value *pointee = inst->getOperand(0);
      dfval->PointTo(inst, pointee);
    } else if (llvm::StoreInst *store_inst =
                   llvm::dyn_cast<llvm::StoreInst>(inst)) {
      llvm::Value *value_operand = store_inst->getValueOperand();
      auto value_operand_type = value_operand->getType();
      if (!value_operand_type->isPointerTy() &&
          !value_operand_type->isFunctionTy()) {
        return;
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