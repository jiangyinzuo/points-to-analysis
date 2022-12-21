#pragma once

#include <cstdlib>
#include <llvm/IR/Function.h>
#include <llvm/IR/IntrinsicInst.h>
#include <llvm/IR/Module.h>
#include <llvm/Pass.h>
#include <llvm/Support/raw_ostream.h>

#include "Dataflow.h"
#include <unordered_map>
#include <unordered_set>

struct GetElementValue {
  llvm::GetElementPtrInst *inst;
};

using PointsToSet = std::unordered_set<llvm::Value *>;

struct PointsToInfo;

// Placement(Load, Alloca) -> Value
void addRetValues(PointsToSet &returnPts, llvm::Value *ret_value,
                  PointsToInfo *dfval);
struct PointsToInfo {
private:
  using PtsMap = std::unordered_map<llvm::Value *, PointsToSet>;
  PtsMap pointsToSets;

public:
  void UpdateExistingKey(PointsToInfo &other) {
    for (auto &[v, pts] : pointsToSets) {
      if (auto it = other.find(v); it != other.end()) {
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
    assert(!llvm::isa<llvm::Function>(v));
    if (llvm::GetElementPtrInst *inst =
            llvm::dyn_cast<llvm::GetElementPtrInst>(v)) {
      return pointsToSets.find(inst->getPointerOperand());
    }
    return pointsToSets.find(v);
  }

  auto operator[](llvm::Value *v) -> decltype(pointsToSets[v]) {
    assert(!llvm::isa<llvm::Function>(v));
    if (llvm::GetElementPtrInst *inst =
            llvm::dyn_cast<llvm::GetElementPtrInst>(v)) {
      return pointsToSets[inst->getPointerOperand()];
    }
    return pointsToSets[v];
  }

  bool operator==(const PointsToInfo &info) const {
    return pointsToSets == info.pointsToSets;
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
  PointsToSet returnPts;
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
      auto initval = *dfval;

      // add Param -> Arg
      int i = 0;
      for (auto &arg : f->args()) {
        llvm::Value *param = callInst->getArgOperand(i);
        arg.dump();
        param->dump();
        if (!param->getType()->isIntegerTy() &&
            !param->getType()->isFloatingPointTy()) {
          if (llvm::isa<llvm::Function>(param)) {
            initval[&arg] = {param};
          } else {
            initval[&arg] = initval[param];
          }
        }
        ++i;
      }
      if (!f->empty()) {
        llvm::errs() << initval;
        compForwardDataflow(f, &visitor, &result, initval);
        auto &lastBB = f->getBasicBlockList().back();
        auto &lastOut = result[&lastBB].second;
        dfval->UpdateExistingKey(lastOut);
        (*dfval)[callInst].merge(visitor.returnPts);
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
      if (llvm::isa<llvm::AllocaInst>(pointer_operand)) {
        // %1 = load i32 (...)**, i32 (...)*** %a_fptr, align 8, !dbg !23
        // store i32 (...)* @minus, i32 (...)** %1, align 8, !dbg !25
        //
        // %1 -> {a_fptr}
        (*dfval)[inst] = {pointer_operand};
        // a_fptr -> {minus}
      } else if (llvm::isa<llvm::LoadInst>(pointer_operand)) {
        // int (*p2)(int, int);
        // int (**p3)(int,int);
        // ...
        // p0 = plus, p1 = minus, p3 = &p0, p3 = &p1;
        // p2 = *p3;
        //
        // %3 = load i32 (i32, i32)**, i32 (i32, i32)*** %p3, align 8, !dbg !27
        // %4 = load i32 (i32, i32)*, i32 (i32, i32)** %3, align 8, !dbg !28
        // store i32 (i32, i32)* %4, i32 (i32, i32)** %p2, align 8, !dbg !29
        //
        // p3 -> {p0, p1}
        // %3 -> {p3}
        // %4 -> {p0, p1}
        auto pts = (*dfval)[pointer_operand]; // {p3}
        decltype(pts) merged;
        for (auto v : pts) {         // p3
          merged.merge((*dfval)[v]); // pts(p3) = {p0, p1}
        }
        (*dfval)[inst] = merged;
        // p2 -> {plus, minus}
      } else if (auto *get_elemptr =
                     llvm::dyn_cast<llvm::GetElementPtrInst>(pointer_operand)) {
        // qq = PARR[0];
        //
        // %arrayidx1 = getelementptr inbounds [1 x i32 (i32, i32)*], [1 x i32
        // (i32, i32)*]* %PAAR, i64 0, i64 0, !dbg !34

        // %0 = load i32 (i32, i32)*, i32 (i32, i32)** %arrayidx1, align 8, !dbg
        // !34

        // store i32 (i32, i32)* %0, i32 (i32, i32)** %qq, align 8, !dbg !33 ret
        // void, !dbg !35

        // %0 -> pts(%PAAR)
        get_elemptr->getPointerOperand()->dump();
        (*dfval)[inst] = (*dfval)[get_elemptr->getPointerOperand()];
        llvm::errs() << *dfval;
      } else if (llvm::isa<llvm::CallInst>(pointer_operand)) {
        llvm::errs() << "CALL\n";
        inst->dump();
        pointer_operand->dump();
        exit(-1);
      } else {
        llvm::errs() << "UNKNOWN pointer_operand type\n";
        inst->dump();
        pointer_operand->dump();
        exit(-1);
      }
      return;
    }

    if (llvm::StoreInst *store_inst = llvm::dyn_cast<llvm::StoreInst>(inst)) {
      llvm::Value *value_operand = store_inst->getValueOperand();
      auto value_operand_type = value_operand->getType();
      if (!value_operand_type->isPointerTy() &&
          !value_operand_type->isFunctionTy()) {
        return;
      }

      // set value_pts
      std::unordered_set<llvm::Value *> value_pts;
      if (llvm::isa<llvm::Function>(value_operand)) {
        // %1 = load i32 (...)**, i32 (...)*** %a_fptr, align 8, !dbg !23
        // store i32 (...)* @minus, i32 (...)** %1, align 8, !dbg !25
        value_pts = {value_operand}; // {minus}
      } else if (llvm::isa<llvm::LoadInst>(value_operand)) {
        // %1 = load i32 (i32, i32)*, i32 (i32, i32)** %p1, align 8, !dbg !32
        // %2 = load i32 (i32, i32)**, i32 (i32, i32)*** %p3, align 8, !dbg !33
        // store i32 (i32, i32)* %1, i32 (i32, i32)** %2, align 8, !dbg !34
        //
        // %1 -> {p1}
        value_pts = (*dfval)[value_operand]; // {p1}
      } else if (llvm::isa<llvm::ConstantPointerNull>(value_operand)) {
        return;
      } else if (llvm::isa<llvm::Argument>(value_operand)) {
        // %p2.addr = alloca void (...)*, align 8
        // store void (...)* %p2, void (...)** %p2.addr, align 8
        value_pts = (*dfval)[value_operand];
      } else if (llvm::isa<llvm::BitCastInst>(value_operand)) {
        // %0 = bitcast i8* %call to i32 (...)**, !dbg !22
        // store i32 (...)** %0, i32 (...)*** %a_fptr, align 8, !dbg !20
        value_pts = (*dfval)[value_operand];
      } else if (llvm::isa<llvm::AllocaInst>(value_operand)) {
        // struct fptr a_fptr;
        // struct fsptr s_fptr;
        // s_fptr.sptr=&a_fptr;
        //
        //  %a_fptr = alloca %struct.fptr, align 8
        // %s_fptr = alloca %struct.fsptr, align 8

        // %sptr = getelementptr inbounds %struct.fsptr, %struct.fsptr* %s_fptr,
        // i32 0, i32 0, !dbg !26

        // store %struct.fptr* %a_fptr, %struct.fptr** %sptr, align 8, !dbg !27
        // s_fptr -> {a_fptr}
        value_pts = {value_operand};
      } else if (llvm::isa<llvm::CallInst>(value_operand)) {
        value_pts = (*dfval)[value_operand]; // test18.c
      } else {
        llvm::errs() << "unimplemented value_operand\n";
        inst->dump();
        value_operand->dump();
        value_operand->getType()->dump();
        exit(-1);
      }

      if (llvm::isa<llvm::LoadInst>(store_inst->getPointerOperand())) {
        // int (*p1)(int,int) = ...;
        // int (**p3)(int,int) = ...;
        // ...
        // *p3 = p1;
        //
        // %1 = load i32 (i32, i32)*, i32 (i32, i32)** %p1, align 8, !dbg !32
        // %2 = load i32 (i32, i32)**, i32 (i32, i32)*** %p3, align 8, !dbg !33
        // store i32 (i32, i32)* %1, i32 (i32, i32)** %2, align 8, !dbg !34
        const auto pts = (*dfval)[store_inst->getPointerOperand()];
        for (auto v : pts) {
          (*dfval)[v] = value_pts;
        }
      } else if (llvm::isa<llvm::AllocaInst>(store_inst->getPointerOperand())) {
        // int (*p2)(int, int) = p1;
        // %0 = load i32 (i32, i32)*, i32 (i32, i32)** %p1, align 8, !dbg !28
        // store i32 (i32, i32)* %0, i32 (i32, i32)** %p2, align 8, !dbg !27
        (*dfval)[store_inst->getPointerOperand()] = value_pts;
      } else if (llvm::isa<llvm::GetElementPtrInst>(
                     store_inst->getPointerOperand())) {
        // field insensitive, same as AllocaInst

        // fff.p66 = plus;
        // %p66 = getelementptr inbounds %struct.FFF, %struct.FFF* %fff, i32 0,
        // i32 0

        // store i32 (i32, i32)* @plus, i32 (i32, i32)** %p66, align 8, !dbg
        // !22
        (*dfval)[store_inst->getPointerOperand()] = value_pts;
      } else {
        llvm::errs() << "UNKNOWN pointer Operand\n";
        store_inst->dump();
        store_inst->getPointerOperand()->dump();
        value_operand->dump();
        exit(-1);
      }

      return;
    }

    if (llvm::BitCastInst *bitcast_inst =
            llvm::dyn_cast<llvm::BitCastInst>(inst)) {
      (*dfval)[inst] = (*dfval)[bitcast_inst->getOperand(0)];
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