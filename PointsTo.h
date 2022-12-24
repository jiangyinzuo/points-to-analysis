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
#include <stack>
#include <unordered_map>
#include <unordered_set>

union Operand;
namespace llvm {
template <class X>
bool isa(Operand &val);

template <class X>
X *dyn_cast(Operand &val);

template <class X>
X *cast(Operand &val);
} // namespace llvm

// -------------------------------------------------------------------------------------------------
// operand type    |  stack mem addr        |  register                      |   fn addr
// -------------------------------------------------------------------------------------------------
// MaybeSet pts[v] |  memory                |  mem addr, register, fn addr   |   /
// -------------------------------------------------------------------------------------------------
// llvm::Value     |  AllocaInst            |  LoadInst, GetElementPtrInst,  |   Function,
//                 |                        |  BitCastInst, CallInst         |   ConstantPointerNull
// -------------------------------------------------------------------------------------------------
union Operand {
public:
  enum class OperandType {
    StackMemAddr,
    HeapMemAddr, // emulate malloc
    Register,
    FnAddr
  };

  Operand() : v(nullptr) {}
  Operand(llvm::Value *v) : v(v) {
    MyAssert(heap_addr > 0);
  }
  explicit Operand(long heap_addr) : heap_addr(heap_addr) {}

  void dump() const {
    print(llvm::errs());
  }

  void print(llvm::raw_ostream &out) const {
    if (heap_addr == 0) {
      out << "null Operand\n";
    } else if (isHeapAddr()) {
      out << "heap addr: " << heap_addr << "\n";
    } else {
      v->print(out);
    }
  }

  std::string getName() {
    if (heap_addr == 0) {
      return "null Operand";
    } else if (isHeapAddr()) {
      return "heap addr: " + std::to_string(heap_addr);
    } else {
      return v->getName();
    }
  }

  bool operator==(const Operand &other) const {
    return v == other.v;
  }

  OperandType GetOperandType() const;

  bool isHeapAddr() const {
    return heap_addr < 0;
  }

private:
  friend class std::hash<Operand>;
  template <class X>
  friend bool llvm::isa(Operand &val);

  template <class X>
  friend X *llvm::dyn_cast(Operand &val);

  template <class X>
  friend X *llvm::cast(Operand &val);
  friend class Heap;

  llvm::Value *v; // always > 0
  long heap_addr; // always < 0
};

template <>
struct std::hash<Operand> {
  std::size_t operator()(Operand const &s) const noexcept {
    std::size_t h1 = std::hash<long>{}(s.heap_addr);
    return h1;
  }
};

namespace llvm {
template <class X>
bool isa(Operand &val) {
  if (val.isHeapAddr()) {
    return false;
  }
  return isa<X>(val.v);
}

template <class X>
X *dyn_cast(Operand &val) {
  if (val.isHeapAddr()) {
    return static_cast<X *>(nullptr);
  }
  return dyn_cast<X>(val.v);
}

template <class X>
X *cast(Operand &val) {
  return cast<X>(val.v);
}
} // namespace llvm

using MaybeSet = std::unordered_set<Operand>;

// parammap
// pamam -> arg
using ParamMap = std::unordered_map<llvm::Argument *, Operand>;

using PointsToSet = std::unordered_map<Operand, MaybeSet>;
using StackFrame = std::deque<PointsToSet>;

class MallocEmulator {
public:
  MallocEmulator() : malloc_addr(-1) {}
  Operand Alloc(llvm::CallInst *malloc_call) {
    if (auto it = h.find(malloc_call); it != h.end()) {
      return it->second;
    }
    Operand result(malloc_addr--);
    h[malloc_call] = result;
    return result;
  }

private:
  std::unordered_map<llvm::CallInst *, Operand> h;
  long malloc_addr;
};

struct PointsToInfo {

public:
  StackFrame stackFrame;

  void PushStackFrame() {
    stackFrame.emplace_back();
  }

  void PopStackFrame() {
    stackFrame.pop_back();
  }

  bool operator==(const PointsToInfo &other) const {
    if (stackFrame.size() != other.stackFrame.size()) {
      return false;
    }
    for (int i = 0; i < stackFrame.size(); ++i) {
      if (stackFrame[i] != other.stackFrame[i]) {
        return false;
      }
    }
    return true;
  }

  const PointsToSet &pointsToSets() const {
    return stackFrame.back();
  }

  MaybeSet GetMaybeAddr(Operand v) {
    switch (v.GetOperandType()) {
    case Operand::OperandType::FnAddr:
      MyAssert(false, v);
    case Operand::OperandType::StackMemAddr:
    case Operand::OperandType::HeapMemAddr:
      return {v};
    case Operand::OperandType::Register:
      return GetMaybeSet(v);
    }
    return {};
  }

  MaybeSet LoadMaybeValuesFromAddr(Operand operand) {
    MaybeSet maybe_set;
    switch (operand.GetOperandType()) {
    case Operand::OperandType::FnAddr:
      maybe_set = {operand};
      break;
    case Operand::OperandType::StackMemAddr:
    case Operand::OperandType::HeapMemAddr:
      maybe_set = GetMaybeSet(operand);
      break;
    case Operand::OperandType::Register:
      MaybeSet maybe_addrs = GetMaybeSet(operand);
      for (auto maybe_addr : maybe_addrs) {
        maybe_set.merge(GetMaybeSet(maybe_addr));
      }
    }
    return maybe_set;
  }

  void StoreMaybeValues(Operand tar, MaybeSet maybe_values) {
    switch (tar.GetOperandType()) {
    case Operand::OperandType::FnAddr:
      MyAssert(false, tar);
    case Operand::OperandType::StackMemAddr:
    case Operand::OperandType::HeapMemAddr:
      PutMaybeSet(tar, maybe_values);
      break;
    case Operand::OperandType::Register:
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
        tar.dump();
        llvm::errs() << "T: Maybe Any!\n";
        exit(-1);
      }
    }
  }

  void Store(Operand src, Operand tar) {
    if (llvm::isa<llvm::ConstantPointerNull>(src)) {
      return;
    } else {
      MaybeSet maybe_values;
      switch (src.GetOperandType()) {
      case Operand::OperandType::FnAddr:
      case Operand::OperandType::StackMemAddr:
      case Operand::OperandType::HeapMemAddr:
        maybe_values = {src};
        break;
      case Operand::OperandType::Register:
        maybe_values = GetMaybeSet(src);
      }

      StoreMaybeValues(tar, maybe_values);
    }
  }

  // Placement(Load, Alloca) -> Value
  void AddRetValues(MaybeSet &returnPts, Operand ret_value) const {
    switch (ret_value.GetOperandType()) {
    case Operand::OperandType::FnAddr:
      returnPts.insert(ret_value);
      return;
    case Operand::OperandType::StackMemAddr:
    case Operand::OperandType::HeapMemAddr:
      MyAssert(false, ret_value);
    case Operand::OperandType::Register:
      returnPts.merge(GetMaybeSet(ret_value));
    }
  }

  void PutMaybeSet(Operand v, MaybeSet maybe_set) {
    MyAssert(!llvm::isa<llvm::Function>(v), v);
    for (auto &pts : stackFrame) {
      if (auto it = pts.find(v); it != pts.end()) {
        it->second = maybe_set;
        return;
      }
    }
    stackFrame.back()[v] = maybe_set;
  }

  MaybeSet GetMaybeSet(Operand v) const {
    MyAssert(!llvm::isa<llvm::Function>(v), v);
    for (auto &pts : stackFrame) {
      if (auto it = pts.find(v); it != pts.end()) {
        return it->second;
      }
    }
    return {};
  }

  void MergeMaybeSet(Operand v, const MaybeSet &other) {
    MyAssert(!llvm::isa<llvm::Function>(v), v);
    for (auto &pts : stackFrame) {
      if (auto it = pts.find(v); it != pts.end()) {
        for (auto ptr : other) {
          it->second.insert(ptr);
        }
        return;
      }
    }
    for (auto ptr : other) {
      stackFrame.back()[v].insert(ptr);
    }
  }

  void MergeStackFrame(const PointsToInfo &other) {
    MyAssert(stackFrame.size() == other.stackFrame.size());
    for (int i = 0; i < stackFrame.size(); ++i) {
      auto &points_to_sets = stackFrame[i];
      for (auto &[v, ptrs] : other.stackFrame[i]) {
        if (points_to_sets[v].empty()) {
          points_to_sets[v] = {}; // add empty MaybeSet (test29.c)
        }
        for (auto p : ptrs) {
          points_to_sets[v].insert(p);
        }
      }
    }
  }
};

inline llvm::raw_ostream &operator<<(llvm::raw_ostream &out,
                                     const PointsToInfo &info) {
  out << "\n";
  int stack_idx = 0;
  for (auto &point_to_sets : info.stackFrame) {
    out << "[" << stack_idx++ << "]\n";
    for (const auto [value, pts] : point_to_sets) {
      out << "var: ";
      value.print(out);
      out << "\npts: ";
      switch (value.GetOperandType()) {
      case Operand::OperandType::FnAddr:
        llvm::errs() << "invalid operand type 'fn addr'\n";
        exit(-1);
      case Operand::OperandType::StackMemAddr:
        out << "S ";
        break;
      case Operand::OperandType::HeapMemAddr:
        out << "H ";
        break;
      case Operand::OperandType::Register:;
        out << "R ";
      }
      out << "size: " << pts.size() << " {";
      for (auto v : pts) {
        if (llvm::isa<llvm::Function>(v)) {
          out << " " << v.getName();
        } else {
          v.print(out);
        }
      }
      out << " }\n";
    }
  }
  return out;
}

using CallOutputMap = std::map<unsigned int, std::unordered_set<std::string>>;
class PointsToVisitor : public DataflowVisitor<struct PointsToInfo> {
private:
  ParamMap paramMap;
  MaybeSet returnPts;
  CallOutputMap &callOutput;
  MallocEmulator &mallocEmulator;

public:
  PointsToVisitor(CallOutputMap &callOutput, MallocEmulator &mallocEmulator) : paramMap(), callOutput(callOutput),
                                                                               mallocEmulator(mallocEmulator) {}

  void addCallOutput(unsigned line, Operand called_operand,
                     PointsToInfo *dfval,
                     std::unordered_set<llvm::Function *> &callset) {
    switch (called_operand.GetOperandType()) {
    case Operand::OperandType::FnAddr:
      callOutput[line].insert(called_operand.getName());
      callset.insert(llvm::cast<llvm::Function>(called_operand));
      break;
    case Operand::OperandType::StackMemAddr:
    case Operand::OperandType::HeapMemAddr:
      MyAssert(false, called_operand);
    case Operand::OperandType::Register:
      for (auto v : dfval->GetMaybeSet(called_operand)) {
        callOutput[line].insert(v.getName());
        callset.insert(llvm::cast<llvm::Function>(v));
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
#ifdef ANALYSIS_VIEW
    llvm::errs() << "[Call BEGIN]\n";
#endif
    // add callOutput
    unsigned line = callInst->getDebugLoc().getLine();
    llvm::Value *called_operand = callInst->getCalledOperand();
    std::unordered_set<llvm::Function *> callset;
    addCallOutput(line, called_operand, dfval, callset);

    for (auto *f : callset) {
#ifdef ANALYSIS_VIEW
      f->dump();
#endif
      PointsToVisitor visitor(callOutput, mallocEmulator);

      // add Param -> Arg
      int i = 0;
      for (auto &arg : f->args()) {
        llvm::Value *param = callInst->getArgOperand(i);
        if (!param->getType()->isIntegerTy() &&
            !param->getType()->isFloatingPointTy()) {
          visitor.paramMap[&arg] = Operand{param};
        }
        ++i;
      }
      if (!f->empty()) {
#ifdef ANALYSIS_VIEW
        llvm::errs() << "[paramMap]\n";
        for (auto v : visitor.paramMap) {
          v.second.dump();
          llvm::errs() << '\n';
        }
#endif

        PointsToInfo initval(*dfval);
        initval.PushStackFrame();
        DataflowResult<PointsToInfo>::Type result;
        compForwardDataflow(f, &visitor, &result, initval);

        auto &child_end = result.rbegin()->second.second;
        child_end.PopStackFrame();
        MyAssert(dfval->stackFrame.size() == child_end.stackFrame.size());
#ifdef ANALYSIS_VIEW
        llvm::errs() << "[father stackframe]\n"
                     << *dfval;
        llvm::errs() << "[child stackframe]\n"
                     << child_end;
#endif

        dfval->stackFrame = child_end.stackFrame;
#ifdef ANALYSIS_VIEW
        llvm::errs() << "[return MaybeSet]\n";
        for (auto ret_v : visitor.returnPts) {
          ret_v.dump();
        }
        llvm::errs() << "\n";
#endif
        dfval->MergeMaybeSet(Operand{callInst}, visitor.returnPts); // test18.c
      } else if (f->getReturnType()->isPointerTy()) {               // malloc
        Operand malloc_result = mallocEmulator.Alloc(callInst);
        dfval->PutMaybeSet(malloc_result, {});
        dfval->PutMaybeSet(callInst, {malloc_result});
      }
    }
#ifdef ANALYSIS_VIEW
    llvm::errs() << "[Call END]\n";
#endif
  }

  void merge(PointsToInfo *dest, const PointsToInfo &src) override {
    dest->MergeStackFrame(src);
  }

  void compDFVal(llvm::Instruction *inst, PointsToInfo *dfval) override {
    if (llvm::isa<llvm::DbgInfoIntrinsic>(inst)) {
      return;
    }
#ifdef ANALYSIS_VIEW
    llvm::errs() << (*dfval);
    llvm::errs() << "--------------\n";
    inst->dump();
#endif
    if (llvm::AllocaInst *alloca = llvm::dyn_cast<llvm::AllocaInst>(inst)) {
      auto atype = alloca->getAllocatedType();
      if (atype->isPointerTy() || atype->isAggregateType()) {
        dfval->PutMaybeSet(inst, {});
      }
    } else if (llvm::CallInst *call_inst = llvm::dyn_cast<llvm::CallInst>(inst); call_inst) {
      if (llvm::IntrinsicInst *intrinsic_inst = llvm::dyn_cast<llvm::IntrinsicInst>(inst)) {
        switch (intrinsic_inst->getIntrinsicID()) {
        case llvm::Intrinsic::memcpy:
          evalIntrinsicMemcpy(intrinsic_inst, dfval);
          break;
        case llvm::Intrinsic::memset:
          break;
        default:
          llvm::errs() << "unimplemented intrinsic\n";
          intrinsic_inst->dump();
          exit(-1);
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
      dfval->AddRetValues(returnPts, ret_value);
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
        dfval->Store(paramMap[arg], store_inst->getPointerOperand());
      } else {
        dfval->Store(value_operand, store_inst->getPointerOperand());
      }
    }
  }
};

class PointsTo : public llvm::ModulePass {
public:
  static char ID; // Pass identification, replacement for typeid

  explicit PointsTo() : ModulePass(ID) {}
  bool runOnModule(llvm::Module &M) override {
    DataflowResult<PointsToInfo>::Type result;

    MallocEmulator malloc_emulator;
    PointsToInfo initval;
    initval.PushStackFrame();
    CallOutputMap callOutput;
    PointsToVisitor visitor(callOutput, malloc_emulator);
    for (auto it = M.rbegin(); it != M.rend(); ++it) {
      auto &f = *it;
#ifdef ANALYSIS_VIEW
      f.dump();
#endif
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
      llvm::errs() << line << " : ";
#ifdef ANALYSIS_VIEW
      llvm::errs() << callset.size() << " ";
#endif
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