/************************************************************************
 *
 * @file Dataflow.h
 *
 * General dataflow framework
 *
 ***********************************************************************/

#ifndef _DATAFLOW_H_
#define _DATAFLOW_H_

#include <llvm/Support/raw_ostream.h>
#include <map>
#include <set>
#include <llvm/IR/BasicBlock.h>
#include <llvm/IR/CFG.h>
#include <llvm/IR/Function.h>

///Base dataflow visitor class, defines the dataflow function

template <class T>
class DataflowVisitor {
public:
    virtual ~DataflowVisitor() { }

    /// Dataflow Function invoked for each basic block 
    /// 
    /// @block the Basic Block
    /// @dfval the input dataflow value
    /// @isforward true to compute dfval forward, otherwise backward
    virtual void compDFVal(llvm::BasicBlock *block, T *dfval, bool isforward) {
        if (isforward == true) {
           for (llvm::BasicBlock::iterator ii=block->begin(), ie=block->end(); 
                ii!=ie; ++ii) {
                llvm::Instruction * inst = &*ii;
                compDFVal(inst, dfval);
           }
        } else {
           for (llvm::BasicBlock::reverse_iterator ii=block->rbegin(), ie=block->rend();
                ii != ie; ++ii) {
                llvm::Instruction * inst = &*ii;
                compDFVal(inst, dfval);
           }
        }
    }

    ///
    /// Dataflow Function invoked for each llvm::Instruction
    ///
    /// @inst the llvm::Instruction
    /// @dfval the input dataflow value
    /// @return true if dfval changed
    virtual void compDFVal(llvm::Instruction *inst, T *dfval ) = 0;

    ///
    /// Merge of two dfvals, dest will be ther merged result
    /// @return true if dest changed
    ///
    virtual void merge( T *dest, const T &src ) = 0;
};

///
/// Dummy class to provide a typedef for the detailed result set
/// For each llvm::BasicBlock, we compute its input dataflow val and its output dataflow val
///
template<class T>
struct DataflowResult {
    typedef typename std::map<llvm::BasicBlock *, std::pair<T, T> > Type;
};

/// 
/// Compute a forward iterated fixedpoint dataflow function, using a user-supplied
/// visitor function. Note that the caller must ensure that the function is
/// in fact a monotone function, as otherwise the fixedpoint may not terminate.
/// 
/// @param fn The function
/// @param visitor A function to compute dataflow vals
/// @param result The results of the dataflow 
/// @initval the Initial dataflow value
template<class T>
void compForwardDataflow(llvm::Function *fn,
    DataflowVisitor<T> *visitor,
    typename DataflowResult<T>::Type *result,
    const T & initval) {
    
    std::set<llvm::BasicBlock *> worklist;

    // Initialize the worklist with all exit blocks
    for (llvm::Function::iterator bi = fn->begin(); bi != fn->end(); ++bi) {
        llvm::BasicBlock * bb = &*bi;
        result->insert(std::make_pair(bb, std::make_pair(initval, initval)));
        worklist.insert(bb);
    }

    // Iteratively compute the dataflow result
    while (!worklist.empty()) {
        llvm::BasicBlock *bb = *worklist.begin();
        worklist.erase(worklist.begin());

        // Merge all incoming value
        T bbexitval = (*result)[bb].first;
        for (auto si = pred_begin(bb), se = pred_end(bb); si != se; si++) {
            llvm::BasicBlock *pred = *si;
            visitor->merge(&bbexitval, (*result)[pred].second);
        }

        (*result)[bb].first = bbexitval;
        visitor->compDFVal(bb, &bbexitval, true);

        // If outgoing value changed, propagate it along the CFG
        if (bbexitval == (*result)[bb].second) continue;
        (*result)[bb].second = bbexitval;

        for (llvm::succ_iterator pi = succ_begin(bb), pe = succ_end(bb); pi != pe; pi++) {
            worklist.insert(*pi);
        }
    }
}
/// 
/// Compute a backward iterated fixedpoint dataflow function, using a user-supplied
/// visitor function. Note that the caller must ensure that the function is
/// in fact a monotone function, as otherwise the fixedpoint may not terminate.
/// 
/// @param fn The function
/// @param visitor A function to compute dataflow vals
/// @param result The results of the dataflow 
/// @initval The initial dataflow value
template<class T>
void compBackwardDataflow(llvm::Function *fn,
    DataflowVisitor<T> *visitor,
    typename DataflowResult<T>::Type *result,
    const T &initval) {

    std::set<llvm::BasicBlock *> worklist;

    // Initialize the worklist with all exit blocks
    for (llvm::Function::iterator bi = fn->begin(); bi != fn->end(); ++bi) {
        llvm::BasicBlock * bb = &*bi;
        result->insert(std::make_pair(bb, std::make_pair(initval, initval)));
        worklist.insert(bb);
    }

    // Iteratively compute the dataflow result
    while (!worklist.empty()) {
        llvm::BasicBlock *bb = *worklist.begin();
        worklist.erase(worklist.begin());

        // Merge all incoming value
        T bbexitval = (*result)[bb].second;
        for (auto si = succ_begin(bb), se = succ_end(bb); si != se; si++) {
            llvm::BasicBlock *succ = *si;
            visitor->merge(&bbexitval, (*result)[succ].first);
        }

        (*result)[bb].second = bbexitval;
        visitor->compDFVal(bb, &bbexitval, false);

        // If outgoing value changed, propagate it along the CFG
        if (bbexitval == (*result)[bb].first) continue;
        (*result)[bb].first = bbexitval;

        for (llvm::pred_iterator pi = pred_begin(bb), pe = pred_end(bb); pi != pe; pi++) {
            worklist.insert(*pi);
        }
    }
}

template<class T>
void printDataflowResult(llvm::raw_ostream &out,
                         const typename DataflowResult<T>::Type &dfresult) {
    for ( typename DataflowResult<T>::Type::const_iterator it = dfresult.begin();
            it != dfresult.end(); ++it ) {
        if (it->first == NULL) out << "*";
        else it->first->dump();
        out << "\n\tin : "
            << it->second.first 
            << "\n\tout :  "
            << it->second.second
            << "\n";
    }
}







#endif /* !_DATAFLOW_H_ */
