#pragma once
#ifndef __SYSU_OPTIMIZER_PLUGIN_HH_
#define __SYSU_OPTIMIZER_PLUGIN_HH_

#include <llvm/ADT/MapVector.h>
#include <llvm/IR/AbstractCallSite.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/PassManager.h>
#include <llvm/Pass.h>
#include <llvm/Passes/PassPlugin.h>
#include <llvm/Support/raw_ostream.h>
#include <llvm/IR/Dominators.h>
#include <llvm/IR/Function.h>
#include <llvm/IR/Instructions.h>
#include <llvm/IR/IRBuilder.h>
#include <vector>
#include <queue>
#include <algorithm>

namespace sysu {

class StaticCallCounter : public llvm::AnalysisInfoMixin<StaticCallCounter> {
public:
    using Result = llvm::MapVector<const llvm::Function *, unsigned>;

    Result run(llvm::Module &M, llvm::ModuleAnalysisManager &);

private:
    // A special type used by analysis passes to provide an address that
    // identifies that particular analysis pass type.
    static llvm::AnalysisKey Key;
    friend struct llvm::AnalysisInfoMixin<StaticCallCounter>;
};

class StaticCallCounterPrinter
        : public llvm::PassInfoMixin<StaticCallCounterPrinter> {
public:
    explicit StaticCallCounterPrinter(llvm::raw_ostream &OutS) : OS(OutS) {}

    llvm::PreservedAnalyses run(llvm::Module &M,
                                llvm::ModuleAnalysisManager &MAM);

private:
    llvm::raw_ostream &OS;
};

class SayHelloAnalysis : public llvm::AnalysisInfoMixin<SayHelloAnalysis> {
public:
    using Result = llvm::SmallVector<llvm::StringRef, 10>;

    Result run(llvm::Module &M, llvm::ModuleAnalysisManager &MAM);

private:
    static llvm::AnalysisKey Key;

    friend class llvm::AnalysisInfoMixin<SayHelloAnalysis>;
};

class SayHello : public llvm::PassInfoMixin<SayHello> {
public:
    llvm::PreservedAnalyses run(llvm::Module &M, llvm::ModuleAnalysisManager &MAM);
};

class LoadStoreOpt : public llvm::PassInfoMixin<LoadStoreOpt> {
private:
    bool modified;
    llvm::DominatorTree domTree;
    llvm::SmallVector<llvm::AllocaInst *, 30> varVector;

    void findStoreInst(llvm::Instruction *inst);
    void findLoadInst(llvm::Instruction *inst, llvm::StoreInst *storeInst);

public:
    llvm::PreservedAnalyses run(llvm::Function &F, llvm::FunctionAnalysisManager &FAM);
};

/* 常量折叠 */
class ConstantFolding : public llvm::PassInfoMixin<ConstantFolding> {
private:
    bool modified;

    void arithInstOpt(llvm::DomTreeNodeBase<llvm::BasicBlock> *root);
public:
    llvm::PreservedAnalyses run(llvm::Function &F, llvm::FunctionAnalysisManager &FAM);
};

class BranchOpt : public llvm::PassInfoMixin<BranchOpt> {
public:
    llvm::PreservedAnalyses run(llvm::Function &F, llvm::FunctionAnalysisManager &FAM);
private:
    bool modified;

    void phiInstOpt(llvm::DomTreeNodeBase<llvm::BasicBlock> *root);

    void condBrInstOpt(llvm::Function &F);

    void unusedBlockElimination(llvm::Function &F);

    void unCondBrInstOpt(llvm::BasicBlock *block);
};

class DeadCodeElimination : public llvm::PassInfoMixin<DeadCodeElimination> {
private:
    bool modified;

    void removeAllocaInst(llvm::BasicBlock &entry);
    void removeDeadInst(llvm::Function &F);

public:
    llvm::PreservedAnalyses run(llvm::Function &F, llvm::FunctionAnalysisManager &FAM);
};

class Test : public llvm::PassInfoMixin<Test> {
public:
    llvm::PreservedAnalyses run(llvm::Function &F, llvm::FunctionAnalysisManager &FAM);
};
} // namespace sysu

extern "C" {
llvm::PassPluginLibraryInfo LLVM_ATTRIBUTE_WEAK llvmGetPassPluginInfo();
}

#endif