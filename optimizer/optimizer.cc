#include "optimizer.hh"

#include <llvm/Passes/PassBuilder.h>

void f(int index=0) {
    llvm::errs() << "yes" << index << "\n";
}
llvm::PreservedAnalyses
sysu::StaticCallCounterPrinter::run(llvm::Module &M,
                                    llvm::ModuleAnalysisManager &MAM) {

    auto DirectCalls = MAM.getResult<sysu::StaticCallCounter>(M);
    OS << "=================================================\n";
    OS << "sysu-optimizer: static analysis results\n";
    OS << "=================================================\n";
    const char *str1 = "NAME", *str2 = "#N DIRECT CALLS";
    OS << llvm::format("%-20s %-10s\n", str1, str2);
    OS << "-------------------------------------------------\n";

    for (auto &CallCount: DirectCalls) {
        OS << llvm::format("%-20s %-10lu\n",
                           CallCount.first->getName().str().c_str(),
                           CallCount.second);
    }

    OS << "-------------------------------------------------\n\n";
    return llvm::PreservedAnalyses::all();
}

sysu::StaticCallCounter::Result
sysu::StaticCallCounter::run(llvm::Module &M, llvm::ModuleAnalysisManager &) {
    llvm::MapVector<const llvm::Function *, unsigned> Res;

    for (auto &Func: M) {
        for (auto &BB: Func) {
            for (auto &Ins: BB) {

                // If this is a call instruction then CB will be not null.
                auto *CB = llvm::dyn_cast<llvm::CallBase>(&Ins);
                if (nullptr == CB) {
                    continue;
                }

                // If CB is a direct function call then DirectInvoc will be not null.
                auto DirectInvoc = CB->getCalledFunction();
                if (nullptr == DirectInvoc) {
                    continue;
                }

                // We have a direct function call - update the count for the function
                // being called.
                auto CallCount = Res.find(DirectInvoc);
                if (Res.end() == CallCount) {
                    CallCount = Res.insert({DirectInvoc, 0}).first;
                }
                ++CallCount->second;
            }
        }
    }

    return Res;
}

llvm::AnalysisKey sysu::StaticCallCounter::Key;
llvm::AnalysisKey sysu::SayHelloAnalysis::Key;

extern "C" {
llvm::PassPluginLibraryInfo LLVM_ATTRIBUTE_WEAK llvmGetPassPluginInfo() {
    return {LLVM_PLUGIN_API_VERSION, "sysu-optimizer-pass", LLVM_VERSION_STRING,
            [](llvm::PassBuilder &PB) {
                // #1 REGISTRATION FOR "opt -passes=sysu-optimizer-pass"
                PB.registerPipelineParsingCallback(
                        [&](llvm::StringRef Name, llvm::ModulePassManager &MPM,
                            llvm::ArrayRef<llvm::PassBuilder::PipelineElement>) {
                            if (Name == "sysu-optimizer-pass") {
                                MPM.addPass(sysu::StaticCallCounterPrinter(llvm::errs()));
                                return true;
                            }
                            return false;
                        });
                // #2 REGISTRATION FOR
                // "MAM.getResult<sysu::StaticCallCounter>(Module)"
                PB.registerAnalysisRegistrationCallback(
                        [](llvm::ModuleAnalysisManager &MAM) {
                            MAM.registerPass([&] { return sysu::StaticCallCounter(); });
                        });
            }};
}
}

llvm::PreservedAnalyses sysu::SayHello::run(llvm::Module &M,
                                            llvm::ModuleAnalysisManager &MAM) {
    auto funcNameVector = MAM.getResult<sysu::SayHelloAnalysis>(M);
    for (auto &name: funcNameVector) {
        llvm::errs() << "Hello! " << name << "\n";
    }
    return llvm::PreservedAnalyses::all();
}

sysu::SayHelloAnalysis::Result
sysu::SayHelloAnalysis::run(llvm::Module &M, llvm::ModuleAnalysisManager &MAM) {
    Result res;
    for (auto &F: M.getFunctionList()) {
        res.push_back(F.getName());
    }
    return res;
}

void sysu::LoadStoreOpt::findLoadInst(llvm::Instruction *inst, llvm::StoreInst *storeInst) {
    /* Store指令既存储了变量，也存储了值 */
    llvm::BasicBlock *storeBlock = storeInst->getParent(); /* store指令所在的block */

    llvm::BasicBlock *curBlock = nullptr;
    if(inst != nullptr) {
        curBlock = inst->getParent();
    }
    else {
        curBlock = storeBlock;
    }

    if(curBlock != storeBlock && pred_size(curBlock) != 1) {
        return;
    }

    /* 只有storeBlock才会到达curBlock */
    while(inst != nullptr) {
        if(inst->getOpcode() == llvm::Instruction::Store) {
            auto *tmpInst = llvm::dyn_cast<llvm::StoreInst>(inst);
            /* 碰到变量的store指令，返回，停止搜索 */
            if(tmpInst->getPointerOperand() == storeInst->getPointerOperand()) {
                return;
            }
        }
        else if(inst->getOpcode() == llvm::Instruction::Load) {
            auto *loadInst = llvm::dyn_cast<llvm::LoadInst>(inst);
            /* 找到关于变量的一条Load指令 */
            if(loadInst->getOperand(0) == storeInst->getPointerOperand()) {
                this->modified = true;
                loadInst->replaceAllUsesWith(storeInst->getValueOperand());
                inst = inst->getNextNode();
                loadInst->eraseFromParent();
                continue;
            }
        }
        inst = inst->getNextNode();
    }
    auto curNode = this->domTree.getNode(curBlock);
    for(auto child : curNode->children()) {
        findLoadInst(&child->getBlock()->front(), storeInst);
    }
}

void sysu::LoadStoreOpt::findStoreInst(llvm::Instruction *inst) {
    if(inst == nullptr) return;

    llvm::BasicBlock *curBlock = inst->getParent(); /* 当前基本块 */
    while(inst != nullptr) {
        if(inst->getOpcode() == llvm::Instruction::Store) {
            auto *storeInst = llvm::dyn_cast<llvm::StoreInst>(inst);
            auto flag = std::find(varVector.begin(), varVector.end(), storeInst->getPointerOperand());
            if(flag != varVector.end()) {
                findLoadInst(inst->getNextNode(), storeInst);
            }
        }
        /* Alloca 指令是不会被删除的 */
        inst = inst->getNextNode();
    }

    /* DFS的方式在支配上中搜索 */
    auto curNode = this->domTree.getNode(curBlock);
    for(auto child : curNode->children()) {
        findStoreInst(&child->getBlock()->front());
    }
}

llvm::PreservedAnalyses sysu::LoadStoreOpt::run(llvm::Function &F, llvm::FunctionAnalysisManager &FAM) {
    if(F.empty()) return llvm::PreservedAnalyses::all();
    this->modified = false;
    this->domTree = llvm::DominatorTree(F);
    llvm::BasicBlock &entryBlock = F .getEntryBlock();
    llvm::Instruction *nextInst = nullptr;
    /* 收集所有的Alloca指令 */
    for(auto &inst : entryBlock.getInstList()) {
        if(inst.getOpcode() == llvm::Instruction::Alloca) {
            auto *allocaInst = llvm::dyn_cast<llvm::AllocaInst>(&inst);
            if(allocaInst->getAllocatedType()->isArrayTy()) {
                continue;
            }
            varVector.push_back(allocaInst);
            nextInst = allocaInst->getNextNode();
        }
    }

    /* AllocaInst之后一定有其他指令，return或者br */
    if(nextInst != nullptr) {
        findStoreInst(nextInst);
    }

    llvm::errs() << "Load and store optimization done.\n";
    return llvm::PreservedAnalyses::all();
}

void sysu::ConstantFolding::arithInstOpt(llvm::DomTreeNodeBase<llvm::BasicBlock> *root) {
    llvm::BasicBlock *curBlock = root->getBlock();

    for(auto iter = curBlock->begin(); iter != curBlock->end(); ++ iter) {
        if(iter->isBinaryOp()) {
            auto *const1 = llvm::dyn_cast<llvm::Constant>(iter->getOperand(0));
            auto *const2 = llvm::dyn_cast<llvm::Constant>(iter->getOperand(1));

            auto opCode = iter->getOpcode();
            llvm::Value *res = nullptr;
            if(const1 == nullptr && const2 != nullptr) {
                if(opCode != llvm::Instruction::SDiv) continue;
                auto *constInt = llvm::dyn_cast<llvm::ConstantInt>(const2);
                if(constInt == nullptr || constInt != llvm::ConstantInt::get(constInt->getType(), 2)) continue;

                llvm::IRBuilder<> builder(&*iter);
                res = builder.CreateAShr(iter->getOperand(0), 1);
            }
            else if(const1 != nullptr && const2 != nullptr) res = llvm::ConstantExpr::get(opCode, const1, const2);
            else continue;

            this->modified = true;
            iter->replaceAllUsesWith(res);
            llvm::Instruction *inst = &*iter;
            iter --;
            inst->eraseFromParent();
        }
        else if(iter->getOpcode() == llvm::Instruction::FCmp || iter->getOpcode() == llvm::Instruction::ICmp) {
            auto *const1 = llvm::dyn_cast<llvm::Constant>(iter->getOperand(0));
            if(const1 == nullptr) {
                continue;
            }
            auto *const2 = llvm::dyn_cast<llvm::Constant>(iter->getOperand(1));
            if(const2 == nullptr) {
                continue;
            }
            auto *cmpInst = llvm::dyn_cast<llvm::CmpInst>(&*iter);
            auto predicate = cmpInst->getPredicate();
            llvm::Value *res = nullptr;
            if(iter->getOpcode() == llvm::Instruction::FCmp) {
                res = llvm::ConstantExpr::getFCmp(predicate, const1, const2);
            }
            else {
                res = llvm::ConstantExpr::getICmp(predicate, const1, const2);
            }
            this->modified = true;
            iter->replaceAllUsesWith(res);
            iter --;
            cmpInst->eraseFromParent();
        }
    }
    for(auto child : root->children()) {
        arithInstOpt(child);
    }
}

llvm::PreservedAnalyses sysu::ConstantFolding::run(llvm::Function &F, llvm::FunctionAnalysisManager &FAM) {
    if(F.empty()) return llvm::PreservedAnalyses::all();
    this->modified = false;
    arithInstOpt(llvm::DominatorTree(F).getRootNode());
    llvm::errs() << "Constant folding done.\n";
    return llvm::PreservedAnalyses::all();
}


void sysu::BranchOpt::phiInstOpt(llvm::DomTreeNodeBase<llvm::BasicBlock> *root) {
    auto *inst = llvm::dyn_cast<llvm::PHINode>(&root->getBlock()->front());
    if(inst != nullptr) {
        llvm::Value *val = nullptr;
        bool flag = true;
        for(int i = 0; i < inst->getNumIncomingValues(); ++ i) {
            if(inst->getIncomingBlock(i) == nullptr) {
                inst->removeIncomingValue(i);
                i --;
                continue;
            }
            if(val == nullptr) val = inst->getIncomingValue(i);
            else if(val != inst->getIncomingValue(i)) {
                flag = false;
                break;
            }
        }
        if(flag) {
            this->modified = true;
            inst->replaceAllUsesWith(val);
            inst->eraseFromParent();
        }
    }
    for(auto &child : root->children()) {
        phiInstOpt(child);
    }
}

void sysu::BranchOpt::condBrInstOpt(llvm::Function &F) {
    /* 遍历所有基本块 */
    for(auto &block : F.getBasicBlockList()) {
        /* 是否最后一条指令为跳转 */
        auto *brInst = llvm::dyn_cast<llvm::BranchInst>(block.getTerminator());
        if(brInst == nullptr || brInst->isUnconditional()) continue;
        /* 是跳转指令并且为有条件跳转 */
        auto *cond = llvm::dyn_cast<llvm::Constant>(brInst->getCondition());
        if(cond == nullptr) continue;
        /* 条件为常量，可以将有跳转变成无跳转 */
        llvm::BranchInst *tmpInst = nullptr;
        if(cond->isZeroValue()) {
            tmpInst = llvm::BranchInst::Create((brInst->getSuccessor(1)));
            auto *phiInst = llvm::dyn_cast<llvm::PHINode>(&brInst->getSuccessor(0)->front());
            if(phiInst != nullptr) {
                phiInst->removeIncomingValue(&block);
            }
        }
        else {
            tmpInst = llvm::BranchInst::Create(brInst->getSuccessor(0));
            auto *phiInst = llvm::dyn_cast<llvm::PHINode>(&brInst->getSuccessor(1)->front());
            if(phiInst != nullptr) {
                phiInst->removeIncomingValue(&block);
            }
        }
        this->modified = true;
        block.getInstList().push_back(tmpInst);
        brInst->eraseFromParent();
    }
}

void sysu::BranchOpt::unusedBlockElimination(llvm::Function &F) {
    auto iter = F.getBasicBlockList().begin();
    iter ++;
    for(; iter != F.getBasicBlockList().end(); ++ iter) {
        llvm::BasicBlock *block = &*iter;
        if(pred_empty(block)) {
            iter --;
            this->modified = true;
            block->replaceSuccessorsPhiUsesWith(nullptr);
            block->eraseFromParent();
        }
    }
}

void sysu::BranchOpt::unCondBrInstOpt(llvm::BasicBlock *block) {
    auto *brInst = llvm::dyn_cast<llvm::BranchInst>(block->getTerminator());
    if(brInst == nullptr) return;

    if(brInst->isUnconditional()) {
        llvm::BasicBlock *destBlock = brInst->getSuccessor(0);
        if(pred_size(destBlock) == 1) {
            brInst->eraseFromParent();
            destBlock->replaceSuccessorsPhiUsesWith(block);
            block->getInstList().splice(block->end(), destBlock->getInstList());
            destBlock->eraseFromParent();
            this->modified = true;
        }
    }
    llvm::DominatorTree domTree(*block->getParent());
    auto curNode = domTree.getNode(block);
    for(auto child : curNode->children()) {
        unCondBrInstOpt(child->getBlock());
    }
}

llvm::PreservedAnalyses sysu::BranchOpt::run(llvm::Function &F, llvm::FunctionAnalysisManager &FAM) {
    if(F.empty()) return llvm::PreservedAnalyses::all();

    this->modified = true;
    while(this->modified) {
        this->modified = false;
        phiInstOpt(llvm::DominatorTree(F).getRootNode());
        condBrInstOpt(F);
        unusedBlockElimination(F);
        phiInstOpt(llvm::DominatorTree(F).getRootNode());
        unCondBrInstOpt(&F.getEntryBlock());
    }

    llvm::errs() << "Branch optimization done.\n";
    return llvm::PreservedAnalyses::all();
}

void sysu::DeadCodeElimination::removeAllocaInst(llvm::BasicBlock &entry) {
   for(auto iter = entry.begin(); iter != entry.end(); iter ++){
        if(iter->getOpcode() != llvm::Instruction::Alloca) break;
        /* Opcode = Alloca */
        if(llvm::dyn_cast<llvm::AllocaInst>(&*iter)->getAllocatedType()->isArrayTy()) {
            continue;
        }
        /* 是否被Load过 */
        bool hasLoadUser = false;
        llvm::SmallVector<llvm::Instruction *, 30> instVector;

        for(auto user : iter->users()) {
            auto *tmpInst = llvm::dyn_cast<llvm::Instruction>(user);
            if(tmpInst->getOpcode() == llvm::Instruction::Load) {
                hasLoadUser = true;
                break;
            }
            instVector.push_back(tmpInst);
        }
        if(!hasLoadUser) {
            this->modified = true;
            for(auto inst : instVector) {
                inst->eraseFromParent();
            }
            llvm::Instruction *tmpInst = &*iter;
            iter --;
            tmpInst->eraseFromParent();
        }
    }
}

void sysu::DeadCodeElimination::removeDeadInst(llvm::Function &F) {
    for(auto &block : F.getBasicBlockList()) {
        for(auto iter = block.begin(); iter != block.end(); ++ iter) {
            if(iter->hasNUsesOrMore(1)) continue;
            auto opCode = iter->getOpcode();
            if(opCode ==  llvm::Instruction::Ret) continue;
            if(opCode == llvm::Instruction::Br) continue;
            if(opCode == llvm::Instruction::Call) continue;
            if(opCode == llvm::Instruction::Store) continue;
            this->modified = true;
            llvm::Instruction *tmpInst = &*iter;
            iter --;
            tmpInst->eraseFromParent();
        }
    }
}

llvm::PreservedAnalyses sysu::DeadCodeElimination::run(llvm::Function &F, llvm::FunctionAnalysisManager &FAM) {
    if(F.empty())  return llvm::PreservedAnalyses::all();
    this->modified = true;
    while(this->modified) {
        this->modified = false;
        removeAllocaInst(F.getEntryBlock());
        removeDeadInst(F);
    }
    llvm::errs() << "Dead code elimination done.\n";
    return llvm::PreservedAnalyses::all();
}

llvm::PreservedAnalyses sysu::Test::run(llvm::Function &F, llvm::FunctionAnalysisManager &FAM) {
    auto iter = F.getEntryBlock().begin();
    iter --;
    F.getEntryBlock().front().eraseFromParent();
    iter ++;
    llvm::errs() << *iter << "\n";
    return llvm::PreservedAnalyses::all();
}