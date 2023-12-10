#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/Type.h>
#include <llvm/Support/JSON.h>
#include <llvm/Support/MemoryBuffer.h>
#include <llvm/Support/raw_ostream.h>
#include <llvm/IR/GlobalVariable.h>
#include <llvm/IR/Constants.h>
#include <llvm/IR/ValueSymbolTable.h>
#include <llvm/Support/Casting.h>
#include <iostream>
#include <stack>
#include <algorithm>
namespace {
llvm::LLVMContext TheContext;
llvm::IRBuilder<> TheBuilder(TheContext);
llvm::Module TheModule("-", TheContext);
std::stack<llvm::BasicBlock *> whileEndStack;
std::stack<llvm::BasicBlock *> whileCondStack;
std::vector<std::string> igFuncName = {"memmove", "sqrt"};

llvm::Value *getArrLVal(llvm::json::Object *exp, std::vector<llvm::Value *> &idxList);
llvm::Value *getExpVal(llvm::json::Object *exp, bool oneBit=false);
void buildIfStmt(llvm::json::Object *ifStmt);
void buildCompoundStmt(llvm::json::Object *compoundStmt);
llvm::Value *getBinaryOpVal(llvm::json::Object *exp);
llvm::Value *buildCallExpr(llvm::json::Object *exp);
void buildStmt(llvm::json::Object *stmt);

llvm::Type *getVarType(llvm::StringRef &typeString, bool *isConstant=nullptr) {
    llvm::Type *res = nullptr;
    llvm::Type *arrayType = nullptr;
    if(typeString.contains("const") && isConstant != nullptr) {
        *isConstant = true;
    }
    if(typeString.contains("int")) {
        res = llvm::Type::getInt32Ty(TheContext);
    }
    else if(typeString.contains("float")) {
        res = llvm::Type::getFloatTy(TheContext);
    }
    else if(typeString.contains("long")) {
        res = llvm::Type::getInt64Ty(TheContext);
    }
    else if(typeString.contains("char")) {
        res = llvm::Type::getInt8Ty(TheContext);
    }

    if(typeString.contains('[')) {
        size_t right = typeString.rfind(']');
        while(right != std::string::npos) {
            size_t left = typeString.rfind('[', right);
            llvm::StringRef tmp = typeString.substr(left + 1, right - left - 1);
            auto size = std::stoi(tmp.str());
            arrayType = llvm::ArrayType::get(res, size);
            res = arrayType;
            right = typeString.rfind(']', left);
        }
    }
    if(typeString.contains('*')) {
        res = res->getPointerTo();
    }
    return res;
}

llvm::Value *getArrLVal(llvm::json::Object *exp, std::vector<llvm::Value *> &idxList) {
    llvm::StringRef kind = exp->getString("kind").getValue();
    llvm::Value *res = nullptr;
    if(kind == "DeclRefExpr") {
        llvm::StringRef varId = exp->getObject("referencedDecl")->getString("id").getValue();
        llvm::Value *var = nullptr;
        if(TheBuilder.GetInsertBlock() == nullptr) {
            var = TheModule.getGlobalVariable(varId);
        }
        else {
            var = TheBuilder.GetInsertBlock()->getParent()->getValueSymbolTable()->lookup(varId);
            if(var == nullptr) {
                var = TheModule.getGlobalVariable(varId);
            }
        }

        std::reverse(idxList.begin() + 1, idxList.end());
        if(var->getType()->getPointerElementType()->isPointerTy()) {
            var = TheBuilder.CreateLoad(var);
            idxList.erase(idxList.begin());
        }

        res = TheBuilder.CreateInBoundsGEP(var, idxList);
    }
    else if(kind == "ArraySubscriptExpr") {
        llvm::json::Array *inner = exp->getArray("inner");
        llvm::Value *idx = getExpVal((*inner)[1].getAsObject());
        idxList.push_back(idx);
        res = getArrLVal((*inner)[0].getAsObject(), idxList);
    }
    else if(kind == "ImplicitCastExpr") {
        llvm::json::Array *inner = exp->getArray("inner");
        res = getArrLVal((*inner)[0].getAsObject(), idxList);
    }
    return res;
}

llvm::Value *cmpWithZero(llvm::Value *val) {
    llvm::Value *res = nullptr;
    if(val->getType()->isFloatTy()) {
        res = TheBuilder.CreateFCmpUNE(val, llvm::ConstantFP::get(llvm::Type::getFloatTy(TheContext), 0));
    }
    else if(val->getType()->isDoubleTy()) {
        res = TheBuilder.CreateFCmpUNE(val, llvm::ConstantFP::get(llvm::Type::getDoubleTy(TheContext), 0));
    }
    else if(val->getType()->isIntegerTy(32)) {
        res = TheBuilder.CreateICmpNE(val, llvm::ConstantInt::get(llvm::Type::getInt32Ty(TheContext), 0));
    }
    else if(val->getType()->isIntegerTy(8)) {
        res = TheBuilder.CreateICmpNE(val, llvm::ConstantInt::get(llvm::Type::getInt8Ty(TheContext), 0));
    }
    return res;
}

void extBitWidth(llvm::Value **l, llvm::Value **r) {
    llvm::Type *lType = (*l)->getType();
    llvm::Type *rType = (*r)->getType();
    if(lType->isIntegerTy(1) && rType->isIntegerTy(32)) {
        *l = TheBuilder.CreateZExt(*l, llvm::Type::getInt32Ty(TheContext));
    }
    else if(lType->isIntegerTy(32) && rType->isIntegerTy(1)) {
        *r = TheBuilder.CreateZExt(*r, llvm::Type::getInt32Ty(TheContext));
    }
}
llvm::Value *getBinaryOpVal(llvm::json::Object *exp) {
    llvm::Value *res = nullptr;
    llvm::StringRef opcode = exp->getString("opcode").getValue();
    llvm::json::Array *inner = exp->getArray("inner");
    llvm::json::Object *lhs = inner->front().getAsObject();
    llvm::json::Object *rhs = (*inner)[1].getAsObject();
    if(opcode == "+") {
        llvm::Value *leftValue = getExpVal(lhs);
        llvm::Value *rightValue = getExpVal(rhs);
        llvm::Type *type = leftValue->getType();
        if(type->isIntegerTy()) {
            res = TheBuilder.CreateAdd(leftValue, rightValue);
        }
        else if(type->isFloatTy() || type->isDoubleTy()) {
            res = TheBuilder.CreateFAdd(leftValue, rightValue);
        }
    }
    else if(opcode == "*") {
        llvm::Value *leftValue = getExpVal(lhs);
        llvm::Value *rightValue = getExpVal(rhs);
        llvm::Type *type = leftValue->getType();
        if(type->isIntegerTy()) {
            res = TheBuilder.CreateMul(leftValue, rightValue);
        }
        else if(type->isFloatTy() || type->isDoubleTy()) {
            res = TheBuilder.CreateFMul(leftValue, rightValue);
        }
    }
    else if(opcode == "-") {
        llvm::Value *leftValue = getExpVal(lhs);
        llvm::Value *rightValue = getExpVal(rhs);
        llvm::Type *type = leftValue->getType();
        if(type->isIntegerTy()) {
            res = TheBuilder.CreateSub(leftValue, rightValue);
        }
        else if(type->isFloatTy() || type->isDoubleTy()) {
            res = TheBuilder.CreateFSub(leftValue, rightValue);
        }
    }
    else if(opcode == "/") {
        llvm::Value *leftValue = getExpVal(lhs);
        llvm::Value *rightValue = getExpVal(rhs);
        llvm::Type *type = leftValue->getType();
        if(type->isIntegerTy()) {
            res = TheBuilder.CreateSDiv(leftValue, rightValue);
        }
        else if(type->isFloatTy() || type->isDoubleTy()) {
            res = TheBuilder.CreateFDiv(leftValue, rightValue);
        }
    }
    else if(opcode == "%") {
        llvm::Value *leftValue = getExpVal(lhs);
        llvm::Value *rightValue = getExpVal(rhs);
        llvm::Type *type = leftValue->getType();
        if(type->isIntegerTy()) {
            res = TheBuilder.CreateSRem(leftValue, rightValue);
        }
        else if(type->isFloatTy() || type->isDoubleTy()) {
            res = TheBuilder.CreateFRem(leftValue, rightValue);
        }
    }
    else if(opcode == ">") {
        llvm::Value *leftValue = getExpVal(lhs);
        llvm::Value *rightValue = getExpVal(rhs);
        llvm::Type *type = leftValue->getType();
        if(type->isIntegerTy()) {
            res = TheBuilder.CreateICmpSGT(leftValue, rightValue);
        }
        else if(type->isFloatTy() || type->isDoubleTy()) {
            res = TheBuilder.CreateFCmpUGT(leftValue, rightValue);
        }
    }
    else if(opcode == "<") {
        llvm::Value *leftValue = getExpVal(lhs);
        llvm::Value *rightValue = getExpVal(rhs);
        llvm::Type *type = leftValue->getType();
        if(type->isIntegerTy()) {
            res = TheBuilder.CreateICmpSLT(leftValue, rightValue);
        }
        else if(type->isFloatTy() || type->isDoubleTy()) {
            res = TheBuilder.CreateFCmpULT(leftValue, rightValue);
        }
    }
    else if(opcode == "==") {
        llvm::Value *leftValue = getExpVal(lhs);
        llvm::Value *rightValue = getExpVal(rhs);
        extBitWidth(&leftValue, &rightValue);
        llvm::Type *type = leftValue->getType();
        if(type->isIntegerTy()) {
            res = TheBuilder.CreateICmpEQ(leftValue, rightValue);
        }

        else if(type->isFloatTy() || type->isDoubleTy()) {
            res = TheBuilder.CreateFCmpUEQ(leftValue, rightValue);
        }
    }
    else if(opcode == "!=") {
        llvm::Value *leftValue = getExpVal(lhs);
        llvm::Value *rightValue = getExpVal(rhs);
        extBitWidth(&leftValue, &rightValue);
        llvm::Type *type = leftValue->getType();
        if(type->isIntegerTy()) {
            res = TheBuilder.CreateICmpNE(leftValue, rightValue);
        }
        else if(type->isFloatTy() || type->isDoubleTy()) {
            res = TheBuilder.CreateFCmpUNE(leftValue, rightValue);
        }
    }
    else if(opcode == ">=") {
        llvm::Value *leftValue = getExpVal(lhs);
        llvm::Value *rightValue = getExpVal(rhs);
        llvm::Type *type = leftValue->getType();
        if(type->isIntegerTy()) {
            res = TheBuilder.CreateICmpSGE(leftValue, rightValue);
        }
        else if(type->isFloatTy() || type->isDoubleTy()) {
            res = TheBuilder.CreateFCmpUGT(leftValue, rightValue);
        }
    }
    else if(opcode == "<=") {
        llvm::Value *leftValue = getExpVal(lhs);
        llvm::Value *rightValue = getExpVal(rhs);
        llvm::Type *type = leftValue->getType();
        if(type->isIntegerTy()) {
            res = TheBuilder.CreateICmpSLE(leftValue, rightValue);
        }
        else if(type->isFloatTy() || type->isDoubleTy()) {
            res = TheBuilder.CreateFCmpULE(leftValue, rightValue);
        }
    }
    else if(opcode == "&&") {
        llvm::Value *leftValue = getExpVal(lhs, true);
        llvm::Type *type = leftValue->getType();

        llvm::BasicBlock *curBlock = TheBuilder.GetInsertBlock();
        llvm::Function *func = curBlock->getParent();
        llvm::BasicBlock *lhsTrue = llvm::BasicBlock::Create(TheContext, "land.lhs.true", func);
        llvm::BasicBlock *merge = llvm::BasicBlock::Create(TheContext, "land.merge");
        TheBuilder.CreateCondBr(leftValue, lhsTrue, merge);

        TheBuilder.SetInsertPoint(lhsTrue);
        llvm::Value *rightValue = getExpVal(rhs, true);
        llvm::BasicBlock *lhsTrueLastBlock = TheBuilder.GetInsertBlock();
        TheBuilder.CreateBr(merge);

        TheBuilder.SetInsertPoint(merge);
        llvm::PHINode *phi = TheBuilder.CreatePHI(llvm::Type::getInt1Ty(TheContext), 2, "merge");
        phi->addIncoming(leftValue, curBlock);
        phi->addIncoming(rightValue, lhsTrueLastBlock);
        func->getBasicBlockList().push_back(merge);
        res = phi;
    }
    else if(opcode == "||") {
        llvm::Value *leftValue = getExpVal(lhs, true);
        llvm::Type *type = leftValue->getType();
        llvm::BasicBlock *curBlock = TheBuilder.GetInsertBlock();
        llvm::Function *func = curBlock->getParent();
        llvm::BasicBlock *lhsFalse = llvm::BasicBlock::Create(TheContext, "lor.lhs.false", func);
        llvm::BasicBlock *merge = llvm::BasicBlock::Create(TheContext, "lor.merge");
        TheBuilder.CreateCondBr(leftValue, merge, lhsFalse);

        TheBuilder.SetInsertPoint(lhsFalse);
        llvm::Value *rightValue = getExpVal(rhs, true);
        llvm::BasicBlock *lhsFalseLastBlock = TheBuilder.GetInsertBlock();
        TheBuilder.CreateBr(merge);


        TheBuilder.SetInsertPoint(merge);
        llvm::PHINode *phi = TheBuilder.CreatePHI(llvm::Type::getInt1Ty(TheContext), 2, "merge");
        phi->addIncoming(leftValue, curBlock);
        phi->addIncoming(rightValue, lhsFalseLastBlock);
        func->getBasicBlockList().push_back(merge);
        res = phi;
    }

    return res;
}

llvm::Value *buildCallExpr(llvm::json::Object *exp) {
    llvm::Value *res = nullptr;
    llvm::json::Array *inner = exp->getArray("inner");
    llvm::json::Array *tmp = inner->front().getAsObject()->getArray("inner");
    llvm::StringRef funcName = tmp->front().getAsObject()->getObject("referencedDecl")->getString("name").getValue();
    llvm::Function *func = TheModule.getFunction(funcName);

    std::vector<llvm::Value *> args;
    size_t idx = 1;
    while(idx < inner->size()) {
        llvm::json::Object *t = (*inner)[idx].getAsObject();
        llvm::Value *arg = getExpVal(t);
        args.push_back(arg);
        idx ++;
    }
    res = TheBuilder.CreateCall(func, args);
    return res;
}

llvm::Value *getExpVal(llvm::json::Object *exp, bool oneBit) {
    llvm::Value *res = nullptr;
    llvm::StringRef kind = exp->getString("kind").getValue();
    if(kind == "IntegerLiteral") {
        uint64_t val = std::stoll(exp->getString("value").getValue().str());
        res = llvm::ConstantInt::get(llvm::Type::getInt32Ty(TheContext), val);
    }
    else if(kind == "FloatingLiteral") {
        llvm::StringRef val = exp->getString("value").getValue();
        res = llvm::ConstantFP::get(llvm::Type::getDoubleTy(TheContext),
                                    llvm::APFloat(llvm::APFloat::IEEEdouble(), val));
    }
    else if(kind == "StringLiteral") {
        llvm::StringRef valString = exp->getString("value").getValue();
        llvm::StringRef typeString = exp->getObject("type")->getString("qualType").getValue();
        auto type = llvm::dyn_cast<llvm::ArrayType>(getVarType(typeString));
        std::vector<llvm::Constant *>val;
        for(int i = 1; i < valString.size() - 1; ++ i) {
            char f = valString[i];
            if(f == '\\') {
                i ++;
                if(valString[i] == '\\') {
                    val.push_back(llvm::ConstantInt::get(llvm::Type::getInt8Ty(TheContext), '\\'));
                }
                else if(valString[i] == 'n') {
                    val.push_back(llvm::ConstantInt::get(llvm::Type::getInt8Ty(TheContext), '\n'));
                }
                else if(valString[i] == '\"') {
                    val.push_back(llvm::ConstantInt::get(llvm::Type::getInt8Ty(TheContext), '\"'));
                }
            }
            else {
                val.push_back(llvm::ConstantInt::get(llvm::Type::getInt8Ty(TheContext), f));
            }
        }
        while(val.size() < type->getArrayNumElements()) {
            val.push_back(llvm::ConstantInt::get(llvm::Type::getInt8Ty(TheContext), 0));
        }
        if(val.size() > type->getArrayNumElements()) {
            while(val.size() > type->getArrayNumElements() - 1) {
                val.pop_back();
            }
            val.push_back(llvm::ConstantInt::get(llvm::Type::getInt8Ty(TheContext), 0));
        }
        res = llvm::ConstantArray::get(type, val);

    }
    else if(kind == "ImplicitCastExpr") {
        llvm::StringRef castKind = exp->getString("castKind").getValue();
        if(castKind == "LValueToRValue") {
            llvm::json::Object *tmp = exp->getArray("inner")->front().getAsObject();
            res = getExpVal(tmp);
        }
        else if(castKind == "NoOp") {
            res = getExpVal(exp->getArray("inner")->front().getAsObject());
        }
        else if(castKind == "BitCast") {
            llvm::Value *val = getExpVal(exp->getArray("inner")->front().getAsObject());
            std::vector<llvm::Value *>idxList = {
                    llvm::ConstantInt::get(llvm::Type::getInt32Ty(TheContext), 0),
                    llvm::ConstantInt::get(llvm::Type::getInt32Ty(TheContext), 0)
            };

            res = TheBuilder.CreateInBoundsGEP(val, idxList);

        }
        else if(castKind == "ArrayToPointerDecay") {
            std::vector<llvm::Value *>idxList = {
                    llvm::ConstantInt::get(llvm::Type::getInt32Ty(TheContext), 0),
                    llvm::ConstantInt::get(llvm::Type::getInt32Ty(TheContext), 0)
            };
            llvm::json::Object *tmp = exp->getArray("inner")->front().getAsObject();
            if(tmp->getString("kind").getValue() == "StringLiteral") {
                llvm::Value *val = getExpVal(tmp);
                auto strArray = TheBuilder.CreateAlloca(val->getType());
                TheBuilder.CreateStore(val, strArray);
                res = TheBuilder.CreateInBoundsGEP(strArray, idxList);
            }
            else {
                res = getArrLVal(exp->getArray("inner")->front().getAsObject(), idxList);
            }
        }
        else if(castKind == "IntegralToFloating") {
            llvm::StringRef type = exp->getObject("type")->getString("qualType").getValue();
            llvm::Value *val = getExpVal(exp->getArray("inner")->front().getAsObject());
            if(type == "double") {
                res = TheBuilder.CreateCast(llvm::Instruction::SIToFP, val, llvm::Type::getDoubleTy(TheContext));
            }
            else if(type == "float") {
                res = TheBuilder.CreateCast(llvm::Instruction::SIToFP, val, llvm::Type::getFloatTy(TheContext));
            }

        }
        else if(castKind == "FloatingCast") {
            llvm::StringRef type = exp->getObject("type")->getString("qualType").getValue();
            llvm::Value *val = getExpVal(exp->getArray("inner")->front().getAsObject());
            if(type == "double") {
                res = TheBuilder.CreateFPCast(val, llvm::Type::getDoubleTy(TheContext));
            }
            else if(type == "float") {
                res = TheBuilder.CreateFPCast(val, llvm::Type::getFloatTy(TheContext));
            }

        }
        else if(castKind == "FloatingToIntegral") {
            llvm::Value *val = getExpVal(exp->getArray("inner")->front().getAsObject());
            res = TheBuilder.CreateCast(llvm::Instruction::FPToSI, val, llvm::Type::getInt32Ty(TheContext));
        }
        else if(castKind == "IntegralCast") {
            llvm::Value *val = getExpVal(exp->getArray("inner")->front().getAsObject());
            llvm::StringRef type = exp->getObject("type")->getString("qualType").getValue();
            if(type == "long long") {
                res = TheBuilder.CreateIntCast(val, llvm::Type::getInt64Ty(TheContext), val);
            }
            else if(type == "int") {
                res = TheBuilder.CreateIntCast(val, llvm::Type::getInt32Ty(TheContext), val);
            }
            else {
                res = val;
            }
        }
    }
    else if(kind == "DeclRefExpr") {
        llvm::StringRef varId = exp->getObject("referencedDecl")->getString("id").getValue();
        llvm::Value *var = nullptr;
        if(TheBuilder.GetInsertBlock() == nullptr) {
            var = TheModule.getGlobalVariable(varId);
        }
        else {
            var = TheBuilder.GetInsertBlock()->getParent()->getValueSymbolTable()->lookup(varId);
            if(var == nullptr) {
                var = TheModule.getGlobalVariable(varId);
            }
        }
        res = TheBuilder.CreateLoad(var);
    }
    else if(kind == "ArraySubscriptExpr") {
        std::vector<llvm::Value *> idxList = {llvm::ConstantInt::get(llvm::Type::getInt32Ty(TheContext), 0)};
        res = TheBuilder.CreateLoad(getArrLVal(exp, idxList));
    }
    else if(kind == "BinaryOperator") {
        res = getBinaryOpVal(exp);
    }
    else if(kind == "CallExpr") {
        res = buildCallExpr(exp);
    }
    else if(kind == "UnaryOperator") {
        llvm::StringRef op = exp->getString("opcode").getValue();
        llvm::json::Array *inner = exp->getArray("inner");
        if(op == "-") {
            llvm::Value *val = getExpVal(inner->front().getAsObject());
            if(val->getType()->isIntegerTy(1)) {
                val = TheBuilder.CreateZExt(val, llvm::Type::getInt32Ty(TheContext));
            }
            if(val->getType()->isIntegerTy()) {
                res = TheBuilder.CreateNeg(val);
            }
            else if(val->getType()->isFloatTy() || val->getType()->isDoubleTy()) {
                res = TheBuilder.CreateFNeg(val);
            }
        }
        else if(op == "+") {
            res = getExpVal(inner->front().getAsObject());
        }
        else if(op == "!") {
            llvm::Value *tmp = getExpVal(inner->front().getAsObject());
            if(!tmp->getType()->isIntegerTy(1)) {
                tmp = cmpWithZero(tmp);
            }
            res = TheBuilder.CreateNot(tmp);
        }
    }
    else if(kind == "ParenExpr") {
        llvm::json::Array *inner = exp->getArray("inner");
        res = getExpVal(inner->front().getAsObject());
    }
    if(oneBit && !res->getType()->isIntegerTy(1)) {
        res = cmpWithZero(res);
    }
    return res;
}

llvm::Type *getFuncRetType(llvm::StringRef &typeString, bool &isVarArg) {
    if(typeString.contains("...")) {
        isVarArg = true;
    }
    llvm::Type *res = nullptr;
    size_t spaceIdx = typeString.find(' ');
    std::string tmp = typeString.substr(0, spaceIdx).str();
    if(tmp == "int") {
        res = llvm::Type::getInt32Ty(TheContext);
    }
    else if(tmp == "void") {
        res = llvm::Type::getVoidTy(TheContext);
    }
    else if(tmp == "float") {
        res = llvm::Type::getFloatTy(TheContext);
    }
    else if(tmp == "long") {
        res = llvm::Type::getInt64Ty(TheContext);
    }
    else if(tmp == "char") {
        res = llvm::Type::getInt8Ty(TheContext);
    }
    return res;
}

void buildRetStmt(llvm::json::Object *retStmt) {
    llvm::json::Array *inner = retStmt->getArray("inner");
    if(inner == nullptr) {
        TheBuilder.CreateRetVoid();
    }
    else {
        llvm::Value *val = getExpVal(inner->front().getAsObject());
        TheBuilder.CreateRet(val);
    }
}

void buildAssiStmt(llvm::json::Object *assiStmt) {
    llvm::json::Array *inner = assiStmt->getArray("inner");
    llvm::json::Object *lhs = (*inner)[0].getAsObject();
    llvm::json::Object *rhs = (*inner)[1].getAsObject();
    std::string lhsKind = lhs->getString("kind").getValue().str();
    llvm::Value *var = nullptr;
    if(lhsKind == "DeclRefExpr") {
        llvm::StringRef varId = lhs->getObject("referencedDecl")->getString("id").getValue();
        var = TheBuilder.GetInsertBlock()->getParent()->getValueSymbolTable()->lookup(varId);
        if(var == nullptr) {
            var = TheModule.getGlobalVariable(varId);
        }
    }
    else if(lhsKind == "ArraySubscriptExpr") {
        std::vector<llvm::Value *> idxList = {llvm::ConstantInt::get(llvm::Type::getInt32Ty(TheContext), 0)};
        var = getArrLVal(lhs, idxList);
    }
    llvm::Value *val = getExpVal(rhs);

    TheBuilder.CreateStore(val, var);
}

void localArrInit(llvm::Value *var, llvm::json::Object *exp, std::vector<llvm::Value *> &idxList) {
    llvm::json::Array *arrayFiller = exp->getArray("array_filler");
    if(arrayFiller == nullptr) {
        llvm::json::Array *inner = exp->getArray("inner");
        int idx = 0;
        for(auto &value : *inner) {
            llvm::StringRef kind = value.getAsObject()->getString("kind").getValue();
            std::vector<llvm::Value *> tmp(idxList);
            tmp.push_back(llvm::ConstantInt::get(llvm::Type::getInt32Ty(TheContext), idx));
            if(kind == "InitListExpr") {
                localArrInit(var, value.getAsObject(), tmp);
            }
            else {
                llvm::Value *val = getExpVal(value.getAsObject());
                auto arrayIdx = TheBuilder.CreateInBoundsGEP(var, tmp);
                TheBuilder.CreateStore(val, arrayIdx);
            }
            idx ++;
        }
    }
    else {
        int idx = 0;
        for(auto &value : *arrayFiller) {
            llvm::StringRef kind = value.getAsObject()->getString("kind").getValue();
            if(kind == "ImplicitValueInitExpr") {
                continue;
            }
            else  {
                std::vector<llvm::Value *> tmp(idxList);
                tmp.push_back(llvm::ConstantInt::get(llvm::Type::getInt32Ty(TheContext), idx));
                if(kind == "InitListExpr") {
                    localArrInit(var, value.getAsObject(), tmp);

                }
                else {
                    llvm::Value *val = getExpVal(value.getAsObject());
                    auto arrayIdx = TheBuilder.CreateInBoundsGEP(var, tmp);
                    TheBuilder.CreateStore(val, arrayIdx);
                }
                idx ++;
            }
        }
    }
}

bool isCharArray(llvm::Type *type) {
    llvm::Type *tmp = type->getArrayElementType();
    while(tmp->isArrayTy()) {
        type = tmp;
        tmp  = type->getArrayElementType();
    }
    if(tmp->isIntegerTy(8)) {
        return true;
    }
    else {
        return false;
    }
}
void buildVarDecl(llvm::json::Object *varDecl) {
    llvm::StringRef varId = varDecl->getString("id").getValue();
    llvm::StringRef varTypeString = varDecl->getObject("type")->getString("qualType").getValue();

    llvm::Type *varType = getVarType(varTypeString);
    llvm::AllocaInst *var = TheBuilder.CreateAlloca(varType, nullptr, varId);

    llvm::json::Array *inner = varDecl->getArray("inner");
    if (inner != nullptr) {
        if (varType->isArrayTy() && !isCharArray(varType)) {
            llvm::Value *cast = TheBuilder.CreateBitCast(var, llvm::Type::getInt8PtrTy(TheContext));
            const llvm::DataLayout &DL = TheModule.getDataLayout();
            TheBuilder.CreateMemSet(cast, llvm::ConstantInt::get(llvm::Type::getInt8Ty(TheContext), 0),
                                    DL.getTypeSizeInBits(var->getType()->getElementType()) / 8, llvm::MaybeAlign(1));
            std::vector<llvm::Value *> idxList = {llvm::ConstantInt::get(llvm::Type::getInt32Ty(TheContext), 0)};
            localArrInit(var, inner->front().getAsObject(), idxList);
        } else {
            llvm::Value *val = getExpVal(inner->front().getAsObject());
            TheBuilder.CreateStore(val, var);
        }
    }
}

void buildDeclStmt(llvm::json::Object *declStmt) {
    llvm::json::Array *inner = declStmt->getArray("inner");
    for(auto &varDecl : *inner) {
        buildVarDecl(varDecl.getAsObject());
    }
}

void buildIfStmt(llvm::json::Object *ifStmt) {
    llvm::json::Array *inner = ifStmt->getArray("inner");
    llvm::Value *cond = getExpVal(inner->front().getAsObject(), true);
    llvm::Function *func = TheBuilder.GetInsertBlock()->getParent();
    llvm::BasicBlock *curBlock = TheBuilder.GetInsertBlock();
    llvm::BasicBlock *endBlock = nullptr;

    llvm::BasicBlock *trueBlock = llvm::BasicBlock::Create(TheContext, "if.then", func);
    TheBuilder.SetInsertPoint(trueBlock);
    llvm::json::Object *trueStmts = (*inner)[1].getAsObject();
    llvm::StringRef kind = trueStmts->getString("kind").getValue();
    if(kind == "CompoundStmt") {
        buildCompoundStmt(trueStmts);
    }
    else {
        buildStmt(trueStmts);
    }
    llvm::BasicBlock *trueEnd = &func->back();
    llvm::BasicBlock *falseEnd = nullptr;

    if(inner->size() == 3) {
        llvm::BasicBlock *falseBlock = llvm::BasicBlock::Create(TheContext, "if.else", func);
        TheBuilder.SetInsertPoint(falseBlock);
        llvm::json::Object *falseStmts = (*inner)[2].getAsObject();
        kind = falseStmts->getString("kind").getValue();
        if(kind == "CompoundStmt") {
            buildCompoundStmt(falseStmts);
        }
        else {
            buildStmt(falseStmts);
        }
        falseEnd = &func->back();
        endBlock = llvm::BasicBlock::Create(TheContext, "if.end");

        TheBuilder.SetInsertPoint(curBlock);
        TheBuilder.CreateCondBr(cond, trueBlock, falseBlock);
        TheBuilder.SetInsertPoint(falseEnd);
        bool tFlag = false, fFlag = false;
        if(trueEnd->getTerminator() == nullptr) {
            tFlag = true;
            TheBuilder.SetInsertPoint(trueEnd);
            TheBuilder.CreateBr(endBlock);
        }
        if(falseEnd->getTerminator() == nullptr) {
            fFlag = true;
            TheBuilder.SetInsertPoint(falseEnd);
            TheBuilder.CreateBr(endBlock);
        }

        if(tFlag || fFlag) {
            func->getBasicBlockList().push_back(endBlock);
            TheBuilder.SetInsertPoint(endBlock);
        }
    }
    else {
        endBlock = llvm::BasicBlock::Create(TheContext, "if.end", func);
        TheBuilder.SetInsertPoint(curBlock);
        TheBuilder.CreateCondBr(cond, trueBlock, endBlock);

        if (trueEnd->getTerminator() == nullptr) {
            TheBuilder.SetInsertPoint(trueEnd);
            TheBuilder.CreateBr(endBlock);
        }
        TheBuilder.SetInsertPoint(endBlock);
    }
}

void buildWhileStmt(llvm::json::Object *stmt) {
    llvm::json::Array *inner = stmt->getArray("inner");
    llvm::Function *func = TheBuilder.GetInsertBlock()->getParent();
    llvm::BasicBlock *condBlock = llvm::BasicBlock::Create(TheContext, "while.cond", func);
    whileCondStack.push(condBlock);
    TheBuilder.CreateBr(condBlock);

    TheBuilder.SetInsertPoint(condBlock);
    llvm::Value *cond = getExpVal(inner->front().getAsObject(), true);
    llvm::BasicBlock *condEndBlock = TheBuilder.GetInsertBlock();

    llvm::BasicBlock *whileBody = llvm::BasicBlock::Create(TheContext, "while.body", func);
    llvm::BasicBlock *whileEnd = llvm::BasicBlock::Create(TheContext, "while.end");
    whileEndStack.push(whileEnd);

    TheBuilder.SetInsertPoint(whileBody);
    llvm::StringRef kind = (*inner)[1].getAsObject()->getString("kind").getValue();
    if(kind == "CompoundStmt") {
        buildCompoundStmt((*inner)[1].getAsObject());
    }
    else {
        buildStmt((*inner)[1].getAsObject());
    }
    llvm::BasicBlock *bodyEnd = &func->back();
    if(bodyEnd->getTerminator() == nullptr) {
        TheBuilder.SetInsertPoint(bodyEnd);
        TheBuilder.CreateBr(condBlock);
    }
    whileCondStack.pop();
    whileEndStack.pop();
    TheBuilder.SetInsertPoint(condEndBlock);
    TheBuilder.CreateCondBr(cond, whileBody, whileEnd);
    func->getBasicBlockList().push_back(whileEnd);
    TheBuilder.SetInsertPoint(whileEnd);

}

void buildBreakStmt() {
    llvm::BasicBlock *lastWhileEndNotMatched = whileEndStack.top();
    TheBuilder.CreateBr(lastWhileEndNotMatched);
}

void buildContinueStmt() {
    llvm::BasicBlock *lastWhileCondNotMatched = whileCondStack.top();
    TheBuilder.CreateBr(lastWhileCondNotMatched);
}

void buildDoStmt(llvm::json::Object *stmt) {
    llvm::Function *func = TheBuilder.GetInsertBlock()->getParent();
    llvm::BasicBlock *doBody = llvm::BasicBlock::Create(TheContext, "do.body", func);

    TheBuilder.CreateBr(doBody);
    TheBuilder.SetInsertPoint(doBody);

    llvm::json::Object *doBodyStmts = stmt->getArray("inner")->front().getAsObject();
    buildCompoundStmt(doBodyStmts);

    llvm::BasicBlock *doBodyEnd = TheBuilder.GetInsertBlock();

    llvm::BasicBlock *doCond = llvm::BasicBlock::Create(TheContext, "do.cond", func);
    if(doBodyEnd->getTerminator() == nullptr) {
        TheBuilder.CreateBr(doCond);
    }
    TheBuilder.SetInsertPoint(doCond);
    llvm::Value *cond = getExpVal((*(stmt->getArray("inner")))[1].getAsObject(), true);
    llvm::BasicBlock *doEnd = llvm::BasicBlock::Create(TheContext, "do.end", func);
    TheBuilder.CreateCondBr(cond, doBody, doEnd);
    TheBuilder.SetInsertPoint(doEnd);
}

void buildStmt(llvm::json::Object *stmt) {
    llvm::StringRef kind = stmt->getString("kind").getValue();
    if(kind == "ReturnStmt") {
        buildRetStmt(stmt);
    }
    else if(kind == "BinaryOperator") {
        llvm::StringRef opcode = stmt->getString("opcode").getValue();
        if(opcode == "=") buildAssiStmt(stmt);
    }
    else if(kind == "DeclStmt") {
        buildDeclStmt(stmt);
    }
    else if(kind == "IfStmt") {
        buildIfStmt(stmt);
    }
    else if(kind == "CallExpr") {
        buildCallExpr(stmt);
    }
    else if(kind == "WhileStmt") {
        buildWhileStmt(stmt);
    }
    else if(kind == "BreakStmt") {
        buildBreakStmt();
    }
    else if(kind == "ContinueStmt") {
        buildContinueStmt();
    }
    else if(kind == "CompoundStmt") {
        buildCompoundStmt(stmt);
    }
    else if(kind == "NullStmt") {
        return;
    }
    else if(kind == "DoStmt") {
        buildDoStmt(stmt);
    }
}

void buildCompoundStmt(llvm::json::Object *compoundStmt) {
    llvm::json::Array *inner = compoundStmt->getArray("inner");
    if(inner == nullptr) return;
    for (auto &stmt : *inner) {
        buildStmt(stmt.getAsObject());
    }
}

void makeFuncImplemented(llvm::Function *func, llvm::json::Array *inner, bool changeArgName=false) {
    auto iter = inner->begin();
    while(iter != inner->end()) {
        llvm::StringRef kind = iter->getAsObject()->getString("kind").getValue();
        if(kind == "CompoundStmt") {
            break;
        }
        iter ++;
    }

    llvm::BasicBlock *entry = llvm::BasicBlock::Create(TheContext, "entry", func);
    TheBuilder.SetInsertPoint(entry);

    auto argIter = func->arg_begin();
    auto iter2 = inner->begin();
    while(argIter != func->arg_end()) {
        if(changeArgName) {
            llvm::StringRef argId = iter2->getAsObject()->getString("id").getValue();
            argIter->setName("_" + argId);
        }
        llvm::AllocaInst *inst = TheBuilder.CreateAlloca(argIter->getType(), nullptr, argIter->getName().substr(1));
        TheBuilder.CreateStore(argIter, inst);
        argIter++;
        iter2 ++;
    }
    buildCompoundStmt(iter->getAsObject());
}

void buildFunctionDecl(llvm::json::Object *funcDecl) {
    llvm::StringRef funcName = funcDecl->getString("name").getValue();

    llvm::json::Array *inner = funcDecl->getArray("inner");

    if(inner == nullptr) {
        auto funcTypeString = funcDecl->getObject("type")->getString("qualType");
        bool isVarArg = false;
        llvm::Type *funcType = getFuncRetType(funcTypeString.getValue(), isVarArg);
        llvm::Function::Create(llvm::FunctionType::get(funcType, std::vector<llvm::Type *>{},
                                                       isVarArg),
                               llvm::Function::ExternalLinkage,
                               funcName,
                               &TheModule);
    }
    else {
        auto prev = funcDecl->getString("previousDecl");
        if(
                prev.hasValue() &&
                std::find(igFuncName.begin(), igFuncName.end(), funcName.str()) == igFuncName.end()
        ) {
            llvm::Function *func = TheModule.getFunction(funcName);
            makeFuncImplemented(func, inner, true);
        }
        else {
            auto funcTypeString = funcDecl->getObject("type")->getString("qualType");
            bool isVarArg = false;
            llvm::Type *funcType = getFuncRetType(funcTypeString.getValue(), isVarArg);

            std::vector<llvm::Type *>parmTypes;
            std::vector<llvm::StringRef> parmIds;
            auto iter = inner->begin();
            while(iter != inner->end()) {
                llvm::json::Object *parm = iter->getAsObject();
                if(parm->getString("kind").getValue() == "ParmVarDecl") {
                    llvm::StringRef parmId = parm->getString("id").getValue();
                    parmIds.push_back(parmId);

                    llvm::StringRef parmTypeString = parm->getObject("type")->getString("qualType").getValue();
                    llvm::Type *parmType = getVarType(parmTypeString);
                    parmTypes.push_back(parmType);
                }
                else {
                    break;
                }
                iter ++;
            }
            llvm::Function *func = llvm::Function::Create(llvm::FunctionType::get(funcType, parmTypes,
                                                                                  isVarArg),
                                                          llvm::Function::ExternalLinkage,
                                                          funcName,
                                                          &TheModule);
            uint64_t idx = 0;
            while(idx < func->arg_size()) {
                auto arg = func->getArg(idx);
                arg->setName("_" + parmIds[idx]);
                idx ++;
            }

            if(iter != inner->end() && iter->getAsObject()->getString("kind").getValue() == "CompoundStmt") {
                makeFuncImplemented(func, inner);
            }
        }
    }
}

llvm::Constant *getGloInitVal(llvm::json::Object *exp) {
    llvm::Constant *res = nullptr;
    llvm::StringRef kind = exp->getString("kind").getValue();
    if(kind == "IntegerLiteral") {
        uint64_t val = std::stoi(exp->getString("value").getValue().str());
        res = llvm::ConstantInt::get(llvm::Type::getInt32Ty(TheContext), val);
    }
    else if(kind == "FloatingLiteral") {
        llvm::StringRef val = exp->getString("value").getValue();
        res = llvm::ConstantFP::get(llvm::Type::getDoubleTy(TheContext),
                                    llvm::APFloat(llvm::APFloat::IEEEdouble(), val));
    }
    else if(kind == "ImplicitCastExpr") {
        llvm::StringRef castKind = exp->getString("castKind").getValue();
        if(castKind == "LValueToRValue") {
            llvm::json::Array *inner = exp->getArray("inner");
            res = getGloInitVal(inner->front().getAsObject());
        }
        else if(castKind == "FloatingCast"){
            llvm::Constant *val = getGloInitVal(exp->getArray("inner")->front().getAsObject());
            llvm::StringRef type = exp->getObject("type")->getString("qualType").getValue();
            if(type == "double") {
                res = llvm::ConstantExpr::getFPCast(val, llvm::Type::getDoubleTy(TheContext));
            }
            else if(type == "float") {
                res = llvm::ConstantExpr::getFPCast(val, llvm::Type::getFloatTy(TheContext));
            }
        }
        else if(castKind == "IntegralToFloating") {
            llvm::StringRef type = exp->getObject("type")->getString("qualType").getValue();
            llvm::Constant *val = getGloInitVal(exp->getArray("inner")->front().getAsObject());
            if(type == "float") {
                res = llvm::ConstantExpr::getSIToFP(val, llvm::Type::getFloatTy(TheContext));
            }
            else if(type == "double") {
                res = llvm::ConstantExpr::getSIToFP(val, llvm::Type::getDoubleTy(TheContext));
            }

        }
        else if(castKind == "FloatingToIntegral") {
            llvm::Constant *val = getGloInitVal(exp->getArray("inner")->front().getAsObject());
            res = llvm::ConstantExpr::getFPToSI(val, llvm::Type::getInt32Ty(TheContext));
        }
    }
    else if(kind == "DeclRefExpr") {
        llvm::StringRef varId = exp->getObject("referencedDecl")->getString("id").getValue();
        llvm::GlobalVariable *var = TheModule.getGlobalVariable(varId);
        res = var->getInitializer();
    }
    else if(kind == "UnaryOperator") {
        llvm::StringRef op = exp->getString("opcode").getValue();
        llvm::Constant *val = getGloInitVal(exp->getArray("inner")->front().getAsObject());

        if(op == "-") {
            if(val->getType()->isIntegerTy(32)) {
                res = llvm::ConstantExpr::getNeg(val);
            }
            else if(val->getType()->isFloatTy() || val->getType()->isDoubleTy()) {
                res = llvm::ConstantExpr::getFNeg(val);
            }
        }
        else if(op == "+") {
            res = getGloInitVal(exp->getArray("inner")->front().getAsObject());
        }
    }
    else if(kind == "BinaryOperator") {
        llvm::StringRef op = exp->getString("opcode").getValue();
        llvm::json::Array *inner = exp->getArray("inner");
        llvm::json::Object *lhs = inner->front().getAsObject();
        llvm::json::Object *rhs = (*inner)[1].getAsObject();
        llvm::Constant *lVal = getGloInitVal(lhs);
        llvm::Constant *rVal = getGloInitVal(rhs);
        if(op == "+") {
            if(lVal->getType()->isIntegerTy(32)) {
                res = llvm::ConstantExpr::getAdd(lVal, rVal);
            }
            else if(lVal->getType()->isFloatTy()) {
                res = llvm::ConstantExpr::getFAdd(lVal, rVal);
            }
        }
        else if(op == "-") {
            if(lVal->getType()->isIntegerTy(32)) {
                res = llvm::ConstantExpr::getSub(lVal, rVal);
            }
            else if(lVal->getType()->isFloatTy()) {
                res = llvm::ConstantExpr::getFSub(lVal, rVal);
            }
        }
        else if(op == "*") {
            if(lVal->getType()->isIntegerTy(32)) {
                res = llvm::ConstantExpr::getMul(lVal, rVal);
            }
            else if(lVal->getType()->isFloatTy()) {
                res = llvm::ConstantExpr::getFMul(lVal, rVal);
            }
        }
        else if(op == "/") {
            if(lVal->getType()->isIntegerTy(32)) {
                res = llvm::ConstantExpr::getSDiv(lVal, rVal);
            }
            else if(lVal->getType()->isFloatTy()) {
                res = llvm::ConstantExpr::getFDiv(lVal, rVal);
            }
        }
    }
    return res;
}

llvm::Constant *getGloArrInitVal(llvm::json::Object *exp) {
    llvm::StringRef typeString = exp->getObject("type")->getString("qualType").getValue();
    llvm::Type *type = getVarType(typeString);

    llvm::json::Array *arrayFiller = exp->getArray("array_filler");
    std::vector<llvm::Constant *>constList;
    if(arrayFiller == nullptr) {
        llvm::json::Array *inner = exp->getArray("inner");
        for(auto &value : *inner) {
            llvm::StringRef kind = value.getAsObject()->getString("kind").getValue();
            if(kind == "InitListExpr")  {
                constList.push_back(getGloArrInitVal(value.getAsObject()));
            }
            else {
                constList.push_back(getGloInitVal(value.getAsObject()));
            }
        }
    }
    else {
        for(auto &value : *arrayFiller) {
            llvm::StringRef kind = value.getAsObject()->getString("kind").getValue();
            if(kind == "ImplicitValueInitExpr") {
                continue;
            }
            else if(kind == "InitListExpr") {
                constList.push_back(getGloArrInitVal(value.getAsObject()));
            }
            else {
                constList.push_back(getGloInitVal(value.getAsObject()));
            }
        }
    }
    while(constList.size() < type->getArrayNumElements()) {
        constList.push_back(llvm::Constant::getNullValue(type));
    }
    return llvm::ConstantArray::get(llvm::dyn_cast<llvm::ArrayType>(type), constList);
}

void buildGlobalVarDecl(llvm::json::Object *globalVarDecl) {
    /* 变量id，变量的唯一标识 */
    llvm::StringRef varId = globalVarDecl->getString("id").getValue();

    /* 变量类型 */
    auto varTypeString = globalVarDecl->getObject("type")->getString("qualType");
    bool isConstant = false;
    llvm::Type *varType = getVarType(varTypeString.getValue(), &isConstant);

    /* 初始值 */
    llvm::Constant *val = nullptr;
    llvm::json::Array *inner = globalVarDecl->getArray("inner");
    if(inner == nullptr) {
        val = llvm::Constant::getNullValue(varType);
    }
    else if(varType->isArrayTy()) {
        std::vector<llvm::Constant *> constantList;
        val = getGloArrInitVal(inner->front().getAsObject());

    }
    else {
        val = getGloInitVal(inner->front().getAsObject());
    }

    /* 创建全局变量 */
    new llvm::GlobalVariable(TheModule, varType, isConstant,
                             llvm::GlobalValue::ExternalLinkage, val, varId);

}

void buildTranslationUnitDecl(llvm::json::Object *object) {
    if (object == nullptr) {
        return;
    }
    if (auto kind = object->get("kind")->getAsString()) {
        assert(*kind == "TranslationUnitDecl");
    } else {
        assert(0);
    }

    if (auto inner = object->getArray("inner")) {
        for (auto &child: *inner) {
            auto tmp = child.getAsObject();
            auto kind = tmp->get("kind")->getAsString();
            if (*kind == "FunctionDecl") {
                llvm::json::Value *isImplicit = tmp->get("isImplicit");
                llvm::StringRef funcName = tmp->getString("name").getValue();
                if(
                        isImplicit == nullptr ||
                        std::find(igFuncName.begin(), igFuncName.end(), funcName.str()) == igFuncName.end()
                        ) {
                    buildFunctionDecl(tmp);
                }
            } else if (*kind == "VarDecl") {
                buildGlobalVarDecl(tmp);
            }
        }
    }
    for(auto &func : TheModule.getFunctionList()) {
        llvm::BasicBlock *ready = llvm::BasicBlock::Create(TheContext, "ready");
        for(auto &block : func.getBasicBlockList()) {
            if(block.getTerminator() == nullptr) {
                TheBuilder.SetInsertPoint(&block);
                if(func.getReturnType()->isVoidTy()) {
                    TheBuilder.CreateRetVoid();
                }
                else {
                    TheBuilder.CreateRet(llvm::Constant::getNullValue(func.getReturnType()));
                }
            }

            std::vector<llvm::Instruction *> toRemove;
            auto iter = block.getInstList().begin();
            while(iter != block.getInstList().end()) {
                auto inst = &*iter;
                iter ++;
                if(inst->getOpcode() == llvm::Instruction::Alloca) {
                    toRemove.push_back(inst);
                }
            }
            for(auto &inst : toRemove) {
                inst->removeFromParent();
                ready->getInstList().push_back(inst);
            }
        }
        llvm::BasicBlock &entry = func.getEntryBlock();
        entry.getInstList().splice(entry.begin(), ready->getInstList());
    }


}

}

int main() {
    auto llvmin = llvm::MemoryBuffer::getFileOrSTDIN("-");
    auto json = llvm::json::parse(llvmin.get()->getBuffer());
    buildTranslationUnitDecl(json->getAsObject());
    TheModule.print(llvm::outs(), nullptr);
}