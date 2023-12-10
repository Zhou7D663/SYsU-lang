%code requires{
#include <llvm/Support/JSON.h>
#include <llvm/Support/MemoryBuffer.h>
#include <llvm/Support/raw_ostream.h>
#include <llvm/ADT/APFloat.h>
#include <llvm/ADT/DenseMap.h>
#include <memory>
#include <vector>
#include <utility>
#include <map>
#include <stack>
#include <iostream>
#include <unordered_map>
class Node;
}

%{
#include "parser.hh"

#define yyerror(x)                                                                 \
    do {                                                                           \
        llvm::errs() << (x);                                                       \
    } while (0)


std::string value2string(llvm::json::Value & value) {
    std::string res;
    llvm::raw_string_ostream tmp(res);
    llvm::json::OStream tmp2(tmp);
    tmp2.value(value);
    tmp.flush();
    return res.substr(1, res.length() - 2);
}

class Node {
public:
    llvm::json::Object object;
    std::vector<Node *> childs;
public:
    Node() {}

    Node(std::initializer_list<std::pair<llvm::json::ObjectKey, llvm::json::Value>> list) {
        for(auto begin = list.begin(); begin != list.end(); ++ begin) {
            object[begin->first] = begin->second;
        }
    }

    void add_kv(llvm::json::ObjectKey key, llvm::json::Value value) {
        object[key] = value;
    }

    void change_kv(llvm::json::ObjectKey key, llvm::json::Value value) {
        object[key] = value;
    }

    llvm::json::Value & get_value(llvm::json::ObjectKey key) {
        return object[key];
    }

    void add_child(Node **child) {
        childs.emplace_back(*child);
        *child = nullptr;
    }

    void free_childs() {
        for(auto &child : childs) {
            child->free_childs();
            delete child;
            child = nullptr;
        }
    }

    llvm::json::Value to_json() {
      llvm::json::Array inner;
      for(auto &child: childs) {
        inner.push_back(child->to_json());
      }
      object["inner"] = llvm::json::Value(std::move(inner));
      return llvm::json::Value(std::move(object));
    }

    std::string if_kv(llvm::json::ObjectKey key) {
        if(object.find(key) != object.end()) {
            return value2string(object[key]);
        }
        return "";
    }

    void print(int depth = 0) {
        yyerror("|");
        for(int i = 0; i < depth; ++i) yyerror(" ");
        yyerror("-" + value2string(object["kind"]) + " " + if_kv("castKind") + " " + if_kv("name") + " " + if_kv("value") + " " + if_kv("type") + " " + if_kv("opcode"));
        for(auto &&child: childs) {
            yyerror("\n");
            child->print(depth + 1);
        }
        if(!depth) yyerror("\n\n");
    }
    ~Node() {}
};

Node * type_judge(Node *item1, Node *item2) {
    auto res = new Node{
        {"kind", "BinaryOperator"}
    };
    if(item1->get_value("type") == item2->get_value("type")) {
        res->add_kv("type", item1->get_value("type"));
        res->add_child(&item1);
        res->add_child(&item2);
    }
    else if(item1->get_value("type") == "double") {
        auto tmp = new Node{
            {"kind", "ImplicitCastExpr"},
            {"type", "double"},
            {"castKind", "typeCast"}
        };
        res->add_kv("type", "double");
        tmp->add_child(&item2);
        res->add_child(&item1);
        res->add_child(&tmp);
    }
    else if(item2->get_value("type") == "double") {
        auto tmp = new Node{
            {"kind", "ImplicitCastExpr"},
            {"type", "double"},
            {"castKind", "typeCast"}
        };
        res->add_kv("type", "double");
        tmp->add_child(&item1);
        res->add_child(&tmp);
        res->add_child(&item2);
    }
    else if(item1->get_value("type") == "float") {
        auto tmp = new Node{
            {"kind", "ImplicitCastExpr"},
            {"type", "float"},
            {"castKind", "typeCast"}
        };
        res->add_kv("type", "float");
        tmp->add_child(&item2);
        res->add_child(&item1);
        res->add_child(&tmp);
    }
    else if(item2->get_value("type") == "float") {
        auto tmp = new Node{
            {"kind", "ImplicitCastExpr"},
            {"type", "float"},
            {"castKind", "typeCast"}
        };
        res->add_kv("type", "float");
        tmp->add_child(&item1);
        res->add_child(&tmp);
        res->add_child(&item2);
    }
    else if(item1->get_value("type") == "long long") {
        auto tmp = new Node{
            {"kind", "ImplicitCastExpr"},
            {"type", "long long"},
            {"castKind", "typeCast"}
        };
        res->add_kv("type", "long long");
        tmp->add_child(&item2);
        res->add_child(&item1);
        res->add_child(&tmp);
    }
    else if(item2->get_value("type") == "long long") {
        auto tmp = new Node{
            {"kind", "ImplicitCastExpr"},
            {"type", "long long"},
            {"castKind", "typeCast"}
        };
        res->add_kv("type", "long long");
        tmp->add_child(&item1);
        res->add_child(&tmp);
        res->add_child(&item2);
    }
    else if(item1->get_value("type") == "long") {
        auto tmp = new Node{
            {"kind", "ImplicitCastExpr"},
            {"type", "long"},
            {"castKind", "typeCast"}
        };
        res->add_kv("type", "long");
        tmp->add_child(&item2);
        res->add_child(&item1);
        res->add_child(&tmp);
    }
    else if(item2->get_value("type") == "long") {
        auto tmp = new Node{
            {"kind", "ImplicitCastExpr"},
            {"type", "long"},
            {"castKind", "typeCast"}
        };
        res->add_kv("type", "long");
        tmp->add_child(&item1);
        res->add_child(&tmp);
        res->add_child(&item2);
    }
    else if(item1->get_value("type") == "unsigned int") {
        auto tmp = new Node{
            {"kind", "ImplicitCastExpr"},
            {"type", "unsigned int"},
            {"castKind", "typeCast"}
        };
        res->add_kv("type", "unnsigned int");
        tmp->add_child(&item2);
        res->add_child(&item1);
        res->add_child(&tmp);
    }
    else {
        auto tmp = new Node{
            {"kind", "ImplicitCastExpr"},
            {"type", "unsigned int"},
            {"castKind", "typeCast"}
        };
        res->add_kv("type", "unsigned int");
        tmp->add_child(&item1);
        res->add_child(&tmp);
        res->add_child(&item2);
    }
    return res;
}

namespace {
    auto llvmin = llvm::MemoryBuffer::getFileOrSTDIN("-");
    auto input = llvmin.get() -> getBuffer();
    auto end = input.end(), it = input.begin();
    auto wk_getline(char endline = "\n"[0]) {
        auto beg = it;
        while (it != end && *it != endline)
            ++ it;
        auto len = it - beg;
        if (it != end && *it == endline)
            ++ it;
        return llvm::StringRef(beg, len);
    }
    Node *root = nullptr;
    std::unordered_map<std::string, std::stack<std::string>> id_map;
    std::unordered_map<std::string, std::vector<std::string>> func_map;
    std::stack<std::string> rety;
}

void update_type(Node *node) {
    /* node->kind = DeclStmt */
    for(auto &child : node->childs) {
        /* child->kind = VarDecl */
        std::string name = value2string(child->get_value("name"));
        id_map[name].pop();
    }
}

auto yylex() {
    auto tk = wk_getline();
    auto b = tk.find("'") + 1, e = tk.rfind("'");
    auto s = tk.substr(b, e - b).str(), t = tk.substr(0, tk.find(" ")).str();
    
    if(t == "numeric_constant") {
        /* 浮点数 */
        long long tmp;
        if(s.find(".") != std::string::npos || s.find("e") != std::string::npos || s.find("E") != std::string::npos
            || s.find("p") != std::string::npos || s.find("P") != std::string::npos) {
            llvm::StringRef str(s);
            llvm::APFloat apf(0.0);
            auto useless = apf.convertFromString(str, llvm::APFloat::rmNearestTiesToEven);
            llvm::SmallString<16> Buffer;
            apf.toString(Buffer);
            std::string value = Buffer.c_str();
            yylval = new Node{
                {"kind", "FloatingLiteral"},
                {"value", value},
                {"type", "double"}
            };
            return T_NUMERIC_CONSTANT;
        }
        /* 十六进制整数 */
        else if(s.find("x") != std::string::npos || s.find("X") != std::string::npos) {
            std::sscanf(s.c_str(), "%llx", &tmp);
        }
        /* 八进制整数 */
        else if(s[0] == '0') {
            std::sscanf(s.c_str(), "%llo", &tmp);
        }
        /* 十进制数 */
        else {
            std::sscanf(s.c_str(), "%lld", &tmp);
        }
        if(tmp > 2147483647 && s[0] == '0') {
            yylval = new Node {
                {"kind", "IntegerLiteral"},
                {"value", std::to_string(tmp)},
                {"type", "unsigned int"}
            };
        }
        else if(tmp > 2147483647) {
            yylval = new Node {
                {"kind", "IntegerLiteral"},
                {"value", std::to_string(tmp)},
                {"type", "long"}
            };
        }
        else if(tmp < -2147483647) {
            yylval = new Node {
                {"kind", "UnaryOperator"},
                {"opcode", "-"},
                {"type", "long"}
            };
            Node *t = new Node{
                {"kind", "IntegerLiteral"},
                {"value", std::to_string(-tmp)},
                {"type", "long"}
            };
            yylval->add_child(&t);
        }
        else{
            yylval = new Node {
                {"kind", "IntegerLiteral"},
                {"value", std::to_string(tmp)},
                {"type", "int"}
            };
        }
        return T_NUMERIC_CONSTANT;
    }
    if(t == "identifier") {
        yylval = new Node{
            {"name", s}
        };
        return T_IDENTIFIER;
    }
    if(t == "string_literal"){
        std::string lr = "\"";
        std::string middle = s.substr(1, s.length() - 2);
        yylval = new Node{
            {"kind", "StringLiteral"},
            {"value", lr + middle + lr},
            {"type", "char"}
        };
        return T_STRING_LITERAL;
    }
    if (t == "int") {
        return T_INT;
    }
    if (t == "char") {
        return T_CHAR;
    }
    if (t == "long") {
        return T_LONG;
    }
    if (t == "return") {
        return T_RETURN;
    }
    if (t == "semi")
        return T_SEMI;
    if (t == "l_paren")
        return T_L_PAREN;
    if (t == "r_paren")
        return T_R_PAREN;
    if (t == "l_brace")
        return T_L_BRACE;
    if (t == "r_brace")
        return T_R_BRACE;
    if (t == "l_square")
        return T_L_SQUARE;
    if (t == "r_square")
        return T_R_SQUARE;
    if (t == "plus")
        return T_PLUS;
    if (t == "minus")
        return T_MINUS;
    if (t == "exclaim")
        return T_EXCLAIM;
    if (t == "star")
        return T_STAR;
    if (t == "slash")
        return T_SLASH;
    if (t == "percent")
        return T_PERCENT;
    if (t == "less")
        return T_LESS;
    if (t == "greater")
        return T_GREATER;
    if (t == "lessequal")
        return T_LESSEQUAL;
    if (t == "greaterequal")
        return T_GREATEREQUAL;
    if (t == "equalequal")
        return T_EQUALEQUAL;
    if (t == "exclaimequal")
        return T_EXCLAIMEQUAL;
    if (t == "ampamp")
        return T_AMPAMP;
    if (t == "pipepipe")
        return T_PIPEPIPE;
    if (t == "const")
        return T_CONST;
    if (t == "equal")
        return T_EQUAL;
    if (t == "comma")
        return T_COMMA;
    if (t == "if")
        return T_IF;
    if (t == "else")
        return T_ELSE;
    if (t == "while")
        return T_WHILE;
    if (t == "do")
        return T_DO;
    if (t == "break")
        return T_BREAK;
    if (t == "continue")
        return T_CONTINUE;
    if (t == "void")
        return T_VOID;
    if (t == "[")
        return T_L_SQUARE;
    if (t == "]")
        return T_R_SQUARE;
    if (t == "ellipsis")
        return T_ELLIPSIS;
    if (t == "float")
        return T_FLOAT;
    return YYEOF;
}

int main() {
    yyparse();
    root->print();
    llvm::outs() << root->to_json() << "\n";
    root->free_childs();
    delete root;
}
%}

%define api.value.type {Node *}

%token T_STAR
%token T_SLASH
%token T_PERCENT
%token T_RETURN
%token T_SEMI
%token T_L_PAREN
%token T_R_PAREN
%token T_L_BRACE
%token T_R_BRACE
%token T_L_SQUARE
%token T_R_SQUARE
%token T_PLUS
%token T_MINUS
%token T_EXCLAIM
%token T_EQUALEQUAL
%token T_EXCLAIMEQUAL
%token T_AMPAMP
%token T_PIPEPIPE
%token T_CONST
%token T_EQUAL
%token T_COMMA
%token T_INT
%token T_CHAR
%token T_LONG
%token T_IF
%token T_ELSE
%token T_WHILE
%token T_DO
%token T_BREAK
%token T_CONTINUE
%token T_NUMERIC_CONSTANT
%token T_IDENTIFIER
%token T_LESS
%token T_GREATER
%token T_LESSEQUAL
%token T_GREATEREQUAL
%token T_VOID
%token T_ELLIPSIS
%token T_FLOAT
%token T_STRING_LITERAL
%right THEN
%right T_ELSE
%start CompUnit


%%
Stmt: T_RETURN T_SEMI {
    $$ = new Node{
        {"kind", "ReturnStmt"}
    };
}
| T_RETURN Exp T_SEMI {
    $$ = new Node{
        {"kind", "ReturnStmt"}
    };
    std::string return_type = rety.top();
    if(!($2->get_value("type") == return_type)) {
        Node *tmp = new Node{
            {"kind", "ImplicitCastExpr"},
            {"type", return_type},
            {"castKind", "typeCast"}
        };
        tmp->add_child(&$2);
        $$->add_child(&tmp);
    }
    else {
        $$->add_child(&$2);   
    }
}
| T_BREAK T_SEMI {
    $$ = new Node{
        {"kind", "BreakStmt"}
    };
}
| T_CONTINUE T_SEMI {
    $$ = new Node{
        {"kind", "ContinueStmt"}
    };
}
| T_SEMI {
    $$ = new Node{
        {"kind", "NullStmt"}
    };
}
| Exp T_SEMI {
    $$ = $1;
    $1 = nullptr;
}
| T_WHILE T_L_PAREN Exp T_R_PAREN Stmt {
    $$ = new Node {
        {"kind", "WhileStmt"}
    };
    $$->add_child(&$3);
    $$->add_child(&$5);
}
| T_IF T_L_PAREN Exp T_R_PAREN Stmt %prec THEN{
    $$ = new Node{
        {"kind", "IfStmt"}
    };
    $$->add_child(&$3);
    $$->add_child(&$5);
}
| T_IF T_L_PAREN Exp T_R_PAREN Stmt T_ELSE Stmt {
    $$ = new Node{
        {"kind", "IfStmt"}
    };
    $$->add_child(&$3);
    $$->add_child(&$5);
    $$->add_child(&$7);
}
| T_DO  Stmt  T_WHILE T_L_PAREN Exp T_R_PAREN T_SEMI {
    $$ = new Node{
        {"kind", "DoStmt"}
    };
    $$->add_child(&$2);
    $$->add_child(&$5);
}
| Block {
    $$ = $1;
    $1 = nullptr;
}
| LVal T_EQUAL Exp T_SEMI{
    std::string type = value2string($1->get_value("type"));
    $$ = new Node {
        {"kind", "BinaryOperator"},
        {"type", type},
        {"opcode", "="}
    };
    if(!($3->get_value("type") == type)) {
        Node *tmp = new Node{
            {"kind", "ImplicitCastExpr"},
            {"type", type},
            {"castKind", "typeCast"}
        };
        tmp->add_child(&$3);
        $$->add_child(&$1);
        $$->add_child(&tmp);
    }
    else {
        $$->add_child(&$1);
        $$->add_child(&$3);
    }
};

BlockItem: Decl {
    $$ = $1;
    $1 = nullptr;
}
| Stmt {
    $$ = $1;
    $1 = nullptr;
};

BlockItemArr: BlockItem {
    $$ = new Node{
        {"kind", "CompoundStmt"}
    };
    $$->add_child(&$1);
}
| BlockItemArr BlockItem {
    $$ = $1;
    $1 = nullptr;
    $$->add_child(&$2);
};

Block: T_L_BRACE T_R_BRACE {
    $$ = new Node{
        {"kind", "CompoundStmt"}
    };
}
| T_L_BRACE BlockItemArr T_R_BRACE {
    $$ = $2;
    $2 = nullptr;
    for(auto &child : $$->childs) {
        /* child = Decl or Stmt */
        if(child->get_value("kind") == "DeclStmt") update_type(child);
    }
};

StringLiteral: T_STRING_LITERAL {
    $$ = $1;
    $1 = nullptr;
}
| StringLiteral T_STRING_LITERAL {
    $$ = $1;
    $1 = nullptr;
    std::string str1 = value2string($$->get_value("value"));
    std::string str2 = value2string($2->get_value("value"));
    std::string new_str = str1.substr(0, str1.length() - 1) + str2.substr(1);
    $$->change_kv("value", new_str);
};

PrimaryExp: T_L_PAREN Exp T_R_PAREN {
    $$ = new Node{
        {"kind", "ParenExpr"},
        {"type", $2->get_value("type")}
    };
    if($2->get_value("kind") == "ImplicitCastExpr") {
        $$->change_kv("kind", "ImplicitCastExpr");
        $2->change_kv("kind", "ParenExpr");
    }
    $$->add_child(&$2);
}
| T_NUMERIC_CONSTANT {
    $$ = $1;
    $1 = nullptr;
}
| StringLiteral {
    $$ = $1;
    $1 = nullptr;
}
| LVal {
    $$ = new Node{
        {"kind", "ImplicitCastExpr"},
    };
    $$->add_kv("type", value2string($1->get_value("type")));
    $$->add_child(&$1);
};

UnaryOp: T_PLUS {
    $$ = new Node {
        {"kind", "UnaryOperator"},
        {"opcode", "+"}
    };
}
| T_MINUS {
    $$ = new Node {
        {"kind", "UnaryOperator"},
        {"opcode", "-"}
    };
}
| T_EXCLAIM {
    $$ = new Node {
        {"kind", "UnaryOperator"},
        {"opcode", "!"}
    };
};

UnaryExp: PrimaryExp {
    $$ = $1;
    $1 = nullptr;
}
| UnaryOp UnaryExp {
    $$ = $1;
    $1 = nullptr;
    if($$->get_value("opcode") == "!") {
        $$->add_kv("type", "int");
    }
    else {
        $$->add_kv("type", $2->get_value("type"));
    }
    $$->add_child(&$2);
}
| T_IDENTIFIER T_L_PAREN T_R_PAREN {
    $$ = new Node{
        {"kind", "CallExpr"}
    };
    Node *tmp = new Node{
        {"kind", "ImplicitCastExpr"}
    };
    std::string name = value2string($1->get_value("name"));
    std::string type = func_map[name][0];
    $$->add_kv("type", type);
    $1->add_kv("kind", "DeclRefExpr");
    tmp->add_child(&$1);
    $$->add_child(&tmp);
}
| T_IDENTIFIER T_L_PAREN FuncRParams T_R_PAREN {
    $$ = new Node{
        {"kind", "CallExpr"}
    };
    std::string name = value2string($1->get_value("name"));
    std::string return_type = func_map[name][0];
    $$->add_kv("type", return_type);
    Node *tmp = new Node{
        {"kind", "ImplicitCastExpr"}
    };
    $1->add_kv("kind", "DeclRefExpr");
    tmp->add_child(&$1);
    $$->add_child(&tmp);
    int i = 1;
    int args = func_map[name].size();
    while($3->childs.size()) {
        Node *child = $3->childs[0];
        $3->childs.erase($3->childs.begin());
        if(i < args) {
            std::string type = func_map[name][i];
            if(value2string(child->get_value("type")) != type && type != "null") {
                Node *node = new Node{
                    {"kind", "ImplicitCastExpr"},
                    {"castKind", "typeCast"},
                    {"type", type}
                };
                node->add_child(&child);
                $$->add_child(&node);
            }
            else{
                $$->add_child(&child);
            }
        }
        else {
            $$->add_child(&child);
        }
        i ++;
    }
    delete $3;
};

MulExp: UnaryExp {
    $$ = $1;
    $1 = nullptr;
}
| MulExp T_STAR UnaryExp {
    $$ = type_judge($1, $3);
    $1 = nullptr;
    $3 = nullptr;
    $$->add_kv("opcode", "*");
}
| MulExp T_SLASH UnaryExp {
    $$ = type_judge($1, $3);
    $1 = nullptr;
    $3 = nullptr;
    $$->add_kv("opcode", "/");
}
| MulExp T_PERCENT UnaryExp {
    $$ = type_judge($1, $3);
    $1 = nullptr;
    $3 = nullptr;
    $$->add_kv("opcode", "%");
};

AddExp: MulExp {
    $$ = $1;
    $1 = nullptr;
}
| AddExp T_PLUS MulExp {
    $$ = type_judge($1, $3);
    $1 = nullptr;
    $3 = nullptr;
    $$->add_kv("opcode", "+");
}
| AddExp T_MINUS MulExp {
    $$ = type_judge($1, $3);
    $1 = nullptr;
    $3 = nullptr;
    $$->add_kv("opcode", "-");
};

RelExp: AddExp {
    $$ = $1;
    $1 = nullptr;
}
| RelExp T_LESS AddExp {
    $$ = type_judge($1, $3);
    $1 = nullptr;
    $3 = nullptr;
    $$->add_kv("opcode", "<");
    $$->change_kv("type", "int");
}
| RelExp T_GREATER AddExp {
    $$ = type_judge($1, $3);
    $1 = nullptr;
    $3 = nullptr;
    $$->add_kv("opcode", ">");
    $$->change_kv("type", "int");
}
| RelExp T_LESSEQUAL AddExp {
    $$ = type_judge($1, $3);
    $1 = nullptr;
    $3 = nullptr;
    $$->add_kv("opcode", "<=");
    $$->change_kv("type", "int");
}
| RelExp T_GREATEREQUAL AddExp {
    $$ = type_judge($1, $3);
    $1 = nullptr;
    $3 = nullptr;
    $$->add_kv("opcode", ">=");
    $$->change_kv("type", "int");
};

EqExp: RelExp {
    $$ = $1;
    $1 = nullptr;
}
| EqExp T_EQUALEQUAL RelExp {
    $$ = type_judge($1, $3);
    $1 = nullptr;
    $3 = nullptr;
    $$->add_kv("opcode", "==");
    $$->change_kv("type", "int");
}
| EqExp T_EXCLAIMEQUAL RelExp {
    $$ = type_judge($1, $3);
    $1 = nullptr;
    $3 = nullptr;
    $$->add_kv("opcode", "!=");
    $$->change_kv("type", "int");
};

LAndExp: EqExp {
    $$ = $1;
    $1 = nullptr;
}
| LAndExp T_AMPAMP EqExp {
    $$ = new Node{
        {"kind", "BinaryOperator"}
    };
    $$->add_child(&$1);
    $$->add_child(&$3);
    $$->add_kv("opcode", "&&");
    $$->change_kv("type", "int");
};

LOrExp: LAndExp {
    $$ = $1;
    $1 = nullptr;
}
| LOrExp T_PIPEPIPE LAndExp {
    $$ = new Node{
        {"kind", "BinaryOperator"}
    };
    $$->add_child(&$1);
    $$->add_child(&$3);
    $$->add_kv("opcode", "||");
    $$->change_kv("type", "int");
};

Exp: LOrExp {
    $$ = $1;
    $1 = nullptr;
};

ConstExp: Exp {
    $$ = $1;
    $1 = nullptr;
};

Type: T_INT {
    $$ = new Node{
        {"type", "int"}
    };
}
| T_CHAR {
    $$ = new Node{
        {"type", "char"}
    };
}
| T_LONG T_LONG {
    $$ = new Node{
        {"type", "long long"}
    };
}
| T_VOID {
    $$ = new Node{
        {"type", "void"}
    };
}
| T_FLOAT {
    $$ = new Node{
        {"type", "float"}
    };
};

ConstInitVal: ConstExp {
    $$ = $1;
    $1 = nullptr;
}
| T_L_BRACE T_R_BRACE {
    $$ = new Node{
        {"kind", "InitListExpr"},
        {"type", "null"}
    };
}
| T_L_BRACE ConstInitValArray T_R_BRACE {
    $$ = new Node {
        {"kind", "InitListExpr"},
        {"type", "null"}
    };
};

ConstInitValArray: ConstInitVal {
    $1->free_childs();
    delete $1;
}
| ConstInitValArray T_COMMA ConstInitVal {
    $3->free_childs();
    delete $3;
};

ConstDef: T_CONST Type T_IDENTIFIER T_EQUAL ConstInitVal {
    $$ = new Node {
        {"kind", "DeclStmt"}
    };
    Node *tmp = nullptr;
    std::string type = value2string($2->get_value("type"));
    std::string name = value2string($3->get_value("name"));
    $3->add_kv("kind", "VarDecl");
    $3->add_kv("type", type);
    if(!($5->get_value("type") == type) && !($5->get_value("type") == "null")) {
        tmp = new Node{
            {"kind", "ImplicitCastExpr"},
            {"type", type},
            {"castKind", "typeCast"}
        };
        tmp->add_child(&$5);
    }
    else {
        tmp = $5;
        $5 = nullptr;
    }
    $3->add_child(&tmp);
    $$->add_child(&$3);
    id_map[name].push(type);
    delete $2;
}
| T_CONST Type T_IDENTIFIER ConstExpArray T_EQUAL ConstInitVal {
    $$ = new Node {
        {"kind", "DeclStmt"}
    };
    Node *tmp = nullptr;
    std::string type = value2string($2->get_value("type"));
    std::string name = value2string($3->get_value("name"));
    type = "const " + type;
    $3->add_kv("kind", "VarDecl");
    $3->add_kv("type", type);
    if(!($6->get_value("type") == type) && !($6->get_value("type") == "null")
        && !(type == "const char" && $6->get_value("kind") == "StringLiteral")) {
        tmp = new Node{
            {"kind", "ImplicitCastExpr"},
            {"type", type},
            {"castKind", "typeCast"}
        };
        tmp->add_child(&$6);
    }
    else {
        tmp = $6;
        $6 = nullptr;
    }
    $3->add_child(&tmp);
    $$->add_child(&$3);
    id_map[name].push(type);
    delete $2;
}
| ConstDef T_COMMA T_IDENTIFIER T_EQUAL ConstInitVal {
    $$ = $1;
    $1 = nullptr;
    std::string name = value2string($3->get_value("name"));
    std::string type = value2string($$->childs[0]->get_value("type"));
    Node *tmp = nullptr;
    if(type.find("const") != std::string::npos) type = type.substr(6);
    $3->add_kv("kind", "VarDecl");
    $3->add_kv("type", type);
    if(!($5->get_value("type") == type) && !($5->get_value("type") == "null")) {
        tmp = new Node{
            {"kind", "ImplicitCastExpr"},
            {"type", type},
            {"castKind", "typeCast"}
        };
        tmp->add_child(&$5);
    }
    else {
        tmp = $5;
        $5 = nullptr;
    }
    $3->add_child(&tmp);
    $$->add_child(&$3);
    id_map[name].push(type);
}
| ConstDef T_COMMA T_IDENTIFIER ConstExpArray T_EQUAL ConstInitVal {
    $$ = $1;
    $1 = nullptr;
    std::string name = value2string($3->get_value("name"));
    std::string type = value2string($$->childs[0]->get_value("type")); 
    Node *tmp = nullptr;
    if(type.find("const") == std::string::npos) type = "const " + type;
    $3->add_kv("kind", "VarDecl");
    $3->add_kv("type", type);
    if(!($6->get_value("type") == type) && !($6->get_value("type") == "null")
        && !(type == "const char" && $6->get_value("kind") == "StringLiteral")) {
        tmp = new Node{
            {"kind", "ImplicitCastExpr"},
            {"type", type},
            {"castKind", "typeCast"}
        };
        tmp->add_child(&$6);
    }
    else {
        tmp = $6;
        $6 = nullptr;
    }
    $3->add_child(&tmp);
    $$->add_child(&$3);
    id_map[name].push(type);
};

ConstDecl: ConstDef T_SEMI {
    $$ = $1;
    $1 = nullptr;
};

ConstExpArray: T_L_SQUARE ConstExp T_R_SQUARE {
    $2->free_childs();
    delete $2;
}
| ConstExpArray T_L_SQUARE ConstExp T_R_SQUARE {
    $3->free_childs();
    delete $3;
};

/*
ConstDef: T_IDENTIFIER T_EQUAL ConstInitVal {
    $$ = $1;
    $1 = nullptr;
    $$->add_kv("kind", "VarDecl");
    $$->add_child(&$3);
}
| T_IDENTIFIER ConstExpArray T_EQUAL ConstInitVal {
    $$ = $1;
    $1 = nullptr;
    $$->add_kv("kind", "VarDecl");
    $$->add_child(&$4);
};

ConstDecl: T_CONST Type ConstDefArr T_SEMI{
    $$ = $3;
    $3 = nullptr;
    for(auto &const_def : $$->childs) {
        if(const_def->childs[0]->get_value("type") != "null" && const_def->childs[0]->get_value("type") != $2->get_value("type")){
            Node *tmp = const_def->childs[0];
            const_def->childs.pop_back();
            Node *tmp_father = new Node{
                {"kind", "ImplicitCastExpr"},
                {"type", $2->get_value("type")},
                {"castKind", "typeCast"}
            };
            tmp_father->add_child(&tmp);
            const_def->add_child(&tmp_father);
        }
        std::string name = value2string(const_def->get_value("name"));
        std::string type = value2string($2->get_value("type"));
        id_map[name].push(type);
    }
    delete $2;
};

ConstDefArr: ConstDef {
    $$ = new Node{
        {"kind", "DeclStmt"}
    };
    $$->add_child(&$1);
}
| ConstDefArr T_COMMA ConstDef {
    $$ = $1;
    $1 = nullptr;
    $$->add_child(&$3);
};
*/

InitVal: Exp {
    $$ = $1;
    $1 = nullptr;
}
| T_L_BRACE T_R_BRACE {
    $$ = new Node{
        {"kind", "InitListExpr"},
        {"type", "null"}
    };
}
| T_L_BRACE InitValArr T_R_BRACE {
    $$ = new Node {
        {"kind", "InitListExpr"},
        {"type", "null"}
    };
}

InitValArr: InitVal {
    $1->free_childs();
    delete $1;
}
| InitValArr T_COMMA InitVal {
    $3->free_childs();
    delete $3;
};

VarDef: Type T_IDENTIFIER {
    $$ = new Node{
        {"kind", "DeclStmt"}
    };
    std::string name = value2string($2->get_value("name"));
    std::string type = value2string($1->get_value("type"));
    $2->add_kv("kind", "VarDecl");
    $2->add_kv("type", type);
    $$->add_child(&$2);
    delete $1;
    id_map[name].push(type);
}
| Type T_IDENTIFIER ConstExpArray {
    $$ = new Node{
        {"kind", "DeclStmt"}
    };
    std::string name = value2string($2->get_value("name"));
    std::string type = value2string($1->get_value("type"));
    $2->add_kv("kind", "VarDecl");
    $2->add_kv("type", type);
    $$->add_child(&$2);
    delete $1;
    id_map[name].push(type);
}
| Type T_IDENTIFIER T_EQUAL InitVal {
    $$ = new Node{
        {"kind", "DeclStmt"}
    };
    std::string name = value2string($2->get_value("name"));
    std::string type = value2string($1->get_value("type"));
    $2->add_kv("kind", "VarDecl");
    $2->add_kv("type", type);
    delete $1;
    id_map[name].push(type);
    Node *tmp = nullptr;
    if(!($4->get_value("type") == type) && !($4->get_value("type") == "null")) {
        tmp = new Node{
            {"kind", "ImplicitCastExpr"},
            {"type", type},
            {"castKind", "typeCast"}
        };
        tmp->add_child(&$4);
    }
    else {
        tmp = $4;
        $4 = nullptr;
    }
    $2->add_child(&tmp);
    $$->add_child(&$2);
}
| Type T_IDENTIFIER ConstExpArray T_EQUAL InitVal {
    $$ = new Node{
        {"kind", "DeclStmt"}
    };
    std::string name = value2string($2->get_value("name"));
    std::string type = value2string($1->get_value("type"));
    $2->add_kv("kind", "VarDecl");
    $2->add_kv("type", type);
    delete $1;
    id_map[name].push(type);
    Node *tmp = nullptr;
    if(!($5->get_value("type") == type) && !($5->get_value("type") == "null")) {
        tmp = new Node{
            {"kind", "ImplicitCastExpr"},
            {"type", type},
            {"castKind", "typeCast"}
        };
        tmp->add_child(&$5);
    }
    else {
        tmp = $5;
        $5 = nullptr;
    }
    $2->add_child(&tmp);
    $$->add_child(&$2);
}
| VarDef T_COMMA T_IDENTIFIER {
    $$ = $1;
    $1 = nullptr;
    std::string name = value2string($3->get_value("name"));
    std::string type = value2string($$->childs[0]->get_value("type"));
    id_map[name].push(type);
    $3->add_kv("kind", "VarDecl");
    $3->add_kv("type", type);
    $$->add_child(&$3);
}
| VarDef T_COMMA T_IDENTIFIER ConstExpArray {
    $$ = $1;
    $1 = nullptr;
    std::string name = value2string($3->get_value("name"));
    std::string type = value2string($$->childs[0]->get_value("type"));
    id_map[name].push(type);
    $3->add_kv("kind", "VarDecl");
    $3->add_kv("type", type);
    $$->add_child(&$3);
}
| VarDef T_COMMA T_IDENTIFIER T_EQUAL InitVal {
    $$ = $1;
    $1 = nullptr;
    std::string name = value2string($3->get_value("name"));
    std::string type = value2string($$->childs[0]->get_value("type"));
    id_map[name].push(type);
    $3->add_kv("kind", "VarDecl");
    $3->add_kv("type", type);
    Node *tmp;
    if(!($5->get_value("type") == type) && !($5->get_value("type") == "null")) {
        tmp = new Node{
            {"kind", "ImplicitCastExpr"},
            {"type", type},
            {"castKind", "typeCast"}
        };
        tmp->add_child(&$5);
    }
    else {
        tmp = $5;
        $5 = nullptr;
    }
    $3->add_child(&tmp);
    $$->add_child(&$3);
}
| VarDef T_COMMA T_IDENTIFIER ConstExpArray T_EQUAL InitVal {
    $$ = $1;
    $1 = nullptr;
    std::string name = value2string($3->get_value("name"));
    std::string type = value2string($$->childs[0]->get_value("type"));
    id_map[name].push(type);
    $3->add_kv("kind", "VarDecl");
    $3->add_kv("type", type);
    Node *tmp;
    if(!($6->get_value("type") == type) && !($6->get_value("type") == "null")) {
        tmp = new Node{
            {"kind", "ImplicitCastExpr"},
            {"type", type},
            {"castKind", "typeCast"}
        };
        tmp->add_child(&$6);
    }
    else {
        tmp = $6;
        $6 = nullptr;
    }
    $3->add_child(&tmp);
    $$->add_child(&$3);
};

VarDecl: VarDef T_SEMI {
    $$ = $1;
    $1 = nullptr;
};

/*
VarDef: T_IDENTIFIER ConstExpArray {
    $$ = $1;
    $1 = nullptr;
    $$->add_kv("kind", "VarDecl");
}
| T_IDENTIFIER {
    $$ = $1;
    $1 = nullptr;
    $$->add_kv("kind", "VarDecl");
}
| T_IDENTIFIER ConstExpArray T_EQUAL InitVal {
    $$ = $1;
    $1 = nullptr;
    $$->add_kv("kind", "VarDecl");
    $$->add_child(&$4);
}
| T_IDENTIFIER T_EQUAL InitVal {
    $$ = $1;
    $1 = nullptr;
    $$->add_kv("kind", "VarDecl");
    $$->add_child(&$3);
};

VarDecl: Type VarDefArr T_SEMI {
    $$ = $2;
    $2 = nullptr;
    for(auto &def : $$->childs) {
        std::string name = value2string(def->get_value("name"));
        std::string type = value2string($1->get_value("type"));
        id_map[name].push(type);
        if(def->childs.size()) {
            if(def->childs[0]->get_value("type") != "null" && def->childs[0]->get_value("type") != $1->get_value("type")) {
                Node *tmp = def->childs[0];
                def->childs.pop_back();
                Node *tmp_father = new Node{
                    {"kind", "ImplicitCastExpr"},
                    {"type", $1->get_value("type")},
                    {"castKind", "typeCast"}
                };
                tmp_father->add_child(&tmp);
                def->add_child(&tmp_father);
            }
        }
    }
};

VarDefArr: VarDef {
    $$ = new Node{
        {"kind", "DeclStmt"}
    };
    $$->add_child(&$1);
}
| VarDefArr T_COMMA VarDef {
    $$->add_child(&$3);
};
*/

Decl: ConstDecl {
    $$ = $1;
    $1 = nullptr;
}
| VarDecl {
    $$ = $1;
    $1 = nullptr;
};

LVal: T_IDENTIFIER {
    $$ = $1;
    $1 = nullptr;
    $$->add_kv("kind", "DeclRefExpr");
    std::string name = value2string($$->get_value("name"));
    $$->add_kv("type", id_map[name].top());
}
| T_IDENTIFIER ExpArr {
    $$ = $2;
    $2 = nullptr;
    std::string name = value2string($1->get_value("name"));
    Node *tmp = $$;
    while(tmp->childs.size()) {
        tmp = tmp->childs[0];
    }
    $1->add_kv("kind", "DeclRefExpr");
    tmp->add_child(&$1);
    std::string type = id_map[name].top();
    if(type.find("const") != std::string::npos) type = type.substr(6);
    $$->add_kv("type", type);
};

ExpArr: T_L_SQUARE Exp T_R_SQUARE {
    $$ = new Node{
        {"kind", "ArraySubscriptExpr"}
    };
    Node *tmp = new Node{
        {"kind", "ImplicitCastExpr"}
    };
    $$->add_child(&tmp);
    $$->add_child(&$2);
}
| T_L_SQUARE Exp T_R_SQUARE ExpArr{
    $$ = $4;
    $4 = nullptr;
    Node *tmp = $$;
    while(tmp->childs.size()) {
        tmp = tmp->childs[0];
    }
    Node *ase = new Node{
        {"kind", "ArraySubscriptExpr"}
    };
    Node *ice = new Node{
        {"kind", "ImplicitCastExpr"}
    };
    ase->add_child(&ice);
    ase->add_child(&$2);
    tmp->add_child(&ase);
};

FuncFParam: Type T_IDENTIFIER {
    $$ = $2;
    $2 = nullptr;
    std::string name = value2string($$->get_value("name"));
    std::string type = value2string($1->get_value("type"));
    $$->add_kv("type", $1->get_value("type"));
    $$->add_kv("kind", "ParmVarDecl");
    id_map[name].push(type);
}
| Type T_IDENTIFIER ArraySquare {
    $$ = $2;
    $2 = nullptr;
    std::string name = value2string($$->get_value("name"));
    std::string type = value2string($1->get_value("type"));
    $$->add_kv("type", $1->get_value("type"));
    $$->add_kv("kind", "ParmVarDecl");
    id_map[name].push(type);
}
| T_CONST Type T_IDENTIFIER {
    $$ = $3;
    $3 = nullptr;
    std::string name = value2string($$->get_value("name"));
    std::string type = value2string($2->get_value("type"));
    $$->add_kv("type", $2->get_value("type"));
    $$->add_kv("kind", "ParmVarDecl");
    id_map[name].push(type);
}
| T_CONST Type T_IDENTIFIER ArraySquare {
    $$ = $3;
    $3 = nullptr;
    std::string name = value2string($$->get_value("name"));
    std::string type = value2string($2->get_value("type"));
    type = "const " + type;
    $$->add_kv("type", type);
    $$->add_kv("kind", "ParmVarDecl");
    id_map[name].push(type);
};

ArraySquare: T_L_SQUARE T_R_SQUARE {}
| ArraySquare  T_L_SQUARE ConstExp T_R_SQUARE {
    $3->free_childs();
    delete $3;
};

FuncFParams: FuncFParam {
    $$ = new Node{
        {"kind", "FunctionDecl"}
    };
    $$->add_child(&$1);
}
| FuncFParams T_COMMA FuncFParam {
    $$ = $1;
    $1 = nullptr;
    $$->add_child(&$3);
}
| FuncFParams T_COMMA T_ELLIPSIS {
    $$ = $1;
    $1 = nullptr;
    Node *tmp = new Node{
        {"kind", "ellipsis"},
        {"type", "null"}
    };
    $$->add_child(&tmp);
};

FuncDefPrefix: Type T_IDENTIFIER T_L_PAREN T_R_PAREN {
    $$ = $2;
    $2 = nullptr;
    $$->add_kv("kind", "FunctionDecl");
    std::string return_type = value2string($1->get_value("type"));
    $$->add_kv("type", return_type);
    std::string name = value2string($$->get_value("name"));
    func_map[name].push_back(return_type);
    delete $1;
    rety.push(return_type);
}
| Type T_IDENTIFIER T_L_PAREN FuncFParams T_R_PAREN {
    $$ = $4;
    $4 = nullptr;
    std::string name = value2string($2->get_value("name"));
    delete $2;
    $$->add_kv("name", name);
    std::string return_type = value2string($1->get_value("type"));
    delete $1;
    $$->add_kv("type", return_type);
    func_map[name].push_back(return_type);
    for(auto &child: $$->childs) {
        /* child->kind = ParmVarDecl */
        std::string type = value2string(child->get_value("type"));
        func_map[name].push_back(type);
    }
    int size = $$->childs.size();
    if($$->childs[size - 1]->get_value("type") == "null") {
        $$->childs.erase($$->childs.end() - 1);
    }
    rety.push(return_type);
};

FuncDef: FuncDefPrefix T_SEMI {
    $$ = $1;
    $1 = nullptr;
    rety.pop();
}
| FuncDefPrefix Block {
    $$ = $1;
    $1 = nullptr;
    $$->add_child(&$2);
    rety.pop();
};

/*
FuncDef: Type T_IDENTIFIER T_L_PAREN T_R_PAREN Block {
    $$ = $2;
    $2 = nullptr;
    $$->add_kv("kind", "FunctionDecl");
    std::string return_type = value2string($1->get_value("type"));
    $$->add_kv("type", return_type);
    $$->add_child(&$5);
    std::string name = value2string($$->get_value("name"));
    if(func_map.find(name) == func_map.end()) {
        func_map[name].push_back(return_type);
    }
    delete $1;
}
| Type T_IDENTIFIER T_L_PAREN FuncFParams T_R_PAREN Block {
    $$ = $4;
    $4 = nullptr;
    std::string name = value2string($2->get_value("name"));
    delete $2;
    $$->add_kv("name", name);
    std::string return_type = value2string($1->get_value("type"));
    delete $1;
    $$->add_kv("type", return_type);
    if(func_map.find(name) == func_map.end()) {
        func_map[name].push_back(return_type);
        for(auto &child: $$->childs) {
            // child->kind = ParmVarDecl 
            std::string type = value2string(child->get_value("type"));
            func_map[name].push_back(type);
        }
    }
    int size = $$->childs.size();
    if($$->childs[size - 1]->get_value("type") == "null") {
        $$->childs.erase($$->childs.end() - 1);
    }
    $$->add_child(&$6);
}
| Type T_IDENTIFIER T_L_PAREN T_R_PAREN T_SEMI {
    $$ = $2;
    $2 = nullptr;
    $$->add_kv("kind", "FunctionDecl");
    std::string return_type = value2string($1->get_value("type"));
    $$->add_kv("type", return_type);
    std::string name = value2string($$->get_value("name"));
    func_map[name].push_back(return_type);
    delete $1;
}
| Type T_IDENTIFIER T_L_PAREN FuncFParams T_R_PAREN T_SEMI {
    $$ = $4;
    $4 = nullptr;
    std::string name = value2string($2->get_value("name"));
    delete $2;
    $$->add_kv("name", name);
    std::string return_type = value2string($1->get_value("type"));
    delete $1;
    $$->add_kv("type", return_type);
    func_map[name].push_back(return_type);
    for(auto &child: $$->childs) {
        // child->kind = ParmVarDecl 
        std::string type = value2string(child->get_value("type"));
        func_map[name].push_back(type);
    }
    int size = $$->childs.size();
    if($$->childs[size - 1]->get_value("type") == "null") {
        $$->childs.erase($$->childs.end() - 1);
    }
};
*/
FuncRParams: Exp {
    $$ = new Node{};
    $$->add_child(&$1);
}
| FuncRParams T_COMMA Exp {
    $$ = $1;
    $1 = nullptr;
    $$->add_child(&$3);
};

Begin: Decl{
    $$ = new Node{
        {"kind", "TranslationUnitDecl"}
    };
    while($1->childs.size()) {
        Node *tmp = $1->childs[0];
        $1->childs.erase($1->childs.begin());
        $$->add_child(&tmp);
    }
    delete $1;
}
| FuncDef {
    for(auto &child: $1->childs) {
        if(child->get_value("kind") == "ParmVarDecl") {
            std::string name = value2string(child->get_value("name"));
            id_map[name].pop();
        }   
    }
    $$ = new Node{
        {"kind", "TranslationUnitDecl"}
    };
    $$->add_child(&$1);
}
| Begin Decl {
    $$ = $1;
    $1 = nullptr;
    while($2->childs.size()) {
        Node *tmp = $2->childs[0];
        $2->childs.erase($2->childs.begin());
        $$->add_child(&tmp);
    }
    delete $2;
}
| Begin FuncDef {
    for(auto &child: $2->childs) {
        if(child->get_value("kind") == "ParmVarDecl") {
            std::string name = value2string(child->get_value("name"));
            id_map[name].pop();
        }   
    }
    $$ = $1;
    $1 = nullptr;
    $$->add_child(&$2);
};

CompUnit: Begin {
    root = $1;
    $1 = nullptr;
};
%%

