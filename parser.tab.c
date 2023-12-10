/* A Bison parser, made by GNU Bison 3.7.5.  */

/* Bison implementation for Yacc-like parsers in C

   Copyright (C) 1984, 1989-1990, 2000-2015, 2018-2021 Free Software Foundation,
   Inc.

   This program is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program.  If not, see <http://www.gnu.org/licenses/>.  */

/* As a special exception, you may create a larger work that contains
   part or all of the Bison parser skeleton and distribute that work
   under terms of your choice, so long as that work isn't itself a
   parser generator using the skeleton or a modified version thereof
   as a parser skeleton.  Alternatively, if you modify or redistribute
   the parser skeleton itself, you may (at your option) remove this
   special exception, which will cause the skeleton and the resulting
   Bison output files to be licensed under the GNU General Public
   License without this special exception.

   This special exception was added by the Free Software Foundation in
   version 2.2 of Bison.  */

/* C LALR(1) parser skeleton written by Richard Stallman, by
   simplifying the original so-called "semantic" parser.  */

/* DO NOT RELY ON FEATURES THAT ARE NOT DOCUMENTED in the manual,
   especially those whose name start with YY_ or yy_.  They are
   private implementation details that can be changed or removed.  */

/* All symbols defined below should begin with yy or YY, to avoid
   infringing on user name space.  This should be done even for local
   variables, as they might otherwise be expanded by user macros.
   There are some unavoidable exceptions within include files to
   define necessary library symbols; they are noted "INFRINGES ON
   USER NAME SPACE" below.  */

/* Identify Bison output, and Bison version.  */
#define YYBISON 30705

/* Bison version string.  */
#define YYBISON_VERSION "3.7.5"

/* Skeleton name.  */
#define YYSKELETON_NAME "yacc.c"

/* Pure parsers.  */
#define YYPURE 0

/* Push parsers.  */
#define YYPUSH 0

/* Pull parsers.  */
#define YYPULL 1




/* First part of user prologue.  */
#line 17 "parser/parser.y"

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
        yyerror("-" + value2string(object["kind"]) + " " + if_kv("castKind") + " " + if_kv("name") + " " + if_kv("value") + " " + if_kv("type"));
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
        {"kind", "Binaryoperator"}
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
    else {
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
        if(s.find(".") != std::string::npos || s.find("e") != std::string::npos || s.find("E") != std::string::npos
            || s.find("p") != std::string::npos || s.find("P") != std::string::npos) {
            llvm::StringRef str(s);
            llvm::APFloat apf(0.0);
            apf.convertFromString(str, llvm::APFloat::rmNearestTiesToEven);
            llvm::SmallString<16> Buffer;
            apf.toString(Buffer);
            std::string value = Buffer.c_str();
            yylval = new Node{
                {"kind", "FloatingLiteral"},
                {"value", value},
                {"type", "double"}
            };
        }
        /* 十六进制整数 */
        else if(s.find("x") != std::string::npos || s.find("X") != std::string::npos) {
            int tmp;
            std::sscanf(s.c_str(), "%x", &tmp);
            yylval = new Node{
                {"kind", "IntegerLiteral"},
                {"value", std::to_string(tmp)},
                {"type", "int"}
            };
        }
        /* 十进制整数 */
        else {
            yylval = new Node{
                {"kind", "IntegerLiteral"},
                {"value", s},
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
    if (t == "...")
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

#line 412 "parser.tab.c"

# ifndef YY_CAST
#  ifdef __cplusplus
#   define YY_CAST(Type, Val) static_cast<Type> (Val)
#   define YY_REINTERPRET_CAST(Type, Val) reinterpret_cast<Type> (Val)
#  else
#   define YY_CAST(Type, Val) ((Type) (Val))
#   define YY_REINTERPRET_CAST(Type, Val) ((Type) (Val))
#  endif
# endif
# ifndef YY_NULLPTR
#  if defined __cplusplus
#   if 201103L <= __cplusplus
#    define YY_NULLPTR nullptr
#   else
#    define YY_NULLPTR 0
#   endif
#  else
#   define YY_NULLPTR ((void*)0)
#  endif
# endif


/* Debug traces.  */
#ifndef YYDEBUG
# define YYDEBUG 0
#endif
#if YYDEBUG
extern int yydebug;
#endif
/* "%code requires" blocks.  */
#line 1 "parser/parser.y"

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

#line 460 "parser.tab.c"

/* Token kinds.  */
#ifndef YYTOKENTYPE
# define YYTOKENTYPE
  enum yytokentype
  {
    YYEMPTY = -2,
    YYEOF = 0,                     /* "end of file"  */
    YYerror = 256,                 /* error  */
    YYUNDEF = 257,                 /* "invalid token"  */
    T_STAR = 258,                  /* T_STAR  */
    T_SLASH = 259,                 /* T_SLASH  */
    T_PERCENT = 260,               /* T_PERCENT  */
    T_RETURN = 261,                /* T_RETURN  */
    T_SEMI = 262,                  /* T_SEMI  */
    T_L_PAREN = 263,               /* T_L_PAREN  */
    T_R_PAREN = 264,               /* T_R_PAREN  */
    T_L_BRACE = 265,               /* T_L_BRACE  */
    T_R_BRACE = 266,               /* T_R_BRACE  */
    T_L_SQUARE = 267,              /* T_L_SQUARE  */
    T_R_SQUARE = 268,              /* T_R_SQUARE  */
    T_PLUS = 269,                  /* T_PLUS  */
    T_MINUS = 270,                 /* T_MINUS  */
    T_EXCLAIM = 271,               /* T_EXCLAIM  */
    T_EQUALEQUAL = 272,            /* T_EQUALEQUAL  */
    T_EXCLAIMEQUAL = 273,          /* T_EXCLAIMEQUAL  */
    T_AMPAMP = 274,                /* T_AMPAMP  */
    T_PIPEPIPE = 275,              /* T_PIPEPIPE  */
    T_CONST = 276,                 /* T_CONST  */
    T_EQUAL = 277,                 /* T_EQUAL  */
    T_COMMA = 278,                 /* T_COMMA  */
    T_INT = 279,                   /* T_INT  */
    T_CHAR = 280,                  /* T_CHAR  */
    T_LONG = 281,                  /* T_LONG  */
    T_IF = 282,                    /* T_IF  */
    T_ELSE = 283,                  /* T_ELSE  */
    T_WHILE = 284,                 /* T_WHILE  */
    T_DO = 285,                    /* T_DO  */
    T_BREAK = 286,                 /* T_BREAK  */
    T_CONTINUE = 287,              /* T_CONTINUE  */
    T_NUMERIC_CONSTANT = 288,      /* T_NUMERIC_CONSTANT  */
    T_IDENTIFIER = 289,            /* T_IDENTIFIER  */
    T_LESS = 290,                  /* T_LESS  */
    T_GREATER = 291,               /* T_GREATER  */
    T_LESSEQUAL = 292,             /* T_LESSEQUAL  */
    T_GREATEREQUAL = 293,          /* T_GREATEREQUAL  */
    T_VOID = 294,                  /* T_VOID  */
    T_ELLIPSIS = 295,              /* T_ELLIPSIS  */
    T_FLOAT = 296,                 /* T_FLOAT  */
    T_STRING_LITERAL = 297         /* T_STRING_LITERAL  */
  };
  typedef enum yytokentype yytoken_kind_t;
#endif

/* Value type.  */
#if ! defined YYSTYPE && ! defined YYSTYPE_IS_DECLARED
typedef Node * YYSTYPE;
# define YYSTYPE_IS_TRIVIAL 1
# define YYSTYPE_IS_DECLARED 1
#endif


extern YYSTYPE yylval;

int yyparse (void);


/* Symbol kind.  */
enum yysymbol_kind_t
{
  YYSYMBOL_YYEMPTY = -2,
  YYSYMBOL_YYEOF = 0,                      /* "end of file"  */
  YYSYMBOL_YYerror = 1,                    /* error  */
  YYSYMBOL_YYUNDEF = 2,                    /* "invalid token"  */
  YYSYMBOL_T_STAR = 3,                     /* T_STAR  */
  YYSYMBOL_T_SLASH = 4,                    /* T_SLASH  */
  YYSYMBOL_T_PERCENT = 5,                  /* T_PERCENT  */
  YYSYMBOL_T_RETURN = 6,                   /* T_RETURN  */
  YYSYMBOL_T_SEMI = 7,                     /* T_SEMI  */
  YYSYMBOL_T_L_PAREN = 8,                  /* T_L_PAREN  */
  YYSYMBOL_T_R_PAREN = 9,                  /* T_R_PAREN  */
  YYSYMBOL_T_L_BRACE = 10,                 /* T_L_BRACE  */
  YYSYMBOL_T_R_BRACE = 11,                 /* T_R_BRACE  */
  YYSYMBOL_T_L_SQUARE = 12,                /* T_L_SQUARE  */
  YYSYMBOL_T_R_SQUARE = 13,                /* T_R_SQUARE  */
  YYSYMBOL_T_PLUS = 14,                    /* T_PLUS  */
  YYSYMBOL_T_MINUS = 15,                   /* T_MINUS  */
  YYSYMBOL_T_EXCLAIM = 16,                 /* T_EXCLAIM  */
  YYSYMBOL_T_EQUALEQUAL = 17,              /* T_EQUALEQUAL  */
  YYSYMBOL_T_EXCLAIMEQUAL = 18,            /* T_EXCLAIMEQUAL  */
  YYSYMBOL_T_AMPAMP = 19,                  /* T_AMPAMP  */
  YYSYMBOL_T_PIPEPIPE = 20,                /* T_PIPEPIPE  */
  YYSYMBOL_T_CONST = 21,                   /* T_CONST  */
  YYSYMBOL_T_EQUAL = 22,                   /* T_EQUAL  */
  YYSYMBOL_T_COMMA = 23,                   /* T_COMMA  */
  YYSYMBOL_T_INT = 24,                     /* T_INT  */
  YYSYMBOL_T_CHAR = 25,                    /* T_CHAR  */
  YYSYMBOL_T_LONG = 26,                    /* T_LONG  */
  YYSYMBOL_T_IF = 27,                      /* T_IF  */
  YYSYMBOL_T_ELSE = 28,                    /* T_ELSE  */
  YYSYMBOL_T_WHILE = 29,                   /* T_WHILE  */
  YYSYMBOL_T_DO = 30,                      /* T_DO  */
  YYSYMBOL_T_BREAK = 31,                   /* T_BREAK  */
  YYSYMBOL_T_CONTINUE = 32,                /* T_CONTINUE  */
  YYSYMBOL_T_NUMERIC_CONSTANT = 33,        /* T_NUMERIC_CONSTANT  */
  YYSYMBOL_T_IDENTIFIER = 34,              /* T_IDENTIFIER  */
  YYSYMBOL_T_LESS = 35,                    /* T_LESS  */
  YYSYMBOL_T_GREATER = 36,                 /* T_GREATER  */
  YYSYMBOL_T_LESSEQUAL = 37,               /* T_LESSEQUAL  */
  YYSYMBOL_T_GREATEREQUAL = 38,            /* T_GREATEREQUAL  */
  YYSYMBOL_T_VOID = 39,                    /* T_VOID  */
  YYSYMBOL_T_ELLIPSIS = 40,                /* T_ELLIPSIS  */
  YYSYMBOL_T_FLOAT = 41,                   /* T_FLOAT  */
  YYSYMBOL_T_STRING_LITERAL = 42,          /* T_STRING_LITERAL  */
  YYSYMBOL_YYACCEPT = 43,                  /* $accept  */
  YYSYMBOL_Stmt = 44,                      /* Stmt  */
  YYSYMBOL_BlockItem = 45,                 /* BlockItem  */
  YYSYMBOL_BlockItemArr = 46,              /* BlockItemArr  */
  YYSYMBOL_Block = 47,                     /* Block  */
  YYSYMBOL_PrimaryExp = 48,                /* PrimaryExp  */
  YYSYMBOL_UnaryOp = 49,                   /* UnaryOp  */
  YYSYMBOL_UnaryExp = 50,                  /* UnaryExp  */
  YYSYMBOL_MulExp = 51,                    /* MulExp  */
  YYSYMBOL_AddExp = 52,                    /* AddExp  */
  YYSYMBOL_RelExp = 53,                    /* RelExp  */
  YYSYMBOL_EqExp = 54,                     /* EqExp  */
  YYSYMBOL_LAndExp = 55,                   /* LAndExp  */
  YYSYMBOL_LOrExp = 56,                    /* LOrExp  */
  YYSYMBOL_Exp = 57,                       /* Exp  */
  YYSYMBOL_ConstExp = 58,                  /* ConstExp  */
  YYSYMBOL_Type = 59,                      /* Type  */
  YYSYMBOL_ConstInitVal = 60,              /* ConstInitVal  */
  YYSYMBOL_ConstInitValArray = 61,         /* ConstInitValArray  */
  YYSYMBOL_ConstDef = 62,                  /* ConstDef  */
  YYSYMBOL_ConstExpArray = 63,             /* ConstExpArray  */
  YYSYMBOL_ConstDecl = 64,                 /* ConstDecl  */
  YYSYMBOL_ConstDefArr = 65,               /* ConstDefArr  */
  YYSYMBOL_InitVal = 66,                   /* InitVal  */
  YYSYMBOL_InitValArr = 67,                /* InitValArr  */
  YYSYMBOL_VarDef = 68,                    /* VarDef  */
  YYSYMBOL_VarDecl = 69,                   /* VarDecl  */
  YYSYMBOL_VarDefArr = 70,                 /* VarDefArr  */
  YYSYMBOL_Decl = 71,                      /* Decl  */
  YYSYMBOL_LVal = 72,                      /* LVal  */
  YYSYMBOL_ExpArr = 73,                    /* ExpArr  */
  YYSYMBOL_FuncFParam = 74,                /* FuncFParam  */
  YYSYMBOL_ArraySquare = 75,               /* ArraySquare  */
  YYSYMBOL_FuncFParams = 76,               /* FuncFParams  */
  YYSYMBOL_FuncDef = 77,                   /* FuncDef  */
  YYSYMBOL_Begin2 = 78,                    /* Begin2  */
  YYSYMBOL_CompUnit = 79                   /* CompUnit  */
};
typedef enum yysymbol_kind_t yysymbol_kind_t;




#ifdef short
# undef short
#endif

/* On compilers that do not define __PTRDIFF_MAX__ etc., make sure
   <limits.h> and (if available) <stdint.h> are included
   so that the code can choose integer types of a good width.  */

#ifndef __PTRDIFF_MAX__
# include <limits.h> /* INFRINGES ON USER NAME SPACE */
# if defined __STDC_VERSION__ && 199901 <= __STDC_VERSION__
#  include <stdint.h> /* INFRINGES ON USER NAME SPACE */
#  define YY_STDINT_H
# endif
#endif

/* Narrow types that promote to a signed type and that can represent a
   signed or unsigned integer of at least N bits.  In tables they can
   save space and decrease cache pressure.  Promoting to a signed type
   helps avoid bugs in integer arithmetic.  */

#ifdef __INT_LEAST8_MAX__
typedef __INT_LEAST8_TYPE__ yytype_int8;
#elif defined YY_STDINT_H
typedef int_least8_t yytype_int8;
#else
typedef signed char yytype_int8;
#endif

#ifdef __INT_LEAST16_MAX__
typedef __INT_LEAST16_TYPE__ yytype_int16;
#elif defined YY_STDINT_H
typedef int_least16_t yytype_int16;
#else
typedef short yytype_int16;
#endif

/* Work around bug in HP-UX 11.23, which defines these macros
   incorrectly for preprocessor constants.  This workaround can likely
   be removed in 2023, as HPE has promised support for HP-UX 11.23
   (aka HP-UX 11i v2) only through the end of 2022; see Table 2 of
   <https://h20195.www2.hpe.com/V2/getpdf.aspx/4AA4-7673ENW.pdf>.  */
#ifdef __hpux
# undef UINT_LEAST8_MAX
# undef UINT_LEAST16_MAX
# define UINT_LEAST8_MAX 255
# define UINT_LEAST16_MAX 65535
#endif

#if defined __UINT_LEAST8_MAX__ && __UINT_LEAST8_MAX__ <= __INT_MAX__
typedef __UINT_LEAST8_TYPE__ yytype_uint8;
#elif (!defined __UINT_LEAST8_MAX__ && defined YY_STDINT_H \
       && UINT_LEAST8_MAX <= INT_MAX)
typedef uint_least8_t yytype_uint8;
#elif !defined __UINT_LEAST8_MAX__ && UCHAR_MAX <= INT_MAX
typedef unsigned char yytype_uint8;
#else
typedef short yytype_uint8;
#endif

#if defined __UINT_LEAST16_MAX__ && __UINT_LEAST16_MAX__ <= __INT_MAX__
typedef __UINT_LEAST16_TYPE__ yytype_uint16;
#elif (!defined __UINT_LEAST16_MAX__ && defined YY_STDINT_H \
       && UINT_LEAST16_MAX <= INT_MAX)
typedef uint_least16_t yytype_uint16;
#elif !defined __UINT_LEAST16_MAX__ && USHRT_MAX <= INT_MAX
typedef unsigned short yytype_uint16;
#else
typedef int yytype_uint16;
#endif

#ifndef YYPTRDIFF_T
# if defined __PTRDIFF_TYPE__ && defined __PTRDIFF_MAX__
#  define YYPTRDIFF_T __PTRDIFF_TYPE__
#  define YYPTRDIFF_MAXIMUM __PTRDIFF_MAX__
# elif defined PTRDIFF_MAX
#  ifndef ptrdiff_t
#   include <stddef.h> /* INFRINGES ON USER NAME SPACE */
#  endif
#  define YYPTRDIFF_T ptrdiff_t
#  define YYPTRDIFF_MAXIMUM PTRDIFF_MAX
# else
#  define YYPTRDIFF_T long
#  define YYPTRDIFF_MAXIMUM LONG_MAX
# endif
#endif

#ifndef YYSIZE_T
# ifdef __SIZE_TYPE__
#  define YYSIZE_T __SIZE_TYPE__
# elif defined size_t
#  define YYSIZE_T size_t
# elif defined __STDC_VERSION__ && 199901 <= __STDC_VERSION__
#  include <stddef.h> /* INFRINGES ON USER NAME SPACE */
#  define YYSIZE_T size_t
# else
#  define YYSIZE_T unsigned
# endif
#endif

#define YYSIZE_MAXIMUM                                  \
  YY_CAST (YYPTRDIFF_T,                                 \
           (YYPTRDIFF_MAXIMUM < YY_CAST (YYSIZE_T, -1)  \
            ? YYPTRDIFF_MAXIMUM                         \
            : YY_CAST (YYSIZE_T, -1)))

#define YYSIZEOF(X) YY_CAST (YYPTRDIFF_T, sizeof (X))


/* Stored state numbers (used for stacks). */
typedef yytype_uint8 yy_state_t;

/* State numbers in computations.  */
typedef int yy_state_fast_t;

#ifndef YY_
# if defined YYENABLE_NLS && YYENABLE_NLS
#  if ENABLE_NLS
#   include <libintl.h> /* INFRINGES ON USER NAME SPACE */
#   define YY_(Msgid) dgettext ("bison-runtime", Msgid)
#  endif
# endif
# ifndef YY_
#  define YY_(Msgid) Msgid
# endif
#endif


#ifndef YY_ATTRIBUTE_PURE
# if defined __GNUC__ && 2 < __GNUC__ + (96 <= __GNUC_MINOR__)
#  define YY_ATTRIBUTE_PURE __attribute__ ((__pure__))
# else
#  define YY_ATTRIBUTE_PURE
# endif
#endif

#ifndef YY_ATTRIBUTE_UNUSED
# if defined __GNUC__ && 2 < __GNUC__ + (7 <= __GNUC_MINOR__)
#  define YY_ATTRIBUTE_UNUSED __attribute__ ((__unused__))
# else
#  define YY_ATTRIBUTE_UNUSED
# endif
#endif

/* Suppress unused-variable warnings by "using" E.  */
#if ! defined lint || defined __GNUC__
# define YY_USE(E) ((void) (E))
#else
# define YY_USE(E) /* empty */
#endif

#if defined __GNUC__ && ! defined __ICC && 407 <= __GNUC__ * 100 + __GNUC_MINOR__
/* Suppress an incorrect diagnostic about yylval being uninitialized.  */
# define YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN                            \
    _Pragma ("GCC diagnostic push")                                     \
    _Pragma ("GCC diagnostic ignored \"-Wuninitialized\"")              \
    _Pragma ("GCC diagnostic ignored \"-Wmaybe-uninitialized\"")
# define YY_IGNORE_MAYBE_UNINITIALIZED_END      \
    _Pragma ("GCC diagnostic pop")
#else
# define YY_INITIAL_VALUE(Value) Value
#endif
#ifndef YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
# define YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
# define YY_IGNORE_MAYBE_UNINITIALIZED_END
#endif
#ifndef YY_INITIAL_VALUE
# define YY_INITIAL_VALUE(Value) /* Nothing. */
#endif

#if defined __cplusplus && defined __GNUC__ && ! defined __ICC && 6 <= __GNUC__
# define YY_IGNORE_USELESS_CAST_BEGIN                          \
    _Pragma ("GCC diagnostic push")                            \
    _Pragma ("GCC diagnostic ignored \"-Wuseless-cast\"")
# define YY_IGNORE_USELESS_CAST_END            \
    _Pragma ("GCC diagnostic pop")
#endif
#ifndef YY_IGNORE_USELESS_CAST_BEGIN
# define YY_IGNORE_USELESS_CAST_BEGIN
# define YY_IGNORE_USELESS_CAST_END
#endif


#define YY_ASSERT(E) ((void) (0 && (E)))

#if !defined yyoverflow

/* The parser invokes alloca or malloc; define the necessary symbols.  */

# ifdef YYSTACK_USE_ALLOCA
#  if YYSTACK_USE_ALLOCA
#   ifdef __GNUC__
#    define YYSTACK_ALLOC __builtin_alloca
#   elif defined __BUILTIN_VA_ARG_INCR
#    include <alloca.h> /* INFRINGES ON USER NAME SPACE */
#   elif defined _AIX
#    define YYSTACK_ALLOC __alloca
#   elif defined _MSC_VER
#    include <malloc.h> /* INFRINGES ON USER NAME SPACE */
#    define alloca _alloca
#   else
#    define YYSTACK_ALLOC alloca
#    if ! defined _ALLOCA_H && ! defined EXIT_SUCCESS
#     include <stdlib.h> /* INFRINGES ON USER NAME SPACE */
      /* Use EXIT_SUCCESS as a witness for stdlib.h.  */
#     ifndef EXIT_SUCCESS
#      define EXIT_SUCCESS 0
#     endif
#    endif
#   endif
#  endif
# endif

# ifdef YYSTACK_ALLOC
   /* Pacify GCC's 'empty if-body' warning.  */
#  define YYSTACK_FREE(Ptr) do { /* empty */; } while (0)
#  ifndef YYSTACK_ALLOC_MAXIMUM
    /* The OS might guarantee only one guard page at the bottom of the stack,
       and a page size can be as small as 4096 bytes.  So we cannot safely
       invoke alloca (N) if N exceeds 4096.  Use a slightly smaller number
       to allow for a few compiler-allocated temporary stack slots.  */
#   define YYSTACK_ALLOC_MAXIMUM 4032 /* reasonable circa 2006 */
#  endif
# else
#  define YYSTACK_ALLOC YYMALLOC
#  define YYSTACK_FREE YYFREE
#  ifndef YYSTACK_ALLOC_MAXIMUM
#   define YYSTACK_ALLOC_MAXIMUM YYSIZE_MAXIMUM
#  endif
#  if (defined __cplusplus && ! defined EXIT_SUCCESS \
       && ! ((defined YYMALLOC || defined malloc) \
             && (defined YYFREE || defined free)))
#   include <stdlib.h> /* INFRINGES ON USER NAME SPACE */
#   ifndef EXIT_SUCCESS
#    define EXIT_SUCCESS 0
#   endif
#  endif
#  ifndef YYMALLOC
#   define YYMALLOC malloc
#   if ! defined malloc && ! defined EXIT_SUCCESS
void *malloc (YYSIZE_T); /* INFRINGES ON USER NAME SPACE */
#   endif
#  endif
#  ifndef YYFREE
#   define YYFREE free
#   if ! defined free && ! defined EXIT_SUCCESS
void free (void *); /* INFRINGES ON USER NAME SPACE */
#   endif
#  endif
# endif
#endif /* !defined yyoverflow */

#if (! defined yyoverflow \
     && (! defined __cplusplus \
         || (defined YYSTYPE_IS_TRIVIAL && YYSTYPE_IS_TRIVIAL)))

/* A type that is properly aligned for any stack member.  */
union yyalloc
{
  yy_state_t yyss_alloc;
  YYSTYPE yyvs_alloc;
};

/* The size of the maximum gap between one aligned stack and the next.  */
# define YYSTACK_GAP_MAXIMUM (YYSIZEOF (union yyalloc) - 1)

/* The size of an array large to enough to hold all stacks, each with
   N elements.  */
# define YYSTACK_BYTES(N) \
     ((N) * (YYSIZEOF (yy_state_t) + YYSIZEOF (YYSTYPE)) \
      + YYSTACK_GAP_MAXIMUM)

# define YYCOPY_NEEDED 1

/* Relocate STACK from its old location to the new one.  The
   local variables YYSIZE and YYSTACKSIZE give the old and new number of
   elements in the stack, and YYPTR gives the new location of the
   stack.  Advance YYPTR to a properly aligned location for the next
   stack.  */
# define YYSTACK_RELOCATE(Stack_alloc, Stack)                           \
    do                                                                  \
      {                                                                 \
        YYPTRDIFF_T yynewbytes;                                         \
        YYCOPY (&yyptr->Stack_alloc, Stack, yysize);                    \
        Stack = &yyptr->Stack_alloc;                                    \
        yynewbytes = yystacksize * YYSIZEOF (*Stack) + YYSTACK_GAP_MAXIMUM; \
        yyptr += yynewbytes / YYSIZEOF (*yyptr);                        \
      }                                                                 \
    while (0)

#endif

#if defined YYCOPY_NEEDED && YYCOPY_NEEDED
/* Copy COUNT objects from SRC to DST.  The source and destination do
   not overlap.  */
# ifndef YYCOPY
#  if defined __GNUC__ && 1 < __GNUC__
#   define YYCOPY(Dst, Src, Count) \
      __builtin_memcpy (Dst, Src, YY_CAST (YYSIZE_T, (Count)) * sizeof (*(Src)))
#  else
#   define YYCOPY(Dst, Src, Count)              \
      do                                        \
        {                                       \
          YYPTRDIFF_T yyi;                      \
          for (yyi = 0; yyi < (Count); yyi++)   \
            (Dst)[yyi] = (Src)[yyi];            \
        }                                       \
      while (0)
#  endif
# endif
#endif /* !YYCOPY_NEEDED */

/* YYFINAL -- State number of the termination state.  */
#define YYFINAL  12
/* YYLAST -- Last index in YYTABLE.  */
#define YYLAST   267

/* YYNTOKENS -- Number of terminals.  */
#define YYNTOKENS  43
/* YYNNTS -- Number of nonterminals.  */
#define YYNNTS  37
/* YYNRULES -- Number of rules.  */
#define YYNRULES  97
/* YYNSTATES -- Number of states.  */
#define YYNSTATES  181

/* YYMAXUTOK -- Last valid token kind.  */
#define YYMAXUTOK   297


/* YYTRANSLATE(TOKEN-NUM) -- Symbol number corresponding to TOKEN-NUM
   as returned by yylex, with out-of-bounds checking.  */
#define YYTRANSLATE(YYX)                                \
  (0 <= (YYX) && (YYX) <= YYMAXUTOK                     \
   ? YY_CAST (yysymbol_kind_t, yytranslate[YYX])        \
   : YYSYMBOL_YYUNDEF)

/* YYTRANSLATE[TOKEN-NUM] -- Symbol number corresponding to TOKEN-NUM
   as returned by yylex.  */
static const yytype_int8 yytranslate[] =
{
       0,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     1,     2,     3,     4,
       5,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    18,    19,    20,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42
};

#if YYDEBUG
  /* YYRLINE[YYN] -- Source line where rule number YYN was defined.  */
static const yytype_int16 yyrline[] =
{
       0,   404,   404,   409,   415,   420,   425,   430,   434,   441,
     448,   456,   463,   467,   489,   493,   498,   504,   510,   515,
     524,   531,   535,   539,   548,   554,   560,   567,   571,   583,
     587,   593,   599,   606,   610,   616,   623,   627,   634,   641,
     648,   656,   660,   667,   675,   679,   687,   691,   699,   704,
     709,   714,   719,   724,   729,   735,   739,   745,   752,   755,
     759,   765,   772,   775,   780,   802,   808,   814,   818,   824,
     831,   834,   838,   843,   848,   854,   861,   884,   890,   894,
     898,   903,   910,   923,   933,   951,   957,   963,   970,   977,
     978,   982,   988,   993,  1003,  1014,  1035,  1074
};
#endif

/** Accessing symbol of state STATE.  */
#define YY_ACCESSING_SYMBOL(State) YY_CAST (yysymbol_kind_t, yystos[State])

#if YYDEBUG || 0
/* The user-facing name of the symbol whose (internal) number is
   YYSYMBOL.  No bounds checking.  */
static const char *yysymbol_name (yysymbol_kind_t yysymbol) YY_ATTRIBUTE_UNUSED;

/* YYTNAME[SYMBOL-NUM] -- String name of the symbol SYMBOL-NUM.
   First, the terminals, then, starting at YYNTOKENS, nonterminals.  */
static const char *const yytname[] =
{
  "\"end of file\"", "error", "\"invalid token\"", "T_STAR", "T_SLASH",
  "T_PERCENT", "T_RETURN", "T_SEMI", "T_L_PAREN", "T_R_PAREN", "T_L_BRACE",
  "T_R_BRACE", "T_L_SQUARE", "T_R_SQUARE", "T_PLUS", "T_MINUS",
  "T_EXCLAIM", "T_EQUALEQUAL", "T_EXCLAIMEQUAL", "T_AMPAMP", "T_PIPEPIPE",
  "T_CONST", "T_EQUAL", "T_COMMA", "T_INT", "T_CHAR", "T_LONG", "T_IF",
  "T_ELSE", "T_WHILE", "T_DO", "T_BREAK", "T_CONTINUE",
  "T_NUMERIC_CONSTANT", "T_IDENTIFIER", "T_LESS", "T_GREATER",
  "T_LESSEQUAL", "T_GREATEREQUAL", "T_VOID", "T_ELLIPSIS", "T_FLOAT",
  "T_STRING_LITERAL", "$accept", "Stmt", "BlockItem", "BlockItemArr",
  "Block", "PrimaryExp", "UnaryOp", "UnaryExp", "MulExp", "AddExp",
  "RelExp", "EqExp", "LAndExp", "LOrExp", "Exp", "ConstExp", "Type",
  "ConstInitVal", "ConstInitValArray", "ConstDef", "ConstExpArray",
  "ConstDecl", "ConstDefArr", "InitVal", "InitValArr", "VarDef", "VarDecl",
  "VarDefArr", "Decl", "LVal", "ExpArr", "FuncFParam", "ArraySquare",
  "FuncFParams", "FuncDef", "Begin2", "CompUnit", YY_NULLPTR
};

static const char *
yysymbol_name (yysymbol_kind_t yysymbol)
{
  return yytname[yysymbol];
}
#endif

#ifdef YYPRINT
/* YYTOKNUM[NUM] -- (External) token number corresponding to the
   (internal) symbol number NUM (which must be that of a token).  */
static const yytype_int16 yytoknum[] =
{
       0,   256,   257,   258,   259,   260,   261,   262,   263,   264,
     265,   266,   267,   268,   269,   270,   271,   272,   273,   274,
     275,   276,   277,   278,   279,   280,   281,   282,   283,   284,
     285,   286,   287,   288,   289,   290,   291,   292,   293,   294,
     295,   296,   297
};
#endif

#define YYPACT_NINF (-143)

#define yypact_value_is_default(Yyn) \
  ((Yyn) == YYPACT_NINF)

#define YYTABLE_NINF (-1)

#define yytable_value_is_error(Yyn) \
  0

  /* YYPACT[STATE-NUM] -- Index in YYTABLE of the portion describing
     STATE-NUM.  */
static const yytype_int16 yypact[] =
{
      19,  -143,  -143,   -11,  -143,  -143,   -17,  -143,  -143,    21,
    -143,    15,  -143,    75,    44,    19,    22,  -143,    46,    56,
    -143,    31,    61,    44,   226,   172,  -143,    60,  -143,  -143,
    -143,  -143,    19,    70,    83,   162,   103,   119,  -143,   125,
    -143,  -143,  -143,   132,  -143,  -143,    60,  -143,   194,    24,
      94,    91,    87,   121,   137,   111,  -143,  -143,  -143,   127,
      61,   139,   138,  -143,  -143,  -143,  -143,   153,  -143,   158,
     141,    60,    60,   156,  -143,  -143,    60,   178,  -143,  -143,
    -143,    60,    60,    60,    60,    60,    60,    60,    60,    60,
      60,    60,    60,    60,  -143,    93,  -143,     6,    60,   138,
    -143,    60,  -143,  -143,    99,  -143,     7,   191,   192,   195,
     189,    60,  -143,  -143,  -143,   194,   194,    24,    24,    24,
      24,    94,    94,    91,    87,    60,    17,   105,  -143,   111,
     200,  -143,   197,   222,   113,  -143,   141,   162,   162,    60,
    -143,   205,   207,   201,  -143,  -143,    60,    17,  -143,  -143,
    -143,   211,  -143,  -143,   222,  -143,   180,  -143,   204,  -143,
    -143,  -143,  -143,    25,   210,  -143,  -143,  -143,    38,  -143,
     162,   217,  -143,    17,  -143,  -143,   222,  -143,  -143,  -143,
    -143
};

  /* YYDEFACT[STATE-NUM] -- Default reduction number in state STATE-NUM.
     Performed when YYTABLE does not specify something else to do.  Zero
     means the default is an error.  */
static const yytype_int8 yydefact[] =
{
       0,    50,    51,     0,    53,    54,     0,    96,    97,     0,
      52,     0,     1,     0,     0,     0,     0,    91,     0,     0,
      94,     0,    85,     0,     0,     0,     6,     0,    18,    24,
      25,    26,     0,     0,     0,     0,     0,     0,    21,    81,
      22,    15,    16,     0,    12,    27,     0,    29,    33,    36,
      41,    44,    46,    48,     0,     0,    79,    80,    14,    23,
      87,     0,    86,    95,    93,    92,     2,     0,    23,     0,
       0,     0,     0,     0,     4,     5,     0,    82,    19,    17,
      28,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     7,    73,    77,     0,     0,    88,
      89,     0,     3,    20,     0,    65,     0,     0,     0,     0,
       0,     0,    30,    31,    32,    34,    35,    37,    38,    39,
      40,    42,    43,    45,    47,     0,     0,    72,    76,     0,
       0,    49,     0,     0,     0,    64,     0,     0,     0,     0,
      83,     0,     0,     0,    67,    75,     0,     0,    78,    13,
      90,     0,    55,    60,     0,    66,     9,     8,     0,    84,
      62,    68,    70,     0,     0,    74,    56,    58,     0,    61,
       0,     0,    69,     0,    63,    57,     0,    10,    11,    71,
      59
};

  /* YYPGOTO[NTERM-NUM].  */
static const yytype_int16 yypgoto[] =
{
    -143,   -34,   185,  -143,    -4,  -143,  -143,   -41,    28,    95,
      43,   147,   136,  -143,   -19,   -99,     3,  -142,  -143,    97,
     142,  -143,  -143,  -136,  -143,   102,  -143,  -143,  -143,   -15,
    -143,   216,   181,  -143,  -143,  -143,  -143
};

  /* YYDEFGOTO[NTERM-NUM].  */
static const yytype_uint8 yydefgoto[] =
{
       0,    41,    42,    43,    44,    45,    46,    47,    48,    49,
      50,    51,    52,    53,   131,   152,    16,   153,   168,   105,
     127,    56,   106,   145,   163,    96,    57,    97,    58,    68,
      77,    17,    62,    18,     7,     8,     9
};

  /* YYTABLE[YYPACT[STATE-NUM]] -- What to do in state STATE-NUM.  If
     positive, shift that token.  If negative, reduce the rule whose
     number is the opposite.  If YYTABLE_NINF, syntax error.  */
static const yytype_uint8 yytable[] =
{
      54,    73,   132,     6,    59,    80,    67,   162,    69,   167,
      20,   165,   169,   128,   135,    10,    54,    11,    21,    63,
      59,    12,    55,    13,    54,    27,   142,   143,    59,   129,
     136,    29,    30,    31,   180,    70,   172,   179,    84,    85,
     112,   113,   114,     1,     2,     3,    55,   164,   173,   175,
      38,    39,   107,   108,    19,    23,    22,   110,     4,    40,
       5,   176,    25,    26,    27,    60,    19,    28,    27,    24,
      29,    30,    31,    61,    29,    30,    31,    32,    71,   130,
       1,     2,     3,    33,    14,    34,    35,    36,    37,    38,
      39,    72,   141,    38,    39,     4,    15,     5,    40,     1,
       2,     3,    40,   156,   157,   125,    92,   144,    90,    91,
      74,   125,   115,   116,     4,   126,     5,   146,    54,    54,
     158,   133,    59,    59,   144,   146,    75,   147,   144,    86,
      87,    88,    89,   121,   122,   154,   177,    76,    25,    26,
      27,    93,    19,    78,    94,    95,    29,    30,    31,    98,
     101,    54,   100,    32,   144,    59,     1,     2,     3,    33,
     102,    34,    35,    36,    37,    38,    39,   103,    25,    26,
      27,     4,    19,     5,    40,   104,    29,    30,    31,    66,
      27,   117,   118,   119,   120,   109,    29,    30,    31,    33,
     111,    34,    35,    36,    37,    38,    39,    81,    82,    83,
     137,   138,   140,   139,    40,    38,    39,   149,   170,    27,
     150,   143,   161,   171,    40,    29,    30,    31,   159,    27,
     160,   151,   166,   174,   178,    29,    30,    31,    79,   124,
      27,   148,   151,   155,    38,    39,    29,    30,    31,   123,
      65,    99,     0,    40,    38,    39,   134,    15,     0,     0,
       1,     2,     3,    40,     0,    38,    39,     0,     0,     0,
       0,     0,     0,     0,    40,     4,    64,     5
};

static const yytype_int16 yycheck[] =
{
      19,    35,   101,     0,    19,    46,    25,   143,    27,   151,
      14,   147,   154,     7,     7,    26,    35,    34,    15,    23,
      35,     0,    19,     8,    43,     8,   125,    10,    43,    23,
      23,    14,    15,    16,   176,    32,    11,   173,    14,    15,
      81,    82,    83,    24,    25,    26,    43,   146,    23,    11,
      33,    34,    71,    72,    10,     9,    34,    76,    39,    42,
      41,    23,     6,     7,     8,    34,    10,    11,     8,    23,
      14,    15,    16,    12,    14,    15,    16,    21,     8,    98,
      24,    25,    26,    27,     9,    29,    30,    31,    32,    33,
      34,     8,   111,    33,    34,    39,    21,    41,    42,    24,
      25,    26,    42,   137,   138,    12,    19,   126,    17,    18,
       7,    12,    84,    85,    39,    22,    41,    12,   137,   138,
     139,    22,   137,   138,   143,    12,     7,    22,   147,    35,
      36,    37,    38,    90,    91,    22,   170,    12,     6,     7,
       8,    20,    10,    11,     7,    34,    14,    15,    16,    22,
      12,   170,    13,    21,   173,   170,    24,    25,    26,    27,
       7,    29,    30,    31,    32,    33,    34,     9,     6,     7,
       8,    39,    10,    41,    42,    34,    14,    15,    16,     7,
       8,    86,    87,    88,    89,    29,    14,    15,    16,    27,
      12,    29,    30,    31,    32,    33,    34,     3,     4,     5,
       9,     9,    13,     8,    42,    33,    34,     7,    28,     8,
      13,    10,    11,     9,    42,    14,    15,    16,    13,     8,
      13,    10,    11,    13,     7,    14,    15,    16,    43,    93,
       8,   129,    10,   136,    33,    34,    14,    15,    16,    92,
      24,    60,    -1,    42,    33,    34,   104,    21,    -1,    -1,
      24,    25,    26,    42,    -1,    33,    34,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    42,    39,    40,    41
};

  /* YYSTOS[STATE-NUM] -- The (internal number of the) accessing
     symbol of state STATE-NUM.  */
static const yytype_int8 yystos[] =
{
       0,    24,    25,    26,    39,    41,    59,    77,    78,    79,
      26,    34,     0,     8,     9,    21,    59,    74,    76,    10,
      47,    59,    34,     9,    23,     6,     7,     8,    11,    14,
      15,    16,    21,    27,    29,    30,    31,    32,    33,    34,
      42,    44,    45,    46,    47,    48,    49,    50,    51,    52,
      53,    54,    55,    56,    57,    59,    64,    69,    71,    72,
      34,    12,    75,    47,    40,    74,     7,    57,    72,    57,
      59,     8,     8,    44,     7,     7,    12,    73,    11,    45,
      50,     3,     4,     5,    14,    15,    35,    36,    37,    38,
      17,    18,    19,    20,     7,    34,    68,    70,    22,    75,
      13,    12,     7,     9,    34,    62,    65,    57,    57,    29,
      57,    12,    50,    50,    50,    51,    51,    52,    52,    52,
      52,    53,    53,    54,    55,    12,    22,    63,     7,    23,
      57,    57,    58,    22,    63,     7,    23,     9,     9,     8,
      13,    57,    58,    10,    57,    66,    12,    22,    68,     7,
      13,    10,    58,    60,    22,    62,    44,    44,    57,    13,
      13,    11,    66,    67,    58,    66,    11,    60,    61,    60,
      28,     9,    11,    23,    13,    11,    23,    44,     7,    66,
      60
};

  /* YYR1[YYN] -- Symbol number of symbol that rule YYN derives.  */
static const yytype_int8 yyr1[] =
{
       0,    43,    44,    44,    44,    44,    44,    44,    44,    44,
      44,    44,    44,    44,    45,    45,    46,    46,    47,    47,
      48,    48,    48,    48,    49,    49,    49,    50,    50,    51,
      51,    51,    51,    52,    52,    52,    53,    53,    53,    53,
      53,    54,    54,    54,    55,    55,    56,    56,    57,    58,
      59,    59,    59,    59,    59,    60,    60,    60,    61,    61,
      62,    62,    63,    63,    64,    65,    65,    66,    66,    66,
      67,    67,    68,    68,    68,    68,    69,    70,    70,    71,
      71,    72,    72,    73,    73,    74,    74,    74,    74,    75,
      75,    76,    76,    76,    77,    77,    78,    79
};

  /* YYR2[YYN] -- Number of symbols on the right hand side of rule YYN.  */
static const yytype_int8 yyr2[] =
{
       0,     2,     2,     3,     2,     2,     1,     2,     5,     5,
       7,     7,     1,     4,     1,     1,     1,     2,     2,     3,
       3,     1,     1,     1,     1,     1,     1,     1,     2,     1,
       3,     3,     3,     1,     3,     3,     1,     3,     3,     3,
       3,     1,     3,     3,     1,     3,     1,     3,     1,     1,
       1,     1,     2,     1,     1,     1,     2,     3,     1,     3,
       3,     4,     3,     4,     4,     1,     3,     1,     2,     3,
       1,     3,     2,     1,     4,     3,     3,     1,     3,     1,
       1,     1,     2,     3,     4,     2,     3,     3,     4,     2,
       4,     1,     3,     3,     5,     6,     1,     1
};


enum { YYENOMEM = -2 };

#define yyerrok         (yyerrstatus = 0)
#define yyclearin       (yychar = YYEMPTY)

#define YYACCEPT        goto yyacceptlab
#define YYABORT         goto yyabortlab
#define YYERROR         goto yyerrorlab


#define YYRECOVERING()  (!!yyerrstatus)

#define YYBACKUP(Token, Value)                                    \
  do                                                              \
    if (yychar == YYEMPTY)                                        \
      {                                                           \
        yychar = (Token);                                         \
        yylval = (Value);                                         \
        YYPOPSTACK (yylen);                                       \
        yystate = *yyssp;                                         \
        goto yybackup;                                            \
      }                                                           \
    else                                                          \
      {                                                           \
        yyerror (YY_("syntax error: cannot back up")); \
        YYERROR;                                                  \
      }                                                           \
  while (0)

/* Backward compatibility with an undocumented macro.
   Use YYerror or YYUNDEF. */
#define YYERRCODE YYUNDEF


/* Enable debugging if requested.  */
#if YYDEBUG

# ifndef YYFPRINTF
#  include <stdio.h> /* INFRINGES ON USER NAME SPACE */
#  define YYFPRINTF fprintf
# endif

# define YYDPRINTF(Args)                        \
do {                                            \
  if (yydebug)                                  \
    YYFPRINTF Args;                             \
} while (0)

/* This macro is provided for backward compatibility. */
# ifndef YY_LOCATION_PRINT
#  define YY_LOCATION_PRINT(File, Loc) ((void) 0)
# endif


# define YY_SYMBOL_PRINT(Title, Kind, Value, Location)                    \
do {                                                                      \
  if (yydebug)                                                            \
    {                                                                     \
      YYFPRINTF (stderr, "%s ", Title);                                   \
      yy_symbol_print (stderr,                                            \
                  Kind, Value); \
      YYFPRINTF (stderr, "\n");                                           \
    }                                                                     \
} while (0)


/*-----------------------------------.
| Print this symbol's value on YYO.  |
`-----------------------------------*/

static void
yy_symbol_value_print (FILE *yyo,
                       yysymbol_kind_t yykind, YYSTYPE const * const yyvaluep)
{
  FILE *yyoutput = yyo;
  YY_USE (yyoutput);
  if (!yyvaluep)
    return;
# ifdef YYPRINT
  if (yykind < YYNTOKENS)
    YYPRINT (yyo, yytoknum[yykind], *yyvaluep);
# endif
  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  YY_USE (yykind);
  YY_IGNORE_MAYBE_UNINITIALIZED_END
}


/*---------------------------.
| Print this symbol on YYO.  |
`---------------------------*/

static void
yy_symbol_print (FILE *yyo,
                 yysymbol_kind_t yykind, YYSTYPE const * const yyvaluep)
{
  YYFPRINTF (yyo, "%s %s (",
             yykind < YYNTOKENS ? "token" : "nterm", yysymbol_name (yykind));

  yy_symbol_value_print (yyo, yykind, yyvaluep);
  YYFPRINTF (yyo, ")");
}

/*------------------------------------------------------------------.
| yy_stack_print -- Print the state stack from its BOTTOM up to its |
| TOP (included).                                                   |
`------------------------------------------------------------------*/

static void
yy_stack_print (yy_state_t *yybottom, yy_state_t *yytop)
{
  YYFPRINTF (stderr, "Stack now");
  for (; yybottom <= yytop; yybottom++)
    {
      int yybot = *yybottom;
      YYFPRINTF (stderr, " %d", yybot);
    }
  YYFPRINTF (stderr, "\n");
}

# define YY_STACK_PRINT(Bottom, Top)                            \
do {                                                            \
  if (yydebug)                                                  \
    yy_stack_print ((Bottom), (Top));                           \
} while (0)


/*------------------------------------------------.
| Report that the YYRULE is going to be reduced.  |
`------------------------------------------------*/

static void
yy_reduce_print (yy_state_t *yyssp, YYSTYPE *yyvsp,
                 int yyrule)
{
  int yylno = yyrline[yyrule];
  int yynrhs = yyr2[yyrule];
  int yyi;
  YYFPRINTF (stderr, "Reducing stack by rule %d (line %d):\n",
             yyrule - 1, yylno);
  /* The symbols being reduced.  */
  for (yyi = 0; yyi < yynrhs; yyi++)
    {
      YYFPRINTF (stderr, "   $%d = ", yyi + 1);
      yy_symbol_print (stderr,
                       YY_ACCESSING_SYMBOL (+yyssp[yyi + 1 - yynrhs]),
                       &yyvsp[(yyi + 1) - (yynrhs)]);
      YYFPRINTF (stderr, "\n");
    }
}

# define YY_REDUCE_PRINT(Rule)          \
do {                                    \
  if (yydebug)                          \
    yy_reduce_print (yyssp, yyvsp, Rule); \
} while (0)

/* Nonzero means print parse trace.  It is left uninitialized so that
   multiple parsers can coexist.  */
int yydebug;
#else /* !YYDEBUG */
# define YYDPRINTF(Args) ((void) 0)
# define YY_SYMBOL_PRINT(Title, Kind, Value, Location)
# define YY_STACK_PRINT(Bottom, Top)
# define YY_REDUCE_PRINT(Rule)
#endif /* !YYDEBUG */


/* YYINITDEPTH -- initial size of the parser's stacks.  */
#ifndef YYINITDEPTH
# define YYINITDEPTH 200
#endif

/* YYMAXDEPTH -- maximum size the stacks can grow to (effective only
   if the built-in stack extension method is used).

   Do not make this value too large; the results are undefined if
   YYSTACK_ALLOC_MAXIMUM < YYSTACK_BYTES (YYMAXDEPTH)
   evaluated with infinite-precision integer arithmetic.  */

#ifndef YYMAXDEPTH
# define YYMAXDEPTH 10000
#endif






/*-----------------------------------------------.
| Release the memory associated to this symbol.  |
`-----------------------------------------------*/

static void
yydestruct (const char *yymsg,
            yysymbol_kind_t yykind, YYSTYPE *yyvaluep)
{
  YY_USE (yyvaluep);
  if (!yymsg)
    yymsg = "Deleting";
  YY_SYMBOL_PRINT (yymsg, yykind, yyvaluep, yylocationp);

  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  YY_USE (yykind);
  YY_IGNORE_MAYBE_UNINITIALIZED_END
}


/* Lookahead token kind.  */
int yychar;

/* The semantic value of the lookahead symbol.  */
YYSTYPE yylval;
/* Number of syntax errors so far.  */
int yynerrs;




/*----------.
| yyparse.  |
`----------*/

int
yyparse (void)
{
    yy_state_fast_t yystate = 0;
    /* Number of tokens to shift before error messages enabled.  */
    int yyerrstatus = 0;

    /* Refer to the stacks through separate pointers, to allow yyoverflow
       to reallocate them elsewhere.  */

    /* Their size.  */
    YYPTRDIFF_T yystacksize = YYINITDEPTH;

    /* The state stack: array, bottom, top.  */
    yy_state_t yyssa[YYINITDEPTH];
    yy_state_t *yyss = yyssa;
    yy_state_t *yyssp = yyss;

    /* The semantic value stack: array, bottom, top.  */
    YYSTYPE yyvsa[YYINITDEPTH];
    YYSTYPE *yyvs = yyvsa;
    YYSTYPE *yyvsp = yyvs;

  int yyn;
  /* The return value of yyparse.  */
  int yyresult;
  /* Lookahead symbol kind.  */
  yysymbol_kind_t yytoken = YYSYMBOL_YYEMPTY;
  /* The variables used to return semantic value and location from the
     action routines.  */
  YYSTYPE yyval;



#define YYPOPSTACK(N)   (yyvsp -= (N), yyssp -= (N))

  /* The number of symbols on the RHS of the reduced rule.
     Keep to zero when no symbol should be popped.  */
  int yylen = 0;

  YYDPRINTF ((stderr, "Starting parse\n"));

  yychar = YYEMPTY; /* Cause a token to be read.  */
  goto yysetstate;


/*------------------------------------------------------------.
| yynewstate -- push a new state, which is found in yystate.  |
`------------------------------------------------------------*/
yynewstate:
  /* In all cases, when you get here, the value and location stacks
     have just been pushed.  So pushing a state here evens the stacks.  */
  yyssp++;


/*--------------------------------------------------------------------.
| yysetstate -- set current state (the top of the stack) to yystate.  |
`--------------------------------------------------------------------*/
yysetstate:
  YYDPRINTF ((stderr, "Entering state %d\n", yystate));
  YY_ASSERT (0 <= yystate && yystate < YYNSTATES);
  YY_IGNORE_USELESS_CAST_BEGIN
  *yyssp = YY_CAST (yy_state_t, yystate);
  YY_IGNORE_USELESS_CAST_END
  YY_STACK_PRINT (yyss, yyssp);

  if (yyss + yystacksize - 1 <= yyssp)
#if !defined yyoverflow && !defined YYSTACK_RELOCATE
    goto yyexhaustedlab;
#else
    {
      /* Get the current used size of the three stacks, in elements.  */
      YYPTRDIFF_T yysize = yyssp - yyss + 1;

# if defined yyoverflow
      {
        /* Give user a chance to reallocate the stack.  Use copies of
           these so that the &'s don't force the real ones into
           memory.  */
        yy_state_t *yyss1 = yyss;
        YYSTYPE *yyvs1 = yyvs;

        /* Each stack pointer address is followed by the size of the
           data in use in that stack, in bytes.  This used to be a
           conditional around just the two extra args, but that might
           be undefined if yyoverflow is a macro.  */
        yyoverflow (YY_("memory exhausted"),
                    &yyss1, yysize * YYSIZEOF (*yyssp),
                    &yyvs1, yysize * YYSIZEOF (*yyvsp),
                    &yystacksize);
        yyss = yyss1;
        yyvs = yyvs1;
      }
# else /* defined YYSTACK_RELOCATE */
      /* Extend the stack our own way.  */
      if (YYMAXDEPTH <= yystacksize)
        goto yyexhaustedlab;
      yystacksize *= 2;
      if (YYMAXDEPTH < yystacksize)
        yystacksize = YYMAXDEPTH;

      {
        yy_state_t *yyss1 = yyss;
        union yyalloc *yyptr =
          YY_CAST (union yyalloc *,
                   YYSTACK_ALLOC (YY_CAST (YYSIZE_T, YYSTACK_BYTES (yystacksize))));
        if (! yyptr)
          goto yyexhaustedlab;
        YYSTACK_RELOCATE (yyss_alloc, yyss);
        YYSTACK_RELOCATE (yyvs_alloc, yyvs);
#  undef YYSTACK_RELOCATE
        if (yyss1 != yyssa)
          YYSTACK_FREE (yyss1);
      }
# endif

      yyssp = yyss + yysize - 1;
      yyvsp = yyvs + yysize - 1;

      YY_IGNORE_USELESS_CAST_BEGIN
      YYDPRINTF ((stderr, "Stack size increased to %ld\n",
                  YY_CAST (long, yystacksize)));
      YY_IGNORE_USELESS_CAST_END

      if (yyss + yystacksize - 1 <= yyssp)
        YYABORT;
    }
#endif /* !defined yyoverflow && !defined YYSTACK_RELOCATE */

  if (yystate == YYFINAL)
    YYACCEPT;

  goto yybackup;


/*-----------.
| yybackup.  |
`-----------*/
yybackup:
  /* Do appropriate processing given the current state.  Read a
     lookahead token if we need one and don't already have one.  */

  /* First try to decide what to do without reference to lookahead token.  */
  yyn = yypact[yystate];
  if (yypact_value_is_default (yyn))
    goto yydefault;

  /* Not known => get a lookahead token if don't already have one.  */

  /* YYCHAR is either empty, or end-of-input, or a valid lookahead.  */
  if (yychar == YYEMPTY)
    {
      YYDPRINTF ((stderr, "Reading a token\n"));
      yychar = yylex ();
    }

  if (yychar <= YYEOF)
    {
      yychar = YYEOF;
      yytoken = YYSYMBOL_YYEOF;
      YYDPRINTF ((stderr, "Now at end of input.\n"));
    }
  else if (yychar == YYerror)
    {
      /* The scanner already issued an error message, process directly
         to error recovery.  But do not keep the error token as
         lookahead, it is too special and may lead us to an endless
         loop in error recovery. */
      yychar = YYUNDEF;
      yytoken = YYSYMBOL_YYerror;
      goto yyerrlab1;
    }
  else
    {
      yytoken = YYTRANSLATE (yychar);
      YY_SYMBOL_PRINT ("Next token is", yytoken, &yylval, &yylloc);
    }

  /* If the proper action on seeing token YYTOKEN is to reduce or to
     detect an error, take that action.  */
  yyn += yytoken;
  if (yyn < 0 || YYLAST < yyn || yycheck[yyn] != yytoken)
    goto yydefault;
  yyn = yytable[yyn];
  if (yyn <= 0)
    {
      if (yytable_value_is_error (yyn))
        goto yyerrlab;
      yyn = -yyn;
      goto yyreduce;
    }

  /* Count tokens shifted since error; after three, turn off error
     status.  */
  if (yyerrstatus)
    yyerrstatus--;

  /* Shift the lookahead token.  */
  YY_SYMBOL_PRINT ("Shifting", yytoken, &yylval, &yylloc);
  yystate = yyn;
  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  *++yyvsp = yylval;
  YY_IGNORE_MAYBE_UNINITIALIZED_END

  /* Discard the shifted token.  */
  yychar = YYEMPTY;
  goto yynewstate;


/*-----------------------------------------------------------.
| yydefault -- do the default action for the current state.  |
`-----------------------------------------------------------*/
yydefault:
  yyn = yydefact[yystate];
  if (yyn == 0)
    goto yyerrlab;
  goto yyreduce;


/*-----------------------------.
| yyreduce -- do a reduction.  |
`-----------------------------*/
yyreduce:
  /* yyn is the number of a rule to reduce with.  */
  yylen = yyr2[yyn];

  /* If YYLEN is nonzero, implement the default value of the action:
     '$$ = $1'.

     Otherwise, the following line sets YYVAL to garbage.
     This behavior is undocumented and Bison
     users should not rely upon it.  Assigning to YYVAL
     unconditionally makes the parser a bit smaller, and it avoids a
     GCC warning that YYVAL may be used uninitialized.  */
  yyval = yyvsp[1-yylen];


  YY_REDUCE_PRINT (yyn);
  switch (yyn)
    {
  case 2: /* Stmt: T_RETURN T_SEMI  */
#line 404 "parser/parser.y"
                      {
    yyval = new Node{
        {"kind", "ReturnStmt"}
    };
}
#line 1727 "parser.tab.c"
    break;

  case 3: /* Stmt: T_RETURN Exp T_SEMI  */
#line 409 "parser/parser.y"
                      {
    yyval = new Node{
        {"kind", "ReturnStmt"}
    };
    yyval->add_child(&yyvsp[-1]);
}
#line 1738 "parser.tab.c"
    break;

  case 4: /* Stmt: T_BREAK T_SEMI  */
#line 415 "parser/parser.y"
                 {
    yyval = new Node{
        {"kind", "BreakStmt"}
    };
}
#line 1748 "parser.tab.c"
    break;

  case 5: /* Stmt: T_CONTINUE T_SEMI  */
#line 420 "parser/parser.y"
                    {
    yyval = new Node{
        {"kind", "ContinueStmt"}
    };
}
#line 1758 "parser.tab.c"
    break;

  case 6: /* Stmt: T_SEMI  */
#line 425 "parser/parser.y"
         {
    yyval = new Node{
        {"kind", "NullStmt"}
    };
}
#line 1768 "parser.tab.c"
    break;

  case 7: /* Stmt: Exp T_SEMI  */
#line 430 "parser/parser.y"
             {
    yyval = yyvsp[-1];
    yyvsp[-1] = nullptr;
}
#line 1777 "parser.tab.c"
    break;

  case 8: /* Stmt: T_WHILE T_L_PAREN Exp T_R_PAREN Stmt  */
#line 434 "parser/parser.y"
                                       {
    yyval = new Node {
        {"kind", "WhileStmt"}
    };
    yyval->add_child(&yyvsp[-2]);
    yyval->add_child(&yyvsp[0]);
}
#line 1789 "parser.tab.c"
    break;

  case 9: /* Stmt: T_IF T_L_PAREN Exp T_R_PAREN Stmt  */
#line 441 "parser/parser.y"
                                    {
    yyval = new Node{
        {"kind", "IfStmt"}
    };
    yyval->add_child(&yyvsp[-2]);
    yyval->add_child(&yyvsp[0]);
}
#line 1801 "parser.tab.c"
    break;

  case 10: /* Stmt: T_IF T_L_PAREN Exp T_R_PAREN Stmt T_ELSE Stmt  */
#line 448 "parser/parser.y"
                                                {
    yyval = new Node{
        {"kind", "IfStmt"}
    };
    yyval->add_child(&yyvsp[-4]);
    yyval->add_child(&yyvsp[-2]);
    yyval->add_child(&yyvsp[0]);
}
#line 1814 "parser.tab.c"
    break;

  case 11: /* Stmt: T_DO Stmt T_WHILE T_L_PAREN Exp T_R_PAREN T_SEMI  */
#line 456 "parser/parser.y"
                                                     {
    yyval = new Node{
        {"kind", "DoStmt"}
    };
    yyval->add_child(&yyvsp[-5]);
    yyval->add_child(&yyvsp[-2]);
}
#line 1826 "parser.tab.c"
    break;

  case 12: /* Stmt: Block  */
#line 463 "parser/parser.y"
        {
    yyval = yyvsp[0];
    yyvsp[0] = nullptr;
}
#line 1835 "parser.tab.c"
    break;

  case 13: /* Stmt: LVal T_EQUAL Exp T_SEMI  */
#line 467 "parser/parser.y"
                         {
    std::string type = value2string(yyvsp[-3]->get_value("type"));
    yyval = new Node {
        {"kind", "BinaryOperator"},
        {"type", type}
    };
    if(!(yyvsp[-1]->get_value("type") == type)) {
        Node *tmp = new Node{
            {"kind", "ImplicitCastExpr"},
            {"type", type},
            {"castKind", "typeCast"}
        };
        tmp->add_child(&yyvsp[-1]);
        yyval->add_child(&yyvsp[-3]);
        yyval->add_child(&tmp);
    }
    else {
        yyval->add_child(&yyvsp[-3]);
        yyval->add_child(&yyvsp[-1]);
    }
}
#line 1861 "parser.tab.c"
    break;

  case 14: /* BlockItem: Decl  */
#line 489 "parser/parser.y"
                {
    yyval = yyvsp[0];
    yyvsp[0] = nullptr;
}
#line 1870 "parser.tab.c"
    break;

  case 15: /* BlockItem: Stmt  */
#line 493 "parser/parser.y"
       {
    yyval = yyvsp[0];
    yyvsp[0] = nullptr;
}
#line 1879 "parser.tab.c"
    break;

  case 16: /* BlockItemArr: BlockItem  */
#line 498 "parser/parser.y"
                        {
    yyval = new Node{
        {"kind", "CompoundStmt"}
    };
    yyval->add_child(&yyvsp[0]);
}
#line 1890 "parser.tab.c"
    break;

  case 17: /* BlockItemArr: BlockItemArr BlockItem  */
#line 504 "parser/parser.y"
                         {
    yyval = yyvsp[-1];
    yyvsp[-1] = nullptr;
    yyval->add_child(&yyvsp[0]);
}
#line 1900 "parser.tab.c"
    break;

  case 18: /* Block: T_L_BRACE T_R_BRACE  */
#line 510 "parser/parser.y"
                           {
    yyval = new Node{
        {"kind", "CompoundStmt"}
    };
}
#line 1910 "parser.tab.c"
    break;

  case 19: /* Block: T_L_BRACE BlockItemArr T_R_BRACE  */
#line 515 "parser/parser.y"
                                   {
    yyval = yyvsp[-1];
    yyvsp[-1] = nullptr;
    for(auto &child : yyval->childs) {
        /* child = Decl or Stmt */
        if(child->get_value("kind") == "DeclStmt") update_type(child);
    }
}
#line 1923 "parser.tab.c"
    break;

  case 20: /* PrimaryExp: T_L_PAREN Exp T_R_PAREN  */
#line 524 "parser/parser.y"
                                    {
    yyval = new Node{
        {"kind", "ParenExpr"},
        {"type", yyvsp[-1]->get_value("type")}
    };
    yyval->add_child(&yyvsp[-1]);
}
#line 1935 "parser.tab.c"
    break;

  case 21: /* PrimaryExp: T_NUMERIC_CONSTANT  */
#line 531 "parser/parser.y"
                     {
    yyval = yyvsp[0];
    yyvsp[0] = nullptr;
}
#line 1944 "parser.tab.c"
    break;

  case 22: /* PrimaryExp: T_STRING_LITERAL  */
#line 535 "parser/parser.y"
                   {
    yyval = yyvsp[0];
    yyvsp[0] = nullptr;
}
#line 1953 "parser.tab.c"
    break;

  case 23: /* PrimaryExp: LVal  */
#line 539 "parser/parser.y"
       {
    yyval = new Node{
        {"kind", "ImplicitCastExpr"},
        {"castKind", "LValToRVal"}
    };
    yyval->add_kv("type", value2string(yyvsp[0]->get_value("type")));
    yyval->add_child(&yyvsp[0]);
}
#line 1966 "parser.tab.c"
    break;

  case 24: /* UnaryOp: T_PLUS  */
#line 548 "parser/parser.y"
                {
    yyval = new Node {
        {"kind", "UnaryOperator"},
        {"opcode", "+"}
    };
}
#line 1977 "parser.tab.c"
    break;

  case 25: /* UnaryOp: T_MINUS  */
#line 554 "parser/parser.y"
          {
    yyval = new Node {
        {"kind", "UnaryOperator"},
        {"opcode", "-"}
    };
}
#line 1988 "parser.tab.c"
    break;

  case 26: /* UnaryOp: T_EXCLAIM  */
#line 560 "parser/parser.y"
            {
    yyval = new Node {
        {"kind", "UnaryOperator"},
        {"opcode", "!"}
    };
}
#line 1999 "parser.tab.c"
    break;

  case 27: /* UnaryExp: PrimaryExp  */
#line 567 "parser/parser.y"
                     {
    yyval = yyvsp[0];
    yyvsp[0] = nullptr;
}
#line 2008 "parser.tab.c"
    break;

  case 28: /* UnaryExp: UnaryOp UnaryExp  */
#line 571 "parser/parser.y"
                   {
    yyval = yyvsp[-1];
    yyvsp[-1] = nullptr;
    if(yyval->get_value("opcode") == "!") {
        yyval->add_kv("type", "int");
    }
    else {
        yyval->add_kv("type", yyvsp[0]->get_value("type"));
    }
    yyval->add_child(&yyvsp[0]);
}
#line 2024 "parser.tab.c"
    break;

  case 29: /* MulExp: UnaryExp  */
#line 583 "parser/parser.y"
                 {
    yyval = yyvsp[0];
    yyvsp[0] = nullptr;
}
#line 2033 "parser.tab.c"
    break;

  case 30: /* MulExp: MulExp T_STAR UnaryExp  */
#line 587 "parser/parser.y"
                         {
    yyval = type_judge(yyvsp[-2], yyvsp[0]);
    yyvsp[-2] = nullptr;
    yyvsp[0] = nullptr;
    yyval->add_kv("opcode", "*");
}
#line 2044 "parser.tab.c"
    break;

  case 31: /* MulExp: MulExp T_SLASH UnaryExp  */
#line 593 "parser/parser.y"
                          {
    yyval = type_judge(yyvsp[-2], yyvsp[0]);
    yyvsp[-2] = nullptr;
    yyvsp[0] = nullptr;
    yyval->add_kv("opcode", "/");
}
#line 2055 "parser.tab.c"
    break;

  case 32: /* MulExp: MulExp T_PERCENT UnaryExp  */
#line 599 "parser/parser.y"
                            {
    yyval = type_judge(yyvsp[-2], yyvsp[0]);
    yyvsp[-2] = nullptr;
    yyvsp[0] = nullptr;
    yyval->add_kv("opcode", "%");
}
#line 2066 "parser.tab.c"
    break;

  case 33: /* AddExp: MulExp  */
#line 606 "parser/parser.y"
               {
    yyval = yyvsp[0];
    yyvsp[0] = nullptr;
}
#line 2075 "parser.tab.c"
    break;

  case 34: /* AddExp: AddExp T_PLUS MulExp  */
#line 610 "parser/parser.y"
                       {
    yyval = type_judge(yyvsp[-2], yyvsp[0]);
    yyvsp[-2] = nullptr;
    yyvsp[0] = nullptr;
    yyval->add_kv("opcode", "+");
}
#line 2086 "parser.tab.c"
    break;

  case 35: /* AddExp: AddExp T_MINUS MulExp  */
#line 616 "parser/parser.y"
                        {
    yyval = type_judge(yyvsp[-2], yyvsp[0]);
    yyvsp[-2] = nullptr;
    yyvsp[0] = nullptr;
    yyval->add_kv("opcode", "-");
}
#line 2097 "parser.tab.c"
    break;

  case 36: /* RelExp: AddExp  */
#line 623 "parser/parser.y"
               {
    yyval = yyvsp[0];
    yyvsp[0] = nullptr;
}
#line 2106 "parser.tab.c"
    break;

  case 37: /* RelExp: RelExp T_LESS AddExp  */
#line 627 "parser/parser.y"
                       {
    yyval = type_judge(yyvsp[-2], yyvsp[0]);
    yyvsp[-2] = nullptr;
    yyvsp[0] = nullptr;
    yyval->add_kv("opcode", "<");
    yyval->change_kv("type", "int");
}
#line 2118 "parser.tab.c"
    break;

  case 38: /* RelExp: RelExp T_GREATER AddExp  */
#line 634 "parser/parser.y"
                          {
    yyval = type_judge(yyvsp[-2], yyvsp[0]);
    yyvsp[-2] = nullptr;
    yyvsp[0] = nullptr;
    yyval->add_kv("opcode", ">");
    yyval->change_kv("type", "int");
}
#line 2130 "parser.tab.c"
    break;

  case 39: /* RelExp: RelExp T_LESSEQUAL AddExp  */
#line 641 "parser/parser.y"
                            {
    yyval = type_judge(yyvsp[-2], yyvsp[0]);
    yyvsp[-2] = nullptr;
    yyvsp[0] = nullptr;
    yyval->add_kv("opcode", "<=");
    yyval->change_kv("type", "int");
}
#line 2142 "parser.tab.c"
    break;

  case 40: /* RelExp: RelExp T_GREATEREQUAL AddExp  */
#line 648 "parser/parser.y"
                               {
    yyval = type_judge(yyvsp[-2], yyvsp[0]);
    yyvsp[-2] = nullptr;
    yyvsp[0] = nullptr;
    yyval->add_kv("opcode", ">=");
    yyval->change_kv("type", "int");
}
#line 2154 "parser.tab.c"
    break;

  case 41: /* EqExp: RelExp  */
#line 656 "parser/parser.y"
              {
    yyval = yyvsp[0];
    yyvsp[0] = nullptr;
}
#line 2163 "parser.tab.c"
    break;

  case 42: /* EqExp: EqExp T_EQUALEQUAL RelExp  */
#line 660 "parser/parser.y"
                            {
    yyval = type_judge(yyvsp[-2], yyvsp[0]);
    yyvsp[-2] = nullptr;
    yyvsp[0] = nullptr;
    yyval->add_kv("opcode", "==");
    yyval->change_kv("type", "int");
}
#line 2175 "parser.tab.c"
    break;

  case 43: /* EqExp: EqExp T_EXCLAIMEQUAL RelExp  */
#line 667 "parser/parser.y"
                              {
    yyval = type_judge(yyvsp[-2], yyvsp[0]);
    yyvsp[-2] = nullptr;
    yyvsp[0] = nullptr;
    yyval->add_kv("opcode", "!=");
    yyval->change_kv("type", "int");
}
#line 2187 "parser.tab.c"
    break;

  case 44: /* LAndExp: EqExp  */
#line 675 "parser/parser.y"
               {
    yyval = yyvsp[0];
    yyvsp[0] = nullptr;
}
#line 2196 "parser.tab.c"
    break;

  case 45: /* LAndExp: LAndExp T_AMPAMP EqExp  */
#line 679 "parser/parser.y"
                         {
    yyval = type_judge(yyvsp[-2], yyvsp[0]);
    yyvsp[-2] = nullptr;
    yyvsp[0] = nullptr;
    yyval->add_kv("opcode", "&&");
    yyval->change_kv("type", "int");
}
#line 2208 "parser.tab.c"
    break;

  case 46: /* LOrExp: LAndExp  */
#line 687 "parser/parser.y"
                {
    yyval = yyvsp[0];
    yyvsp[0] = nullptr;
}
#line 2217 "parser.tab.c"
    break;

  case 47: /* LOrExp: LOrExp T_PIPEPIPE LAndExp  */
#line 691 "parser/parser.y"
                            {
    yyval = type_judge(yyvsp[-2], yyvsp[0]);
    yyvsp[-2] = nullptr;
    yyvsp[0] = nullptr;
    yyval->add_kv("opcode", "||");
    yyval->change_kv("type", "int");
}
#line 2229 "parser.tab.c"
    break;

  case 48: /* Exp: LOrExp  */
#line 699 "parser/parser.y"
            {
    yyval = yyvsp[0];
    yyvsp[0] = nullptr;
}
#line 2238 "parser.tab.c"
    break;

  case 49: /* ConstExp: Exp  */
#line 704 "parser/parser.y"
              {
    yyval = yyvsp[0];
    yyvsp[0] = nullptr;
}
#line 2247 "parser.tab.c"
    break;

  case 50: /* Type: T_INT  */
#line 709 "parser/parser.y"
            {
    yyval = new Node{
        {"type", "int"}
    };
}
#line 2257 "parser.tab.c"
    break;

  case 51: /* Type: T_CHAR  */
#line 714 "parser/parser.y"
         {
    yyval = new Node{
        {"type", "char"}
    };
}
#line 2267 "parser.tab.c"
    break;

  case 52: /* Type: T_LONG T_LONG  */
#line 719 "parser/parser.y"
                {
    yyval = new Node{
        {"type", "long long"}
    };
}
#line 2277 "parser.tab.c"
    break;

  case 53: /* Type: T_VOID  */
#line 724 "parser/parser.y"
         {
    yyval = new Node{
        {"type", "void"}
    };
}
#line 2287 "parser.tab.c"
    break;

  case 54: /* Type: T_FLOAT  */
#line 729 "parser/parser.y"
          {
    yyval = new Node{
        {"type", "float"}
    };
}
#line 2297 "parser.tab.c"
    break;

  case 55: /* ConstInitVal: ConstExp  */
#line 735 "parser/parser.y"
                       {
    yyval = yyvsp[0];
    yyvsp[0] = nullptr;
}
#line 2306 "parser.tab.c"
    break;

  case 56: /* ConstInitVal: T_L_BRACE T_R_BRACE  */
#line 739 "parser/parser.y"
                      {
    yyval = new Node{
        {"kind", "InitListExpr"},
        {"type", "null"}
    };
}
#line 2317 "parser.tab.c"
    break;

  case 57: /* ConstInitVal: T_L_BRACE ConstInitValArray T_R_BRACE  */
#line 745 "parser/parser.y"
                                        {
    yyval = new Node {
        {"kind", "InitListExpr"},
        {"type", "null"}
    };
}
#line 2328 "parser.tab.c"
    break;

  case 58: /* ConstInitValArray: ConstInitVal  */
#line 752 "parser/parser.y"
                                {
    delete yyvsp[0];
}
#line 2336 "parser.tab.c"
    break;

  case 59: /* ConstInitValArray: ConstInitValArray T_COMMA ConstInitVal  */
#line 755 "parser/parser.y"
                                         {
    delete yyvsp[0];
}
#line 2344 "parser.tab.c"
    break;

  case 60: /* ConstDef: T_IDENTIFIER T_EQUAL ConstInitVal  */
#line 759 "parser/parser.y"
                                            {
    yyval = yyvsp[-2];
    yyvsp[-2] = nullptr;
    yyval->add_kv("kind", "VarDecl");
    yyval->add_child(&yyvsp[0]);
}
#line 2355 "parser.tab.c"
    break;

  case 61: /* ConstDef: T_IDENTIFIER ConstExpArray T_EQUAL ConstInitVal  */
#line 765 "parser/parser.y"
                                                  {
    yyval = yyvsp[-3];
    yyvsp[-3] = nullptr;
    yyval->add_kv("kind", "VarDecl");
    yyval->add_child(&yyvsp[0]);
}
#line 2366 "parser.tab.c"
    break;

  case 62: /* ConstExpArray: T_L_SQUARE ConstExp T_R_SQUARE  */
#line 772 "parser/parser.y"
                                              {
    delete yyvsp[-1];
}
#line 2374 "parser.tab.c"
    break;

  case 63: /* ConstExpArray: ConstExpArray T_L_SQUARE ConstExp T_R_SQUARE  */
#line 775 "parser/parser.y"
                                               {
    delete yyvsp[-1];
}
#line 2382 "parser.tab.c"
    break;

  case 64: /* ConstDecl: T_CONST Type ConstDefArr T_SEMI  */
#line 780 "parser/parser.y"
                                          {
    yyval = yyvsp[-1];
    yyvsp[-1] = nullptr;
    for(auto &const_def : yyval->childs) {
        if(const_def->childs[0]->get_value("type") != "null" && const_def->childs[0]->get_value("type") != yyvsp[-2]->get_value("type")){
            Node *tmp = const_def->childs[0];
            const_def->childs.pop_back();
            Node *tmp_father = new Node{
                {"kind", "ImplicitCastExpr"},
                {"type", yyvsp[-2]->get_value("type")},
                {"castKind", "typeCast"}
            };
            tmp_father->add_child(&tmp);
            const_def->add_child(&tmp_father);
        }
        std::string name = value2string(const_def->get_value("name"));
        std::string type = value2string(yyvsp[-2]->get_value("type"));
        id_map[name].push(type);
    }
    delete yyvsp[-2];
}
#line 2408 "parser.tab.c"
    break;

  case 65: /* ConstDefArr: ConstDef  */
#line 802 "parser/parser.y"
                      {
    yyval = new Node{
        {"kind", "DeclStmt"}
    };
    yyval->add_child(&yyvsp[0]);
}
#line 2419 "parser.tab.c"
    break;

  case 66: /* ConstDefArr: ConstDefArr T_COMMA ConstDef  */
#line 808 "parser/parser.y"
                               {
    yyval = yyvsp[-2];
    yyvsp[-2] = nullptr;
    yyval->add_child(&yyvsp[0]);
}
#line 2429 "parser.tab.c"
    break;

  case 67: /* InitVal: Exp  */
#line 814 "parser/parser.y"
             {
    yyval = yyvsp[0];
    yyvsp[0] = nullptr;
}
#line 2438 "parser.tab.c"
    break;

  case 68: /* InitVal: T_L_BRACE T_R_BRACE  */
#line 818 "parser/parser.y"
                      {
    yyval = new Node{
        {"kind", "InitListExpr"},
        {"type", "null"}
    };
}
#line 2449 "parser.tab.c"
    break;

  case 69: /* InitVal: T_L_BRACE InitValArr T_R_BRACE  */
#line 824 "parser/parser.y"
                                 {
    yyval = new Node {
        {"kind", "InitListExpr"},
        {"type", "null"}
    };
}
#line 2460 "parser.tab.c"
    break;

  case 70: /* InitValArr: InitVal  */
#line 831 "parser/parser.y"
                    {
    delete yyvsp[0];
}
#line 2468 "parser.tab.c"
    break;

  case 71: /* InitValArr: InitValArr T_COMMA InitVal  */
#line 834 "parser/parser.y"
                             {
    delete yyvsp[0];
}
#line 2476 "parser.tab.c"
    break;

  case 72: /* VarDef: T_IDENTIFIER ConstExpArray  */
#line 838 "parser/parser.y"
                                   {
    yyval = yyvsp[-1];
    yyvsp[-1] = nullptr;
    yyval->add_kv("kind", "VarDecl");
}
#line 2486 "parser.tab.c"
    break;

  case 73: /* VarDef: T_IDENTIFIER  */
#line 843 "parser/parser.y"
               {
    yyval = yyvsp[0];
    yyvsp[0] = nullptr;
    yyval->add_kv("kind", "VarDecl");
}
#line 2496 "parser.tab.c"
    break;

  case 74: /* VarDef: T_IDENTIFIER ConstExpArray T_EQUAL InitVal  */
#line 848 "parser/parser.y"
                                             {
    yyval = yyvsp[-3];
    yyvsp[-3] = nullptr;
    yyval->add_kv("kind", "VarDecl");
    yyval->add_child(&yyvsp[0]);
}
#line 2507 "parser.tab.c"
    break;

  case 75: /* VarDef: T_IDENTIFIER T_EQUAL InitVal  */
#line 854 "parser/parser.y"
                               {
    yyval = yyvsp[-2];
    yyvsp[-2] = nullptr;
    yyval->add_kv("kind", "VarDecl");
    yyval->add_child(&yyvsp[0]);
}
#line 2518 "parser.tab.c"
    break;

  case 76: /* VarDecl: Type VarDefArr T_SEMI  */
#line 861 "parser/parser.y"
                               {
    yyval = yyvsp[-1];
    yyvsp[-1] = nullptr;
    for(auto &def : yyval->childs) {
        std::string name = value2string(def->get_value("name"));
        std::string type = value2string(yyvsp[-2]->get_value("type"));
        id_map[name].push(type);
        if(def->childs.size()) {
            if(def->childs[0]->get_value("type") != "null" && def->childs[0]->get_value("type") != yyvsp[-2]->get_value("type")) {
                Node *tmp = def->childs[0];
                def->childs.pop_back();
                Node *tmp_father = new Node{
                    {"kind", "ImplicitCastExpr"},
                    {"type", yyvsp[-2]->get_value("type")},
                    {"castKind", "typeCast"}
                };
                tmp_father->add_child(&tmp);
                def->add_child(&tmp_father);
            }
        }
    }
}
#line 2545 "parser.tab.c"
    break;

  case 77: /* VarDefArr: VarDef  */
#line 884 "parser/parser.y"
                  {
    yyval = new Node{
        {"kind", "DeclStmt"}
    };
    yyval->add_child(&yyvsp[0]);
}
#line 2556 "parser.tab.c"
    break;

  case 78: /* VarDefArr: VarDefArr T_COMMA VarDef  */
#line 890 "parser/parser.y"
                           {
    yyval->add_child(&yyvsp[0]);
}
#line 2564 "parser.tab.c"
    break;

  case 79: /* Decl: ConstDecl  */
#line 894 "parser/parser.y"
                {
    yyval = yyvsp[0];
    yyvsp[0] = nullptr;
}
#line 2573 "parser.tab.c"
    break;

  case 80: /* Decl: VarDecl  */
#line 898 "parser/parser.y"
          {
    yyval = yyvsp[0];
    yyvsp[0] = nullptr;
}
#line 2582 "parser.tab.c"
    break;

  case 81: /* LVal: T_IDENTIFIER  */
#line 903 "parser/parser.y"
                   {
    yyval = yyvsp[0];
    yyvsp[0] = nullptr;
    yyval->add_kv("kind", "DeclRefExpr");
    std::string name = value2string(yyval->get_value("name"));
    yyval->add_kv("type", id_map[name].top());
}
#line 2594 "parser.tab.c"
    break;

  case 82: /* LVal: T_IDENTIFIER ExpArr  */
#line 910 "parser/parser.y"
                      {
    yyval = yyvsp[0];
    yyvsp[0] = nullptr;
    std::string name = value2string(yyvsp[-1]->get_value("name"));
    Node *tmp = yyval;
    while(tmp->childs.size()) {
        tmp = tmp->childs[0];
    }
    yyvsp[-1]->add_kv("kind", "DeclRefExpr");
    tmp->add_child(&yyvsp[-1]);
    yyval->add_kv("type", id_map[name].top());
}
#line 2611 "parser.tab.c"
    break;

  case 83: /* ExpArr: T_L_SQUARE Exp T_R_SQUARE  */
#line 923 "parser/parser.y"
                                  {
    yyval = new Node{
        {"kind", "ArraySubscriptExpr"}
    };
    Node *tmp = new Node{
        {"kind", "ImplicitCastExpr"}
    };
    yyval->add_child(&tmp);
    yyval->add_child(&yyvsp[-1]);
}
#line 2626 "parser.tab.c"
    break;

  case 84: /* ExpArr: ExpArr T_L_SQUARE Exp T_R_SQUARE  */
#line 933 "parser/parser.y"
                                   {
    yyval = yyvsp[-3];
    yyvsp[-3] = nullptr;
    Node *tmp = yyval;
    while(tmp->childs.size()) {
        tmp = yyval->childs[0];
    }
    Node *ase = new Node{
        {"kind", "ArraySubscriptExpr"}
    };
    Node *ice = new Node{
        {"kind", "ImplicitCastExpr"}
    };
    ase->add_child(&ice);
    ase->add_child(&yyvsp[-1]);
    tmp->add_child(&ase);
}
#line 2648 "parser.tab.c"
    break;

  case 85: /* FuncFParam: Type T_IDENTIFIER  */
#line 951 "parser/parser.y"
                              {
    yyval = yyvsp[0];
    yyvsp[0] = nullptr;
    yyval->add_kv("type", yyvsp[-1]->get_value("type"));
    yyval->add_kv("kind", "ParamVarDecl");
}
#line 2659 "parser.tab.c"
    break;

  case 86: /* FuncFParam: Type T_IDENTIFIER ArraySquare  */
#line 957 "parser/parser.y"
                                {
    yyval = yyvsp[-1];
    yyvsp[-1] = nullptr;
    yyval->add_kv("type", yyvsp[-2]->get_value("type"));
    yyval->add_kv("kind", "ParamVarDecl");
}
#line 2670 "parser.tab.c"
    break;

  case 87: /* FuncFParam: T_CONST Type T_IDENTIFIER  */
#line 963 "parser/parser.y"
                            {
    std::cout << "Yes" << std::endl;
    yyval = yyvsp[0];
    yyvsp[0] = nullptr;
    yyval->add_kv("type", yyvsp[-2]->get_value("type"));
    yyval->add_kv("kind", "ParamVarDecl");
}
#line 2682 "parser.tab.c"
    break;

  case 88: /* FuncFParam: T_CONST Type T_IDENTIFIER ArraySquare  */
#line 970 "parser/parser.y"
                                        {
    yyval = yyvsp[-1];
    yyvsp[-1] = nullptr;
    yyval->add_kv("type", yyvsp[-3]->get_value("type"));
    yyval->add_kv("kind", "ParamVarDecl");
}
#line 2693 "parser.tab.c"
    break;

  case 89: /* ArraySquare: T_L_SQUARE T_R_SQUARE  */
#line 977 "parser/parser.y"
                                   {}
#line 2699 "parser.tab.c"
    break;

  case 90: /* ArraySquare: ArraySquare T_L_SQUARE ConstExp T_R_SQUARE  */
#line 978 "parser/parser.y"
                                              {
    delete yyvsp[-1];
}
#line 2707 "parser.tab.c"
    break;

  case 91: /* FuncFParams: FuncFParam  */
#line 982 "parser/parser.y"
                        {
    yyval = new Node{
        {"kind", "FunctionDecl"}
    };
    yyval->add_child(&yyvsp[0]);
}
#line 2718 "parser.tab.c"
    break;

  case 92: /* FuncFParams: FuncFParams T_COMMA FuncFParam  */
#line 988 "parser/parser.y"
                                 {
    yyval = yyvsp[-2];
    yyvsp[-2] = nullptr;
    yyval->add_child(&yyvsp[0]);
}
#line 2728 "parser.tab.c"
    break;

  case 93: /* FuncFParams: FuncFParams T_COMMA T_ELLIPSIS  */
#line 993 "parser/parser.y"
                                 {
    yyval = yyvsp[-2];
    yyvsp[-2] = nullptr;
    Node *tmp = new Node{
        {"kind", "ellipsis"},
        {"type", "null"}
    };
    yyval->add_child(&tmp);
}
#line 2742 "parser.tab.c"
    break;

  case 94: /* FuncDef: Type T_IDENTIFIER T_L_PAREN T_R_PAREN Block  */
#line 1003 "parser/parser.y"
                                                     {
    yyval = yyvsp[-3];
    yyvsp[-3] = nullptr;
    yyval->add_kv("kind", "FunctionDecl");
    std::string return_type = value2string(yyvsp[-4]->get_value("type"));
    yyval->add_kv("returnType", return_type);
    yyval->add_child(&yyvsp[0]);
    std::string name = value2string(yyval->get_value("name"));
    func_map[name].push_back(return_type);
    delete yyvsp[-4];
}
#line 2758 "parser.tab.c"
    break;

  case 95: /* FuncDef: Type T_IDENTIFIER T_L_PAREN FuncFParams T_R_PAREN Block  */
#line 1014 "parser/parser.y"
                                                          {
    yyval = yyvsp[-2];
    yyvsp[-2] = nullptr;
    std::string name = value2string(yyvsp[-4]->get_value("name"));
    delete yyvsp[-4];
    yyval->add_kv("name", yyvsp[-4]->get_value("name"));
    std::string return_type = value2string(yyvsp[-5]->get_value("type"));
    delete yyvsp[-5];
    yyval->add_kv("returnType", return_type);
    func_map[name].push_back(return_type);
    for(auto &child: yyval->childs) {
        /* child->kind = ParamVarDecl */
        std::string type = value2string(child->get_value("type"));
        func_map[name].push_back(type);
    }
    int size = yyval->childs.size();
    if(yyval->childs[size - 1]->get_value("type") == "null") {
        yyval->childs.erase(yyval->childs.end() - 1);
    }
    yyval->add_child(&yyvsp[0]);
}
#line 2784 "parser.tab.c"
    break;

  case 96: /* Begin2: FuncDef  */
#line 1035 "parser/parser.y"
                {
    yyval = new Node{
        {"kind", "TranslationUnitDecl"}
    };
    yyval->add_child(&yyvsp[0]);
}
#line 2795 "parser.tab.c"
    break;

  case 97: /* CompUnit: Begin2  */
#line 1074 "parser/parser.y"
                 {
    root = yyvsp[0];
    yyvsp[0] = nullptr;
}
#line 2804 "parser.tab.c"
    break;


#line 2808 "parser.tab.c"

      default: break;
    }
  /* User semantic actions sometimes alter yychar, and that requires
     that yytoken be updated with the new translation.  We take the
     approach of translating immediately before every use of yytoken.
     One alternative is translating here after every semantic action,
     but that translation would be missed if the semantic action invokes
     YYABORT, YYACCEPT, or YYERROR immediately after altering yychar or
     if it invokes YYBACKUP.  In the case of YYABORT or YYACCEPT, an
     incorrect destructor might then be invoked immediately.  In the
     case of YYERROR or YYBACKUP, subsequent parser actions might lead
     to an incorrect destructor call or verbose syntax error message
     before the lookahead is translated.  */
  YY_SYMBOL_PRINT ("-> $$ =", YY_CAST (yysymbol_kind_t, yyr1[yyn]), &yyval, &yyloc);

  YYPOPSTACK (yylen);
  yylen = 0;

  *++yyvsp = yyval;

  /* Now 'shift' the result of the reduction.  Determine what state
     that goes to, based on the state we popped back to and the rule
     number reduced by.  */
  {
    const int yylhs = yyr1[yyn] - YYNTOKENS;
    const int yyi = yypgoto[yylhs] + *yyssp;
    yystate = (0 <= yyi && yyi <= YYLAST && yycheck[yyi] == *yyssp
               ? yytable[yyi]
               : yydefgoto[yylhs]);
  }

  goto yynewstate;


/*--------------------------------------.
| yyerrlab -- here on detecting error.  |
`--------------------------------------*/
yyerrlab:
  /* Make sure we have latest lookahead translation.  See comments at
     user semantic actions for why this is necessary.  */
  yytoken = yychar == YYEMPTY ? YYSYMBOL_YYEMPTY : YYTRANSLATE (yychar);
  /* If not already recovering from an error, report this error.  */
  if (!yyerrstatus)
    {
      ++yynerrs;
      yyerror (YY_("syntax error"));
    }

  if (yyerrstatus == 3)
    {
      /* If just tried and failed to reuse lookahead token after an
         error, discard it.  */

      if (yychar <= YYEOF)
        {
          /* Return failure if at end of input.  */
          if (yychar == YYEOF)
            YYABORT;
        }
      else
        {
          yydestruct ("Error: discarding",
                      yytoken, &yylval);
          yychar = YYEMPTY;
        }
    }

  /* Else will try to reuse lookahead token after shifting the error
     token.  */
  goto yyerrlab1;


/*---------------------------------------------------.
| yyerrorlab -- error raised explicitly by YYERROR.  |
`---------------------------------------------------*/
yyerrorlab:
  /* Pacify compilers when the user code never invokes YYERROR and the
     label yyerrorlab therefore never appears in user code.  */
  if (0)
    YYERROR;

  /* Do not reclaim the symbols of the rule whose action triggered
     this YYERROR.  */
  YYPOPSTACK (yylen);
  yylen = 0;
  YY_STACK_PRINT (yyss, yyssp);
  yystate = *yyssp;
  goto yyerrlab1;


/*-------------------------------------------------------------.
| yyerrlab1 -- common code for both syntax error and YYERROR.  |
`-------------------------------------------------------------*/
yyerrlab1:
  yyerrstatus = 3;      /* Each real token shifted decrements this.  */

  /* Pop stack until we find a state that shifts the error token.  */
  for (;;)
    {
      yyn = yypact[yystate];
      if (!yypact_value_is_default (yyn))
        {
          yyn += YYSYMBOL_YYerror;
          if (0 <= yyn && yyn <= YYLAST && yycheck[yyn] == YYSYMBOL_YYerror)
            {
              yyn = yytable[yyn];
              if (0 < yyn)
                break;
            }
        }

      /* Pop the current state because it cannot handle the error token.  */
      if (yyssp == yyss)
        YYABORT;


      yydestruct ("Error: popping",
                  YY_ACCESSING_SYMBOL (yystate), yyvsp);
      YYPOPSTACK (1);
      yystate = *yyssp;
      YY_STACK_PRINT (yyss, yyssp);
    }

  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  *++yyvsp = yylval;
  YY_IGNORE_MAYBE_UNINITIALIZED_END


  /* Shift the error token.  */
  YY_SYMBOL_PRINT ("Shifting", YY_ACCESSING_SYMBOL (yyn), yyvsp, yylsp);

  yystate = yyn;
  goto yynewstate;


/*-------------------------------------.
| yyacceptlab -- YYACCEPT comes here.  |
`-------------------------------------*/
yyacceptlab:
  yyresult = 0;
  goto yyreturn;


/*-----------------------------------.
| yyabortlab -- YYABORT comes here.  |
`-----------------------------------*/
yyabortlab:
  yyresult = 1;
  goto yyreturn;


#if !defined yyoverflow
/*-------------------------------------------------.
| yyexhaustedlab -- memory exhaustion comes here.  |
`-------------------------------------------------*/
yyexhaustedlab:
  yyerror (YY_("memory exhausted"));
  yyresult = 2;
  goto yyreturn;
#endif


/*-------------------------------------------------------.
| yyreturn -- parsing is finished, clean up and return.  |
`-------------------------------------------------------*/
yyreturn:
  if (yychar != YYEMPTY)
    {
      /* Make sure we have latest lookahead translation.  See comments at
         user semantic actions for why this is necessary.  */
      yytoken = YYTRANSLATE (yychar);
      yydestruct ("Cleanup: discarding lookahead",
                  yytoken, &yylval);
    }
  /* Do not reclaim the symbols of the rule whose action triggered
     this YYABORT or YYACCEPT.  */
  YYPOPSTACK (yylen);
  YY_STACK_PRINT (yyss, yyssp);
  while (yyssp != yyss)
    {
      yydestruct ("Cleanup: popping",
                  YY_ACCESSING_SYMBOL (+*yyssp), yyvsp);
      YYPOPSTACK (1);
    }
#ifndef yyoverflow
  if (yyss != yyssa)
    YYSTACK_FREE (yyss);
#endif

  return yyresult;
}

#line 1078 "parser/parser.y"


