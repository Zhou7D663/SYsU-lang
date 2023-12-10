#### 1. 实验过程

在模板的基础上进行代码编写。

1. 先从整个Module入手，Module中只有两个部分，全局变量的声明、初始化和函数的声明、实现；

2. 对于函数的实现，只需要处理CompoundStmt就可以了，而这又相当于按顺序处理一个个Stmt；

3. Stmt有很多种类型，分别处理：

   - 局部变量定义DeclStmt
   - 二元赋值语句
   - IfStmt
   - WhileStmt
   - 函数调用CallExpr
   - Return语句
   - ……

   IfStmt和WhileStmt又可以调用CompoundStmt或者直接处理Stmt；

4. 实现了一个函数专门用于获得表达式Exp的值，里面需要处理：

   - 非赋值的二元运算符
   - 整数字面量IntegerLiteral
   - 字符串字面量StringLiteral
   - 浮点数字面量FloatLiteral
   - 隐式类型转换
   - 函数调用CallExpr
   - 一元运算符
   - ……

   该函数又可以递归调用自己，几乎所有需要用到表达式的值的地方，都可以调用该函数。



#### 2. 遇到的困难及解决方法

1. 对于形如