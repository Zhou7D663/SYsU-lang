#### 1. 实验过程

只通过了`parser-1`。

模板参考自[SYsU-lang/parser.y 模板](https://github.com/arcsysu/SYsU-lang/blob/latest/parser/parser.y)，思路是差不多的，但是定义了自己的数据结构。

按照 [实验二语法分析 Wiki ](https://github.com/arcsysu/SYsU-lang/wiki/实验二语法分析)中的文法编写语法规则段，通过对比clang的语法分析输出确定`kind`、`name`和`value`，通过对`parser-1`的单元测试不断调整代码，主要工作大部分在类型判断和`ImplicitCastExpr`的判断上。



#### 2. 遇到的问题及解决方法

1. `IF-Else`的移入/规约冲突

   解决办法：参考[Bison IF-ELSE 移入/规约冲突解决办法](https://stackoverflow.com/questions/12731922/reforming-the-grammar-to-remove-shift-reduce-conflict-in-if-then-else)

2. 我觉得`ImplicitCastExpr`的判断是`parser-1`中比较难的地方，除了左值到右值的转换，还有类型转换（Integer到Float，Float到Integer等等），后者除了变量赋值时需要考虑，还有函数传参时也要考虑，通过建立符号表记录变量的类型和函数的返回值、参数类型就可以解决。

   函数的符号表是容易建立的，而变量的符号表不容易，因为需要考虑全局变量和局部变量，更详细一点，如果一个变量在一个Block（花括号{}包起来的语句块）之前被定义，那么在该Block中再定义一个相同名字的变量，Block中变量的类型就会覆盖Block之前相同名字变量的类型。

   解决方法：对每一个变量使用一个栈来存储类型，每当遇到一个变量的定义时，就在该变量对应的栈中，将此定义中的类型压栈，之后每次需要使用一个变量的类型时，就按照其栈顶的类型来处理。在一个Block规约完成时，代表该Block结束了，那么就在该Block中，寻找在此Block中被定义的所有的变量（寻找kind为VarDecl的节点），然后将这些变量对应的栈都进行一次出栈操作，弹出栈顶的类型。



#### 3. 自动评测结果

![image-20230411154805803](C:\Users\Szy\AppData\Roaming\Typora\typora-user-images\image-20230411154805803.png)

