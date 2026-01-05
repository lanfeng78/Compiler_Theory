# Compiler_Theory
编译原理第五次作业

#### `cgen.cc` - 主要实现文件

包含所有表达式节点的`code()`方法实现，以及类表、方法表、对象初始化等代码生成逻辑。

#### `cgen.h` - 头文件

定义了`CgenClassTable`、`CgenNode`、`Environment`等关键类。

#### `cool-tree.h` - AST节点定义

定义了所有表达式节点类，需要添加辅助方法如`GetActuals()`、`GetCases()`等。

#### `cool-tree.handcode.h` - 手写实现

定义了`code()`方法的签名，需要修改为接受`Environment`参数。
