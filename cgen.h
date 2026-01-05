#include <assert.h>
#include <stdio.h>
#include "emit.h"
#include "cool-tree.h"
#include "symtab.h"

// 在cgen.h开头添加
#include <vector>
#include <map>
#include <list>
#include "stringtab.h"

// 前向声明
class Environment;

enum Basicness     {Basic, NotBasic};
#define TRUE 1
#define FALSE 0

class CgenClassTable;
typedef CgenClassTable *CgenClassTableP;

class CgenNode;
typedef CgenNode *CgenNodeP;

// CgenClassTable类

// 管理类的代码生成：

// - 生成类名表（class_nameTab）
// - 生成对象表（class_objTab）
// - 生成分发表（dispatch tables）
// - 生成原型对象（prototype objects）
// - 生成初始化方法（init methods）
// - 生成类方法（class methods）
class CgenClassTable : public SymbolTable<Symbol,CgenNode> {
private:
   List<CgenNode> *nds;
   ostream& str;
   int stringclasstag;
   int intclasstag;
   int boolclasstag;

   // 添加私有成员
   CgenNode* root_node;
   CgenNode* current_class;           // 当前正在处理的类
   std::map<Symbol, CgenNode*> class_map;  // 类名->节点映射
   std::vector<CgenNode*> class_nodes;     // 所有类节点

// The following methods emit code for
// constants and global declarations.

   void code_global_data();
   void code_global_text();
   void code_bools(int);
   void code_select_gc();
   void code_constants();

// The following creates an inheritance graph from
// a list of classes.  The graph is implemented as
// a tree of `CgenNode', and class names are placed
// in the base class symbol table.

   void install_basic_classes();
   void install_class(CgenNodeP nd);
   void install_classes(Classes cs);
   void build_inheritance_tree();
   void set_relations(CgenNodeP nd);
public:
   CgenClassTable(Classes, ostream& str);
   ~CgenClassTable();
   void code();
   CgenNodeP root();

   // PA5需要的新方法
   void code_class_nameTab();
   void code_class_objTab();
   void code_dispatchTabs();
   void code_protObjs();
   void code_class_inits();
   void code_class_methods();
   
   // 辅助方法
   CgenNode* GetClassNode(Symbol class_name);
   std::vector<CgenNode*> GetClassNodes();
   int GetClassTag(Symbol class_name);
   std::map<Symbol, int> GetClassTags();
   
   // 当前类管理
   CgenNode* GetCurrentClass() { return current_class; }
   void SetCurrentClass(CgenNode* node) { current_class = node; }
   
   // 标签管理
   int GetStringClassTag() { return stringclasstag; }
   int GetIntClassTag() { return intclasstag; }
   int GetBoolClassTag() { return boolclasstag; }
};

// CgenNode类

// 表示一个类的代码生成节点：

// - 获取类的所有方法（包括继承的）
// - 获取类的所有属性（包括继承的）
// - 生成原型对象代码
// - 生成初始化代码
// - 生成方法代码
class CgenNode : public class__class {
private: 
   CgenNodeP parentnd;                        // Parent of class
   List<CgenNode> *children;                  // Children of class
   Basicness basic_status;                    // `Basic' if class is basic
                                              // `NotBasic' otherwise
   // PA5需要的新成员
   int class_tag;                            // 类标签
   std::vector<attr_class*> m_full_attribs;  // 完整属性列表（包括继承的）
   std::vector<method_class*> m_full_methods; // 完整方法列表
   std::map<Symbol, int> m_attrib_idx_tab;   // 属性名->索引映射
   std::map<Symbol, int> m_dispatch_idx_tab; // 方法名->分发表索引
   std::map<Symbol, Symbol> m_dispatch_class_tab; // 方法名->定义类
public:
   CgenNode(Class_ c,
            Basicness bstatus,
            CgenClassTableP class_table);

   void add_child(CgenNodeP child);
   List<CgenNode> *get_children() { return children; }
   void set_parentnd(CgenNodeP p);
   CgenNodeP get_parentnd() { return parentnd; }
   int basic() { return (basic_status == Basic); }

   // PA5需要的新方法
   
   // 类标签
   int get_class_tag() { return class_tag; }
   void set_class_tag(int tag) { class_tag = tag; }
   
   // 代码生成方法
   void code_protObj(ostream& s);
   void code_init(ostream& s);
   void code_methods(CgenClassTable *ct, ostream& s);
   
   // 属性相关方法
   std::vector<attr_class*> GetFullAttribs();
   std::vector<attr_class*> GetAttribs();  // 只获取当前类的属性
   std::map<Symbol, int> GetAttribIdxTab() { return m_attrib_idx_tab; }
   int GetAttribCount() { return GetFullAttribs().size(); }
   
   // 方法相关方法
   std::vector<method_class*> GetFullMethods();
   std::vector<method_class*> GetMethods();  // 只获取当前类的方法
   std::map<Symbol, int> GetDispatchIdxTab() { return m_dispatch_idx_tab; }
   std::map<Symbol, Symbol> GetDispatchClassTab() { return m_dispatch_class_tab; }
   
   // 继承链
   std::vector<CgenNode*> GetInheritance();
   
   // 辅助方法
   Symbol get_name() { return name; }
   bool has_parent() { return parentnd != NULL; }
};

class BoolConst 
{
 private: 
  int val;
 public:
  BoolConst(int);
  void code_def(ostream&, int boolclasstag);
  void code_ref(ostream&) const;
};

// Environment类

// 用于跟踪代码生成时的环境信息：

// - **变量查找**：查找let变量、参数、属性
// - **作用域管理**：进入/退出作用域
// - **类信息**：当前类节点
class Environment {
private:
    // 变量条目结构
    struct VarEntry {
        Symbol name;           // 变量名
        int offset;            // 偏移量（相对于SP或FP）
        bool is_param;         // 是否是参数
        int scope_level;       // 作用域层级
    };
    
    // 符号表栈（支持嵌套作用域）
    std::vector<std::vector<VarEntry>> var_stack;
    
    // 参数列表（专门存储参数）
    std::vector<VarEntry> params;
    
    // 当前作用域层级
    int current_scope;
    
    // 当前类
    CgenNode* current_class;
    
    // GC障碍物计数（栈上的对象引用）
    int obstacle_count;

public:
    Environment() : current_scope(0), current_class(NULL), obstacle_count(0) {
        // 初始化最外层作用域
        var_stack.push_back(std::vector<VarEntry>());
    }
    
    // ========== 变量查找 ==========
    
    // 查找let变量（在栈上）
    int LookUpVar(Symbol sym) {
        // 从内层作用域向外层查找
        for (int i = var_stack.size() - 1; i >= 0; i--) {
            for (const auto& entry : var_stack[i]) {
                if (entry.name == sym && !entry.is_param) {
                    // 返回偏移量（相对于SP）
                    // 偏移量计算：从内向外，需要考虑作用域
                    return entry.offset;
                }
            }
        }
        return -1;  // 未找到
    }
    
    // 查找参数（在帧上）
    int LookUpParam(Symbol sym) {
        for (const auto& entry : params) {
            if (entry.name == sym) {
                // 参数偏移量：相对于FP
                // 偏移从3开始：0=FP, 1=SELF, 2=RA, 3=第一个参数
                return entry.offset;
            }
        }
        return -1;  // 未找到
    }
    
    // 查找属性（在对象上）
    int LookUpAttrib(Symbol sym) {
        if (!current_class) return -1;
        
        // 获取类的完整属性表
        std::vector<attr_class*> attribs = current_class->GetFullAttribs();
        std::map<Symbol, int> idx_tab = current_class->GetAttribIdxTab();
        
        if (idx_tab.find(sym) != idx_tab.end()) {
            return idx_tab[sym];  // 返回属性索引
        }
        return -1;  // 未找到
    }
    
    // ========== 作用域管理 ==========
    
    void EnterScope() {
        current_scope++;
        var_stack.push_back(std::vector<VarEntry>());
    }
    
    void ExitScope() {
        if (var_stack.size() > 1) {
            var_stack.pop_back();
            current_scope--;
        }
    }
    
    // ========== 变量添加 ==========
    
    // 添加let变量（在栈上）
    void AddVar(Symbol sym) {
        if (var_stack.empty()) {
            var_stack.push_back(std::vector<VarEntry>());
        }
        
        // 计算偏移量：栈从高地址向低地址增长
        // 第一个let变量在SP+1，第二个在SP+2，以此类推
        int offset = 1;  // 偏移从1开始（0是返回地址槽）
        for (const auto& scope : var_stack) {
            for (const auto& entry : scope) {
                if (!entry.is_param) {
                    offset = std::max(offset, entry.offset + 1);
                }
            }
        }
        
        VarEntry entry = {sym, offset, false, current_scope};
        var_stack.back().push_back(entry);
    }
    
    // 添加参数（在帧上）
    void AddParam(Symbol sym) {
        // 参数偏移从3开始：FP+3, FP+4, FP+5, ...
        int offset = params.size() + 3;
        VarEntry entry = {sym, offset, true, 0};
        params.push_back(entry);
    }
    
    // ========== 类管理 ==========
    
    void SetCurrentClass(CgenNode* node) {
        current_class = node;
    }
    
    CgenNode* GetCurrentClass() {
        return current_class;
    }
    
    // ========== GC管理 ==========
    
    void AddObstacle() {
        obstacle_count++;
    }
    
    void RemoveObstacle() {
        if (obstacle_count > 0) {
            obstacle_count--;
        }
    }
    
    int GetObstacleCount() {
        return obstacle_count;
    }
    
    // ========== 辅助方法 ==========
    
    // 获取参数数量
    int GetParamCount() {
        return params.size();
    }
    
    // 清空参数列表（用于新的方法）
    void ClearParams() {
        params.clear();
    }
    
    // 获取所有let变量总数
    int GetTotalVarCount() {
        int count = 0;
        for (const auto& scope : var_stack) {
            for (const auto& entry : scope) {
                if (!entry.is_param) {
                    count++;
                }
            }
        }
        return count;
    }
    
    // 调试：打印环境状态
    void Dump() {
        std::cout << "=== Environment Dump ===" << std::endl;
        std::cout << "Current class: " << (current_class ? current_class->get_name()->get_string() : "NULL") << std::endl;
        std::cout << "Current scope: " << current_scope << std::endl;
        std::cout << "Obstacle count: " << obstacle_count << std::endl;
        
        std::cout << "\nParameters (" << params.size() << "):" << std::endl;
        for (const auto& entry : params) {
            std::cout << "  " << entry.name->get_string() << " @ FP+" << entry.offset*4 << std::endl;
        }
        
        std::cout << "\nVariables by scope:" << std::endl;
        for (int i = 0; i < var_stack.size(); i++) {
            std::cout << "Scope " << i << " (" << var_stack[i].size() << " vars):" << std::endl;
            for (const auto& entry : var_stack[i]) {
                if (!entry.is_param) {
                    std::cout << "  " << entry.name->get_string() << " @ SP+" << entry.offset*4 << std::endl;
                }
            }
        }
        std::cout << "=== End Dump ===" << std::endl;
    }
};
