
//**************************************************************
//
// Code generator SKELETON
//
// Read the comments carefully. Make sure to
//    initialize the base class tags in
//       `CgenClassTable::CgenClassTable'
//
//    Add the label for the dispatch tables to
//       `IntEntry::code_def'
//       `StringEntry::code_def'
//       `BoolConst::code_def'
//
//    Add code to emit everyting else that is needed
//       in `CgenClassTable::code'
//
//
// The files as provided will produce code to begin the code
// segments, declare globals, and emit constants.  You must
// fill in the rest.
//
//**************************************************************

#include "cgen.h"
#include "cgen_gc.h"

extern void emit_string_constant(ostream& str, char *s);
extern int cgen_debug;

//
// Three symbols from the semantic analyzer (semant.cc) are used.
// If e : No_type, then no code is generated for e.
// Special code is generated for new SELF_TYPE.
// The name "self" also generates code different from other references.
//
//////////////////////////////////////////////////////////////////////
//
// Symbols
//
// For convenience, a large number of symbols are predefined here.
// These symbols include the primitive type and method names, as well
// as fixed names used by the runtime system.
//
//////////////////////////////////////////////////////////////////////
Symbol 
       arg,
       arg2,
       Bool,
       concat,
       cool_abort,
       copy,
       Int,
       in_int,
       in_string,
       IO,
       length,
       Main,
       main_meth,
       No_class,
       No_type,
       Object,
       out_int,
       out_string,
       prim_slot,
       self,
       SELF_TYPE,
       Str,
       str_field,
       substr,
       type_name,
       val;
//
// Initializing the predefined symbols.
//
static void initialize_constants(void)
{
  arg         = idtable.add_string("arg");
  arg2        = idtable.add_string("arg2");
  Bool        = idtable.add_string("Bool");
  concat      = idtable.add_string("concat");
  cool_abort  = idtable.add_string("abort");
  copy        = idtable.add_string("copy");
  Int         = idtable.add_string("Int");
  in_int      = idtable.add_string("in_int");
  in_string   = idtable.add_string("in_string");
  IO          = idtable.add_string("IO");
  length      = idtable.add_string("length");
  Main        = idtable.add_string("Main");
  main_meth   = idtable.add_string("main");
//   _no_class is a symbol that can't be the name of any 
//   user-defined class.
  No_class    = idtable.add_string("_no_class");
  No_type     = idtable.add_string("_no_type");
  Object      = idtable.add_string("Object");
  out_int     = idtable.add_string("out_int");
  out_string  = idtable.add_string("out_string");
  prim_slot   = idtable.add_string("_prim_slot");
  self        = idtable.add_string("self");
  SELF_TYPE   = idtable.add_string("SELF_TYPE");
  Str         = idtable.add_string("String");
  str_field   = idtable.add_string("_str_field");
  substr      = idtable.add_string("substr");
  type_name   = idtable.add_string("type_name");
  val         = idtable.add_string("_val");
}

static char *gc_init_names[] =
  { "_NoGC_Init", "_GenGC_Init", "_ScnGC_Init" };
static char *gc_collect_names[] =
  { "_NoGC_Collect", "_GenGC_Collect", "_ScnGC_Collect" };


//  BoolConst is a class that implements code generation for operations
//  on the two booleans, which are given global names here.
BoolConst falsebool(FALSE);
BoolConst truebool(TRUE);

//*********************************************************
//
// Define method for code generation
//
// This is the method called by the compiler driver
// `cgtest.cc'. cgen takes an `ostream' to which the assembly will be
// emmitted, and it passes this and the class list of the
// code generator tree to the constructor for `CgenClassTable'.
// That constructor performs all of the work of the code
// generator.
//
//*********************************************************

void program_class::cgen(ostream &os) 
{
  // spim wants comments to start with '#'
  os << "# start of generated code\n";

  initialize_constants();
  CgenClassTable *codegen_classtable = new CgenClassTable(classes,os);

  os << "\n# end of generated code\n";
}


//////////////////////////////////////////////////////////////////////////////
//
//  emit_* procedures
//
//  emit_X  writes code for operation "X" to the output stream.
//  There is an emit_X for each opcode X, as well as emit_ functions
//  for generating names according to the naming conventions (see emit.h)
//  and calls to support functions defined in the trap handler.
//
//  Register names and addresses are passed as strings.  See `emit.h'
//  for symbolic names you can use to refer to the strings.
//
//////////////////////////////////////////////////////////////////////////////

static void emit_load(char *dest_reg, int offset, char *source_reg, ostream& s)
{
  s << LW << dest_reg << " " << offset * WORD_SIZE << "(" << source_reg << ")" 
    << endl;
}

static void emit_store(char *source_reg, int offset, char *dest_reg, ostream& s)
{
  s << SW << source_reg << " " << offset * WORD_SIZE << "(" << dest_reg << ")"
      << endl;
}

static void emit_load_imm(char *dest_reg, int val, ostream& s)
{ s << LI << dest_reg << " " << val << endl; }

static void emit_load_address(char *dest_reg, char *address, ostream& s)
{ s << LA << dest_reg << " " << address << endl; }

static void emit_partial_load_address(char *dest_reg, ostream& s)
{ s << LA << dest_reg << " "; }

static void emit_load_bool(char *dest, const BoolConst& b, ostream& s)
{
  emit_partial_load_address(dest,s);
  b.code_ref(s);
  s << endl;
}

static void emit_load_string(char *dest, StringEntry *str, ostream& s)
{
  emit_partial_load_address(dest,s);
  str->code_ref(s);
  s << endl;
}

static void emit_load_int(char *dest, IntEntry *i, ostream& s)
{
  emit_partial_load_address(dest,s);
  i->code_ref(s);
  s << endl;
}

static void emit_move(char *dest_reg, char *source_reg, ostream& s)
{ s << MOVE << dest_reg << " " << source_reg << endl; }

static void emit_neg(char *dest, char *src1, ostream& s)
{ s << NEG << dest << " " << src1 << endl; }

static void emit_add(char *dest, char *src1, char *src2, ostream& s)
{ s << ADD << dest << " " << src1 << " " << src2 << endl; }

static void emit_addu(char *dest, char *src1, char *src2, ostream& s)
{ s << ADDU << dest << " " << src1 << " " << src2 << endl; }

static void emit_addiu(char *dest, char *src1, int imm, ostream& s)
{ s << ADDIU << dest << " " << src1 << " " << imm << endl; }

static void emit_div(char *dest, char *src1, char *src2, ostream& s)
{ s << DIV << dest << " " << src1 << " " << src2 << endl; }

static void emit_mul(char *dest, char *src1, char *src2, ostream& s)
{ s << MUL << dest << " " << src1 << " " << src2 << endl; }

static void emit_sub(char *dest, char *src1, char *src2, ostream& s)
{ s << SUB << dest << " " << src1 << " " << src2 << endl; }

static void emit_sll(char *dest, char *src1, int num, ostream& s)
{ s << SLL << dest << " " << src1 << " " << num << endl; }

static void emit_jalr(char *dest, ostream& s)
{ s << JALR << "\t" << dest << endl; }

static void emit_jal(char *address,ostream &s)
{ s << JAL << address << endl; }

static void emit_return(ostream& s)
{ s << RET << endl; }

static void emit_gc_assign(ostream& s)
{ s << JAL << "_GenGC_Assign" << endl; }

static void emit_disptable_ref(Symbol sym, ostream& s)
{  s << sym << DISPTAB_SUFFIX; }

static void emit_init_ref(Symbol sym, ostream& s)
{ s << sym << CLASSINIT_SUFFIX; }

static void emit_label_ref(int l, ostream &s)
{ s << "label" << l; }

static void emit_protobj_ref(Symbol sym, ostream& s)
{ s << sym << PROTOBJ_SUFFIX; }

static void emit_method_ref(Symbol classname, Symbol methodname, ostream& s)
{ s << classname << METHOD_SEP << methodname; }

static void emit_label_def(int l, ostream &s)
{
  emit_label_ref(l,s);
  s << ":" << endl;
}

static void emit_beqz(char *source, int label, ostream &s)
{
  s << BEQZ << source << " ";
  emit_label_ref(label,s);
  s << endl;
}

static void emit_beq(char *src1, char *src2, int label, ostream &s)
{
  s << BEQ << src1 << " " << src2 << " ";
  emit_label_ref(label,s);
  s << endl;
}

static void emit_bne(char *src1, char *src2, int label, ostream &s)
{
  s << BNE << src1 << " " << src2 << " ";
  emit_label_ref(label,s);
  s << endl;
}

static void emit_bleq(char *src1, char *src2, int label, ostream &s)
{
  s << BLEQ << src1 << " " << src2 << " ";
  emit_label_ref(label,s);
  s << endl;
}

static void emit_blt(char *src1, char *src2, int label, ostream &s)
{
  s << BLT << src1 << " " << src2 << " ";
  emit_label_ref(label,s);
  s << endl;
}

static void emit_blti(char *src1, int imm, int label, ostream &s)
{
  s << BLT << src1 << " " << imm << " ";
  emit_label_ref(label,s);
  s << endl;
}

static void emit_bgti(char *src1, int imm, int label, ostream &s)
{
  s << BGT << src1 << " " << imm << " ";
  emit_label_ref(label,s);
  s << endl;
}

static void emit_branch(int l, ostream& s)
{
  s << BRANCH;
  emit_label_ref(l,s);
  s << endl;
}

//
// Push a register on the stack. The stack grows towards smaller addresses.
//
static void emit_push(char *reg, ostream& str)
{
  emit_store(reg,0,SP,str);
  emit_addiu(SP,SP,-4,str);
}

//
// Fetch the integer value in an Int object.
// Emits code to fetch the integer value of the Integer object pointed
// to by register source into the register dest
//
static void emit_fetch_int(char *dest, char *source, ostream& s)
{ emit_load(dest, DEFAULT_OBJFIELDS, source, s); }

//
// Emits code to store the integer value contained in register source
// into the Integer object pointed to by dest.
//
static void emit_store_int(char *source, char *dest, ostream& s)
{ emit_store(source, DEFAULT_OBJFIELDS, dest, s); }


static void emit_test_collector(ostream &s)
{
  emit_push(ACC, s);
  emit_move(ACC, SP, s); // stack end
  emit_move(A1, ZERO, s); // allocate nothing
  s << JAL << gc_collect_names[cgen_Memmgr] << endl;
  emit_addiu(SP,SP,4,s);
  emit_load(ACC,0,SP,s);
}

static void emit_gc_check(char *source, ostream &s)
{
  if (source != (char*)A1) emit_move(A1, source, s);
  s << JAL << "_gc_check" << endl;
}


///////////////////////////////////////////////////////////////////////////////
//
// coding strings, ints, and booleans
//
// Cool has three kinds of constants: strings, ints, and booleans.
// This section defines code generation for each type.
//
// All string constants are listed in the global "stringtable" and have
// type StringEntry.  StringEntry methods are defined both for String
// constant definitions and references.
//
// All integer constants are listed in the global "inttable" and have
// type IntEntry.  IntEntry methods are defined for Int
// constant definitions and references.
//
// Since there are only two Bool values, there is no need for a table.
// The two booleans are represented by instances of the class BoolConst,
// which defines the definition and reference methods for Bools.
//
///////////////////////////////////////////////////////////////////////////////

//
// Strings
//
void StringEntry::code_ref(ostream& s)
{
  s << STRCONST_PREFIX << index;
}

//
// Emit code for a constant String.
// You should fill in the code naming the dispatch table.
//

void StringEntry::code_def(ostream& s, int stringclasstag)
{
  IntEntryP lensym = inttable.add_int(len);

  // Add -1 eye catcher
  s << WORD << "-1" << endl;

  code_ref(s);  s  << LABEL                                             // label
      << WORD << stringclasstag << endl                                 // tag
      << WORD << (DEFAULT_OBJFIELDS + STRING_SLOTS + (len+4)/4) << endl // size
      << WORD;


 /***** Add dispatch information for class String ******/

      s << endl;                                              // dispatch table
      s << WORD;  lensym->code_ref(s);  s << endl;            // string length
  emit_string_constant(s,str);                                // ascii string
  s << ALIGN;                                                 // align to word
}

//
// StrTable::code_string
// Generate a string object definition for every string constant in the 
// stringtable.
//
void StrTable::code_string_table(ostream& s, int stringclasstag)
{  
  for (List<StringEntry> *l = tbl; l; l = l->tl())
    l->hd()->code_def(s,stringclasstag);
}

//
// Ints
//
void IntEntry::code_ref(ostream &s)
{
  s << INTCONST_PREFIX << index;
}

//
// Emit code for a constant Integer.
// You should fill in the code naming the dispatch table.
//

void IntEntry::code_def(ostream &s, int intclasstag)
{
  // Add -1 eye catcher
  s << WORD << "-1" << endl;

  code_ref(s);  s << LABEL                                // label
      << WORD << intclasstag << endl                      // class tag
      << WORD << (DEFAULT_OBJFIELDS + INT_SLOTS) << endl  // object size
      << WORD; 

 /***** Add dispatch information for class Int ******/

      s << endl;                                          // dispatch table
      s << WORD << str << endl;                           // integer value
}


//
// IntTable::code_string_table
// Generate an Int object definition for every Int constant in the
// inttable.
//
void IntTable::code_string_table(ostream &s, int intclasstag)
{
  for (List<IntEntry> *l = tbl; l; l = l->tl())
    l->hd()->code_def(s,intclasstag);
}


//
// Bools
//
BoolConst::BoolConst(int i) : val(i) { assert(i == 0 || i == 1); }

void BoolConst::code_ref(ostream& s) const
{
  s << BOOLCONST_PREFIX << val;
}
  
//
// Emit code for a constant Bool.
// You should fill in the code naming the dispatch table.
//

void BoolConst::code_def(ostream& s, int boolclasstag)
{
  // Add -1 eye catcher
  s << WORD << "-1" << endl;

  code_ref(s);  s << LABEL                                  // label
      << WORD << boolclasstag << endl                       // class tag
      << WORD << (DEFAULT_OBJFIELDS + BOOL_SLOTS) << endl   // object size
      << WORD;

 /***** Add dispatch information for class Bool ******/

      s << endl;                                            // dispatch table
      s << WORD << val << endl;                             // value (0 or 1)
}

//////////////////////////////////////////////////////////////////////////////
//
//  CgenClassTable methods
//
//////////////////////////////////////////////////////////////////////////////

// 类名表（class_nameTab）
// 存储所有类的名称字符串地址：
void CgenClassTable::code_class_nameTab() {
    str << CLASSNAMETAB << LABEL;
    std::vector<CgenNode*> class_nodes = GetClassNodes();
    for (CgenNode* class_node : class_nodes) {
        Symbol class_name = class_node->name;
        StringEntry* str_entry = stringtable.lookup_string(class_name->get_string());
        str << WORD;
        str_entry->code_ref(str);
        str << endl;
    }
}

// 对象表（class_objTab）
// 每个类占两个条目：protObj地址和init方法地址：
void CgenClassTable::code_class_objTab() {
    str << CLASSOBJTAB << LABEL;
    std::vector<CgenNode*> class_nodes = GetClassNodes();
    for (CgenNode* class_node : class_nodes) {
        str << WORD;
        emit_protobj_ref(class_node->name, str);
        str << endl;
        str << WORD;
        emit_init_ref(class_node->name, str);
        str << endl;
    }
}

// 分发表（dispatch tables）
// 每个类一个分发表，包含所有方法的地址：
void CgenClassTable::code_dispatchTabs() {
    std::vector<CgenNode*> class_nodes = GetClassNodes();
    for (CgenNode* class_node : class_nodes) {
        emit_disptable_ref(class_node->name, str);
        str << LABEL;
        std::vector<method_class*> methods = class_node->GetFullMethods();
        std::map<Symbol, int> idx_tab = class_node->GetDispatchIdxTab();
        for (method_class* method : methods) {
            str << WORD;
            emit_method_ref(class_node->name, method->name, str);
            str << endl;
        }
    }
}

// 原型对象（prototype objects）
// 每个类一个原型对象，包含类标签、大小、分发表地址和所有属性的初始值：
void CgenNode::code_protObj(ostream& s) {
    std::vector<attr_class*> attribs = GetFullAttribs();
    
    s << WORD << "-1" << endl;  // GC标记
    s << get_name() << PROTOBJ_SUFFIX << LABEL;
    s << WORD << class_tag << "\t# class tag" << endl;
    s << WORD << (DEFAULT_OBJFIELDS + attribs.size()) << "\t# size" << endl;
    s << WORD << get_name() << DISPTAB_SUFFIX << endl;
    
    // 为每个属性生成初始值
    for (attr_class* attr : attribs) {
        if (attr->type_decl == Int) {
            s << WORD;
            inttable.lookup_string("0")->code_ref(s);
            s << "\t# int(0)" << endl;
        } else if (attr->type_decl == Bool) {
            s << WORD;
            falsebool.code_ref(s);
            s << "\t# bool(0)" << endl;
        } else if (attr->type_decl == Str) {
            s << WORD;
            stringtable.lookup_string("")->code_ref(s);
            s << "\t# str()" << endl;
        } else {
            s << WORD << "0\t# void" << endl;
        }
    }
}

// 初始化方法（init methods）
void CgenNode::code_init(ostream& s) {
    s << get_name() << CLASSINIT_SUFFIX << LABEL;
    
    // 1. 保存寄存器
    emit_addiu(SP, SP, -12, s);
    emit_store(FP, 3, SP, s);
    emit_store(SELF, 2, SP, s);
    emit_store(RA, 1, SP, s);
    emit_addiu(FP, SP, 4, s);
    emit_move(SELF, ACC, s);
    
    // 2. 调用父类init
    Symbol parent = get_parentnd()->name;
    if (parent != No_class) {
        emit_jal(parent->get_string() + CLASSINIT_SUFFIX, s);
    }
    
    // 3. 初始化属性
    std::vector<attr_class*> attribs = GetAttribs();
    std::map<Symbol, int> idx_tab = GetAttribIdxTab();
    for (attr_class* attr : attribs) {
        int idx = idx_tab[attr->name];
        if (attr->init->IsEmpty()) {
            // 使用默认值
            if (attr->type_decl == Str) {
                emit_load_string(ACC, stringtable.lookup_string(""), s);
            } else if (attr->type_decl == Int) {
                emit_load_int(ACC, inttable.lookup_string("0"), s);
            } else if (attr->type_decl == Bool) {
                emit_load_bool(ACC, BoolConst(0), s);
            }
        } else {
            // 计算初始表达式
            Environment env;
            env.m_class_node = this;
            attr->init->code(s, env);
        }
        emit_store(ACC, 3 + idx, SELF, s);
    }
    
    // 4. 返回self
    emit_move(ACC, SELF, s);
    
    // 5. 恢复寄存器
    emit_load(FP, 3, SP, s);
    emit_load(SELF, 2, SP, s);
    emit_load(RA, 1, SP, s);
    emit_addiu(SP, SP, 12, s);
    emit_return(s);
}

// 方法代码生成
void method_class::code(ostream& s, CgenNode* class_node) {
    emit_method_ref(class_node->name, name, s);
    s << LABEL;
    
    // 1. 保存寄存器
    emit_addiu(SP, SP, -12, s);
    emit_store(FP, 3, SP, s);
    emit_store(SELF, 2, SP, s);
    emit_store(RA, 1, SP, s);
    emit_addiu(FP, SP, 4, s);
    emit_move(SELF, ACC, s);
    
    // 2. 设置参数环境
    Environment env;
    env.m_class_node = class_node;
    for (int i = formals->first(); formals->more(i); i = formals->next(i)) {
        env.AddParam(formals->nth(i)->GetName());
    }
    
    // 3. 生成方法体代码
    expr->code(s, env);
    
    // 4. 恢复寄存器
    emit_load(FP, 3, SP, s);
    emit_load(SELF, 2, SP, s);
    emit_load(RA, 1, SP, s);
    emit_addiu(SP, SP, 12, s);
    
    // 5. 弹出参数
    emit_addiu(SP, SP, GetArgNum() * 4, s);
    
    // 6. 返回
    emit_return(s);
}


//***************************************************
//
//  Emit code to start the .data segment and to
//  declare the global names.
//
//***************************************************

void CgenClassTable::code_global_data()
{
  Symbol main    = idtable.lookup_string(MAINNAME);
  Symbol string  = idtable.lookup_string(STRINGNAME);
  Symbol integer = idtable.lookup_string(INTNAME);
  Symbol boolc   = idtable.lookup_string(BOOLNAME);

  str << "\t.data\n" << ALIGN;
  //
  // The following global names must be defined first.
  //
  str << GLOBAL << CLASSNAMETAB << endl;
  str << GLOBAL; emit_protobj_ref(main,str);    str << endl;
  str << GLOBAL; emit_protobj_ref(integer,str); str << endl;
  str << GLOBAL; emit_protobj_ref(string,str);  str << endl;
  str << GLOBAL; falsebool.code_ref(str);  str << endl;
  str << GLOBAL; truebool.code_ref(str);   str << endl;
  str << GLOBAL << INTTAG << endl;
  str << GLOBAL << BOOLTAG << endl;
  str << GLOBAL << STRINGTAG << endl;

  //
  // We also need to know the tag of the Int, String, and Bool classes
  // during code generation.
  //
  str << INTTAG << LABEL
      << WORD << intclasstag << endl;
  str << BOOLTAG << LABEL 
      << WORD << boolclasstag << endl;
  str << STRINGTAG << LABEL 
      << WORD << stringclasstag << endl;    
}


//***************************************************
//
//  Emit code to start the .text segment and to
//  declare the global names.
//
//***************************************************

void CgenClassTable::code_global_text()
{
  str << GLOBAL << HEAP_START << endl
      << HEAP_START << LABEL 
      << WORD << 0 << endl
      << "\t.text" << endl
      << GLOBAL;
  emit_init_ref(idtable.add_string("Main"), str);
  str << endl << GLOBAL;
  emit_init_ref(idtable.add_string("Int"),str);
  str << endl << GLOBAL;
  emit_init_ref(idtable.add_string("String"),str);
  str << endl << GLOBAL;
  emit_init_ref(idtable.add_string("Bool"),str);
  str << endl << GLOBAL;
  emit_method_ref(idtable.add_string("Main"), idtable.add_string("main"), str);
  str << endl;
}

void CgenClassTable::code_bools(int boolclasstag)
{
  falsebool.code_def(str,boolclasstag);
  truebool.code_def(str,boolclasstag);
}

void CgenClassTable::code_select_gc()
{
  //
  // Generate GC choice constants (pointers to GC functions)
  //
  str << GLOBAL << "_MemMgr_INITIALIZER" << endl;
  str << "_MemMgr_INITIALIZER:" << endl;
  str << WORD << gc_init_names[cgen_Memmgr] << endl;
  str << GLOBAL << "_MemMgr_COLLECTOR" << endl;
  str << "_MemMgr_COLLECTOR:" << endl;
  str << WORD << gc_collect_names[cgen_Memmgr] << endl;
  str << GLOBAL << "_MemMgr_TEST" << endl;
  str << "_MemMgr_TEST:" << endl;
  str << WORD << (cgen_Memmgr_Test == GC_TEST) << endl;
}


//********************************************************
//
// Emit code to reserve space for and initialize all of
// the constants.  Class names should have been added to
// the string table (in the supplied code, is is done
// during the construction of the inheritance graph), and
// code for emitting string constants as a side effect adds
// the string's length to the integer table.  The constants
// are emmitted by running through the stringtable and inttable
// and producing code for each entry.
//
//********************************************************

void CgenClassTable::code_constants() {
    // 添加必需的常量
    stringtable.add_string("");      // 空字符串
    inttable.add_string("0");       // 整数0
    
    // 生成所有字符串常量
    stringtable.code_string_table(str, stringclasstag);
    
    // 生成所有整数常量
    inttable.code_string_table(str, intclasstag);
    
    // 生成布尔常量（true和false）
    code_bools(boolclasstag);
}


CgenClassTable::CgenClassTable(Classes classes, ostream& s) : nds(NULL) , str(s)
{
   stringclasstag = 0 /* Change to your String class tag here */;
   intclasstag =    0 /* Change to your Int class tag here */;
   boolclasstag =   0 /* Change to your Bool class tag here */;

   enterscope();
   if (cgen_debug) cout << "Building CgenClassTable" << endl;
   install_basic_classes();
   install_classes(classes);
   build_inheritance_tree();

   code();
   exitscope();
}

void CgenClassTable::install_basic_classes()
{

// The tree package uses these globals to annotate the classes built below.
  //curr_lineno  = 0;
  Symbol filename = stringtable.add_string("<basic class>");

//
// A few special class names are installed in the lookup table but not
// the class list.  Thus, these classes exist, but are not part of the
// inheritance hierarchy.
// No_class serves as the parent of Object and the other special classes.
// SELF_TYPE is the self class; it cannot be redefined or inherited.
// prim_slot is a class known to the code generator.
//
  addid(No_class,
	new CgenNode(class_(No_class,No_class,nil_Features(),filename),
			    Basic,this));
  addid(SELF_TYPE,
	new CgenNode(class_(SELF_TYPE,No_class,nil_Features(),filename),
			    Basic,this));
  addid(prim_slot,
	new CgenNode(class_(prim_slot,No_class,nil_Features(),filename),
			    Basic,this));

// 
// The Object class has no parent class. Its methods are
//        cool_abort() : Object    aborts the program
//        type_name() : Str        returns a string representation of class name
//        copy() : SELF_TYPE       returns a copy of the object
//
// There is no need for method bodies in the basic classes---these
// are already built in to the runtime system.
//
  install_class(
   new CgenNode(
    class_(Object, 
	   No_class,
	   append_Features(
           append_Features(
           single_Features(method(cool_abort, nil_Formals(), Object, no_expr())),
           single_Features(method(type_name, nil_Formals(), Str, no_expr()))),
           single_Features(method(copy, nil_Formals(), SELF_TYPE, no_expr()))),
	   filename),
    Basic,this));

// 
// The IO class inherits from Object. Its methods are
//        out_string(Str) : SELF_TYPE          writes a string to the output
//        out_int(Int) : SELF_TYPE               "    an int    "  "     "
//        in_string() : Str                    reads a string from the input
//        in_int() : Int                         "   an int     "  "     "
//
   install_class(
    new CgenNode(
     class_(IO, 
            Object,
            append_Features(
            append_Features(
            append_Features(
            single_Features(method(out_string, single_Formals(formal(arg, Str)),
                        SELF_TYPE, no_expr())),
            single_Features(method(out_int, single_Formals(formal(arg, Int)),
                        SELF_TYPE, no_expr()))),
            single_Features(method(in_string, nil_Formals(), Str, no_expr()))),
            single_Features(method(in_int, nil_Formals(), Int, no_expr()))),
	   filename),	    
    Basic,this));

//
// The Int class has no methods and only a single attribute, the
// "val" for the integer. 
//
   install_class(
    new CgenNode(
     class_(Int, 
	    Object,
            single_Features(attr(val, prim_slot, no_expr())),
	    filename),
     Basic,this));

//
// Bool also has only the "val" slot.
//
    install_class(
     new CgenNode(
      class_(Bool, Object, single_Features(attr(val, prim_slot, no_expr())),filename),
      Basic,this));

//
// The class Str has a number of slots and operations:
//       val                                  ???
//       str_field                            the string itself
//       length() : Int                       length of the string
//       concat(arg: Str) : Str               string concatenation
//       substr(arg: Int, arg2: Int): Str     substring
//       
   install_class(
    new CgenNode(
      class_(Str, 
	     Object,
             append_Features(
             append_Features(
             append_Features(
             append_Features(
             single_Features(attr(val, Int, no_expr())),
            single_Features(attr(str_field, prim_slot, no_expr()))),
            single_Features(method(length, nil_Formals(), Int, no_expr()))),
            single_Features(method(concat, 
				   single_Formals(formal(arg, Str)),
				   Str, 
				   no_expr()))),
	    single_Features(method(substr, 
				   append_Formals(single_Formals(formal(arg, Int)), 
						  single_Formals(formal(arg2, Int))),
				   Str, 
				   no_expr()))),
	     filename),
        Basic,this));

}

// CgenClassTable::install_class
// CgenClassTable::install_classes
//
// install_classes enters a list of classes in the symbol table.
//
void CgenClassTable::install_class(CgenNodeP nd)
{
  Symbol name = nd->get_name();

  if (probe(name))
    {
      return;
    }

  // The class name is legal, so add it to the list of classes
  // and the symbol table.
  nds = new List<CgenNode>(nd,nds);
  addid(name,nd);
}

void CgenClassTable::install_classes(Classes cs)
{
  for(int i = cs->first(); cs->more(i); i = cs->next(i))
    install_class(new CgenNode(cs->nth(i),NotBasic,this));
}

//
// CgenClassTable::build_inheritance_tree
//
void CgenClassTable::build_inheritance_tree()
{
  for(List<CgenNode> *l = nds; l; l = l->tl())
      set_relations(l->hd());
}

//
// CgenClassTable::set_relations
//
// Takes a CgenNode and locates its, and its parent's, inheritance nodes
// via the class table.  Parent and child pointers are added as appropriate.
//
void CgenClassTable::set_relations(CgenNodeP nd)
{
  CgenNode *parent_node = probe(nd->get_parent());
  nd->set_parentnd(parent_node);
  parent_node->add_child(nd);
}

void CgenNode::add_child(CgenNodeP n)
{
  children = new List<CgenNode>(n,children);
}

void CgenNode::set_parentnd(CgenNodeP p)
{
  assert(parentnd == NULL);
  assert(p != NULL);
  parentnd = p;
}



void CgenClassTable::code()
{
  if (cgen_debug) cout << "coding global data" << endl;
  code_global_data();

  if (cgen_debug) cout << "choosing gc" << endl;
  code_select_gc();

  if (cgen_debug) cout << "coding constants" << endl;
  code_constants();

//                 Add your code to emit
//                   - prototype objects
//                   - class_nameTab
//                   - dispatch tables
//

  if (cgen_debug) cout << "coding global text" << endl;
  code_global_text();

//                 Add your code to emit
//                   - object initializer
//                   - the class methods
//                   - etc...

}


CgenNodeP CgenClassTable::root()
{
   return probe(Object);
}


///////////////////////////////////////////////////////////////////////
//
// CgenNode methods
//
///////////////////////////////////////////////////////////////////////

CgenNode::CgenNode(Class_ nd, Basicness bstatus, CgenClassTableP ct) :
   class__class((const class__class &) *nd),
   parentnd(NULL),
   children(NULL),
   basic_status(bstatus)
{ 
   stringtable.add_string(name->get_string());          // Add class name to string table
}


//******************************************************************
//
//   Fill in the following methods to produce code for the
//   appropriate expression.  You may add or remove parameters
//   as you wish, but if you do, remember to change the parameters
//   of the declarations in `cool-tree.h'  Sample code for
//   constant integers, strings, and booleans are provided.
//
//*****************************************************************
// 赋值表达式
void assign_class::code(ostream& s, Environment env) {
    s << "\t# Assign. First eval the expr." << endl;
    expr->code(s, env);

    s << "\t# Now find the lvalue." << endl;
    int idx;

    if ((idx = env.LookUpVar(name)) != -1) {
        s << "\t# It is a let variable." << endl;
        emit_store(ACC, idx + 1, SP, s);
        if (cgen_Memmgr == 1) {
            emit_addiu(A1, SP, 4 * (idx + 1), s);
            emit_jal("_GenGC_Assign", s);
        }
    } else if ((idx = env.LookUpParam(name)) != -1){
        s << "\t# It is a param." << endl;
        emit_store(ACC, idx + 3, FP, s);
        if (cgen_Memmgr == 1) {
            emit_addiu(A1, FP, 4 * (idx + 3), s);
            emit_jal("_GenGC_Assign", s);
        }
    } else if ((idx = env.LookUpAttrib(name)) != -1) {
        s << "\t# It is an attribute." << endl;
        emit_store(ACC, idx + 3, SELF, s);
        if (cgen_Memmgr == 1) {
            emit_addiu(A1, SELF, 4 * (idx + 3), s);
            emit_jal("_GenGC_Assign", s);
        }
    } else {
        s << "Error! assign to what?" << endl;
    }
}
// 静态分发（明确指定类型的方法调用）
void static_dispatch_class::code(ostream &s, Environment env) {
  s << "\t# Static dispatch: " << name->get_string() << endl;
    
    // 1. 计算所有参数并压栈（从右到左）
    s << "\t# Evaluate and push arguments (right to left)" << endl;
    std::vector<Expression> actuals = GetActuals();
    int arg_count = actuals.size();
    
    for (int i = arg_count - 1; i >= 0; i--) {
        actuals[i]->code(s, env);
        emit_push(ACC, s);
        env.AddObstacle();
    }
    s << endl;
    
    // 2. 计算对象表达式
    s << "\t# Evaluate object expression" << endl;
    expr->code(s, env);
    s << endl;
    
    // 3. 检查对象是否为void
    s << "\t# Check if object is void" << endl;
    int label_not_void = labelnum++;
    emit_bne(ACC, ZERO, label_not_void, s);
    emit_load_string(ACC, stringtable.lookup_string("0"), s);
    emit_load_imm(T1, 1, s);
    emit_jal("_dispatch_abort", s);
    emit_label_def(label_not_void, s);
    ++labelnum;
    s << endl;
    
    // 4. 使用指定的类型（type_name）的分发表
    s << "\t# Load dispatch table of static type: " << type_name->get_string() << endl;
    std::string disp_tab = type_name->get_string();
    disp_tab += DISPTAB_SUFFIX;
    emit_load_address(T1, disp_tab.c_str(), s);
    s << endl;
    
    // 5. 获取方法索引
    s << "\t# Get method index" << endl;
    CgenNode* class_node = codegen_classtable->GetClassNode(type_name);
    if (!class_node) {
        s << "\t# Error: Class not found" << endl;
        return;
    }
    std::map<Symbol, int> idx_tab = class_node->GetDispatchIdxTab();
    if (idx_tab.find(name) == idx_tab.end()) {
        s << "\t# Error: Method not found in dispatch table" << endl;
        return;
    }
    int idx = idx_tab[name];
    s << "\t# Method index: " << idx << endl;
    s << endl;
    
    // 6. 加载方法地址并调用
    s << "\t# Load method address and call" << endl;
    emit_load(T1, idx, T1, s);
    emit_jalr(T1, s);
    s << endl;
    
    // 7. 清理参数栈
    s << "\t# Clean up arguments from stack" << endl;
    emit_addiu(SP, SP, arg_count * 4, s);
    s << endl;
}
// 方法调用
void dispatch_class::code(ostream& s, Environment env) {
    s << "\t# Dispatch. First eval and save the params." << endl;
    std::vector<Expression> actuals = GetActuals();

    for (Expression expr : actuals) {
        expr->code(s, env);
        emit_push(ACC, s);
        env.AddObstacle();
    }

    s << "\t# eval the obj in dispatch." << endl;
    expr->code(s, env);

    s << "\t# if obj = void: abort" << endl;
    emit_bne(ACC, ZERO, labelnum, s);
    s << LA << ACC << " str_const0" << endl;
    emit_load_imm(T1, 1, s);
    emit_jal("_dispatch_abort", s);

    emit_label_def(labelnum, s);
    ++labelnum;

    // Get current class name;
    Symbol _class_name = env.m_class_node->name;
    if (expr->get_type() != SELF_TYPE) {
        _class_name = expr->get_type();
    }

    CgenNode* _class_node = codegen_classtable->GetClassNode(_class_name);
    s << "\t# Now we locate the method in the dispatch table." << endl;
    s << "\t# t1 = self.dispTab" << endl;
    emit_load(T1, 2, ACC, s);
    s << endl;

    int idx = _class_node->GetDispatchIdxTab()[name];
    s << "\t# t1 = dispTab[offset]" << endl;
    emit_load(T1, idx, T1, s);
    s << endl;

    s << "\t# jumpto " << name << endl;
    emit_jalr(T1, s);
    s << endl;
}
// 条件表达式
void cond_class::code(ostream& s, Environment env) {
    // 1. 计算条件表达式
    pred->code(s, env);
    
    // 2. 提取布尔值
    emit_fetch_int(T1, ACC, s);
    
    // 3. 生成标签
    int label_false = labelnum++;
    int label_finish = labelnum++;
    
    // 4. 条件跳转
    emit_beq(T1, ZERO, label_false, s);
    
    // 5. then分支
    then_exp->code(s, env);
    emit_branch(label_finish, s);
    
    // 6. else分支
    emit_label_def(label_false, s);
    else_exp->code(s, env);
    
    // 7. 结束标签
    emit_label_def(label_finish, s);
}
// 循环表达式
void loop_class::code(ostream& s, Environment env) {
    int start = labelnum++;
    int finish = labelnum++;
    
    // 1. 开始标签
    emit_label_def(start, s);
    
    // 2. 计算条件
    pred->code(s, env);
    emit_fetch_int(T1, ACC, s);
    
    // 3. 如果条件为假，跳转到结束
    emit_beq(T1, ZERO, finish, s);
    
    // 4. 执行循环体
    body->code(s, env);
    
    // 5. 跳回开始
    emit_branch(start, s);
    
    // 6. 结束标签
    emit_label_def(finish, s);
    
    // 7. 循环表达式返回void
    emit_move(ACC, ZERO, s);
}
// case表达式（类型判断）
void typcase_class::code(ostream &s, Environment env) {
  s << "\t# Case expression" << endl;
    
    // 1. 计算表达式
    s << "\t# Evaluate case expression" << endl;
    expr->code(s, env);
    s << endl;
    
    // 2. 检查对象是否为void
    s << "\t# Check if object is void" << endl;
    int label_not_void = labelnum++;
    emit_bne(ACC, ZERO, label_not_void, s);
    emit_load_string(ACC, stringtable.lookup_string("0"), s);
    emit_load_imm(T1, 1, s);
    emit_jal("_case_abort2", s);
    emit_label_def(label_not_void, s);
    ++labelnum;
    s << endl;
    
    // 3. 保存表达式值
    s << "\t# Save expression value" << endl;
    emit_push(ACC, s);
    env.AddObstacle();
    s << endl;
    
    // 4. 加载对象的类标签
    s << "\t# Load object class tag" << endl;
    emit_load(T1, 0, ACC, s);  // T1 = object.tag
    s << endl;
    
    // 5. 处理每个分支
    std::vector<branch_class*> cases = GetCases();
    int case_count = cases.size();
    std::vector<int> case_labels(case_count);
    
    s << "\t# Process " << case_count << " case branches" << endl;
    
    // 生成分支检查
    for (int i = 0; i < case_count; i++) {
        branch_class* branch = cases[i];
        Symbol type_name = branch->type_decl;
        
        // 为每个分支生成标签
        case_labels[i] = labelnum++;
        int next_label = labelnum++;
        
        s << "\t# Branch " << i << ": type " << type_name->get_string() << endl;
        
        // 如果类型是SELF_TYPE，使用当前对象的类型
        if (type_name == SELF_TYPE) {
            // 对于SELF_TYPE，检查是否是当前类或其子类
            // 这里简化处理：直接匹配当前类标签
            // 实际实现需要更复杂的子类检查
            s << "\t# Self type - check current class" << endl;
            emit_load(T2, 0, SELF, s);  // T2 = self.tag
            emit_beq(T1, T2, case_labels[i], s);
        } else {
            // 获取类型对应的类标签
            int type_tag = codegen_classtable->GetClassTag(type_name);
            s << "\t# Type tag: " << type_tag << endl;
            
            // 检查标签是否匹配
            emit_load_imm(T2, type_tag, s);
            emit_beq(T1, T2, case_labels[i], s);
            
            // 还需要检查是否是子类（简化实现，只检查精确匹配）
            // 完整实现需要遍历继承链
        }
    }
    
    // 6. 如果没有匹配的分支，调用_case_abort
    s << "\t# No branch matched - abort" << endl;
    emit_load_string(ACC, stringtable.lookup_string("0"), s);
    emit_load_imm(T1, 1, s);
    emit_jal("_case_abort", s);
    s << endl;
    
    // 7. 生成各个分支的代码
    for (int i = 0; i < case_count; i++) {
        branch_class* branch = cases[i];
        
        // 分支标签
        emit_label_def(case_labels[i], s);
        s << "\t# Branch " << i << " body" << endl;
        
        // 进入新作用域
        env.EnterScope();
        
        // 恢复表达式值
        emit_addiu(SP, SP, 4, s);
        emit_load(ACC, 0, SP, s);
        env.RemoveObstacle();
        
        // 添加分支变量到环境
        env.AddVar(branch->name);
        
        // 生成分支表达式代码
        branch->expr->code(s, env);
        
        // 退出作用域
        env.ExitScope();
        
        // 跳转到case结束
        int case_end_label = labelnum++;
        emit_branch(case_end_label, s);
        emit_label_def(case_end_label, s);
    }
    
    s << "\t# Case expression complete" << endl;
}
// 块表达式
void block_class::code(ostream& s, Environment env) {
    // 依次执行所有表达式，最后一个表达式的值作为块的值
    for (int i = body->first(); body->more(i); i = body->next(i)) {
        body->nth(i)->code(s, env);
    }
}
// let表达式
void let_class::code(ostream& s, Environment env) {
    // 1. 计算初始值
    init->code(s, env);
    
    // 2. 如果没有初始值，使用默认值
    if (init->IsEmpty()) {
        if (type_decl == Str) {
            emit_load_string(ACC, stringtable.lookup_string(""), s);
        } else if (type_decl == Int) {
            emit_load_int(ACC, inttable.lookup_string("0"), s);
        } else if (type_decl == Bool) {
            emit_load_bool(ACC, BoolConst(0), s);
        }
    }
    
    // 3. 将初始值压栈
    emit_push(ACC, s);
    
    // 4. 进入新作用域并添加变量
    env.EnterScope();
    env.AddVar(identifier);
    
    // 5. 计算body表达式
    body->code(s, env);
    
    // 6. 退出作用域（弹出变量）
    emit_addiu(SP, SP, 4, s);
}
// 加法
void plus_class::code(ostream& s, Environment env) {
    s << "\t# Int operation : Add" << endl;
    s << "\t# First eval e1 and push." << endl;
    e1->code(s, env);
    emit_push(ACC, s);
    env.AddObstacle();
    s << endl;

    s << "\t# Then eval e2 and make a copy for result." << endl;
    e2->code(s, env);
    emit_jal("Object.copy", s);
    s << endl;

    s << "\t# Let's pop e1 to t1, move e2 to t2" << endl;
    emit_addiu(SP, SP, 4, s);
    emit_load(T1, 0, SP, s);
    emit_move(T2, ACC, s);
    s << endl;

    s << "\t# Extract the int inside the object." << endl;
    emit_load(T1, 3, T1, s);
    emit_load(T2, 3, T2, s);
    s << endl;

    s << "\t# Modify the int inside t2." << endl;
    emit_add(T3, T1, T2, s);
    emit_store(T3, 3, ACC, s);
    s << endl;
}
// 减法
void sub_class::code(ostream &s, Environment env) {
  s << "\t# Int operation : Subtract" << endl;
    s << "\t# First eval e1 and push." << endl;
    e1->code(s, env);
    emit_push(ACC, s);
    env.AddObstacle();
    s << endl;

    s << "\t# Then eval e2 and make a copy for result." << endl;
    e2->code(s, env);
    emit_push(ACC, s);
    env.AddObstacle();
    
    emit_load_int(ACC, inttable.lookup_string("0"), s);
    emit_jal("Object.copy", s);
    s << endl;

    s << "\t# Let's pop e2 to t2, then e1 to t1" << endl;
    emit_addiu(SP, SP, 4, s);
    emit_load(T2, 0, SP, s);
    emit_addiu(SP, SP, 4, s);
    emit_load(T1, 0, SP, s);
    s << endl;

    s << "\t# Extract the int inside the object." << endl;
    emit_load(T1, 3, T1, s);
    emit_load(T2, 3, T2, s);
    s << endl;

    s << "\t# Modify the int inside new object." << endl;
    emit_sub(T3, T1, T2, s);
    emit_store(T3, 3, ACC, s);
    s << endl;
}
// 乘法
void mul_class::code(ostream &s, Environment env) {
  s << "\t# Int operation : Multiply" << endl;
    s << "\t# First eval e1 and push." << endl;
    e1->code(s, env);
    emit_push(ACC, s);
    env.AddObstacle();
    s << endl;

    s << "\t# Then eval e2 and make a copy for result." << endl;
    e2->code(s, env);
    emit_push(ACC, s);
    env.AddObstacle();
    
    emit_load_int(ACC, inttable.lookup_string("0"), s);
    emit_jal("Object.copy", s);
    s << endl;

    s << "\t# Let's pop e2 to t2, then e1 to t1" << endl;
    emit_addiu(SP, SP, 4, s);
    emit_load(T2, 0, SP, s);
    emit_addiu(SP, SP, 4, s);
    emit_load(T1, 0, SP, s);
    s << endl;

    s << "\t# Extract the int inside the object." << endl;
    emit_load(T1, 3, T1, s);
    emit_load(T2, 3, T2, s);
    s << endl;

    s << "\t# Modify the int inside new object." << endl;
    emit_mul(T3, T1, T2, s);
    emit_store(T3, 3, ACC, s);
    s << endl;
}
// 除法
void divide_class::code(ostream &s, Environment env) {
  s << "\t# Int operation : Divide" << endl;
    s << "\t# First eval e1 and push." << endl;
    e1->code(s, env);
    emit_push(ACC, s);
    env.AddObstacle();
    s << endl;

    s << "\t# Then eval e2 and make a copy for result." << endl;
    e2->code(s, env);
    emit_push(ACC, s);
    env.AddObstacle();
    
    emit_load_int(ACC, inttable.lookup_string("0"), s);
    emit_jal("Object.copy", s);
    s << endl;

    s << "\t# Let's pop e2 to t2, then e1 to t1" << endl;
    emit_addiu(SP, SP, 4, s);
    emit_load(T2, 0, SP, s);
    emit_addiu(SP, SP, 4, s);
    emit_load(T1, 0, SP, s);
    s << endl;

    s << "\t# Extract the int inside the object." << endl;
    emit_load(T1, 3, T1, s);
    emit_load(T2, 3, T2, s);
    s << endl;

    s << "\t# Check for division by zero" << endl;
    int label_not_zero = labelnum++;
    emit_bne(T2, ZERO, label_not_zero, s);
    emit_load_string(ACC, stringtable.lookup_string("0"), s);
    emit_load_imm(T1, 1, s);
    emit_jal("_div_by_zero_abort", s);
    emit_label_def(label_not_zero, s);
    s << endl;

    s << "\t# Modify the int inside new object." << endl;
    emit_div(T3, T1, T2, s);
    emit_store(T3, 3, ACC, s);
    s << endl;
}
// 取负
void neg_class::code(ostream &s, Environment env) {
  s << "\t# Int operation : Negate" << endl;
    s << "\t# First eval e1" << endl;
    e1->code(s, env);
    s << endl;

    s << "\t# Make a copy for result." << endl;
    emit_jal("Object.copy", s);
    s << endl;

    s << "\t# Extract the int inside the object." << endl;
    emit_load(T1, 3, ACC, s);
    s << endl;

    s << "\t# Modify the int inside new object." << endl;
    emit_neg(T2, T1, s);
    emit_store(T2, 3, ACC, s);
    s << endl;
}
// 小于比较
void lt_class::code(ostream &s, Environment env) {
  s << "\t# Int comparison : Less than" << endl;
    s << "\t# First eval e1 and push." << endl;
    e1->code(s, env);
    emit_push(ACC, s);
    env.AddObstacle();
    s << endl;

    s << "\t# Then eval e2" << endl;
    e2->code(s, env);
    s << endl;

    s << "\t# Let's pop e1 to t1, move e2 to t2" << endl;
    emit_addiu(SP, SP, 4, s);
    emit_load(T1, 0, SP, s);
    emit_move(T2, ACC, s);
    s << endl;

    s << "\t# Extract the int inside the object." << endl;
    emit_load(T1, 3, T1, s);
    emit_load(T2, 3, T2, s);
    s << endl;

    s << "\t# Compare and set boolean result." << endl;
    int label_true = labelnum++;
    int label_finish = labelnum++;
    
    emit_blt(T1, T2, label_true, s);
    
    // false branch
    emit_load_bool(ACC, BoolConst(0), s);
    emit_branch(label_finish, s);
    
    // true branch
    emit_label_def(label_true, s);
    emit_load_bool(ACC, BoolConst(1), s);
    
    // finish label
    emit_label_def(label_finish, s);
    s << endl;
}
// 相等比较
void eq_class::code(ostream &s, Environment env) {
  s << "\t# Equality comparison" << endl;
    s << "\t# First eval e1 and push." << endl;
    e1->code(s, env);
    emit_push(ACC, s);
    env.AddObstacle();
    s << endl;

    s << "\t# Then eval e2" << endl;
    e2->code(s, env);
    s << endl;

    s << "\t# Let's pop e1 to t1, move e2 to t2" << endl;
    emit_addiu(SP, SP, 4, s);
    emit_load(T1, 0, SP, s);
    emit_move(T2, ACC, s);
    s << endl;

    s << "\t# Compare for equality." << endl;
    int label_true = labelnum++;
    int label_finish = labelnum++;
    
    // 对于基本类型（Int, Bool, String），比较值
    if (e1->get_type() == Int || e1->get_type() == Bool || e1->get_type() == Str) {
        // 提取值进行比较
        emit_load(T1, 3, T1, s);
        emit_load(T2, 3, T2, s);
        emit_beq(T1, T2, label_true, s);
    } else {
        // 对于对象，比较地址（引用相等）
        emit_beq(T1, T2, label_true, s);
    }
    
    // false branch
    emit_load_bool(ACC, BoolConst(0), s);
    emit_branch(label_finish, s);
    
    // true branch
    emit_label_def(label_true, s);
    emit_load_bool(ACC, BoolConst(1), s);
    
    // finish label
    emit_label_def(label_finish, s);
    s << endl;
}
// 小于等于比较
void leq_class::code(ostream &s, Environment env) {
  s << "\t# Int comparison : Less than or equal" << endl;
    s << "\t# First eval e1 and push." << endl;
    e1->code(s, env);
    emit_push(ACC, s);
    env.AddObstacle();
    s << endl;

    s << "\t# Then eval e2" << endl;
    e2->code(s, env);
    s << endl;

    s << "\t# Let's pop e1 to t1, move e2 to t2" << endl;
    emit_addiu(SP, SP, 4, s);
    emit_load(T1, 0, SP, s);
    emit_move(T2, ACC, s);
    s << endl;

    s << "\t# Extract the int inside the object." << endl;
    emit_load(T1, 3, T1, s);
    emit_load(T2, 3, T2, s);
    s << endl;

    s << "\t# Compare and set boolean result." << endl;
    int label_true = labelnum++;
    int label_finish = labelnum++;
    
    emit_bleq(T1, T2, label_true, s);
    
    // false branch
    emit_load_bool(ACC, BoolConst(0), s);
    emit_branch(label_finish, s);
    
    // true branch
    emit_label_def(label_true, s);
    emit_load_bool(ACC, BoolConst(1), s);
    
    // finish label
    emit_label_def(label_finish, s);
    s << endl;
}
// 取补（布尔非）
void comp_class::code(ostream &s, Environment env) {
  s << "\t# Boolean complement" << endl;
    s << "\t# First eval e1" << endl;
    e1->code(s, env);
    s << endl;

    s << "\t# Extract the boolean inside the object." << endl;
    emit_load(T1, 3, ACC, s);
    s << endl;

    s << "\t# Create result boolean object." << endl;
    // 先假设结果为true
    emit_load_bool(ACC, BoolConst(1), s);
    
    s << "\t# If original is true, set result to false" << endl;
    int label_finish = labelnum++;
    emit_beq(T1, ZERO, label_finish, s);
    
    // 原值为true，设为false
    emit_load_bool(ACC, BoolConst(0), s);
    
    // finish label
    emit_label_def(label_finish, s);
    s << endl;
}
// 基本类型常量
// 整数常量
void int_const_class::code(ostream& s, Environment env) {
    // 从整数表中加载常量地址到ACC
    emit_load_int(ACC, inttable.lookup_string(token->get_string()), s);
}
// 字符串常量
void string_const_class::code(ostream& s, Environment env) {
    // 从字符串表中加载常量地址到ACC
    emit_load_string(ACC, stringtable.lookup_string(token->get_string()), s);
}
// 布尔常量
void bool_const_class::code(ostream& s, Environment env) {
    // 加载布尔常量地址到ACC
    emit_load_bool(ACC, BoolConst(val), s);
}
// new表达式
void new__class::code(ostream& s, Environment env) {
    if (type_name == SELF_TYPE) {
        // SELF_TYPE需要运行时确定
        emit_load_address(T1, "class_objTab", s);
        emit_load(T2, 0, SELF, s);  // T2 = self的类标签
        emit_sll(T2, T2, 3, s);      // 乘以8（每个类在objTab中占2个字）
        emit_addu(T1, T1, T2, s);
        emit_push(T1, s);
        emit_load(ACC, 0, T1, s);    // 加载protObj
        emit_jal("Object.copy", s);
        emit_load(T1, 1, SP, s);
        emit_addiu(SP, SP, 4, s);
        emit_load(T1, 1, T1, s);     // 加载init地址
        emit_jalr(T1, s);
    } else {
        // 静态类型，直接加载protObj
        std::string protobj = type_name->get_string() + PROTOBJ_SUFFIX;
        emit_load_address(ACC, protobj.c_str(), s);
        emit_jal("Object.copy", s);
        std::string init = type_name->get_string() + CLASSINIT_SUFFIX;
        emit_jal(init.c_str(), s);
    }
}
// isvoid表达式（检查对象是否为void）
void isvoid_class::code(ostream &s, Environment env) {
  s << "\t# Isvoid expression" << endl;
    
    // 1. 计算表达式
    s << "\t# Evaluate expression" << endl;
    e1->code(s, env);
    s << endl;
    
    // 2. 检查是否为void（ACC == 0）
    s << "\t# Check if result is void (ACC == 0)" << endl;
    int label_not_void = labelnum++;
    int label_finish = labelnum++;
    
    emit_bne(ACC, ZERO, label_not_void, s);
    
    // 是void：返回true
    s << "\t# Is void - return true" << endl;
    emit_load_bool(ACC, BoolConst(1), s);
    emit_branch(label_finish, s);
    
    // 不是void：返回false
    emit_label_def(label_not_void, s);
    s << "\t# Not void - return false" << endl;
    emit_load_bool(ACC, BoolConst(0), s);
    
    // 结束标签
    emit_label_def(label_finish, s);
    s << endl;
}
// 空表达式（没有表达式的占位符）
void no_expr_class::code(ostream &s, Environment env) {
  s << "\t# No expression" << endl;
    
    // 空表达式通常出现在没有显式初始化值的地方
    // 对于不同类型的默认值处理在let表达式中处理
    // 这里只需要确保ACC中有有效的对象引用
    
    // 根据get_type()返回适当的默认值
    if (get_type() == Int) {
        s << "\t# Default Int value" << endl;
        emit_load_int(ACC, inttable.lookup_string("0"), s);
    } else if (get_type() == Bool) {
        s << "\t# Default Bool value" << endl;
        emit_load_bool(ACC, BoolConst(0), s);
    } else if (get_type() == Str) {
        s << "\t# Default String value" << endl;
        emit_load_string(ACC, stringtable.lookup_string(""), s);
    } else {
        s << "\t# Default object (void)" << endl;
        emit_move(ACC, ZERO, s);  // void对象
    }
    s << endl;
}
// 对象引用
void object_class::code(ostream& s, Environment env) {
    int idx;
    
    // 1. 查找let变量
    if ((idx = env.LookUpVar(name)) != -1) {
        emit_load(ACC, idx + 1, SP, s);  // 从栈中加载
    }
    // 2. 查找参数
    else if ((idx = env.LookUpParam(name)) != -1) {
        emit_load(ACC, idx + 3, FP, s);  // 从帧指针加载
    }
    // 3. 查找属性
    else if ((idx = env.LookUpAttrib(name)) != -1) {
        emit_load(ACC, idx + 3, SELF, s); // 从self对象加载
    }
    // 4. self引用
    else if (name == self) {
        emit_move(ACC, SELF, s);
    }
}


