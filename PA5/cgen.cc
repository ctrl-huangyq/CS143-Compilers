
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
#include <set>

using std::vector;
using std::string;
using std::pair;
using std::stringstream;
using std::make_pair;
using std::array;
using std::tuple;
using std::get;
using std::set;

extern void emit_string_constant(ostream &str, char *s);

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
static void initialize_constants(void) {
    arg = idtable.add_string("arg");
    arg2 = idtable.add_string("arg2");
    Bool = idtable.add_string("Bool");
    concat = idtable.add_string("concat");
    cool_abort = idtable.add_string("abort");
    copy = idtable.add_string("copy");
    Int = idtable.add_string("Int");
    in_int = idtable.add_string("in_int");
    in_string = idtable.add_string("in_string");
    IO = idtable.add_string("IO");
    length = idtable.add_string("length");
    Main = idtable.add_string("Main");
    main_meth = idtable.add_string("main");
//   _no_class is a symbol that can't be the name of any 
//   user-defined class.
    No_class = idtable.add_string("_no_class");
    No_type = idtable.add_string("_no_type");
    Object = idtable.add_string("Object");
    out_int = idtable.add_string("out_int");
    out_string = idtable.add_string("out_string");
    prim_slot = idtable.add_string("_prim_slot");
    self = idtable.add_string("self");
    SELF_TYPE = idtable.add_string("SELF_TYPE");
    Str = idtable.add_string("String");
    str_field = idtable.add_string("_str_field");
    substr = idtable.add_string("substr");
    type_name = idtable.add_string("type_name");
    val = idtable.add_string("_val");
}

static char *gc_init_names[] =
        {"_NoGC_Init", "_GenGC_Init", "_ScnGC_Init"};
static char *gc_collect_names[] =
        {"_NoGC_Collect", "_GenGC_Collect", "_ScnGC_Collect"};


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
// emitted, and it passes this and the class list of the
// code generator tree to the constructor for `CgenClassTable'.
// That constructor performs all the work of the code
// generator.
//
//*********************************************************

void program_class::cgen(ostream &os) {
    // spim wants comments to start with '#'
    os << "# start of generated code\n";

    initialize_constants();
    [[maybe_unused]] auto *codegen_classtable = new CgenClassTable(classes, os);

    os << "# end of generated code\n";
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

static void emit_load(char *dest_reg, int offset, char *source_reg, ostream &s) {
    s << LW << dest_reg << " " << offset * WORD_SIZE << "(" << source_reg << ")"
      << endl;
}

static void emit_store(char *source_reg, int offset, char *dest_reg, ostream &s) {
    s << SW << source_reg << " " << offset * WORD_SIZE << "(" << dest_reg << ")"
      << endl;
}

static void emit_load_imm(char *dest_reg, int val, ostream &s) { s << LI << dest_reg << " " << val << endl; }

static void emit_load_address(char *dest_reg, char *address, ostream &s) {
    s << LA << dest_reg << " " << address << endl;
}

static void emit_partial_load_address(char *dest_reg, ostream &s) { s << LA << dest_reg << " "; }

static void emit_load_bool(char *dest, const BoolConst &b, ostream &s) {
    emit_partial_load_address(dest, s);
    b.code_ref(s);
    s << endl;
}

static void emit_load_string(char *dest, StringEntry *str, ostream &s) {
    emit_partial_load_address(dest, s);
    str->code_ref(s);
    s << endl;
}

static void emit_load_int(char *dest, IntEntry *i, ostream &s) {
    emit_partial_load_address(dest, s);
    i->code_ref(s);
    s << endl;
}

static void emit_move(char *dest_reg, char *source_reg, ostream &s) {
    s << MOVE << dest_reg << " " << source_reg << endl;
}

static void emit_neg(char *dest, char *src1, ostream &s) { s << NEG << dest << " " << src1 << endl; }

static void emit_add(char *dest, char *src1, char *src2, ostream &s) {
    s << ADD << dest << " " << src1 << " " << src2 << endl;
}

static void emit_addu(char *dest, char *src1, char *src2, ostream &s) {
    s << ADDU << dest << " " << src1 << " " << src2 << endl;
}

static void emit_addiu(char *dest, char *src1, int imm, ostream &s) {
    s << ADDIU << dest << " " << src1 << " " << imm << endl;
}

static void emit_div(char *dest, char *src1, char *src2, ostream &s) {
    s << DIV << dest << " " << src1 << " " << src2 << endl;
}

static void emit_mul(char *dest, char *src1, char *src2, ostream &s) {
    s << MUL << dest << " " << src1 << " " << src2 << endl;
}

static void emit_sub(char *dest, char *src1, char *src2, ostream &s) {
    s << SUB << dest << " " << src1 << " " << src2 << endl;
}

static void emit_sll(char *dest, char *src1, int num, ostream &s) {
    s << SLL << dest << " " << src1 << " " << num << endl;
}

static void emit_jalr(char *dest, ostream &s) { s << JALR << "\t" << dest << endl; }

static void emit_jal(char *address, ostream &s) { s << JAL << address << endl; }

static void emit_return(ostream &s) { s << RET << endl; }

static void emit_gc_assign(ostream &s) { s << JAL << "_GenGC_Assign" << endl; }

static void emit_disptable_ref(Symbol sym, ostream &s) { s << sym << DISPTAB_SUFFIX; }

static void emit_init_ref(Symbol sym, ostream &s) { s << sym << CLASSINIT_SUFFIX; }

static void emit_label_ref(int l, ostream &s) { s << "label" << l; }

static void emit_protobj_ref(Symbol sym, ostream &s) { s << sym << PROTOBJ_SUFFIX; }

static void emit_method_ref(Symbol classname, Symbol methodname, ostream &s) {
    s << classname << METHOD_SEP << methodname;
}

static void emit_label_def(int l, ostream &s) {
    emit_label_ref(l, s);
    s << ":" << endl;
}

static void emit_beqz(char *source, int label, ostream &s) {
    s << BEQZ << source << " ";
    emit_label_ref(label, s);
    s << endl;
}

static void emit_beq(char *src1, char *src2, int label, ostream &s) {
    s << BEQ << src1 << " " << src2 << " ";
    emit_label_ref(label, s);
    s << endl;
}

static void emit_bne(char *src1, char *src2, int label, ostream &s) {
    s << BNE << src1 << " " << src2 << " ";
    emit_label_ref(label, s);
    s << endl;
}

static void emit_bleq(char *src1, char *src2, int label, ostream &s) {
    s << BLEQ << src1 << " " << src2 << " ";
    emit_label_ref(label, s);
    s << endl;
}

static void emit_blt(char *src1, char *src2, int label, ostream &s) {
    s << BLT << src1 << " " << src2 << " ";
    emit_label_ref(label, s);
    s << endl;
}

static void emit_blti(char *src1, int imm, int label, ostream &s) {
    s << BLT << src1 << " " << imm << " ";
    emit_label_ref(label, s);
    s << endl;
}

static void emit_bgti(char *src1, int imm, int label, ostream &s) {
    s << BGT << src1 << " " << imm << " ";
    emit_label_ref(label, s);
    s << endl;
}

static void emit_branch(int l, ostream &s) {
    s << BRANCH;
    emit_label_ref(l, s);
    s << endl;
}

//
// Push a register on the stack. The stack grows towards smaller addresses.
//
static void emit_push(char *reg, ostream &str) {
    emit_store(reg, 0, SP, str);
    emit_addiu(SP, SP, -4, str);
}

static void emit_pop(char *reg, ostream &str) {
    emit_load(reg, 1, SP, str);
    emit_addiu(SP, SP, 4, str);
}

static void emit_pop(ostream &str) {
    emit_addiu(SP, SP, 4, str);
}

//
// Fetch the integer value in an Int object.
// Emits code to fetch the integer value of the Integer object pointed
// to by register source into the register dest
//
static void emit_fetch_int(char *dest, char *source, ostream &s) { emit_load(dest, DEFAULT_OBJFIELDS, source, s); }

static void emit_fetch_bool(char *dest, char *source, ostream &s) { emit_fetch_int(dest, source, s); }

//
// Emits code to store the integer value contained in register source
// into the Integer object pointed to by dest.
//
static void emit_store_int(char *source, char *dest, ostream &s) { emit_store(source, DEFAULT_OBJFIELDS, dest, s); }


static void emit_test_collector(ostream &s) {
    emit_push(ACC, s);
    emit_move(ACC, SP, s); // stack end
    emit_move(A1, ZERO, s); // allocate nothing
    s << JAL << gc_collect_names[cgen_Memmgr] << endl;
    emit_addiu(SP, SP, 4, s);
    emit_load(ACC, 0, SP, s);
}

static void emit_gc_check(char *source, ostream &s) {
    if (source != (char *) A1) emit_move(A1, source, s);
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
void StringEntry::code_ref(ostream &s) {
    s << STRCONST_PREFIX << index;
}

//
// Emit code for a constant String.
// You should fill in the code naming the dispatch table.
//

void StringEntry::code_def(ostream &s, int stringclasstag) {
    IntEntryP lensym = inttable.add_int(len);

    // Add -1 eye-catcher
    s << WORD << "-1" << endl;

    code_ref(s);
    s << LABEL                                             // label
      << WORD << stringclasstag << endl                                 // tag
      << WORD << (DEFAULT_OBJFIELDS + STRING_SLOTS + (len + 4) / 4) << endl // size
      << WORD;


    /***** Add dispatch information for class String ******/

    s << "String" << DISPTAB_SUFFIX << endl;                                              // dispatch table
    s << WORD;
    lensym->code_ref(s);
    s << endl;            // string length
    emit_string_constant(s, str);                                // ascii string
    s << ALIGN;                                                 // align to word
}

//
// StrTable::code_string
// Generate a string object definition for every string constant in the 
// stringtable.
//
void StrTable::code_string_table(ostream &s, int stringclasstag) {
    for (List<StringEntry> *l = tbl; l; l = l->tl())
        l->hd()->code_def(s, stringclasstag);
}

//
// Ints
//
void IntEntry::code_ref(ostream &s) {
    s << INTCONST_PREFIX << index;
}

//
// Emit code for a constant Integer.
// You should fill in the code naming the dispatch table.
//

void IntEntry::code_def(ostream &s, int intclasstag) {
    // Add -1 eye-catcher
    s << WORD << "-1" << endl;

    code_ref(s);
    s << LABEL                                // label
      << WORD << intclasstag << endl                      // class tag
      << WORD << (DEFAULT_OBJFIELDS + INT_SLOTS) << endl  // object size
      << WORD;

    /***** Add dispatch information for class Int ******/

    s << "Int" << DISPTAB_SUFFIX << endl;                                          // dispatch table
    s << WORD << str << endl;                           // integer value
}


//
// IntTable::code_string_table
// Generate an Int object definition for every Int constant in the
// inttable.
//
void IntTable::code_string_table(ostream &s, int intclasstag) {
    for (List<IntEntry> *l = tbl; l; l = l->tl())
        l->hd()->code_def(s, intclasstag);
}


//
// Bools
//
BoolConst::BoolConst(int i) : val(i) { assert(i == 0 || i == 1); }

void BoolConst::code_ref(ostream &s) const {
    s << BOOLCONST_PREFIX << val;
}

//
// Emit code for a constant Bool.
// You should fill in the code naming the dispatch table.
//

void BoolConst::code_def(ostream &s, int boolclasstag) {
    // Add -1 eye-catcher
    s << WORD << "-1" << endl;

    code_ref(s);
    s << LABEL                                  // label
      << WORD << boolclasstag << endl                       // class tag
      << WORD << (DEFAULT_OBJFIELDS + BOOL_SLOTS) << endl   // object size
      << WORD;

    /***** Add dispatch information for class Bool ******/

    s << "Bool" << DISPTAB_SUFFIX << endl;                                            // dispatch table
    s << WORD << val << endl;                             // value (0 or 1)
}

//////////////////////////////////////////////////////////////////////////////
//
//  CgenClassTable methods
//
//////////////////////////////////////////////////////////////////////////////

//***************************************************
//
//  Emit code to start the .data segment and to
//  declare the global names.
//
//***************************************************

void CgenClassTable::code_global_data() {
    Symbol main = idtable.lookup_string(MAINNAME);
    Symbol string = idtable.lookup_string(STRINGNAME);
    Symbol integer = idtable.lookup_string(INTNAME);
    Symbol boolc = idtable.lookup_string(BOOLNAME);

    str << "\t.data\n" << ALIGN;
    //
    // The following global names must be defined first.
    //
    str << GLOBAL << CLASSNAMETAB << endl;
    str << GLOBAL;
    emit_protobj_ref(main, str);
    str << endl;
    str << GLOBAL;
    emit_protobj_ref(integer, str);
    str << endl;
    str << GLOBAL;
    emit_protobj_ref(string, str);
    str << endl;
    str << GLOBAL;
    falsebool.code_ref(str);
    str << endl;
    str << GLOBAL;
    truebool.code_ref(str);
    str << endl;
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

void CgenClassTable::code_global_text() {
    str << GLOBAL << HEAP_START << endl
        << HEAP_START << LABEL
        << WORD << 0 << endl
        << "\t.text" << endl
        << GLOBAL;
    emit_init_ref(idtable.add_string("Main"), str);
    str << endl << GLOBAL;
    emit_init_ref(idtable.add_string("Int"), str);
    str << endl << GLOBAL;
    emit_init_ref(idtable.add_string("String"), str);
    str << endl << GLOBAL;
    emit_init_ref(idtable.add_string("Bool"), str);
    str << endl << GLOBAL;
    emit_method_ref(idtable.add_string("Main"), idtable.add_string("main"), str);
    str << endl;
}

void CgenClassTable::code_bools(int boolclasstag) {
    falsebool.code_def(str, boolclasstag);
    truebool.code_def(str, boolclasstag);
}

void CgenClassTable::code_select_gc() {
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
// Emit code to reserve space for and initialize all
// the constants.  Class names should have been added to
// the string table (in the supplied code, is done
// during the construction of the inheritance graph), and
// code for emitting string constants as a side effect adds
// the string's length to the integer table.  The constants
// are emitted by running through the stringtable and inttable
// and producing code for each entry.
//
//********************************************************

void CgenClassTable::code_constants() {
    //
    // Add constants that are required by the code generator.
    //
    stringtable.add_string("");
    inttable.add_string("0");
    //TODO: See whether we can change the order of coding string table and int table
    stringtable.code_string_table(str, stringclasstag);
    inttable.code_string_table(str, intclasstag);
    code_bools(boolclasstag);
}

CgenClassTable::CgenClassTable(Classes classes, ostream &s) : nds(NULL), str(s) {
    stringclasstag = 4 /* Change to your String class tag here */;
    intclasstag = 2 /* Change to your Int class tag here */;
    boolclasstag = 3 /* Change to your Bool class tag here */;
    objectclasstag = 0;
    ioclasstag = 1;
    specialclasstag = -2;

    enterscope();
    if (cgen_debug) cout << "Building CgenClassTable" << endl;
    install_basic_classes();
    install_classes(classes);
    build_inheritance_tree();

    code();
    exitscope();
}

void CgenClassTable::install_basic_classes() {

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
          new CgenNode(class_(No_class, No_class, nil_Features(), filename),
                       Basic, this, specialclasstag));
    addid(SELF_TYPE,
          new CgenNode(class_(SELF_TYPE, No_class, nil_Features(), filename),
                       Basic, this, specialclasstag));
    addid(prim_slot,
          new CgenNode(class_(prim_slot, No_class, nil_Features(), filename),
                       Basic, this, specialclasstag));

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
                    Basic, this, objectclasstag));

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
                    Basic, this, ioclasstag));

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
                    Basic, this, intclasstag));

//
// Bool also has only the "val" slot.
//
    install_class(
            new CgenNode(
                    class_(Bool, Object, single_Features(attr(val, prim_slot, no_expr())), filename),
                    Basic, this, boolclasstag));

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
                    Basic, this, stringclasstag));

}

// CgenClassTable::install_class
// CgenClassTable::install_classes
//
// install_classes enters a list of classes in the symbol table.
//
void CgenClassTable::install_class(CgenNodeP nd) {
    Symbol name = nd->get_name();

    if (probe(name)) {
        return;
    }

    // The class name is legal, so add it to the list of classes
    // and the symbol table.
    nds = new List<CgenNode>(nd, nds);
    addid(name, nd);
}

void CgenClassTable::install_classes(Classes cs) {
    for (int i = cs->first(); cs->more(i); i = cs->next(i))
        install_class(new CgenNode(cs->nth(i), NotBasic, this, i + 5));
}

//
// CgenClassTable::build_inheritance_tree
//
void CgenClassTable::build_inheritance_tree() {
    for (List<CgenNode> *l = nds; l; l = l->tl())
        set_relations(l->hd());
}

//
// CgenClassTable::set_relations
//
// Takes a CgenNode and locates its, and its parent's, inheritance nodes
// via the class table.  Parent and child pointers are added as appropriate.
//
void CgenClassTable::set_relations(CgenNodeP nd) {
    CgenNode *parent_node = probe(nd->get_parent());
    nd->set_parentnd(parent_node);
    parent_node->add_child(nd);
}

void CgenNode::add_child(CgenNodeP n) {
    children = new List<CgenNode>(n, children);
}

void CgenNode::set_parentnd(CgenNodeP p) {
    assert(parentnd == NULL);
    assert(p != NULL);
    parentnd = p;
}

Features class__class::get_features() {
    return features;
}

Symbol method_class::get_name() {
    return name;
}

char *method_class::get_name_str() {
    return name->get_string();
}

Symbol attr_class::get_name() {
    return name;
}

char *attr_class::get_name_str() {
    return name->get_string();
}

int num_attrs(Features features) {
    auto res = 0;
    for (auto i = 0; i < features->len(); i++) {
        if (dynamic_cast<attr_class *>(features->nth(i))) {
            res++;
        }
    }
    return res;
};

template<typename T>
string get_entry_ref(T s) {
    stringstream ss;
    s->code_ref(ss);
    return ss.str();
}

string empty_string;
string zero_int;

void code_protobj(ostream &os, CgenNode *cgen_node) {
    if (cgen_node == NULL) {
        return;
    }
    if (!cgen_node->disptab.empty()) {
        return;
    }
    auto parent_node = cgen_node->get_parentnd();
    if (parent_node != NULL) {
        if (parent_node->disptab.empty() &&
            strcmp(parent_node->get_name()->get_string(), No_class->get_string()) != 0) {
            code_protobj(os, cgen_node->get_parentnd());
        }
    }
    auto &disptab = cgen_node->disptab;
    auto &attrtab = cgen_node->attrtab;
    auto features = cgen_node->get_features();
    auto name = cgen_node->get_name();
    if (empty_string.empty()) {
        empty_string = get_entry_ref(stringtable.lookup_string(""));
    }
    if (zero_int.empty()) {
        zero_int = get_entry_ref(inttable.lookup_string("0"));
    }
    if (cgen_node->get_parentnd() != NULL) {
        if (cgen_node->get_parentnd()->get_name() != No_class) {
            disptab = cgen_node->get_parentnd()->disptab;
            attrtab = cgen_node->get_parentnd()->attrtab;
        }
    }
    os << name << PROTOBJ_SUFFIX << LABEL;
    os << WORD << cgen_node->get_class_tag() << endl;
    os << WORD << num_attrs(features) + attrtab.size() + 3 << endl;
    os << WORD << name << DISPTAB_SUFFIX << endl;
    for (auto attr: attrtab) {
        auto attr_type = attr.second->type_decl;
        if (attr_type == Int) {
            os << WORD << zero_int << endl;
        } else if (attr_type == Str) {
            os << WORD << empty_string << endl;
        } else if (attr_type == Bool) {
            os << WORD << "bool_const0" << endl;
        } else {
            os << WORD << "0" << endl;
        }
    }
    for (auto i = 0; i < features->len(); i++) {
        auto feature = features->nth(i);
        if (typeid(*feature) == typeid(method_class)) {
            auto method = dynamic_cast<method_class *>(feature);
            bool find = false;
            for (auto &p: disptab) {
                if (get<1>(p) == method->get_name_str()) {
                    get<0>(p) = name;
                    find = true;
                    break;
                }
            }
            if (!find) {
                disptab.emplace_back(cgen_node->get_name(), method->get_name_str(), method->return_type);
            }
        } else if (typeid(*feature) == typeid(attr_class)) {
            auto attr = dynamic_cast<attr_class *>(feature);
            auto attr_type = attr->type_decl;
            if (attr_type == Int) {
                os << WORD << zero_int << endl;
            } else if (attr_type == Str) {
                os << WORD << empty_string << endl;
            } else if (attr_type == Bool) {
                os << WORD << "bool_const0" << endl;
            } else {
                os << WORD << "0" << endl;
            }
            cgen_node->attrtab.emplace_back(name, attr);
        }
    }
    os << WORD << "-1" << endl;
}

void code_protobjs(ostream &os, List<CgenNode> *nodes) {
    while (nodes != NULL) {
        auto node = nodes->hd();
        if (strcmp(node->get_name()->get_string(), No_class->get_string()) != 0) {
            code_protobj(os, node);
        }
        nodes = nodes->tl();
    }
}

int nodes_count;

void code_class_name_tab(ostream &os, List<CgenNode> *nodes) {
    auto nodes_ = nodes;
    while (nodes_ != NULL) {
        auto node = nodes_->hd();
        if (strcmp(node->get_name()->get_string(), No_class->get_string()) != 0) {
            nodes_count++;
        }
        nodes_ = nodes_->tl();
    }
    vector<string> class_names(nodes_count);
    while (nodes != NULL) {
        auto cgen_node = nodes->hd();
        auto name = get_entry_ref(stringtable.lookup_string(cgen_node->get_name()->get_string()));
        class_names[cgen_node->get_class_tag()] = name;
        nodes = nodes->tl();
    }
    os << CLASSNAMETAB << LABEL;
    for (auto &class_name: class_names) {
        os << WORD << class_name << endl;
    }
}

void code_class_objtab(ostream &os, List<CgenNode> *nodes) {
    vector<string> objtab(2 * nodes_count);
    while (nodes != NULL) {
        auto node = nodes->hd();
        auto name = string(node->get_name()->get_string());
        auto protobj = name + PROTOBJ_SUFFIX;
        objtab[node->get_class_tag() * 2] = protobj;
        auto init = name + CLASSINIT_SUFFIX;
        objtab[node->get_class_tag() * 2 + 1] = init;
        nodes = nodes->tl();
    }
    os << CLASSOBJTAB << LABEL;
    for (auto &obj: objtab) {
        os << WORD << obj << endl;
    }
}

void emit_disp_tab(ostream &os, vector<tuple<Symbol, string, Symbol>> disptab, Symbol class_name) {
    emit_disptable_ref(class_name, os);
    os << LABEL;
    for (auto &i: disptab) {
        os << WORD << get<0>(i) << "." << get<1>(i) << endl;
    }
}

void code_dispatch_tables(ostream &os, List<CgenNode> *nodes) {
    while (nodes != NULL) {
        auto cgen_node = nodes->hd();
        emit_disp_tab(os, cgen_node->disptab, cgen_node->get_name());
        nodes = nodes->tl();
    }
}

struct VarInfo {
    const char *reg;
    int offset;
    Symbol type;
};

Symbol current_class;
SymbolTable<string, VarInfo> symtab;

void code_object_init(ostream &os, CgenNode *cgen_node) {
    os << cgen_node->get_name() << CLASSINIT_SUFFIX << LABEL;
    emit_addiu(SP, SP, -12, os);
    emit_store(FP, 3, SP, os);
    emit_store(SELF, 2, SP, os);
    emit_store(RA, 1, SP, os);
    emit_addiu(FP, SP, 4, os);
    emit_move(SELF, ACC, os);
    if (cgen_node->get_parentnd() != NULL && cgen_node->get_parentnd()->get_name() != No_class) {
        char *parent_name = new char[strlen(cgen_node->get_parentnd()->get_name()->get_string()) +
                                     strlen(CLASSINIT_SUFFIX) + 1];
        strcpy(parent_name, cgen_node->get_parentnd()->get_name()->get_string());
        parent_name = strcat(parent_name, CLASSINIT_SUFFIX);
        emit_jal(parent_name, os);
        delete[] parent_name;
    }
    symtab.enterscope();
    for (auto i = 0; i < cgen_node->attrtab.size(); i++) {
        auto attr = cgen_node->attrtab[i].second;
        symtab.addid(attr->get_name_str(), new VarInfo{SELF, i + 3, attr->type_decl});
    }
    for (auto i = 0; i < cgen_node->attrtab.size(); i++) {
        auto attr_ = cgen_node->attrtab[i];
        if (attr_.first == cgen_node->name) {
            auto attr = attr_.second;
            auto type = attr->type_decl;
            if (!attr->init->empty()) {
                attr->init->code(os);
                emit_store(ACC, i + 3, SELF, os);
            }
        }
    }
    symtab.exitscope();
    emit_move(ACC, SELF, os);
    emit_load(FP, 3, SP, os);
    emit_load(SELF, 2, SP, os);
    emit_load(RA, 1, SP, os);
    emit_addiu(SP, SP, 12, os);
    emit_return(os);
}

int next_offset;

void code_object_inits(ostream &os, List<CgenNode> *nodes) {
    while (nodes != NULL) {
        current_class = nodes->hd()->get_name();
        next_offset = -1;
        code_object_init(os, nodes->hd());
        nodes = nodes->tl();
    }
}

void code_class_methods(ostream &os, CgenNode *cgen_node) {
    symtab.enterscope();
    current_class = cgen_node->get_name();
    auto features = cgen_node->features;
    for (auto i = 0; i < cgen_node->attrtab.size(); i++) {
        auto attr = cgen_node->attrtab[i].second;
        symtab.addid(attr->get_name_str(), new VarInfo{SELF, i + 3, attr->type_decl});
    }
    for (auto i = 0; i < features->len(); i++) {
        auto feature = features->nth(i);
        if (typeid(*feature) == typeid(method_class)) {
            auto method = dynamic_cast<method_class *>(feature);
            method->code(os, cgen_node);
        }
    }
    symtab.exitscope();
}

array<const char *, 5> base_classes = {"Object", "Bool", "Int", "String", "IO"};

void code_classes_methods(ostream &os, List<CgenNode> *nodes) {
    while (nodes != NULL) {
        auto node = nodes->hd();
        for (auto base_class: base_classes) {
            if (strcmp(node->get_name()->get_string(), base_class) == 0) {
                goto next_node;
            }
        }
        code_class_methods(os, node);
    next_node:
        nodes = nodes->tl();
    }
}

char *filename_str_const;
List<CgenNode> *node_list;

void CgenClassTable::code() {
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

    if (cgen_debug) cout << "coding prototype objects" << endl;
    code_protobjs(str, nds);

    if (cgen_debug) cout << "coding class_nameTab" << endl;
    code_class_name_tab(str, nds);

    if (cgen_debug) cout << "coding class_objTab" << endl;
    code_class_objtab(str, nds);

    if (cgen_debug) cout << "coding dispatch tables" << endl;
    code_dispatch_tables(str, nds);

    if (cgen_debug) cout << "coding global text" << endl;
    code_global_text();

//                 Add your code to emit
//                   - object initializer
//                   - the class methods
//                   - etc...

    node_list = nds;
    filename_str_const = strdup(
            get_entry_ref(stringtable.lookup_string(nds->hd()->get_filename()->get_string())).c_str());

    if (cgen_debug) cout << "coding object initializer" << endl;
    code_object_inits(str, nds);

    if (cgen_debug) cout << "coding class methods" << endl;
    code_classes_methods(str, nds);

    free(filename_str_const);
}


CgenNodeP CgenClassTable::root() {
    return probe(Object);
}


///////////////////////////////////////////////////////////////////////
//
// CgenNode methods
//
///////////////////////////////////////////////////////////////////////

CgenNode::CgenNode(Class_ nd, Basicness bstatus, CgenClassTableP ct, int tag = -1) :
        class__class((const class__class &) *nd),
        parentnd(NULL),
        children(NULL),
        basic_status(bstatus),
        class_tag(tag) {
    stringtable.add_string(name->get_string());          // Add class name to string table
}

Symbol formal_class::get_name() {
    return name;
}

char *formal_class::get_name_str() {
    return name->get_string();
}

void method_class::code(ostream &os, Class__class *c) {
    symtab.enterscope();
    emit_method_ref(c->get_name(), name, os);
    os << LABEL;
    emit_addiu(SP, SP, -12, os);
    emit_store(FP, 3, SP, os);
    emit_store(SELF, 2, SP, os);
    emit_store(RA, 1, SP, os);
    emit_addiu(FP, SP, WORD_SIZE, os);
    emit_move(SELF, ACC, os);
    auto arg_offset = formals->len() + 2;
    for (auto i = 0; i < formals->len(); i++) {
        auto formal = dynamic_cast<formal_class *>(formals->nth(i));
        symtab.addid(formal->get_name_str(), new VarInfo{FP, arg_offset, formal->type_decl});
        arg_offset--;
    }
    next_offset = -1;
    expr->code(os);
    emit_load(FP, 3, SP, os);
    emit_load(SELF, 2, SP, os);
    emit_load(RA, 1, SP, os);
    emit_addiu(SP, SP, 12 + WORD_SIZE * formals->len(), os);
    emit_return(os);
    symtab.exitscope();
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

int label_count = 0;

Symbol assign_class::code(ostream &s) {
    expr->code(s);
    auto var = symtab.lookup(name->get_string());
    emit_store(ACC, var->offset, const_cast<char *>(var->reg), s);
    return var->type;
}

pair<Symbol, int>
find_method_in_disptab(const vector<tuple<Symbol, string, Symbol>> &disptab, const char *method) {
    for (auto i = 0; i < disptab.size(); i++) {
        auto pair = disptab[i];
        if (get<1>(pair) == method) {
            return {get<2>(pair), i};
        }
    }
}

CgenNode *find_cgen_node_in_nds(List<CgenNode> *nodes, Symbol class_name) {
    while (nodes != NULL) {
        auto node = nodes->hd();
        if (strcmp(node->get_name()->get_string(), class_name->get_string()) == 0) {
            return node;
        }
        nodes = nodes->tl();
    }
    return nullptr;
}

Symbol static_dispatch_class::code(ostream &s) {
    s << "# static dispatch: " << type_name << "." << name << endl;
    for (auto i = 0; i < actual->len(); i++) {
        auto actual_arg = actual->nth(i);
        actual_arg->code(s);
        emit_push(ACC, s);
    }
    auto expr_type = expr->code(s);
    emit_bne(ACC, ZERO, label_count, s);
    emit_load_address(ACC, filename_str_const, s);
    emit_load_imm(T1, line_number, s);
    emit_jal("_dispatch_abort", s);
    emit_label_def(label_count, s);
    label_count++;
    auto type_name_disptab = new char[strlen(type_name->get_string()) + strlen(DISPTAB_SUFFIX) + 1];
    strcpy(type_name_disptab, type_name->get_string());
    strcat(type_name_disptab, DISPTAB_SUFFIX);
    emit_load_address(T1, type_name_disptab, s);
    delete[] type_name_disptab;
    auto type_node = find_cgen_node_in_nds(node_list, type_name);
    auto method = find_method_in_disptab(type_node->disptab, name->get_string());
    auto offset = method.second;
    emit_load(T1, offset, T1, s);
    emit_jalr(T1, s);
    s << "# end static dispatch: " << type_name << "." << name << endl;
    if (method.first == SELF_TYPE) {
        return expr_type;
    } else {
        return method.first;
    }
}

Symbol dispatch_class::code(ostream &s) {
    s << "# dispatch: " << name << endl;
    for (auto i = 0; i < actual->len(); i++) {
        auto actual_arg = actual->nth(i);
        actual_arg->code(s);
        emit_push(ACC, s);
    }
    auto expr_type = expr->code(s);
    emit_bne(ACC, ZERO, label_count, s);
    emit_load_address(ACC, filename_str_const, s);
    emit_load_imm(T1, line_number, s);
    emit_jal("_dispatch_abort", s);
    emit_label_def(label_count, s);
    label_count++;
    emit_load(T1, 2, ACC, s);
    auto type_node = find_cgen_node_in_nds(node_list, expr_type);
    auto method = find_method_in_disptab(type_node->disptab, name->get_string());
    auto offset = method.second;
    emit_load(T1, offset, T1, s);
    emit_jalr(T1, s);
    s << "# end dispatch: " << name << endl;
    if (method.first == SELF_TYPE) {
        return expr_type;
    } else {
        return method.first;
    }
}

CgenNode *lub(CgenNode *a, CgenNode *b) {
    set<CgenNode *> a_ancestors;
    while (strcmp(a->get_name()->get_string(), No_class->get_string()) != 0) {
        a_ancestors.insert(a);
        a = a->get_parentnd();
    }
    while (true) {
        if (a_ancestors.find(b) != a_ancestors.end()) {
            return b;
        }
        b = b->get_parentnd();
    }
}

CgenNode *lub(CgenNode *a, Symbol b) {
    auto b_node = find_cgen_node_in_nds(node_list, b);
    return lub(a, b_node);
}

CgenNode *lub(Symbol a, Symbol b) {
    auto a_node = find_cgen_node_in_nds(node_list, a);
    return lub(a_node, b);
}

Symbol cond_class::code(ostream &s) {
    pred->code(s);
    emit_fetch_bool(T1, ACC, s);
    emit_beqz(T1, label_count, s);
    int label = label_count;
    label_count++;
    auto then_type = then_exp->code(s);
    emit_branch(label_count, s);
    int label2 = label_count;
    label_count++;
    emit_label_def(label, s);
    auto else_type = else_exp->code(s);
    emit_label_def(label2, s);
    return lub(then_type, else_type)->get_name();
}

Symbol loop_class::code(ostream &s) {
    emit_label_def(label_count, s);
    int label = label_count;
    label_count++;
    pred->code(s);
    emit_fetch_bool(T1, ACC, s);
    emit_beqz(T1, label_count, s);
    int label2 = label_count;
    label_count++;
    body->code(s);
    emit_branch(label, s);
    emit_label_def(label2, s);
    emit_move(ACC, ZERO, s);
    return Object;
}

void code_case_branches(CgenNode *node, ostream &os) {
    char *str = new char[10];
    sprintf(str, "%d", node->get_class_tag());
    emit_beq(T2, str, label_count, os);
    delete[] str;
    auto children = node->get_children();
    while (children != NULL) {
        code_case_branches(children->hd(), os);
        children = children->tl();
    }
}

Symbol typcase_class::code(ostream &s) {
    expr->code(s);
    emit_push(ACC, s);
    next_offset--;
    emit_beqz(ACC, label_count, s);
    label_count++;
    emit_load(T2, TAG_OFFSET, ACC, s);
    int final_label = label_count;
    label_count++;
    CgenNode *return_type;
    auto label_count_ = label_count;
    auto type_nodes = new CgenNode *[cases->len()];
    for (auto i = 0; i < cases->len(); i++) {
        auto case_ = dynamic_cast<branch_class *>(cases->nth(i));
        auto type = case_->type_decl;
        auto type_node = find_cgen_node_in_nds(node_list, type);
        type_nodes[i] = type_node;
        char *str = new char[10];
        sprintf(str, "%d", type_node->get_class_tag());
        emit_beq(T2, str, label_count, s);
        delete[] str;
        label_count++;
        if (i == 0) {
            return_type = type_node;
        } else {
            return_type = lub(return_type, type_node);
        }
    }
    auto tmp = label_count;
    label_count = label_count_;
    label_count_ = tmp;
    for (auto i = 0; i < cases->len(); i++) {
        auto type_node = type_nodes[i];
        code_case_branches(type_node, s);
        label_count++;
    }
    label_count = label_count_;
    emit_jal("_case_abort", s);
    emit_label_def(final_label - 1, s);
    emit_load_address(ACC, filename_str_const, s);
    emit_load_imm(T1, line_number, s);
    emit_jal("_case_abort2", s);
    for (auto i = 0; i < cases->len(); i++) {
        emit_label_def(final_label + i + 1, s);
        label_count++;
        auto case_ = dynamic_cast<branch_class *>(cases->nth(i));
        symtab.enterscope();
        symtab.addid(case_->name->get_string(), new VarInfo{FP, next_offset + 1, case_->type_decl});
        case_->expr->code(s);
        emit_branch(final_label, s);
    }
    emit_label_def(final_label, s);
    emit_pop(s);
    next_offset++;
    return return_type->get_name();
}

Symbol block_class::code(ostream &s) {
    Symbol type;
    for (auto i = 0; i < body->len(); i++) {
        type = body->nth(i)->code(s);
    }
    return type;
}

Symbol let_class::code(ostream &s) {
    symtab.enterscope();
    stringstream ss;
    auto type_ = init->code(ss);
    if (type_ == No_type) {
        if (type_decl == Int) {
            emit_load_address(ACC, const_cast<char *>(zero_int.c_str()), s);
        } else if (type_decl == Str) {
            emit_load_address(ACC, const_cast<char *>(empty_string.c_str()), s);
        } else if (type_decl == Bool) {
            emit_load_address(ACC, "bool_const0", s);
        } else {
            emit_load_imm(ACC, 0, s);
        }
    } else {
        s << ss.str();
    }
    emit_push(ACC, s);
    symtab.addid(identifier->get_string(), new VarInfo{FP, next_offset, type_decl});
    next_offset--;
    auto type = body->code(s);
    emit_pop(s);
    next_offset++;
    symtab.exitscope();
    return type;
}

Symbol plus_class::code(ostream &s) {
    e1->code(s);
    emit_fetch_int(T1, ACC, s);
    emit_push(T1, s);
    emit_jal("Object.copy", s);
    emit_push(ACC, s);
    e2->code(s);
    emit_pop(T3, s);
    emit_pop(T1, s);
    emit_fetch_int(T2, ACC, s);
    emit_add(T1, T1, T2, s);
    emit_move(ACC, T3, s);
    emit_store_int(T1, ACC, s);
    return Int;
}

Symbol sub_class::code(ostream &s) {
    e1->code(s);
    emit_fetch_int(T1, ACC, s);
    emit_push(T1, s);
    emit_jal("Object.copy", s);
    emit_push(ACC, s);
    e2->code(s);
    emit_pop(T3, s);
    emit_pop(T1, s);
    emit_fetch_int(T2, ACC, s);
    emit_sub(T1, T1, T2, s);
    emit_move(ACC, T3, s);
    emit_store_int(T1, ACC, s);
    return Int;
}

Symbol mul_class::code(ostream &s) {
    e1->code(s);
    emit_fetch_int(T1, ACC, s);
    emit_push(T1, s);
    emit_jal("Object.copy", s);
    emit_push(ACC, s);
    e2->code(s);
    emit_pop(T3, s);
    emit_pop(T1, s);
    emit_fetch_int(T2, ACC, s);
    emit_mul(T1, T1, T2, s);
    emit_move(ACC, T3, s);
    emit_store_int(T1, ACC, s);
    return Int;
}

Symbol divide_class::code(ostream &s) {
    e1->code(s);
    emit_fetch_int(T1, ACC, s);
    emit_push(T1, s);
    emit_jal("Object.copy", s);
    emit_push(ACC, s);
    e2->code(s);
    emit_pop(T3, s);
    emit_pop(T1, s);
    emit_fetch_int(T2, ACC, s);
    emit_div(T1, T1, T2, s);
    emit_move(ACC, T3, s);
    emit_store_int(T1, ACC, s);
    return Int;
}

Symbol neg_class::code(ostream &s) {
    e1->code(s);
    emit_jal("Object.copy", s);
    emit_fetch_int(T1, ACC, s);
    emit_neg(T1, T1, s);
    emit_store_int(T1, ACC, s);
    return Int;
}

Symbol lt_class::code(ostream &s) {
    e1->code(s);
    emit_fetch_int(T1, ACC, s);
    emit_push(T1, s);
    e2->code(s);
    emit_fetch_int(T2, ACC, s);
    emit_pop(T1, s);
    emit_load_address(ACC, "bool_const1", s);
    emit_blt(T1, T2, label_count, s);
    emit_load_address(ACC, "bool_const0", s);
    emit_label_def(label_count, s);
    label_count++;
    return Bool;
}

Symbol eq_class::code(ostream &s) {
    e1->code(s);
    emit_push(ACC, s);
    e2->code(s);
    emit_move(T2, ACC, s);
    emit_pop(T1, s);
    emit_load_address(ACC, "bool_const1", s);
    emit_beq(T1, T2, label_count, s);
    emit_load_address(A1, "bool_const0", s);
    emit_jal("equality_test", s);
    emit_label_def(label_count, s);
    label_count++;
    return Bool;
}

Symbol leq_class::code(ostream &s) {
    e1->code(s);
    emit_fetch_int(T1, ACC, s);
    emit_push(T1, s);
    e2->code(s);
    emit_fetch_int(T2, ACC, s);
    emit_pop(T1, s);
    emit_load_address(ACC, "bool_const1", s);
    emit_bleq(T1, T2, label_count, s);
    emit_load_address(ACC, "bool_const0", s);
    emit_label_def(label_count, s);
    label_count++;
    return Bool;
}

Symbol comp_class::code(ostream &s) {
    e1->code(s);
    emit_fetch_bool(T1, ACC, s);
    emit_load_address(ACC, "bool_const1", s);
    emit_beqz(T1, label_count, s);
    emit_load_address(ACC, "bool_const0", s);
    emit_label_def(label_count, s);
    label_count++;
    return Bool;
}

Symbol int_const_class::code(ostream &s) {
    //
    // Need to be sure we have an IntEntry *, not an arbitrary Symbol
    //
    emit_load_int(ACC, inttable.lookup_string(token->get_string()), s);
    return Int;
}

Symbol string_const_class::code(ostream &s) {
    emit_load_string(ACC, stringtable.lookup_string(token->get_string()), s);
    return Str;
}

Symbol bool_const_class::code(ostream &s) {
    emit_load_bool(ACC, BoolConst(val), s);
    return Bool;
}

Symbol new__class::code(ostream &s) {
    if (type_name == SELF_TYPE) {
        emit_load_address(T1, CLASSOBJTAB, s);
        emit_load(T2, 0, SELF, s);
        emit_sll(T2, T2, 3, s);
        emit_addu(T1, T1, T2, s);
        emit_load(ACC, 0, T1, s);
        emit_push(T1, s);
        emit_jal("Object.copy", s);
        emit_pop(T1, s);
        emit_addiu(T1, T1, 4, s);
        emit_load(T2, 0, T1, s);
        emit_jalr(T2, s);
        return current_class;
    } else {
        auto protobj = new char[strlen(type_name->get_string()) + strlen(PROTOBJ_SUFFIX) + 1];
        strcpy(protobj, type_name->get_string());
        strcat(protobj, PROTOBJ_SUFFIX);
        emit_load_address(ACC, protobj, s);
        delete[] protobj;
        emit_jal("Object.copy", s);
        auto init = new char[strlen(type_name->get_string()) + strlen(CLASSINIT_SUFFIX) + 1];
        strcpy(init, type_name->get_string());
        strcat(init, CLASSINIT_SUFFIX);
        emit_jal(init, s);
        delete[] init;
        return type_name;
    }
}

Symbol isvoid_class::code(ostream &s) {
    e1->code(s);
    emit_move(T1, ACC, s);
    emit_load_address(ACC, "bool_const1", s);
    emit_beqz(T1, label_count, s);
    emit_load_address(ACC, "bool_const0", s);
    emit_label_def(label_count, s);
    label_count++;
    return Bool;
}

Symbol no_expr_class::code(ostream &s) {
    return No_type;
}

Symbol object_class::code(ostream &s) {
    if (name == self) {
        emit_move(ACC, SELF, s);
        return current_class;
    } else {
        auto symbol = symtab.lookup(name->get_string());
        s << "# " << name << " at " << WORD_SIZE * symbol->offset << "(" << symbol->reg << ")" << endl;
        emit_load(ACC, symbol->offset, const_cast<char *>(symbol->reg), s);
        return symbol->type;
    }
}