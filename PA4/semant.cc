#include "semant.h"
#include "cool-tree.h"
#include "utilities.h"

#include <algorithm>
#include <cstdlib>
#include <iostream>
#include <list>
#include <queue>
#include <set>
#include <vector>
#include <string>
#include <iterator>

using std::cout;
using std::endl;
using std::map;
using std::queue;
using std::set;
using std::vector;
using std::pair;
using std::make_pair;
using std::flush;
using std::any_of;
using std::string;

extern int semant_debug;
extern char *curr_filename;
Symbol symbol_filename;

//////////////////////////////////////////////////////////////////////
//
// Symbols
//
// For convenience, a large number of symbols are predefined here.
// These symbols include the primitive type and method names, as well
// as fixed names used by the runtime system.
//
//////////////////////////////////////////////////////////////////////
static Symbol arg, arg2, Bool, concat, cool_abort, copy, Int, in_int, in_string,
        IO, length, Main, main_meth, No_class, No_type, Object, out_int, out_string,
        prim_slot, self, SELF_TYPE, Str, str_field, substr, type_name, val;

//
// Initializing the predefined symbols.
//
static void initialize_constants() {
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

Symbol class__class::get_name() {
    return name;
}

char *class__class::get_name_str() {
    return name->get_string();
}

Symbol class__class::get_parent() {
    return parent;
}

char *class__class::get_parent_str() {
    return parent->get_string();
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

bool method_class::type() {
    return true;
}

Formals method_class::get_formals() {
    return formals;
}

Symbol method_class::get_type() {
    return return_type;
}

Symbol attr_class::get_name() {
    return name;
}

char *attr_class::get_name_str() {
    return name->get_string();
}

bool attr_class::type() {
    return false;
}

Symbol attr_class::get_type() {
    return type_decl;
}

Formals attr_class::get_formals() {
    return nullptr;
}

Symbol formal_class::get_name() {
    return name;
}

char *formal_class::get_name_str() {
    return name->get_string();
}

Symbol formal_class::get_type() {
    return type_decl;
}

char *formal_class::get_type_str() {
    return type_decl->get_string();
}

Symbol branch_class::get_type() {
    return type_decl;
}

Expression branch_class::get_expr() {
    return expr;
}

Symbol branch_class::get_name() {
    return name;
}

Class *Class::find(Class_ f_cls) {
    if (cls == f_cls) {
        return this;
    }
    for (auto child: children) {
        Class *ret = child->find(f_cls);
        if (ret != nullptr) {
            return ret;
        }
    }
    return nullptr;
}

Class *Class::find(Symbol name) {
    if (cls->get_name() == name) {
        return this;
    }
    for (auto child: children) {
        Class *ret = child->find(name);
        if (ret != nullptr) {
            return ret;
        }
    }
    return nullptr;
}

bool Class::insert(Class_ father, Class_ child) {
    auto fa = find(father);
    if (fa == nullptr) {
        return false;
    }
    auto p_cls = new Class(child, fa);
    fa->children.push_back(p_cls);
    return true;
}

bool Class::insert(Symbol father, Class_ child) {
    auto fa = find(father);
    if (fa == nullptr) {
        return false;
    }
    auto p_cls = new Class(child, fa);
    fa->children.push_back(p_cls);
    return true;
}

ClassTable *class_table;

bool exists(vector<Class_> &classes, char *cls) {
    return any_of(classes.begin(), classes.end(), [&](Class_ c) {
        return strcmp(c->get_name_str(), cls) == 0;
    });
}

int inherits(Class *node, Classes_ &classes) {
    int errs = 0;
    if (strcmp(node->cls->get_name_str(), "Object") != 0) {
        auto parent = node->parent;
        auto &parent_ = classes[parent->cls->get_name_str()];
        auto &cls = classes[node->cls->get_name_str()];
        for (auto &method: parent_.methods) {
            cls.methods.insert(method);
            if (cls.methods.find(method.first) != cls.methods.end()) {
                auto &m = cls.methods[method.first];
                if (strcmp(method.second.return_type->get_string(), m.return_type->get_string()) != 0) {
                    cerr << node->cls->get_filename() << ":" << node->cls->get_line_number() << ": In redefined method "
                         << method.first << ", return type " << m.return_type
                         << " is different from original return type " << method.second.return_type << "\n";
                    errs++;
                    continue;
                }
                if (method.second.arguments.size() != m.arguments.size()) {
                    cerr << node->cls->get_filename() << ":" << node->cls->get_line_number()
                         << ": Incompatible number of formal parameters in redefined method " << method.first << "\n";
                    errs++;
                    continue;
                }
                for (int i = 0; i < m.arguments.size(); i++) {
                    if (strcmp(m.arguments[i].second->get_string(), method.second.arguments[i].second->get_string()) !=
                        0) {
                        cerr << node->cls->get_filename() << ":" << node->cls->get_line_number()
                             << ": In redefined method " << method.first << ", parameter type " << m.arguments[i].second
                             << " is different from original type " << method.second.arguments[i].first << "\n";
                        errs++;
                        break;
                    }
                }
            }
        }
        for (auto &attr: parent_.attrs) {
            if (cls.attrs.find(attr.first) != cls.attrs.end()) {
                cerr << node->cls->get_filename() << ":" << node->cls->get_line_number() << ": " << "Attribute "
                     << attr.first
                     << " is an attribute of an inherited class.\n";
                errs++;
            } else {
                cls.attrs.insert(attr);
            }
        }
    }
    for (auto cls: node->children) {
        errs += inherits(cls, classes);
    }
    return errs;
}

ClassTable::ClassTable(Classes classes) : semant_errors(0), error_stream(cerr) {
    curr_filename = "";
    vector<Class_> class_vec;
    install_basic_classes();
    const char *base_classes_str[]{"Int", "String", "Bool", "IO", "Object"};
    set<char *> class_set;
    int main_lineno;
    bool have_main = false;
    bool main2 = false;
    for (int i = 0; i < classes->len(); i++) {
        if (strcmp(curr_filename, "") == 0) {
            curr_filename = (*classes)[i]->get_filename()->get_string();
            symbol_filename = (*classes)[i]->get_filename();
        }
        if (strcmp((*classes)[i]->get_name_str(), "SELF_TYPE") == 0) {
            semant_error((*classes)[i]) << "SELF_TYPE cannot be the name of a class.\n";
        }
        for (int j = 0; j < 3; j++) {
            if (strcmp((*classes)[i]->get_parent_str(), base_classes_str[j]) ==
                0) {
                semant_error((*classes)[i])
                        << "Class " << (*classes)[i]->get_name()
                        << " cannot inherit class " << base_classes_str[j] << ".\n";
            }
            if (strcmp((*classes)[i]->get_parent_str(), "SELF_TYPE") == 0) {
                semant_error((*classes)[i])
                        << "Class " << (*classes)[i]->get_name()
                        << " cannot inherit class SELF_TYPE.\n";
            }
        }
        for (auto &j: base_classes_str) {
            if (strcmp((*classes)[i]->get_name_str(), j) ==
                0) {
                semant_error((*classes)[i]) << "Redefinition of basic class "
                                            << j << ".\n";
                return;
            }
        }
        if (class_set.find((*classes)[i]->get_name_str()) != class_set.end()) {
            semant_error((*classes)[i])
                    << "Class " << (*classes)[i]->get_name()
                    << " was previously defined.\n";
            return;
        }
        Features features = (*classes)[i]->get_features();
        set<string> method_set;
        set<string> attr_set;
        ClassMembers class_members;
        for (int j = 0; j < features->len(); j++) {
            if ((*features)[j]->type()) {
                if (method_set.find((*features)[j]->get_name_str()) !=
                    method_set.end()) {
                    semant_errors++;
                    error_stream << curr_filename << ":"
                                 << (*features)[j]->get_line_number()
                                 << ": Method "
                                 << (*features)[j]->get_name()
                                 << " is multiply defined.\n";
                }
                method_set.insert((*features)[j]->get_name_str());
                auto formals = (*features)[j]->get_formals();
                vector<pair<string, Symbol>> arguments;
                for (int k = 0; k < formals->len(); k++) {
                    if (strcmp(formals->nth(k)->get_name_str(), "self") == 0) {
                        semant_error(symbol_filename, formals->nth(k))
                                << "'self' cannot be the name of a formal parameter.\n";
                        continue;
                    }
                    if (strcmp(formals->nth(k)->get_type_str(), "SELF_TYPE") == 0) {
                        semant_error(symbol_filename, formals->nth(k))
                                << "Formal parameter " << formals->nth(k)->get_name()
                                << " cannot have type SELF_TYPE.\n";
                    }
                    if (any_of(arguments.begin(), arguments.end(), [&](pair<string, Symbol> &p) {
                        return p.first == formals->nth(k)->get_name_str();
                    })) {
                        semant_error(symbol_filename, formals->nth(k))
                                << "Formal parameter "
                                << formals->nth(k)->get_name()
                                << " is multiply defined.\n";
                        continue;
                    }
                    arguments.emplace_back((*formals)[k]->get_name_str(), (*formals)[k]->get_type());
                }
                class_members.methods[(*features)[j]->get_name_str()] =
                        {arguments, (*features)[j]->get_type()};
            } else {
                if (attr_set.find((*features)[j]->get_name_str()) !=
                    attr_set.end()) {
                    semant_errors++;
                    error_stream << curr_filename << ":"
                                 << (*features)[j]->get_line_number()
                                 << ": Attribute "
                                 << (*features)[j]->get_name()
                                 << " is multiply defined in class.\n";
                }
                if (strcmp((*features)[j]->get_name_str(), "self") == 0) {
                    semant_errors++;
                    error_stream
                            << curr_filename << ":"
                            << (*features)[j]->get_line_number()
                            << ": 'self' cannot be the name of an attribute.\n";
                }
                attr_set.insert((*features)[j]->get_name_str());
                class_members.attrs[(*features)[j]->get_name_str()] =
                        (*features)[j]->get_type();
            }
        }
        if (!have_main && strcmp((*classes)[i]->get_name_str(), "Main") == 0) {
            have_main = true;
            main_lineno = (*classes)[i]->get_line_number();
            for (int j = 0; j < features->len(); j++) {
                if ((*features)[j]->type()) {
                    if (!main2 &&
                        strcmp((*features)[j]->get_name_str(), "main") == 0) {
                        main2 = true;
                    }
                }
            }
        }
        class_set.insert((*classes)[i]->get_name_str());
        class_vec.push_back((*classes)[i]);
        m_classes[(*classes)[i]->get_name_str()] = class_members;
    }
    if (!have_main) {
        semant_errors++;
        error_stream << "Class Main is not defined.\n";
    } else {
        if (!main2) {
            semant_errors++;
            error_stream << curr_filename << ":" << main_lineno
                         << ": No 'main' method in class Main.\n";
        }
    }
    for (auto class_: class_vec) {
        char *parent_name = class_->get_parent_str();
        if (strcmp(parent_name, "Object") == 0 || strcmp(parent_name, "IO") == 0) {
            continue;
        }
        bool found = exists(class_vec, parent_name);
        if (!found) {
            semant_error(class_)
                    << "Class " << class_->get_name()
                    << " inherits from an undefined class " << parent_name << ".\n";
        }
    }
    if (semant_errors != 0) {
        return;
    }
    queue<Class_> bad_classes;
    class_root = new Class(base_classes[0]);
    for (int i = 1; i < 5; i++) {
        class_root->insert(base_classes[0], base_classes[i]);
    }
    for (auto cls: base_classes) {
        auto features = cls->get_features();
        ClassMembers class_members;
        for (int j = 0; j < features->len(); j++) {
            if ((*features)[j]->type()) {
                auto formals = (*features)[j]->get_formals();
                vector<pair<string, Symbol>> arguments;
                for (int k = 0; k < formals->len(); k++) {
                    if (strcmp((*formals)[k]->get_name_str(), "self") == 0) {
                        semant_error(symbol_filename, (*formals)[k])
                                << "'self' cannot be the name of a formal parameter.\n";
                    }
                    arguments.emplace_back((*formals)[k]->get_name_str(), (*formals)[k]->get_type());
                }
                class_members.methods[(*features)[j]->get_name_str()] =
                        {arguments, (*features)[j]->get_type()};
            } else {
                class_members.attrs[(*features)[j]->get_name_str()] =
                        (*features)[j]->get_type();
            }
        }
        m_classes[cls->get_name_str()] = class_members;
    }
    for (auto class_: class_vec) {
        if (!class_root->insert(class_->get_parent(), class_)) {
            bad_classes.push(class_);
        }
    }
    int len;
    while (!bad_classes.empty()) {
        len = bad_classes.size();
        for (int i = 0; i < len; i++) {
            Class_ class_ = bad_classes.front();
            bad_classes.pop();
            if (!class_root->insert(class_->get_parent(), class_)) {
                bad_classes.push(class_);
            }
        }
        if (bad_classes.size() == len) {
            for (int i = 0; i < len; i++) {
                auto class_ = bad_classes.front();
                bad_classes.pop();
                semant_error(class_)
                        << "Class " << class_->get_name()
                        << ", or an ancestor of " << class_->get_name()
                        << ", is involved in an inheritance cycle.\n";
            }
            return;
        }
    }
    semant_errors += inherits(class_root, m_classes);
}

void ClassTable::install_basic_classes() {

    // The tree package uses these globals to annotate the classes built below.
    // curr_lineno  = 0;
    Symbol filename = stringtable.add_string("<basic class>");

    // The following demonstrates how to create dummy parse trees to
    // refer to basic Cool classes.  There's no need for method
    // bodies -- these are already built into the runtime system.

    // IMPORTANT: The results of the following expressions are
    // stored in local variables.  You will want to do something
    // with those variables at the end of this method to make this
    // code meaningful.

    //
    // The Object class has no parent class. Its methods are
    //        abort() : Object    aborts the program
    //        type_name() : Str   returns a string representation of class name
    //        copy() : SELF_TYPE  returns a copy of the object
    //
    // There is no need for method bodies in the basic classes---these
    // are already built in to the runtime system.

    Class_ Object_class = class_(
            Object, No_class,
            append_Features(
                    append_Features(single_Features(method(cool_abort, nil_Formals(),
                                                           Object, no_expr())),
                                    single_Features(method(type_name, nil_Formals(),
                                                           Str, no_expr()))),
                    single_Features(method(copy, nil_Formals(), SELF_TYPE, no_expr()))),
            filename);

    //
    // The IO class inherits from Object. Its methods are
    //        out_string(Str) : SELF_TYPE       writes a string to the output
    //        out_int(Int) : SELF_TYPE            "    an int    "  "     "
    //        in_string() : Str                 reads a string from the input
    //        in_int() : Int                      "   an int     "  "     "
    //
    Class_ IO_class = class_(
            IO, Object,
            append_Features(
                    append_Features(
                            append_Features(
                                    single_Features(method(out_string,
                                                           single_Formals(formal(arg, Str)),
                                                           SELF_TYPE, no_expr())),
                                    single_Features(method(out_int,
                                                           single_Formals(formal(arg, Int)),
                                                           SELF_TYPE, no_expr()))),
                            single_Features(
                                    method(in_string, nil_Formals(), Str, no_expr()))),
                    single_Features(method(in_int, nil_Formals(), Int, no_expr()))),
            filename);

    //
    // The Int class has no methods and only a single attribute, the
    // "val" for the integer.
    //
    Class_ Int_class =
            class_(Int, Object, single_Features(attr(val, prim_slot, no_expr())),
                   filename);

    //
    // Bool also has only the "val" slot.
    //
    Class_ Bool_class =
            class_(Bool, Object, single_Features(attr(val, prim_slot, no_expr())),
                   filename);

    //
    // The class Str has a number of slots and operations:
    //       val                                  the length of the string
    //       str_field                            the string itself
    //       length() : Int                       returns length of the string
    //       concat(arg: Str) : Str               performs string concatenation
    //       substr(arg: Int, arg2: Int): Str     substring selection
    //
    Class_ Str_class = class_(
            Str, Object,
            append_Features(
                    append_Features(
                            append_Features(
                                    append_Features(
                                            single_Features(attr(val, Int, no_expr())),
                                            single_Features(attr(str_field, prim_slot, no_expr()))),
                                    single_Features(
                                            method(length, nil_Formals(), Int, no_expr()))),
                            single_Features(method(concat, single_Formals(formal(arg, Str)),
                                                   Str, no_expr()))),
                    single_Features(
                            method(substr,
                                   append_Formals(single_Formals(formal(arg, Int)),
                                                  single_Formals(formal(arg2, Int))),
                                   Str, no_expr()))),
            filename);

    base_classes[0] = Object_class;
    base_classes[1] = IO_class;
    base_classes[2] = Int_class;
    base_classes[3] = Bool_class;
    base_classes[4] = Str_class;
}

////////////////////////////////////////////////////////////////////
//
// semant_error is an overloaded function for reporting errors
// during semantic analysis.  There are three versions:
//
//    ostream& ClassTable::semant_error()
//
//    ostream& ClassTable::semant_error(Class_ c)
//       print line number and filename for `c'
//
//    ostream& ClassTable::semant_error(Symbol filename, tree_node *t)
//       print a line number and filename
//
///////////////////////////////////////////////////////////////////

ostream &ClassTable::semant_error(Class_ c) {
    return semant_error(c->get_filename(), c);
}

ostream &ClassTable::semant_error(Symbol filename, tree_node *t) {
    error_stream << filename << ":" << t->get_line_number() << ": ";
    return semant_error();
}

ostream &ClassTable::semant_error() {
    semant_errors++;
    return error_stream;
}

Classes_ classes;
Class *class_root;
Symbol current_class;
std::list<pair<string, Symbol>> extra_env;

bool can_assign_to(Class *a, Symbol b) {                 // a <- b
    if (a->find(b) != nullptr) {
        return true;
    }
    return false;
}

bool can_assign_to(Symbol a, Symbol b) {                 // a <- b
    if (strcmp(a->get_string(), "SELF_TYPE") == 0) {
        if (strcmp(b->get_string(), "SELF_TYPE") != 0) {
            return false;
        }
        return true;
    } else {
        if (strcmp(b->get_string(), "SELF_TYPE") == 0) {
            b = current_class;
        }
    }
    if (strcmp(b->get_string(), "self") == 0) {
        b = current_class;
    }
    auto a_node = class_root->find(a);
    return can_assign_to(a_node, b);
}

pair<string, Symbol> find(std::list<pair<string, Symbol>> &env, const string &name) {
    for (auto &e: env) {
        if (e.first == name) {
            return e;
        }
    }
    return make_pair("", Object);
}

pair<string, Symbol> find(std::list<pair<string, Symbol>> &env, Symbol name) {
    return find(env, name->get_string());
}

Symbol assign_class::check() {
    auto right_type = expr->check();
    auto p = find(extra_env, name);
    auto left_type = p.second;
    if (strcmp(right_type->get_string(), "SELF_TYPE") == 0) {
        right_type = current_class;
    }
    if (strcmp(left_type->get_string(), "self") == 0) {
        class_table->semant_error(symbol_filename, this) << "Cannot assign to self.\n";
    }
    if (strcmp(left_type->get_string(), "SELF_TYPE") == 0) {
        left_type = current_class;
    }
    if (p.first.empty()) {
        auto &attrs = classes[current_class->get_string()].attrs;
        bool found = false;
        for (auto &attr: attrs) {
            if (attr.first == name->get_string()) {
                left_type = attr.second;
                found = true;
                break;
            }
        }
        if (!found) {
            class_table->semant_error(symbol_filename, this) << "Assignment to undeclared variable " << name << ".\n";
        }
    }
    if (!can_assign_to(left_type, right_type)) {
        class_table->semant_error(symbol_filename, this) << "Type " << right_type
                                                         << " of assigned expression does not conform to declared type "
                                                         << left_type << " of identifier " << name << ".\n";
    }
    set_type(right_type);
    return right_type;
}

Symbol static_dispatch_class::check() {
    auto cls = expr->check();
    auto static_type = type_name->get_string();
    auto static_type_node = class_root->find(type_name);
    if (static_type_node == nullptr) {
        class_table->semant_error(symbol_filename, this) << "Static dispatch to undefined class " << static_type
                                                         << ".\n";
        set_type(Object);
        return Object;
    }
    if (!can_assign_to(static_type_node, cls)) {
        class_table->semant_error(symbol_filename, this) << "Expression type " << cls << " does not conform to "
                                                         << "static dispatch type " << static_type << ".\n";
        set_type(Object);
        return Object;
    }
    auto method_str = name->get_string();
    if (classes[static_type].methods.find(method_str) == classes[static_type].methods.end()) {
        class_table->semant_error(symbol_filename, this) << "Dispatch to undefined method " << method_str << ".\n";
        set_type(Object);
        return Object;
    }
    auto &method = classes[static_type].methods[method_str];
    auto &arguments = method.arguments;
    auto return_type = method.return_type;
    if (strcmp(return_type->get_string(), "SELF_TYPE") == 0) {
        return_type = cls;
    }
    if (actual->len() != arguments.size()) {
        class_table->semant_error(symbol_filename, this) << "Method " << method_str
                                                         << " called with wrong number of arguments.\n";
    }
    for (int i = 0; i < actual->len(); i++) {
        auto t = actual->nth(i)->check();
        if (strcmp(t->get_string(), "SELF_TYPE") == 0) {
            t = current_class;
        }
        if (!can_assign_to(arguments[i].second, t)) {
            class_table->semant_error(symbol_filename, this) << "In call of method " << method_str << ", type " << t
                                                             << " of parameter " << i + 1
                                                             << " does not conform to declared type "
                                                             << arguments[i].second << ".\n";
        }
    }
    set_type(return_type);
    return return_type;
}

Symbol dispatch_class::check() {
    auto cls = expr->check();
    bool self_ = false;
    if (strcmp(cls->get_string(), "self") == 0) {
        cls = current_class;
        self_ = true;
    }
    auto cls_str = cls->get_string();
    auto method_str = name->get_string();
    if (classes[cls_str].methods.find(method_str) == classes[cls_str].methods.end()) {
        class_table->semant_error(symbol_filename, this) << "Dispatch to undefined method " << method_str << ".\n";
        set_type(Object);
        return Object;
    }
    auto &method = classes[cls_str].methods[method_str];
    auto &arguments = method.arguments;
    auto return_type = method.return_type;
    if (strcmp(return_type->get_string(), "SELF_TYPE") == 0) {
        return_type = cls;
    } else {
        self_ = false;
    }
    if (actual->len() != arguments.size()) {
        class_table->semant_error(symbol_filename, this) << "Method " << method_str
                                                         << " called with wrong number of arguments.\n";
    }
    for (int i = 0; i < actual->len(); i++) {
        auto t = actual->nth(i)->check();
        if (!can_assign_to(arguments[i].second, t)) {
            class_table->semant_error(symbol_filename, this) << "In call of method " << method_str << ", type " << t
                                                             << " of parameter " << i + 1
                                                             << " does not conform to declared type "
                                                             << arguments[i].second << ".\n";
        }
    }
    if (self_) {
        set_type(SELF_TYPE);
    } else {
        set_type(return_type);
    }
    return return_type;
}

Class *lca_class(Class *p, Class *q) {
    if (p->cls->get_name() == Object || q->cls->get_name() == Object) {
        return class_root->find(Object);
    }
    set<string> ancestors_of_p;
    while (p != nullptr) {
        ancestors_of_p.insert(p->cls->get_name_str());
        p = p->parent;
    }
    while (q != nullptr) {
        if (ancestors_of_p.find(q->cls->get_name_str()) != ancestors_of_p.end()) {
            return q;
        }
        q = q->parent;
    }
}

Symbol cond_class::check() {
    if (pred->check() != Bool) {
        class_table->semant_error(symbol_filename, this) << "Predicate of 'if' does not have type Bool.\n";
    }
    auto then_node = class_root->find(then_exp->check());
    auto else_node = class_root->find(else_exp->check());
    auto type = lca_class(then_node, else_node)->cls->get_name();
    set_type(type);
    return type;
}

Symbol loop_class::check() {
    if (pred->check() != Bool) {
        class_table->semant_error(symbol_filename, this) << "Predicate of 'while' does not have type Bool.\n";
    }
    body->check();
    set_type(Object);
    return Object;
}

Symbol typcase_class::check() {
    expr->check();
    ::set<string> types;
    Class *return_type_node = nullptr;
    for (int i = 0; i < cases->len(); i++) {
        auto case_ = cases->nth(i);
        auto type = case_->get_type();
        auto type_str = type->get_string();
        if (types.find(type_str) != types.end()) {
            class_table->semant_error(symbol_filename, this) << "Duplicate branch "
                                                             << type << " in "
                                                             << "switch statement.\n";
        }
        types.insert(type_str);
        extra_env.emplace_front(case_->get_name()->get_string(), type);
        auto expr_type_node = class_root->find(case_->get_expr()->check());
        extra_env.pop_front();
        if (return_type_node == nullptr) {
            return_type_node = expr_type_node;
        } else {
            return_type_node = lca_class(return_type_node, expr_type_node);
        }
    }
    set_type(return_type_node->cls->get_name());
    return return_type_node->cls->get_name();
}

Symbol block_class::check() {
    Symbol type;
    for (int i = 0; i < body->len(); i++) {
        type = body->nth(i)->check();
    }
    if (strcmp(type->get_string(), "self") == 0) {
        type = SELF_TYPE;
    }
    set_type(type);
    return type;
}

Symbol let_class::check() {
    if (strcmp(identifier->get_string(), "self") == 0) {
        class_table->semant_error(symbol_filename, this) << "'self' cannot be bound in a 'let' expression.\n";
    }
    auto init_type = init->check();
    extra_env.emplace_front(identifier->get_string(), type_decl);
    auto type = body->check();
    extra_env.pop_front();
    if (init_type != No_type) {
        auto t = type_decl;
        if (!can_assign_to(t, init_type)) {
            class_table->semant_error(symbol_filename, this) << "Inferred type " << init_type
                                                             << " of initialization of " << identifier
                                                             << " does not conform to identifier's declared type "
                                                             << type_decl << ".\n";
        }
    }
    set_type(type);
    return type;
}


Symbol plus_class::check() {
    auto type_e1 = e1->check();
    auto type_e2 = e2->check();
    if (type_e1 != Int || type_e2 != Int) {
        class_table->semant_error(symbol_filename, this) << "non-Int arguments: " << type_e1 << " + " << type_e2
                                                         << endl;
    }
    set_type(Int);
    return Int;
}

Symbol sub_class::check() {
    auto type_e1 = e1->check();
    auto type_e2 = e2->check();
    if (type_e1 != Int || type_e2 != Int) {
        class_table->semant_error(symbol_filename, this) << "non-Int arguments: " << type_e1 << " - " << type_e2
                                                         << endl;
    }
    set_type(Int);
    return Int;
}

Symbol mul_class::check() {
    auto type_e1 = e1->check();
    auto type_e2 = e2->check();
    if (type_e1 != Int || type_e2 != Int) {
        class_table->semant_error(symbol_filename, this) << "non-Int arguments: " << type_e1 << " * " << type_e2
                                                         << endl;
    }
    set_type(Int);
    return Int;
}

Symbol divide_class::check() {
    auto type_e1 = e1->check();
    auto type_e2 = e2->check();
    if (type_e1 != Int || type_e2 != Int) {
        class_table->semant_error(symbol_filename, this) << "non-Int arguments: " << type_e1 << " / " << type_e2
                                                         << ".\n";
    }
    set_type(Int);
    return Int;
}

Symbol neg_class::check() {
    auto type_e1 = e1->check();
    if (type_e1 != Int) {
        class_table->semant_error(symbol_filename, this) << "Argument of '~' has type " << type_e1
                                                         << " instead of Int.\n";
    }
    set_type(Int);
    return Int;
}

Symbol lt_class::check() {
    auto type_e1 = e1->check();
    auto type_e2 = e2->check();
    if (type_e1 != Int || type_e2 != Int) {
        class_table->semant_error(symbol_filename, this) << "non-Int arguments: " << type_e1 << " < " << type_e2
                                                         << ".\n";
    }
    set_type(Bool);
    return Bool;
}

Symbol eq_class::check() {
    auto type_e1 = e1->check();
    auto type_e2 = e2->check();
    auto base = false;
    for (auto type: {Int, Bool, Str}) {
        if (strcmp(type_e1->get_string(), type->get_string()) == 0 ||
            strcmp(type_e2->get_string(), type->get_string()) == 0) {
            base = true;
            break;
        }
    }
    if (base && type_e1 != type_e2) {
        class_table->semant_error(symbol_filename, this) << "Illegal comparison with a basic type.\n";
    }
    set_type(Bool);
    return Bool;
}

Symbol leq_class::check() {
    auto type_e1 = e1->check();
    auto type_e2 = e2->check();
    if (type_e1 != Int || type_e2 != Int) {
        class_table->semant_error(symbol_filename, this) << "non-Int arguments: " << type_e1 << " <= " << type_e2
                                                         << ".\n";
    }
    set_type(Bool);
    return Bool;
}

Symbol comp_class::check() {
    auto type_e1 = e1->check();
    if (type_e1 != Bool) {
        class_table->semant_error(symbol_filename, this) << "Argument of 'not' has type " << type_e1
                                                         << " instead of Bool.\n";
    }
    set_type(Bool);
    return Bool;
}

Symbol int_const_class::check() {
    set_type(Int);
    return Int;
}

Symbol bool_const_class::check() {
    set_type(Bool);
    return Bool;
}

Symbol string_const_class::check() {
    set_type(Str);
    return Str;
}

Symbol new__class::check() {
    set_type(type_name);
    return type_name;
}

Symbol isvoid_class::check() {
    e1->check();
    set_type(Bool);
    return Bool;
}

Symbol no_expr_class::check() {
    set_type(No_type);
    return No_type;
}

Symbol object_class::check() {
    if (strcmp(name->get_string(), "self") == 0) {
        set_type(SELF_TYPE);
        return self;
    }
    auto extra_found = find(extra_env, name);
    auto type = extra_found.second;
    if (extra_found.first.empty()) {
        auto &attrs = classes[current_class->get_string()].attrs;
        auto found = attrs.find(name->get_string());
        if (found != attrs.end()) {
            type = found->second;
        } else {
            class_table->semant_error(symbol_filename, this) << "Undeclared identifier " << name << "." << endl;
            type = Object;
        }
    }
    set_type(type);
    return type;
}

void method_class::check() {
    for (int i = 0; i < formals->len(); i++) {
        auto formal = formals->nth(i);
        extra_env.emplace_front(formal->get_name_str(), formal->get_type());
    }
    auto expr_type = expr->check();
    for (int i = 0; i < formals->len(); i++) {
        extra_env.pop_front();
    }
    if (strcmp(expr_type->get_string(), "self") == 0) {
        expr_type = SELF_TYPE;
    }
    auto expr_type_ = expr_type;
    auto return_type_node = class_root->find(return_type);
    if (strcmp(return_type->get_string(), "SELF_TYPE") != 0 && return_type_node == nullptr) {
        class_table->semant_error(symbol_filename, this) << "Undeclared return type " << return_type << "." << endl;
    } else if (!can_assign_to(return_type, expr_type_)) {
        class_table->semant_error(symbol_filename, this) << "Inferred return type " << expr_type << " of method "
                                                         << name << " does not conform to declared return type "
                                                         << return_type << ".\n";
    }
}

void attr_class::check() {
    auto type = init->check();
    if (strcmp(type->get_string(), "self") == 0) {
        type = current_class;
    }
    if (type != No_type && !can_assign_to(type_decl, type)) {
        class_table->semant_error(symbol_filename, this) << "Inferred type " << type << " of initialization of "
                                                         << name << " does not conform to declared type "
                                                         << type_decl << "." << endl;
    }
}

void check(Class_ class_) {
    current_class = class_->get_name();
    auto features = class_->get_features();
    for (int i = 0; i < features->len(); i++) {
        features->nth(i)->check();
    }
}

void check(Classes classes_) {
    for (int i = 0; i < classes_->len(); i++) {
        check(classes_->nth(i));
    }
}

void ClassTable::dump(Class *c, Classes_ &classes_) {
    if (c == nullptr) {
        return;
    }
    if (c->parent == nullptr) {
        cout << c->cls->get_name() << ":\n";
    } else {
        cout << c->cls->get_name() << " inherits "
             << c->parent->cls->get_name() << ":\n";
    }
    cout << "    "
         << "methods: ";
    auto cls = classes_[c->cls->get_name_str()];
    for (auto &p: cls.methods) {
        cout << p.first << "(";
        std::for_each(p.second.arguments.begin(), p.second.arguments.end(),
                      [](auto &s) { cout << s.first << ": " << s.second << ", "; });
        if (!p.second.arguments.empty()) {
            cout << "\b\b";
        }
        cout << "): " << p.second.return_type << "\n             ";
    }
    cout << "\n    "
         << "attrs:   ";
    for (auto &p: cls.attrs) {
        cout << p.first << ": " << p.second
             << "\n             ";
    }
    cout << "\n\n";
    for (auto cs: c->children) {
        dump(cs, classes_);
    }
}

void ClassTable::dump() {
    dump(class_root, m_classes);
    cout << flush << endl;
}

/*   This is the entry point to the semantic checker.

     Your checker should do the following two things:

     1) Check that the program is semantically correct
     2) Decorate the abstract syntax tree with type information
        by setting the `type' field in each Expression node.
        (see `tree.h')

     You are free to first do 1), make sure you catch all semantic
     errors. Part 2) can be done in a second stage, when you want
     to build mycoolc.
 */
void program_class::semant() {
    initialize_constants();

    /* ClassTable constructor may do some semantic analysis */
    ClassTable *classtable = new ClassTable(classes);
    if (classtable->errors()) {
        cerr << "Compilation halted due to static semantic errors." << endl;
        exit(1);
    }
    class_table = classtable;
    class_root = class_table->class_root;

    /* some semantic analysis code may go here */
    check(classes);

    if (classtable->errors()) {
        cerr << "Compilation halted due to static semantic errors." << endl;
        exit(1);
    }
}
