#ifndef SEMANT_H_
#define SEMANT_H_

#include "cool-tree.h"
#include "list.h"
#include "stringtab.h"
#include "symtab.h"
#include <cassert>
#include <iostream>
#include <map>
#include <vector>
#include <set>

#define TRUE 1
#define FALSE 0

class ClassTable;

typedef ClassTable *ClassTableP;

class Class {
public:
    Class *parent;
    std::vector<Class *> children;
    Class_ cls;

public:
    Class(Class_ c, Class *pa) : parent(pa), cls(c) {}

    explicit Class(Class_ c) : Class(c, nullptr) {}

    Class() : Class(nullptr) {}

    bool insert(Class_ father, Class_ child);

    bool insert(Symbol father, Class_ child);

    Class *find(Class_ f_cls);

    Class *find(Symbol name);
};

struct ClassMembers {
    struct method {
        std::vector<std::pair<std::string, Symbol>> arguments;
        Symbol return_type;
    };
    std::map<std::string, method> methods;
    std::map<std::string, Symbol> attrs;
};

struct Classes_ {
    static std::map<std::string, ClassMembers> classes;
public:
    ClassMembers &operator[](const std::string &name) {
        return classes[name];
    }
};

std::map<std::string, ClassMembers> Classes_::classes;

// This is a structure that may be used to contain the semantic
// information such as the inheritance graph.  You may use it or not as
// you like: it is only here to provide a container for the supplied
// methods.

class ClassTable {
private:
    int semant_errors;
    ostream &error_stream;
    Class_ base_classes[5];

    void install_basic_classes();

    void dump(Class *c, Classes_ &classes_);

public:
    Class *class_root;
    Classes_ m_classes;

    ClassTable(Classes);

    int errors() {
        return semant_errors;
    }

    ostream &semant_error();

    ostream &semant_error(Class_ c);

    ostream &semant_error(Symbol filename, tree_node *t);

    void dump();
};

#endif
