#include "data_structures.h"
#include <vector>
#include <cstring>
#include <stdlib.h>

extern "C" void initString(struct String *str, char *elements) {
    int n = strlen(elements) + 1;
    str->elements = new char[n];
    str->length = n-1;
    memcpy(str->elements, elements, n);
}

extern "C" char getChar(struct String str, int index) {
    if (index < 0 || index >= str.length) {
        fprintf(stderr, "Fatal Error: Index Out Of Bounds\n");
        exit(1);
    }
    return str.elements[index];
}

extern "C" int lenstr(struct String str) {
    return str.length;
}

extern "C" void prints(struct String str) {
    printf("\"%s\"\n", str.elements);
}

extern "C" void initList(struct List *list, int element_size) {
    list->length = 0;
    list->element_size = element_size;
    list->list = reinterpret_cast<char *>(new std::vector<char *>());
}

extern "C" void getElement(struct List list, int index, void *ret) {
    std::vector<char *>* current_list = reinterpret_cast<std::vector<char *>*>(list.list);
    int n = (int)current_list->size();
    if (index < 0 || index >= n) {
        fprintf(stderr, "Fatal Error: Index Out Of Bounds\n");
        exit(1);
    }
    char *element = current_list->at(index);
    memcpy(ret, element, list.element_size);
}

extern "C" void addElement(struct List list, void *element) {
    std::vector<char *>* current_list = reinterpret_cast<std::vector<char *>*>(list.list);
    char *copy = new char[list.element_size];
    memcpy(copy, element, list.element_size);
    current_list->push_back(copy);
}