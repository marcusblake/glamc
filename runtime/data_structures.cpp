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

extern "C" char getChar(struct String *str, int index) {
    if (index < 0 || index >= str->length) {
        fprintf(stderr, "Fatal Error: Index Out Of Bounds\n");
        exit(1);
    }
    return str->elements[index];
}

extern "C" int lenstr(struct String *str) {
    return str->length;
}

extern "C" void prints(struct String *str) {
    printf("\"%s\"\n", str->elements);
}

extern "C" void initList(struct List *list, int element_size, int num, char *elements) {
    list->length = 0;
    list->element_size = element_size;
    std::vector<char *>* array = new std::vector<char *>();
    for (int count = 0; count < num; count++) {
        char *element = new char[element_size];
        char *curr = elements + element_size * count;
        memcpy(element, curr, element_size);
        array->push_back(element);
        list->length++;
    }
    list->list = reinterpret_cast<char *>(array);
}

extern "C" void getElement(struct List *list, int index, char *ret) {
    std::vector<char *>* current_list = reinterpret_cast<std::vector<char *>*>(list->list);
    int n = (int)current_list->size();
    if (index < 0 || index >= n) {
        fprintf(stderr, "Fatal Error: Index Out Of Bounds\n");
        exit(1);
    }
    char *element = current_list->at(index);
    memcpy(ret, element, list->element_size);
}

extern "C" void addElement(struct List *list, char *element) {
    std::vector<char *>* current_list = reinterpret_cast<std::vector<char *>*>(list->list);
    char *copy = new char[list->element_size];
    memcpy(copy, element, list->element_size);
    list->length++;
    current_list->push_back(copy);
}




extern "C" void setElement(struct List *list, int index, char *element) {
    std::vector<char *>* current_list = reinterpret_cast<std::vector<char *>*>(list->list);
    int n = (int)current_list->size();
    if (index < 0 || index >= n) {
        fprintf(stderr, "Fatal Error: Index Out Of Bounds\n");
        exit(1);
    }
    char *copy = new char[list->element_size];
    memcpy(copy, element, list->element_size);
    current_list->at(index) = copy;
}

extern "C" void popElement(struct List *list) {
    std::vector<char *>* current_list = reinterpret_cast<std::vector<char *>*>(list->list);
    int n = (int)current_list->size();
    if (n == 0) {
        fprintf(stderr, "Fatal Error: Pop from empty list\n");
        exit(1);
    }
    current_list->pop_back(); /* Let the memory leak? Will do garbage collection eventually */
    list->length--;
}


extern "C" int lenlist(struct List *list) {
    std::vector<char *>* current_list = reinterpret_cast<std::vector<char *>*>(list->list);
    return (int)current_list->size();
}



