#include "data_structures.h"
#include <vector>
#include <cstring>
#include <string>
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

extern "C" void concat(struct String *l, struct String *r, struct String *n_str) {
    int lLength = l->length;
    int rLength = r->length + 1; /* Add null terminator from this string */
    int n = lLength + rLength;
    n_str->length = n-1;
    n_str->elements = new char[n];
    memcpy(n_str->elements, l->elements, lLength);
    memcpy(n_str->elements + lLength, r->elements, rLength);
}

extern "C" bool compare_string(struct String *l, struct String *r, bool op) {
    int ans;
    ans = strcmp(l->elements, r->elements);
    if (op == 0) { /* equals operation */
        return ans == 0;
    }
    return ans != 0;
    
}

extern "C" void prints(struct String *str) {
    printf("%s\n", str->elements);
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

extern "C" void subSequence(struct List *list, int start, int end, struct List *newList) {
    int n = lenlist(list);
    if (start < 0 || start > n || end < 0 || end > n || start > end) {
        fprintf(stderr, "Fatal Error: Invalid Indices\n");
        exit(1);
    }
    initList(newList, list->element_size, 0, (char *)NULL); /* Initialize list */
    std::vector<char *>* current_list = reinterpret_cast<std::vector<char *>*>(list->list);
    std::vector<char *>* copy_list = reinterpret_cast<std::vector<char *>*>(newList->list);
    int element_size = list->element_size;
    for (int i = start; i < end; i++) {
        char *element = current_list->at(i);
        char *copy = new char[element_size];   
        memcpy(copy, element, element_size);
        copy_list->push_back(copy);
    }
}

extern "C" void make(struct List *list, int element_size, int num, char *initializer) {
    list->length = num;
    list->element_size = element_size;
    std::vector<char *>* array = new std::vector<char *>();
    for (int count = 0; count < num; count++) {
        char *element = new char[element_size];
        memcpy(element, initializer, element_size);
        array->push_back(element);
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


extern "C" void split(struct String *str, char delimeter, struct List *list) {
    /* Initialize list */
    initList(list, sizeof(struct String), 0, (char *)0);

    std::string temp;
    struct String newString;
    for (int index = 0; index < str->length; index++) {
        char current = str->elements[index];
        if (current == delimeter) {
            char *c_string = const_cast<char*>(temp.c_str());
            initString(&newString, c_string);
            addElement(list, (char *)&newString);
            temp = "";
        } else {
            temp += current;
        }
    }

    if (temp != "") {
        char *c_string = const_cast<char*>(temp.c_str());
        initString(&newString, c_string);
        addElement(list, (char *)&newString);
    }
}


extern "C" void join(struct List *list, char delimeter,  struct String *str) {
    std::string temp = "";
    struct String my_string;
    int n = list->length;
    for (int i = 0; i < n; i++) {
        getElement(list, i, (char *)&my_string);
        temp.append(my_string.elements);
        if (i != n-1) {
            temp.append(1, delimeter);
        }
    }
    char *c_string = const_cast<char*>(temp.c_str());
    initString(str, c_string);
}


