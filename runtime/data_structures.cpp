#include "data_structures.h"
#include "gc.h"
#include <vector>
#include <cstring>
#include <string>
#include <stdlib.h>

extern "C" char *gmalloc(int size) {
    if (GCUtil::CURRENT_MEMORY >= GCUtil::MEMORY_LIMIT) {
        // collect();
        GCUtil::MEMORY_LIMIT *= 2;
    }
    char *a = (char *)malloc(size);
    if (a == NULL) {
        fprintf(stderr, "Fatal Error: Bad Malloc!");
        exit(1);
    }
    memset(a, 0, size);
    GCUtil::CURRENT_MEMORY += size;
    uintptr_t address = (uintptr_t)a;
    // printf("Memory address %lu\n", address);
    // printf("Current stuff %lu\n", GCUtil::CURRENT_MEMORY);
    GCUtil::pointers[address] = size;
    return a;
}

extern "C" void initString(struct String *str, char *elements) {
    int n = strlen(elements) + 1;
    str->elements = (char *)gmalloc((size_t)n);
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
    n_str->elements = (char *)gmalloc((size_t)n);
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
    // fprintf(stderr, "hi initList\n");
    list->length = 0;
    list->element_size = element_size;
    int max_size = num + 1000; // set a buffer for array
    list->max_size = max_size;
    char *array = (char *) gmalloc((size_t)max_size * element_size);
    char *curr_element;
    char *curr_array;
    for (int count = 0; count < num; count++) {
        curr_element = elements + element_size * count;
        curr_array = array + element_size * count;
        memcpy(curr_array, curr_element, element_size);
        list->length++;
    }
    list->list = array;
    // fprintf(stderr, "bye initList\n");
}

extern "C" void subSequence(struct List *list, int start, int end, struct List *newList) {
    int n = lenlist(list);
    if (start < 0 || start > n || end < 0 || end > n || start > end) {
        fprintf(stderr, "Fatal Error: Invalid Indices\n");
        exit(1);
    }
    char *elements = list->list + (list->element_size * start);
    initList(newList, list->element_size, end - start, elements); /* Initialize list */
}

extern "C" void make(struct List *list, int element_size, int num, char *initializer) {
    // fprintf(stderr, "start");
    list->length = 0;
    list->element_size = element_size;
    size_t size = num + 1000;
    char *array = (char *) gmalloc(size * sizeof(char *));
    char *curr;
    for (int count = 0; count < num; count++) {
        curr = array + element_size * count;
        memcpy(curr, initializer, element_size);
        list->length++;
    }
    // fprintf(stderr, "stuff");
    list->list = array;
}

extern "C" void getElement(struct List *list, int index, char *ret) {
    // fprintf(stderr, "hi getElement\n");
    char *current_list = list->list;
    int n = (int)list->length;
    if (index < 0 || index >= n) {
        fprintf(stderr, "Fatal Error: Index Out Of Bounds\n");
        exit(1);
    }
    char *element = current_list + list->element_size*index;
    memcpy(ret, element, list->element_size);
    // fprintf(stderr, "bye getElement\n");
}

extern "C" void addElement(struct List *list, char *element) {
    // fprintf(stderr, "hi addElement\n");
    char *current_list = list->list;
    if (list->length == list->max_size) {
        list->max_size *= 2;
        char *new_list = (char *) gmalloc(list->max_size * list->element_size);
        memcpy(new_list, current_list, list->element_size * list->length);
        list->list = new_list;
        current_list = new_list;
    }
    
    char *index = current_list + list->element_size*list->length;
    memcpy(index, element, list->element_size);
    list->length++;
    // fprintf(stderr, "bye addElement\n");
}

extern "C" void setElement(struct List *list, int index, char *element) {
    char *current_list = list->list;
    int n = (int)list->length;
    if (index < 0 || index >= n) {
        fprintf(stderr, "Fatal Error: Index Out Of Bounds\n");
        exit(1);
    }
    char *curr_element = current_list + list->element_size * index;
    memcpy(curr_element, element, list->element_size);
}

extern "C" void popElement(struct List *list) {
    char *current_list = list->list;
    int n = (int)list->length;
    if (n == 0) {
        fprintf(stderr, "Fatal Error: Pop from empty list\n");
        exit(1);
    }
    list->length--;
    char *index = current_list + list->element_size * list->length;
    memset(index, 0, list->element_size); /* Let the memory leak? Will do garbage collection eventually */
}


extern "C" int lenlist(struct List *list) {
    return (int)list->length;
}


extern "C" void split(struct String *str, char delimeter, struct List *list) {
    /* Initialize list */
    initList(list, sizeof(struct String*), 0, (char *)0);
    std::string temp;
    struct String *newString;
    for (int index = 0; index < str->length; index++) {
        newString = (struct String *)gmalloc(sizeof(struct String));
        char current = str->elements[index];
        if (current == delimeter) {
            char *c_string = const_cast<char*>(temp.c_str());
            initString(newString, c_string);
            addElement(list, (char *)&newString);
            temp = "";
        } else {
            temp += current;
        }
    }

    if (temp != "") {
        newString = (struct String *)gmalloc(sizeof(struct String));
        char *c_string = const_cast<char*>(temp.c_str());
        initString(newString, c_string);
        addElement(list, (char *)&newString);
    }
}


extern "C" void join(struct List *list, char delimeter,  struct String *str) {
    std::string temp = "";
    struct String *my_string;
    int n = list->length;
    for (int i = 0; i < n; i++) {
        getElement(list, i, (char *)&my_string);
        temp.append(my_string->elements);
        if (i != n-1) {
            temp.append(1, delimeter);
        }
    }
    char *c_string = const_cast<char*>(temp.c_str());
    initString(str, c_string);
}


