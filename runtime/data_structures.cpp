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
        fprintf(stderr, "Fatal Error: Index Out Of Bounds");
        exit(1);
    }
    return str->elements[index];
}

extern "C" void prints(struct String *str) {
    printf("%s\n", str->elements);
}

extern "C" void concat(struct String *l, struct String *r, struct String *n_str);