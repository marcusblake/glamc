#include "gc.h"
#include <unordered_map>



extern "C" void *gmalloc(size_t size) {
    void *a = malloc(size);
    if (a == NULL) {
        fprintf(stderr, "Fatal Error: Bad Malloc!");
        exit(1);
    }
    return a;
}