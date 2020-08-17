#ifndef GC_H
#define GC_H
#include <unordered_set>
#include <stdio.h>

class HeapNodes {
    std::unordered_set<void *> pointers;
};

extern "C" void gcInit(void *bottom);
extern "C" void *gmalloc(size_t size);
extern "C" void gfree(void *pointer);
extern "C" void crawl_stack();
extern "C" void mark();
extern "C" void sweep();
extern "C" void depth_first_search();
extern "C" void gcStop();
#endif