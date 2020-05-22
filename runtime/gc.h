#ifndef GC_H
#define GC_H
#include <unordered_set>
class HeapNodes {
    std::unordered_set<void *> pointers;
};

void gcInit(void *bottom);
void *gmalloc(size_t size);
void gfree(void *pointer);
void crawl_stack();
void mark();
void sweep();
void depth_first_search();
void gcStop();
#endif