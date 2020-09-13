#ifndef GC_H
#define GC_H
#include <unordered_map>
#include <vector>
#include <stdio.h>
#include <stdint.h>

class GCUtil {
    public:
        static unsigned long MEMORY_LIMIT;
        static unsigned long CURRENT_MEMORY;
        static void *stack_bottom;
        static std::unordered_map<uintptr_t, size_t> pointers;
};

extern "C" void gcInit(void *bottom);
void *gmalloc(size_t size);
void gfree(void *pointer);
void collect();
std::vector<int> crawl_memory(void *bottom, void *top);
void depth_first_search();
void gcStop();
#endif