#ifndef GC_H
#define GC_H
#include <unordered_map>
#include <vector>
#include <stdio.h>
#include <stdint.h>
#include <string.h>

class GCUtil {
    public:
        static unsigned long MEMORY_LIMIT;
        static unsigned long CURRENT_MEMORY;
        static void *stack_bottom;
        static std::unordered_map<uintptr_t, size_t> pointers;
};

unsigned long GCUtil::MEMORY_LIMIT = 1024;
unsigned long GCUtil::CURRENT_MEMORY = 0;
void *GCUtil::stack_bottom = nullptr;
std::unordered_map<uintptr_t, size_t> GCUtil::pointers;

extern "C" void gcInit(void *bottom);
void gfree(void *pointer);
void collect();
std::vector<int> crawl_memory(void *bottom, void *top);
void depth_first_search();
void gcStop();
#endif