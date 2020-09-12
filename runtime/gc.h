#ifndef GC_H
#define GC_H
#include <unordered_map>
#include <vector>
#include <stdio.h>
#include <stdint.h>

unsigned long MEMORY_LIMIT = 1024;
unsigned long CURRENT_MEMORY;

void *stack_bottom;
std::unordered_map<uintptr_t, size_t> pointers;

extern "C" void gcInit(void *bottom);
void *gmalloc(size_t size);
void gfree(void *pointer);
void collect();
std::vector<int> crawl_memory(void *bottom, void *top);
void depth_first_search();
void gcStop();
#endif