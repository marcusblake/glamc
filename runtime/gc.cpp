#include "gc.h"
#include <unordered_set>
#include <stack>

extern "C" void gcInit(void *bottom) {
    stack_bottom = bottom;
    CURRENT_MEMORY = 0;
}

std::vector<int> crawl_memory(void *bottom, void *top) {
    std::vector<int> addresses;
}

void collect() {
    char *a;
    void *stack_top = (void *)&a;
    std::vector<uintptr_t> roots = crawl_memory(stack_bottom, stack_top);
    std::unordered_set<uintptr_t> visited;

    for (uintptr_t &root : roots) {
        depth_first_search(root, visited);
    }

    for (auto &pair : pointers) {
        uintptr_t pointer = pair.first;
        if (visited.find(pointer) == visited.end()) {
            void *unused_memory = (void *)pointer;
            gfree(unused_memory);
        }
    }
}

void depth_first_search(uintptr_t root, std::unordered_set<uintptr_t> &visited) {
    std::stack<uintptr_t> q = { root };

    while (!q.empty()) {
        uintptr_t node = stack.pop();
        char *begin_buffer = (char *)node;
        char *end_buffer = begin_buffer + pointers[node]; // Get the end of the allocated buffer to prevent accessing invalid memory
        std::vector<uintptr_t> neighbors = crawl_memory(begin_buffer, end_buffer);
        for (uintptr_t &neighbor : neighbors) {
            if (visited.find(neighbor) == visited.end()) {
                visited.insert(neighbor);
                stack.push(neighbor);
            }
        }
    }
}

void *gmalloc(size_t size) {
    if (CURRENT_MEMORY >= MEMORY_LIMIT) {
        collect();
        MEMORY_LIMIT *= 2;
    }
    void *a = malloc(size);
    if (a == NULL) {
        fprintf(stderr, "Fatal Error: Bad Malloc!");
        exit(1);
    }
    CURRENT_MEMORY += size;
    uintptr_t address = (uintptr_t)a;
    pointers[address] = size;
    return a;
}

void gfree(void *pointer) {
    free(pointer);
    uintptr_t address = (uintptr_t)pointer;
    if (pointers.find(address) != pointers.end()) {
        CURRENT_MEMORY -= pointers[address];
        pointers.erase(address);
    }
}