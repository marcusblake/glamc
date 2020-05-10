#ifndef GLAMC_STDLIB_H
#define GLAMC_STDLIB_H
    #include "data_structures.h"
    extern "C" void printchar(char d);
    extern "C" void printint(int d);
    extern "C" void printbool(int d);
    extern "C" void printfloat(float d);
    extern "C" void printstr(struct String *str);
#endif