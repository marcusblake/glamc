#ifndef GLAMC_STDLIB_H
#define GLAMC_STDLIB_H
    #include "data_structures.h"
    extern "C" void printb(int d);
    extern "C" void read(struct String *filename, struct String *buffer);
    extern "C" void write(struct String *filename, struct String *content);
    // extern "C" int string_to_int(string s);
    // extern "C" string int_to_string(int i);
    extern "C" int float_to_int(float f);
    extern "C" float int_to_float(int i);
    extern "C" int char_to_int(char c);
    extern "C" char int_to_char(int i);
#endif