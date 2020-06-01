#include "glamc_stdlib.h"
#include <stdio.h>
#include <string.h>
#include <fstream>

extern "C" void printb(int d) {
    if (d == 0) {
        printf("%s\n", "false");
    } else {
        printf("%s\n", "true");
    }
}


extern "C" void read(struct String *filename, struct String *content) {
    std::ifstream fs(filename->elements);

    fs.seekg(0, std::ios::end); /* Seek to end of file */
    size_t size = fs.tellg(); /* Get size of file */
    fs.seekg(0, std::ios::beg);

    char *buffer = new char[size + 1];
    fs.read(buffer, size);
    buffer[size] = 0; /* Add null terminator */

    fs.close();

    initString(content, buffer); /* Create string using file contents */

    delete[] buffer;
}

extern "C" void write(struct String *filename, struct String *content) {
    std::ofstream fs(filename->elements);
    size_t size = (size_t)lenstr(content);
    fs.write(content->elements, size);
}


// extern "C" int string_to_int(string s) {
//     return atoi(s);
// }

// extern "C" string int_to_string(int i) {
//     return to_string(i);
// }

extern "C" int float_to_int(float f) {
    return (int)f;
}

extern "C" float int_to_float(int i) {
    return (float)i;
}

extern "C" int char_to_int(char c) {
    return (int)c;
}

extern "C" char int_to_char(int i) {
    return (char)i;
}