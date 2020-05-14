#include "glamc_stdlib.h"
#include <stdio.h>
#include <string.h>

extern "C" void printb(int d) {
    if (d == 0) {
        printf("%s\n", "false");
    } else {
        printf("%s\n", "true");
    }
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