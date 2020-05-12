#include "glamc_stdlib.h"
#include <stdio.h>

extern "C" void printb(int d) {
    if (d == 0) {
        printf("%s\n", "true");
    } else {
        printf("%s\n", "false");
    }
}