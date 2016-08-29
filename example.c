#include <stdio.h>

int main(void) {
    int acc = 0;
    static int mem[0];
    goto qd;

qd:
    acc = 1;
    goto na;

na:
    printf("%d\n", acc);
}
