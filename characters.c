#include "test.h"
#include <stdio.h>

// auto-header: test.h

static int ft_isupper(char c) {
        return c >= 'A' && c <= 'Z';
}

int ft_tolower(char c) {
        return ft_isupper(c) ? c + 32 : c;
}

static int ft_islower(char c) {
        return c >= 'a' && c <= 'z';
}

int ft_toupper(char c) {
        return ft_islower(c) ? c - 32 : c;
}
