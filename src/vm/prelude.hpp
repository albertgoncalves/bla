#ifndef __PRELUDE_H__
#define __PRELUDE_H__

#include <stdint.h>
#include <stdio.h>

typedef int32_t  i32;
typedef uint32_t u32;

#define null nullptr

typedef FILE        File;
typedef struct stat FileStat;

#define OK    0
#define ERROR 1

#define EXIT()                                                       \
    {                                                                \
        fprintf(stderr, "%s:%s:%d\n", __FILE__, __func__, __LINE__); \
        _exit(ERROR);                                                \
    }

#define EXIT_WITH(x)                                                         \
    {                                                                        \
        fprintf(stderr, "%s:%s:%d `%s`\n", __FILE__, __func__, __LINE__, x); \
        _exit(ERROR);                                                        \
    }

#define EXIT_IF(condition)     \
    if (condition) {           \
        EXIT_WITH(#condition); \
    }

#define STATIC_ASSERT(condition) static_assert(condition, "!(" #condition ")")

#endif
