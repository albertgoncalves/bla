#ifndef __PRELUDE_H__
#define __PRELUDE_H__

#include <stdint.h>
#include <stdio.h>
#include <time.h>
#include <unistd.h>

typedef int32_t i32;

typedef uint32_t u32;
typedef uint64_t u64;

typedef struct timespec Time;
typedef struct stat     FileStat;

#define U64_MAX 0xFFFFFFFFFFFFFFFF
#define U32_MAX 0xFFFFFFFF

#define SECOND_TO_MICRO 1000000
#define MILLI_TO_MICRO  1000
#define MICRO_TO_NANO   1000

#define OK    0
#define ERROR 1

typedef enum {
    FALSE = 0,
    TRUE  = 1,
} Bool;

#define EXIT()                                                       \
    {                                                                \
        fflush(stdout);                                              \
        fprintf(stderr, "%s:%s:%d\n", __FILE__, __func__, __LINE__); \
        _exit(ERROR);                                                \
    }

#define EXIT_WITH(x)                                                         \
    {                                                                        \
        fflush(stdout);                                                      \
        fprintf(stderr, "%s:%s:%d `%s`\n", __FILE__, __func__, __LINE__, x); \
        _exit(ERROR);                                                        \
    }

#define EXIT_IF(condition)     \
    if (condition) {           \
        EXIT_WITH(#condition); \
    }

#define STATIC_ASSERT(condition) _Static_assert(condition, "!(" #condition ")")

static u64 get_monotonic(Time* time) {
    EXIT_IF(clock_gettime(CLOCK_MONOTONIC, time));
    return (((u64)time->tv_sec) * SECOND_TO_MICRO) +
           (((u64)time->tv_nsec) / MICRO_TO_NANO);
}

#endif
