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

#define NANO_PER_SECOND  1000000000llu
#define NANO_PER_MILLI   1000000llu
#define MICRO_PER_SECOND 1000000llu
#define NANO_PER_MICRO   (NANO_PER_SECOND / MICRO_PER_SECOND)

#define OK    0
#define ERROR 1

typedef enum {
    FALSE = 0,
    TRUE  = 1,
} Bool;

#if 1
    #define EXIT()                                                       \
        {                                                                \
            fflush(stdout);                                              \
            fprintf(stderr, "%s:%s:%d\n", __FILE__, __func__, __LINE__); \
            _exit(ERROR);                                                \
        }
    #define EXIT_WITH(x)               \
        {                              \
            fflush(stdout);            \
            fprintf(stderr,            \
                    "%s:%s:%d `%s`\n", \
                    __FILE__,          \
                    __func__,          \
                    __LINE__,          \
                    x);                \
            _exit(ERROR);              \
        }
    #define EXIT_IF(condition)     \
        if (condition) {           \
            EXIT_WITH(#condition); \
        }
#else
    #define EXIT() \
        {}
    #define EXIT_WITH(_) \
        {}
    #define EXIT_IF(condition) \
        if (condition) {       \
        }
#endif

#define STATIC_ASSERT(condition) _Static_assert(condition, "!(" #condition ")")

static u64 get_monotonic(void) {
    Time time;
    EXIT_IF(clock_gettime(CLOCK_MONOTONIC, &time));
    return (((u64)time.tv_sec) * NANO_PER_SECOND) + ((u64)time.tv_nsec);
}

#endif
