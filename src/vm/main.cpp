#include <fcntl.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <sys/mman.h>
#include <sys/stat.h>
#include <unistd.h>

typedef uint32_t u32;
typedef int32_t  i32;

#define null nullptr

typedef FILE        File;
typedef struct stat FileStat;

#define EXIT_SUCCESS 0
#define EXIT_FAILURE 1

#define CAP_STACK   (1 << 10)
#define CAP_THREADS 1

#define EXIT()                                                       \
    {                                                                \
        fprintf(stderr, "%s:%s:%d\n", __FILE__, __func__, __LINE__); \
        _exit(EXIT_FAILURE);                                         \
    }

#define EXIT_WITH(x)                                                         \
    {                                                                        \
        fprintf(stderr, "%s:%s:%d `%s`\n", __FILE__, __func__, __LINE__, x); \
        _exit(EXIT_FAILURE);                                                 \
    }

#define EXIT_IF(condition)     \
    if (condition) {           \
        EXIT_WITH(#condition); \
    }

enum Inst {
    INST_HALT = 0,
    INST_PUSH = 1,
    INST_COPY = 2,
    INST_STORE = 3,
    INST_DROP = 4,
    INST_RSRV = 5,
    INST_SWAP = 6,
    INST_JUMP = 7,
    INST_JIFZ = 8,
    INST_ADD = 9,
    INST_SUB = 10,
    INST_MUL = 11,
    INST_DIV = 12,
    INST_EQ = 13,
    INST_NEG = 14,
    INST_NOT = 15,
};

union Node {
    i32 as_i32;
    u32 as_u32;
};

struct Stack {
    Node nodes[CAP_STACK];
    u32  top;
};

struct Thread {
    Stack stack;
    u32   insts_index;
    bool  alive;
};

struct Program {
    const u32* insts;
    u32        insts_len;
};

struct Memory {
    Thread  threads[CAP_THREADS];
    u32     threads_len;
    Program program;
};

static void step(Program program, Thread* thread) {
    EXIT_IF(program.insts_len < thread->insts_index);
    switch (static_cast<Inst>(program.insts[thread->insts_index++])) {
    case INST_HALT: {
        thread->alive = false;
        break;
    }
    case INST_PUSH: {
        EXIT_IF(program.insts_len < thread->insts_index);
        u32 value = program.insts[thread->insts_index++];
        EXIT_IF(CAP_STACK <= thread->stack.top);
        thread->stack.nodes[thread->stack.top++].as_u32 = value;
        break;
    }
    case INST_COPY: {
        EXIT_IF(thread->stack.top == 0);
        EXIT_IF(program.insts_len < thread->insts_index);
        u32 offset = program.insts[thread->insts_index++];
        EXIT_IF((thread->stack.top - 1) < offset);
        Node node = thread->stack.nodes[(thread->stack.top - 1) - offset];
        EXIT_IF(CAP_STACK <= thread->stack.top);
        thread->stack.nodes[thread->stack.top++].as_u32 = node.as_u32;
        break;
    }
    case INST_STORE: {
        EXIT_IF(thread->stack.top == 0);
        Node node = thread->stack.nodes[--thread->stack.top];
        EXIT_IF(program.insts_len < thread->insts_index);
        u32 offset = program.insts[thread->insts_index++];
        EXIT_IF((thread->stack.top - 1) < offset);
        thread->stack.nodes[(thread->stack.top - 1) - offset].as_u32 =
            node.as_u32;
        break;
    }
    case INST_DROP: {
        EXIT_IF(program.insts_len < thread->insts_index);
        u32 n = program.insts[thread->insts_index++];
        EXIT_IF(n == 0);
        EXIT_IF(thread->stack.top < n);
        thread->stack.top -= n;
        break;
    }
    case INST_RSRV: {
        EXIT_IF(program.insts_len < thread->insts_index);
        u32 n = program.insts[thread->insts_index++];
        EXIT_IF(n == 0);
        thread->stack.top += n;
        EXIT_IF(CAP_STACK < thread->stack.top);
        break;
    }
    case INST_SWAP: {
        EXIT_IF(thread->stack.top < 2);
        Node r = thread->stack.nodes[--thread->stack.top];
        Node l = thread->stack.nodes[--thread->stack.top];
        thread->stack.nodes[thread->stack.top++] = r;
        thread->stack.nodes[thread->stack.top++] = l;
        break;
    }
    case INST_JUMP: {
        EXIT_IF(thread->stack.top == 0);
        thread->insts_index = thread->stack.nodes[--thread->stack.top].as_u32;
        break;
    }
    case INST_JIFZ: {
        EXIT_IF(thread->stack.top < 2);
        Node condition = thread->stack.nodes[--thread->stack.top];
        Node jump = thread->stack.nodes[--thread->stack.top];
        if (condition.as_u32 == 0) {
            thread->insts_index = jump.as_u32;
        }
        break;
    }
    case INST_ADD: {
        EXIT_IF(thread->stack.top < 2);
        Node r = thread->stack.nodes[--thread->stack.top];
        Node l = thread->stack.nodes[--thread->stack.top];
        thread->stack.nodes[thread->stack.top++].as_i32 = l.as_i32 + r.as_i32;
        break;
    }
    case INST_SUB: {
        EXIT_IF(thread->stack.top < 2);
        Node r = thread->stack.nodes[--thread->stack.top];
        Node l = thread->stack.nodes[--thread->stack.top];
        thread->stack.nodes[thread->stack.top++].as_i32 = l.as_i32 - r.as_i32;
        break;
    }
    case INST_MUL: {
        EXIT_IF(thread->stack.top < 2);
        Node r = thread->stack.nodes[--thread->stack.top];
        Node l = thread->stack.nodes[--thread->stack.top];
        thread->stack.nodes[thread->stack.top++].as_i32 = l.as_i32 * r.as_i32;
        break;
    }
    case INST_DIV: {
        EXIT_IF(thread->stack.top < 2);
        Node r = thread->stack.nodes[--thread->stack.top];
        Node l = thread->stack.nodes[--thread->stack.top];
        thread->stack.nodes[thread->stack.top++].as_i32 = l.as_i32 / r.as_i32;
        break;
    }
    case INST_EQ: {
        EXIT_IF(thread->stack.top < 2);
        Node r = thread->stack.nodes[--thread->stack.top];
        Node l = thread->stack.nodes[--thread->stack.top];
        thread->stack.nodes[thread->stack.top++].as_u32 = l.as_u32 == r.as_u32;
        break;
    }
    case INST_NEG: {
        EXIT_IF(thread->stack.top == 0);
        thread->stack.nodes[thread->stack.top - 1].as_i32 =
            -(thread->stack.nodes[thread->stack.top - 1].as_i32);
        break;
    }
    case INST_NOT: {
        EXIT_IF(thread->stack.top == 0);
        thread->stack.nodes[thread->stack.top - 1].as_i32 =
            !(thread->stack.nodes[thread->stack.top - 1].as_i32);
        break;
    }
    default:
        EXIT();
    }
}

static Memory* alloc_memory() {
    void* address = mmap(null,
                         sizeof(Memory),
                         PROT_READ | PROT_WRITE,
                         MAP_ANONYMOUS | MAP_PRIVATE,
                         -1,
                         0);
    EXIT_IF(address == MAP_FAILED);
    return reinterpret_cast<Memory*>(address);
}

static Thread* alloc_thread(Memory* memory) {
    EXIT_IF(CAP_THREADS <= memory->threads_len);
    Thread* thread = &memory->threads[memory->threads_len++];
    thread->insts_index = 0;
    thread->alive = true;
    return thread;
}

static Program read_program(const char* path) {
    i32 file = open(path, O_RDONLY);
    EXIT_IF(file < 0);
    FileStat stat;
    EXIT_IF(fstat(file, &stat) < 0)
    Program program = {};
    program.insts_len = static_cast<u32>(stat.st_size) / sizeof(u32);
    {
        void* address = mmap(null,
                             static_cast<u32>(stat.st_size),
                             PROT_READ,
                             MAP_SHARED,
                             file,
                             0);
        EXIT_IF(address == MAP_FAILED);
        program.insts = reinterpret_cast<u32*>(address);
    }
    close(file);
    return program;
}

i32 main(i32 n, const char** args) {
    if (n < 2) {
        fprintf(stderr,
                "$ %s path/to/bytecode.blc\n",
                realpath(args[0], null));
        _exit(EXIT_SUCCESS);
    }
    EXIT_IF(n < 2);
    Memory* memory = alloc_memory();
    Thread* thread = alloc_thread(memory);
    memory->program = read_program(args[1]);
    while (thread->alive) {
        step(memory->program, thread);
    }
    EXIT_IF(thread->stack.top != 1);
    printf("%d\n", thread->stack.nodes[thread->stack.top - 1].as_i32);
    return EXIT_SUCCESS;
}
