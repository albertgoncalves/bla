#include "prelude.hpp"

#include <fcntl.h>
#include <stdlib.h>
#include <sys/mman.h>
#include <sys/stat.h>
#include <unistd.h>

#define CAP_STACK   (1 << 10)
#define CAP_THREADS 1

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
    INST_PRCH = 16,
    INST_PRI32 = 17,
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

static void inst_halt(Thread* thread) {
    thread->alive = false;
}

static void inst_push(Program program, Thread* thread) {
    EXIT_IF(program.insts_len < thread->insts_index);
    u32 value = program.insts[thread->insts_index++];
    EXIT_IF(CAP_STACK <= thread->stack.top);
    thread->stack.nodes[thread->stack.top++].as_u32 = value;
}

static void inst_copy(Program program, Thread* thread) {
    EXIT_IF(thread->stack.top == 0);
    EXIT_IF(program.insts_len < thread->insts_index);
    u32 offset = program.insts[thread->insts_index++];
    EXIT_IF((thread->stack.top - 1) < offset);
    Node node = thread->stack.nodes[(thread->stack.top - 1) - offset];
    EXIT_IF(CAP_STACK <= thread->stack.top);
    thread->stack.nodes[thread->stack.top++].as_u32 = node.as_u32;
}

static void inst_store(Program program, Thread* thread) {
    EXIT_IF(thread->stack.top == 0);
    Node node = thread->stack.nodes[--thread->stack.top];
    EXIT_IF(program.insts_len < thread->insts_index);
    u32 offset = program.insts[thread->insts_index++];
    EXIT_IF((thread->stack.top - 1) < offset);
    thread->stack.nodes[(thread->stack.top - 1) - offset].as_u32 = node.as_u32;
}

static void inst_drop(Program program, Thread* thread) {
    EXIT_IF(program.insts_len < thread->insts_index);
    u32 n = program.insts[thread->insts_index++];
    EXIT_IF(n == 0);
    EXIT_IF(thread->stack.top < n);
    thread->stack.top -= n;
}

static void inst_rsrv(Program program, Thread* thread) {
    EXIT_IF(program.insts_len < thread->insts_index);
    u32 n = program.insts[thread->insts_index++];
    EXIT_IF(n == 0);
    thread->stack.top += n;
    EXIT_IF(CAP_STACK < thread->stack.top);
}

static void inst_swap(Thread* thread) {
    EXIT_IF(thread->stack.top < 2);
    Node r = thread->stack.nodes[--thread->stack.top];
    Node l = thread->stack.nodes[--thread->stack.top];
    thread->stack.nodes[thread->stack.top++] = r;
    thread->stack.nodes[thread->stack.top++] = l;
}

static void inst_jump(Thread* thread) {
    EXIT_IF(thread->stack.top == 0);
    thread->insts_index = thread->stack.nodes[--thread->stack.top].as_u32;
}

static void inst_jifz(Thread* thread) {
    EXIT_IF(thread->stack.top < 2);
    Node condition = thread->stack.nodes[--thread->stack.top];
    Node jump = thread->stack.nodes[--thread->stack.top];
    if (condition.as_u32 == 0) {
        thread->insts_index = jump.as_u32;
    }
}

#define INST_BINOP(f, op, type)                              \
    static void f(Thread* thread) {                          \
        EXIT_IF(thread->stack.top < 2);                      \
        Node r = thread->stack.nodes[--thread->stack.top];   \
        Node l = thread->stack.nodes[--thread->stack.top];   \
        thread->stack.nodes[thread->stack.top++].as_##type = \
            l.as_##type op r.as_##type;                      \
    }

INST_BINOP(inst_add, +, i32)
INST_BINOP(inst_sub, -, i32)
INST_BINOP(inst_mul, *, i32)
INST_BINOP(inst_div, /, i32)
INST_BINOP(inst_eq, ==, u32)

#define INST_UNOP(f, op, type)                                        \
    static void f(Thread* thread) {                                   \
        EXIT_IF(thread->stack.top == 0);                              \
        thread->stack.nodes[thread->stack.top - 1].as_##type =        \
            op(thread->stack.nodes[thread->stack.top - 1].as_##type); \
    }

INST_UNOP(inst_neg, -, i32)
INST_UNOP(inst_not, !, u32)

static void inst_prch(Thread* thread) {
    EXIT_IF(thread->stack.top == 0);
    putchar(thread->stack.nodes[--thread->stack.top].as_i32);
}

static void inst_pri32(Thread* thread) {
    EXIT_IF(thread->stack.top == 0);
    printf("%d", thread->stack.nodes[--thread->stack.top].as_i32);
}

static void step(Program program, Thread* thread) {
    EXIT_IF(program.insts_len < thread->insts_index);
    switch (static_cast<Inst>(program.insts[thread->insts_index++])) {
    case INST_HALT: {
        return inst_halt(thread);
    }
    case INST_PUSH: {
        return inst_push(program, thread);
    }
    case INST_COPY: {
        return inst_copy(program, thread);
    }
    case INST_STORE: {
        return inst_store(program, thread);
    }
    case INST_DROP: {
        return inst_drop(program, thread);
    }
    case INST_RSRV: {
        return inst_rsrv(program, thread);
    }
    case INST_SWAP: {
        return inst_swap(thread);
    }
    case INST_JUMP: {
        return inst_jump(thread);
    }
    case INST_JIFZ: {
        return inst_jifz(thread);
    }
    case INST_ADD: {
        return inst_add(thread);
    }
    case INST_SUB: {
        return inst_sub(thread);
    }
    case INST_MUL: {
        return inst_mul(thread);
    }
    case INST_DIV: {
        return inst_div(thread);
    }
    case INST_EQ: {
        return inst_eq(thread);
    }
    case INST_NEG: {
        return inst_neg(thread);
    }
    case INST_NOT: {
        return inst_not(thread);
    }
    case INST_PRCH: {
        return inst_prch(thread);
    }
    case INST_PRI32: {
        return inst_pri32(thread);
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
    EXIT_IF(thread->stack.top != 0);
    return EXIT_SUCCESS;
}
