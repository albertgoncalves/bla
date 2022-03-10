#include "prelude.hpp"

#include <fcntl.h>
#include <stdlib.h>
#include <sys/mman.h>
#include <sys/stat.h>
#include <unistd.h>

#define CAP_STACK   (1 << 12)
#define CAP_THREADS (1 << 4)
#define CAP_HEAP    (1 << 9)

#define THREAD_STEPS 100

enum Inst {
    INST_HALT  = 0,
    INST_PUSH  = 1,
    INST_COPY  = 2,
    INST_STORE = 3,
    INST_DROP  = 4,
    INST_RSRV  = 5,
    INST_SWAP  = 6,
    INST_JUMP  = 7,
    INST_JIFZ  = 8,
    INST_ADD   = 9,
    INST_SUB   = 10,
    INST_MUL   = 11,
    INST_DIV   = 12,
    INST_EQ    = 13,
    INST_NEG   = 14,
    INST_NOT   = 15,
    INST_ALLOC = 16,
    INST_SAVE  = 17,
    INST_READ  = 18,
    INST_HLEN  = 19,
    INST_SPAWN = 20,
    INST_PRCH  = 100,
    INST_PRI32 = 101,
};

STATIC_ASSERT(sizeof(Inst) <= sizeof(u32));

union Node {
    i32 as_i32;
    u32 as_u32;
};

struct Stack {
    Node nodes[CAP_STACK];
    u32  top;
};

struct Heap {
    u32 buffer[CAP_HEAP];
    u32 len;
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
    Heap    heap;
    Thread  threads[CAP_THREADS];
    u32     threads_len;
    Program program;
};

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

static Thread* alloc_thread(Memory* memory, u32 insts_index) {
    EXIT_IF(CAP_THREADS <= memory->threads_len);
    Thread* thread      = &memory->threads[memory->threads_len++];
    thread->insts_index = insts_index;
    thread->alive       = true;
    return thread;
}

static Program read_program(const char* path) {
    i32 file = open(path, O_RDONLY);
    EXIT_IF(file < 0);
    FileStat stat;
    EXIT_IF(fstat(file, &stat) < 0)
    Program program;
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

static void push_thread(Thread* thread, u32 x) {
    EXIT_IF(CAP_STACK <= thread->stack.top);
    thread->stack.nodes[thread->stack.top++].as_u32 = x;
}

static void inst_halt(Thread* thread) {
    thread->alive = false;
}

static void inst_push(Program program, Thread* thread) {
    EXIT_IF(program.insts_len <= thread->insts_index);
    u32 x = program.insts[thread->insts_index++];
    EXIT_IF(CAP_STACK <= thread->stack.top);
    thread->stack.nodes[thread->stack.top++].as_u32 = x;
}

static void inst_copy(Program program, Thread* thread) {
    EXIT_IF(thread->stack.top == 0);
    EXIT_IF(program.insts_len <= thread->insts_index);
    u32 offset = program.insts[thread->insts_index++];
    EXIT_IF((thread->stack.top - 1) < offset);
    Node node = thread->stack.nodes[(thread->stack.top - 1) - offset];
    EXIT_IF(CAP_STACK <= thread->stack.top);
    thread->stack.nodes[thread->stack.top++].as_u32 = node.as_u32;
}

static void inst_store(Program program, Thread* thread) {
    EXIT_IF(thread->stack.top == 0);
    Node node = thread->stack.nodes[--thread->stack.top];
    EXIT_IF(program.insts_len <= thread->insts_index);
    u32 offset = program.insts[thread->insts_index++];
    EXIT_IF(thread->stack.top == 0);
    EXIT_IF((thread->stack.top - 1) < offset);
    thread->stack.nodes[(thread->stack.top - 1) - offset].as_u32 = node.as_u32;
}

static void inst_drop(Program program, Thread* thread) {
    EXIT_IF(program.insts_len <= thread->insts_index);
    u32 n = program.insts[thread->insts_index++];
    EXIT_IF(n == 0);
    EXIT_IF(thread->stack.top < n);
    thread->stack.top -= n;
}

static void inst_rsrv(Program program, Thread* thread) {
    EXIT_IF(program.insts_len <= thread->insts_index);
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
    Node jump      = thread->stack.nodes[--thread->stack.top];
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

static void inst_alloc(Thread* thread, Heap* heap) {
    EXIT_IF(CAP_HEAP <= heap->len);
    EXIT_IF(thread->stack.top == 0);
    u32 n = thread->stack.nodes[--thread->stack.top].as_u32;
    EXIT_IF(n == 0);
    thread->stack.nodes[thread->stack.top++].as_u32 = heap->len;
    EXIT_IF(CAP_HEAP < (heap->len + n));
    heap->len += n;
}

static void inst_save(Thread* thread, Heap* heap) {
    EXIT_IF(thread->stack.top < 3);
    i32 offset     = thread->stack.nodes[--thread->stack.top].as_i32;
    i32 base       = thread->stack.nodes[--thread->stack.top].as_i32;
    i32 heap_index = base + offset;
    EXIT_IF(heap_index < 0);
    EXIT_IF(CAP_HEAP <= heap_index);
    heap->buffer[heap_index] = thread->stack.nodes[--thread->stack.top].as_u32;
}

static void inst_read(Thread* thread, Heap* heap) {
    EXIT_IF(thread->stack.top < 2);
    i32 offset     = thread->stack.nodes[--thread->stack.top].as_i32;
    i32 base       = thread->stack.nodes[--thread->stack.top].as_i32;
    i32 heap_index = base + offset;
    EXIT_IF(heap_index < 0);
    EXIT_IF(CAP_HEAP <= heap_index);
    thread->stack.nodes[thread->stack.top++].as_u32 = heap->buffer[heap_index];
}

static void inst_hlen(Thread* thread, Heap* heap) {
    EXIT_IF(thread->stack.top == 0);
    u32 heap_len = thread->stack.nodes[--thread->stack.top].as_u32;
    EXIT_IF(CAP_HEAP < heap_len);
    heap->len = heap_len;
}

static void inst_spawn(Memory* memory, Thread* thread_parent) {
    EXIT_IF(CAP_THREADS <= memory->threads_len);
    EXIT_IF(thread_parent->stack.top < 4);
    u32 func = thread_parent->stack.nodes[--thread_parent->stack.top].as_u32;
    u32 addr = thread_parent->stack.nodes[--thread_parent->stack.top].as_u32;
    u32 jump_child =
        thread_parent->stack.nodes[--thread_parent->stack.top].as_u32;
    u32 jump_parent =
        thread_parent->stack.nodes[--thread_parent->stack.top].as_u32;
    Thread* thread_child = alloc_thread(memory, thread_parent->insts_index);
    push_thread(thread_child, jump_child);
    push_thread(thread_child, addr);
    push_thread(thread_child, func);
    thread_parent->insts_index = jump_parent;
}

static void inst_prch(Thread* thread) {
    EXIT_IF(thread->stack.top == 0);
    putchar(thread->stack.nodes[--thread->stack.top].as_i32);
}

static void inst_pri32(Thread* thread) {
    EXIT_IF(thread->stack.top == 0);
    printf("%d", thread->stack.nodes[--thread->stack.top].as_i32);
}

static void step(Memory* memory, Thread* thread) {
    EXIT_IF(memory->program.insts_len <= thread->insts_index);
    switch (static_cast<Inst>(memory->program.insts[thread->insts_index++])) {
    case INST_HALT: {
        return inst_halt(thread);
    }
    case INST_PUSH: {
        return inst_push(memory->program, thread);
    }
    case INST_COPY: {
        return inst_copy(memory->program, thread);
    }
    case INST_STORE: {
        return inst_store(memory->program, thread);
    }
    case INST_DROP: {
        return inst_drop(memory->program, thread);
    }
    case INST_RSRV: {
        return inst_rsrv(memory->program, thread);
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
    case INST_ALLOC: {
        return inst_alloc(thread, &memory->heap);
    }
    case INST_SAVE: {
        return inst_save(thread, &memory->heap);
    }
    case INST_READ: {
        return inst_read(thread, &memory->heap);
    }
    case INST_HLEN: {
        return inst_hlen(thread, &memory->heap);
    }
    case INST_SPAWN: {
        return inst_spawn(memory, thread);
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

static bool any_alive(Memory* memory) {
    for (u32 i = 0; i < memory->threads_len; ++i) {
        if (memory->threads[i].alive) {
            return true;
        }
    }
    return false;
}

i32 main(i32 n, const char** args) {
    if (n < 2) {
        fprintf(stderr,
                "$ %s path/to/bytecode.blc\n",
                realpath(args[0], null));
        _exit(ERROR);
    }
    EXIT_IF(n < 2);
    Memory* memory      = alloc_memory();
    Thread* thread_main = alloc_thread(memory, 0);
    memory->program     = read_program(args[1]);
    while (any_alive(memory)) {
        for (u32 i = 0; i < memory->threads_len; ++i) {
            Thread* thread = &memory->threads[i];
            for (u32 j = 0; j < THREAD_STEPS; ++j) {
                if (!thread->alive) {
                    break;
                }
                step(memory, thread);
            }
        }
    }
    EXIT_IF(thread_main->stack.top != 0);
    return OK;
}
