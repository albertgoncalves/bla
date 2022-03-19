#include "prelude.hpp"

#include <fcntl.h>
#include <stdlib.h>
#include <sys/mman.h>
#include <sys/stat.h>

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

    INST_ADD = 9,
    INST_SUB = 10,
    INST_MUL = 11,
    INST_DIV = 12,

    INST_AND = 13,
    INST_OR  = 14,
    INST_SHL = 15,
    INST_SHR = 16,

    INST_NEG = 20,
    INST_NOT = 21,

    INST_EQ = 30,

    INST_ALLOC = 40,
    INST_SAVE  = 41,
    INST_READ  = 42,
    INST_HLEN  = 43,

    INST_SPAWN = 50,
    INST_SLPMS = 51,

    INST_PRCH  = 100,
    INST_PRI32 = 101,

    INST_EXIT = 200,
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
    bool  sleeping;
    u64   wake_at;
};

struct Program {
    const u32* insts;
    u32        insts_len;
};

struct Memory {
    Heap    heap;
    Thread  threads[CAP_THREADS];
    Thread* threads_stack[CAP_THREADS];
    u32     threads_alive;
    u32     threads_sleeping;
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
    Memory* memory = reinterpret_cast<Memory*>(address);
    for (u32 i = 0; i < CAP_THREADS; ++i) {
        memory->threads[i].alive = false;
        memory->threads_stack[i] = &memory->threads[i];
    }
    return memory;
}

static Thread* alloc_thread(Memory* memory, u32 insts_index) {
    EXIT_IF(CAP_THREADS <= memory->threads_alive);
    Thread* thread =
        memory->threads_stack[(CAP_THREADS - memory->threads_alive++) - 1];
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

static bool inst_halt(Memory* memory, Thread* thread) {
    EXIT_IF(memory->threads_alive == 0);
    thread->alive = false;
    memory->threads_stack[(CAP_THREADS - --memory->threads_alive) - 1] =
        thread;
    return false; // NOTE: Let's signal we should switch to another thread.
}

static bool inst_push(Program program, Thread* thread) {
    EXIT_IF(program.insts_len <= thread->insts_index);
    u32 x = program.insts[thread->insts_index++];
    EXIT_IF(CAP_STACK <= thread->stack.top);
    thread->stack.nodes[thread->stack.top++].as_u32 = x;
    return true; // NOTE: Let's signal we can continue executing this thread.
}

static bool inst_copy(Program program, Thread* thread) {
    EXIT_IF(thread->stack.top == 0);
    EXIT_IF(program.insts_len <= thread->insts_index);
    u32 offset = program.insts[thread->insts_index++];
    EXIT_IF((thread->stack.top - 1) < offset);
    Node node = thread->stack.nodes[(thread->stack.top - 1) - offset];
    EXIT_IF(CAP_STACK <= thread->stack.top);
    thread->stack.nodes[thread->stack.top++].as_u32 = node.as_u32;
    return true;
}

static bool inst_store(Program program, Thread* thread) {
    EXIT_IF(thread->stack.top == 0);
    Node node = thread->stack.nodes[--thread->stack.top];
    EXIT_IF(program.insts_len <= thread->insts_index);
    u32 offset = program.insts[thread->insts_index++];
    EXIT_IF(thread->stack.top == 0);
    EXIT_IF((thread->stack.top - 1) < offset);
    thread->stack.nodes[(thread->stack.top - 1) - offset].as_u32 = node.as_u32;
    return true;
}

static bool inst_drop(Program program, Thread* thread) {
    EXIT_IF(program.insts_len <= thread->insts_index);
    u32 n = program.insts[thread->insts_index++];
    EXIT_IF(n == 0);
    EXIT_IF(thread->stack.top < n);
    thread->stack.top -= n;
    return true;
}

static bool inst_rsrv(Program program, Thread* thread) {
    EXIT_IF(program.insts_len <= thread->insts_index);
    u32 n = program.insts[thread->insts_index++];
    EXIT_IF(n == 0);
    thread->stack.top += n;
    EXIT_IF(CAP_STACK < thread->stack.top);
    return true;
}

static bool inst_swap(Thread* thread) {
    EXIT_IF(thread->stack.top < 2);
    Node r = thread->stack.nodes[--thread->stack.top];
    Node l = thread->stack.nodes[--thread->stack.top];
    thread->stack.nodes[thread->stack.top++] = r;
    thread->stack.nodes[thread->stack.top++] = l;
    return true;
}

static bool inst_jump(Thread* thread) {
    EXIT_IF(thread->stack.top == 0);
    thread->insts_index = thread->stack.nodes[--thread->stack.top].as_u32;
    return true;
}

static bool inst_jifz(Thread* thread) {
    EXIT_IF(thread->stack.top < 2);
    Node condition = thread->stack.nodes[--thread->stack.top];
    Node jump      = thread->stack.nodes[--thread->stack.top];
    if (condition.as_u32 == 0) {
        thread->insts_index = jump.as_u32;
    }
    return true;
}

#define INST_BINOP(f, op, type)                              \
    static bool f(Thread* thread) {                          \
        EXIT_IF(thread->stack.top < 2);                      \
        Node r = thread->stack.nodes[--thread->stack.top];   \
        Node l = thread->stack.nodes[--thread->stack.top];   \
        thread->stack.nodes[thread->stack.top++].as_##type = \
            l.as_##type op r.as_##type;                      \
        return true;                                         \
    }

INST_BINOP(inst_add, +, i32)
INST_BINOP(inst_sub, -, i32)
INST_BINOP(inst_mul, *, i32)
INST_BINOP(inst_div, /, i32)
INST_BINOP(inst_and, &, u32)
INST_BINOP(inst_or, |, u32)
INST_BINOP(inst_shl, <<, u32)
INST_BINOP(inst_shr, >>, u32)
INST_BINOP(inst_eq, ==, u32)

#define INST_UNOP(f, op, type)                                        \
    static bool f(Thread* thread) {                                   \
        EXIT_IF(thread->stack.top == 0);                              \
        thread->stack.nodes[thread->stack.top - 1].as_##type =        \
            op(thread->stack.nodes[thread->stack.top - 1].as_##type); \
        return true;                                                  \
    }

INST_UNOP(inst_neg, -, i32)
INST_UNOP(inst_not, !, u32)

static bool inst_alloc(Thread* thread, Heap* heap) {
    EXIT_IF(CAP_HEAP <= heap->len);
    EXIT_IF(thread->stack.top == 0);
    u32 n = thread->stack.nodes[--thread->stack.top].as_u32;
    EXIT_IF(n == 0);
    thread->stack.nodes[thread->stack.top++].as_u32 = heap->len;
    EXIT_IF(CAP_HEAP < (heap->len + n));
    heap->len += n;
    return true;
}

static bool inst_save(Thread* thread, Heap* heap) {
    EXIT_IF(thread->stack.top < 3);
    i32 offset     = thread->stack.nodes[--thread->stack.top].as_i32;
    i32 base       = thread->stack.nodes[--thread->stack.top].as_i32;
    i32 heap_index = base + offset;
    EXIT_IF(heap_index < 0);
    EXIT_IF(CAP_HEAP <= heap_index);
    heap->buffer[heap_index] = thread->stack.nodes[--thread->stack.top].as_u32;
    return true;
}

static bool inst_read(Thread* thread, Heap* heap) {
    EXIT_IF(thread->stack.top < 2);
    i32 offset     = thread->stack.nodes[--thread->stack.top].as_i32;
    i32 base       = thread->stack.nodes[--thread->stack.top].as_i32;
    i32 heap_index = base + offset;
    EXIT_IF(heap_index < 0);
    EXIT_IF(CAP_HEAP <= heap_index);
    thread->stack.nodes[thread->stack.top++].as_u32 = heap->buffer[heap_index];
    return true;
}

static bool inst_hlen(Thread* thread, Heap* heap) {
    EXIT_IF(thread->stack.top == 0);
    u32 heap_len = thread->stack.nodes[--thread->stack.top].as_u32;
    EXIT_IF(CAP_HEAP < heap_len);
    heap->len = heap_len;
    return true;
}

static bool inst_spawn(Memory* memory, Thread* thread_parent) {
    EXIT_IF(CAP_THREADS <= memory->threads_alive);
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
    return true;
}

static bool inst_slpms(Memory* memory, Thread* thread, Time* time) {
    EXIT_IF(thread->stack.top == 0);
    u32 milliseconds = thread->stack.nodes[--thread->stack.top].as_u32;
    thread->wake_at  = get_monotonic(time) + (milliseconds * MILLI_TO_MICRO);
    thread->sleeping = true;
    ++memory->threads_sleeping;
    return false;
}

static bool inst_prch(Thread* thread) {
    EXIT_IF(thread->stack.top == 0);
    putchar(thread->stack.nodes[--thread->stack.top].as_i32);
    return true;
}

static bool inst_pri32(Thread* thread) {
    EXIT_IF(thread->stack.top == 0);
    printf("%d", thread->stack.nodes[--thread->stack.top].as_i32);
    return true;
}

[[noreturn]] static void inst_exit(Thread* thread) {
    EXIT_IF(thread->stack.top == 0);
    _exit(thread->stack.nodes[--thread->stack.top].as_i32);
}

static bool step(Memory* memory, Thread* thread, Time* time) {
    EXIT_IF(memory->program.insts_len <= thread->insts_index);
    switch (static_cast<Inst>(memory->program.insts[thread->insts_index++])) {
    case INST_HALT: {
        return inst_halt(memory, thread);
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
    case INST_AND: {
        return inst_and(thread);
    }
    case INST_OR: {
        return inst_or(thread);
    }
    case INST_SHL: {
        return inst_shl(thread);
    }
    case INST_SHR: {
        return inst_shr(thread);
    }
    case INST_NEG: {
        return inst_neg(thread);
    }
    case INST_NOT: {
        return inst_not(thread);
    }
    case INST_EQ: {
        return inst_eq(thread);
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
    case INST_SLPMS: {
        return inst_slpms(memory, thread, time);
    }
    case INST_PRCH: {
        return inst_prch(thread);
    }
    case INST_PRI32: {
        return inst_pri32(thread);
    }
    case INST_EXIT: {
        inst_exit(thread);
    }
    default:
        EXIT();
    }
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
    Time time;
    while (0 < memory->threads_alive) {
        if (memory->threads_sleeping == memory->threads_alive) {
            u64 next_wake_at = U64_MAX;
            for (u32 i = 0; i < CAP_THREADS; ++i) {
                Thread* thread = &memory->threads[i];
                if (!thread->alive) {
                    continue;
                }
                if (!thread->sleeping) {
                    continue;
                }
                if (thread->wake_at < next_wake_at) {
                    next_wake_at = thread->wake_at;
                }
            }
            EXIT_IF(next_wake_at == U64_MAX);
            u64 now = get_monotonic(&time);
            if (now < next_wake_at) {
                u64 microseconds = next_wake_at - now;
                EXIT_IF(U32_MAX < microseconds);
                EXIT_IF(usleep(static_cast<u32>(microseconds)));
            }
        }
        for (u32 i = 0; i < CAP_THREADS; ++i) {
            Thread* thread = &memory->threads[i];
            if (!thread->alive) {
                continue;
            }
            if (thread->sleeping) {
                if (get_monotonic(&time) < thread->wake_at) {
                    continue;
                }
                thread->sleeping = false;
                --memory->threads_sleeping;
            }
            for (u32 _ = 0; _ < THREAD_STEPS; ++_) {
                // NOTE: If `step(...)` returns `false` we should switch to
                // another thread.
                if (!step(memory, thread, &time)) {
                    break;
                }
            }
        }
    }
    EXIT_IF(thread_main->stack.top != 0);
    return OK;
}
