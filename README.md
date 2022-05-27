# bla

Imperative, concurrent, scripting language with static type-checking. Very much a work-in-progress.

Needed things
---
* [Nix](https://nixos.org/download.html)

Quick start
---
```console
$ cd path/to/bla
$ nix-shell
[nix-shell:path/to/bla]$ ./scripts/build.sh                 # build compiler and vm
[nix-shell:path/to/bla]$ ./scripts/test.py                  # run tests
[nix-shell:path/to/bla]$ cat > script.bla << EOF            # create example script
> main() {
>     n = 40;
>     a = 0;
>     b = 1;
>     loop {
>         if n == 0 {
>             break 0;
>         }
>         c = a + b;
>         a = b;
>         b = c;
>         n = n - 1;
>     }
>     @print_i32(a);
>     @print_char(10);
>     return;
> }
> EOF
[nix-shell:path/to/bla]$ ./bin/com script.bla bytecode.blc  # compile script to bytecode
[nix-shell:path/to/bla]$ ./bin/vm bytecode.blc              # run program via bytecode
102334155
```

---
Head over to `path/to/bla/ex` for more example programs!

Roadmap
---
- [x] [Recursive descent](https://en.wikipedia.org/wiki/Recursive_descent_parser) parser with [Pratt parsing](https://en.wikipedia.org/wiki/Operator-precedence_parser#Pratt_parsing)
- [x] Stack-based, bytecode virtual machine
- [ ] Imperative control flow
  - [x] Function recursion without need for forward-declaration
  - [x] `if`, `loop`, and early `return` control flow semantics
  - [x] Slightly more powerful `break` and `continue` semantics (i.e. `break 0;` breaks out of `loop` as expected, `break 1;` breaks out of **2** layers of `loop`, etc.), checked at compile-time
  - [ ] Support for `else` control flow
  - [ ] Remove need for `return` on unreachable code paths
- [x] Compile-time static type-checking (albeit very basic!)
- [ ] Type-checked variable scopes
  - [x] Function scopes
  - [ ] Ad-hoc block scopes
  - [x] Compile-time error on unreachable code
- [x] First-class functions (via type-checked function pointers)
- [ ] Manual memory management
  - [x] Globally-accessible `heap` memory (i.e. one big, pre-allocated array)
  - [ ] Read and write slices of `heap` memory in units smaller than `i32`
  - [ ] May need to break down `@alloc_heap` into more granular operations?
  - [ ] Shrink or grow `heap` dynamically?
- [ ] Concurrency
  - [x] Concurrency primitives (i.e. `@spawn`) that operate at the instruction-level, similar in spirit to **Erlang**'s [preemptive reduction mechanism](https://hamidreza-s.github.io/erlang/scheduling/real-time/preemptive/migration/2016/02/09/erlang-scheduler-details.html)
  - [x] Concurrent `@sleep`-ing
  - [ ] Concurrent `IO`
  - [ ] Some kind of run queue to manage thread selection?
  - [ ] Dynamic thread allocation
- [ ] Errors
  - [x] Line and column numbers for syntax and type errors
  - [ ] Actual error messages (currently only the location of the error is provided)
  - [ ] Exceptions?
  - [ ] Run-time stack traces?
- [ ] Support platforms other than Linux
- [ ] Some kind of standard library?
- [ ] Documentation for syntax and semantics
- [ ] Support for `string` primitives
- [ ] Pointer types (currently `addr` is just a `void*`)
