#!/usr/bin/env bash

set -euo pipefail

(
    for x in "$WD/src/com"/*.hs; do
        (
            hlint "$x"
            ormolu -i --no-cabal "$x"
        ) &
    done
    clang-format -i -verbose "$WD/src/vm/"* &
    for _ in $(jobs -p); do
        wait -n
    done
)
(
    flags=(
        "-fdiagnostics-color=always"
        -funbox-strict-fields
        "-i$WD/src/com"
        -O
        "-optl -fuse-ld=lld"
        "-outputdir $WD/build"
        -Wall
        -Wcompat
        -Werror
        -Widentities
        -Wincomplete-record-updates
        -Wincomplete-uni-patterns
        -with-rtsopts "-A8m"
        -Wmonomorphism-restriction
        -Wpartial-fields
        -Wredundant-constraints
        -Wunused-packages
        -Wunused-type-patterns
    )
    ghc "${flags[@]}" -o "$WD/bin/com" "$WD/src/com/Main.hs"
)
(
    flags=(
        -D_GNU_SOURCE
        "-ferror-limit=1"
        "-fsanitize=address"
        "-fsanitize=bounds"
        "-fsanitize=float-divide-by-zero"
        "-fsanitize=implicit-conversion"
        "-fsanitize=integer"
        "-fsanitize=nullability"
        "-fsanitize=undefined"
        -fshort-enums
        "-march=native"
        -O1
        "-std=c99"
        -Werror
        -Weverything
        -Wno-covered-switch-default
        -Wno-declaration-after-statement
        -Wno-disabled-macro-expansion
        -Wno-extra-semi-stmt
        -Wno-padded
        -Wno-reserved-macro-identifier
    )
    mold -run clang "${flags[@]}" -o "$WD/bin/vm" "$WD/src/vm/main.c"
)
