with import <nixpkgs> {};
mkShell.override { stdenv = llvmPackages_14.stdenv; } {
    buildInputs = [
        ghc
        hlint
        llvmPackages_14.lld
        mold
        ormolu
        python3Packages.flake8
        shellcheck
    ];
    APPEND_LIBRARY_PATH = lib.makeLibraryPath [
        gmp
        libffi
    ];
    shellHook = ''
        export LD_LIBRARY_PATH="$APPEND_LIBRARY_PATH:$LD_LIBRARY_PATH"
        . .shellhook
    '';
    hardeningDisable = [ "all" ];
}
