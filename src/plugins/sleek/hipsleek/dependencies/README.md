# Dependencies
Binaries, extra files, and packages required for [hipsleek] to run, but are not included in that repository.

Built on `loris`, which is Ubuntu-14.04 with `uname -a` returning:
```
Linux loris-5 3.16.0-77-generic #99~14.04.1-Ubuntu SMP Tue Jun 28 19:17:10 UTC 2016 x86_64 x86_64 x86_64 GNU/Linux
```

So, these dependencies should work on an Ubuntu-like system.

## Installation
1. Make sure all binaries are in some directory listed in the `$PATH` environment variable. In other words, for each binary with name `b`, running `which b` returns the path to `b`.
1. Either:
   1. Copy `mona_predicates.mona` to `/usr/local/lib/`, or
   1. Copy `mona_predicates.mona` to the same directory where you would invoke the `mona` prover. For example, if you were to run `./hip -tp mona testcases/ex5.c` at the root directory of the [hipsleek] repository, then copy `mona_predicates.mona` to the root directory of the [hipsleek] repository.
1. Copy `mona_1.4-18` to `/usr/local/`.
1. Copy `reduce` to `/usr/local/etc/`.
1. Symlink to `/usr/local/mona_1.4-18/bin/mona` with the name `mona`, such that `mona` is in some directory listed in the `$PATH` environment variable.
1. Add `/usr/local/etc/reduce/bin` to your `$PATH`, such that `redcsl` is in some directory listed in the `$PATH` environment variable. Note that a symlink does not work in general, because `/usr/local/etc/reduce/bin/redcsl` is a script that runs commands relative to the script location.

### System-specific settings
If your system is different in certain ways than `loris`, then you may need to perform some extra steps. If you installed before performing some of these extra steps, you should reinstall. Alternatively, you can perform some of these extra steps in such a way that reinstallation is not needed.

1. Your system information may be different than `loris`'. In that case, `redcsl` from the `reduce` package may not run properly:
   ```
   $ redcsl
   Failed to find a version of reduce that you have built
   try ./configure; make to build one.
   ```
   So, after all argument processing in `reduce/scripts/run.sh`, insert these two lines:
   ```
   exec $here/../cslbuild/x86_64-unknown-ubuntu12.04/csl/$ap $*
   exit 0
   ```
   such that the file now looks like:
   ```
   #! /bin/sh
   # This is called as
   #    run directory-it-is-in appname scriptname args
   here="$1"
   ap="$2"
   scr="$3"
   shift
   shift
   shift
   exec $here/../cslbuild/x86_64-unknown-ubuntu12.04/csl/$ap $*
   exit 0
   ```
1. Your system may be missing `libffi.so.6`, such that `fixcalc` may not run properly. For example, from the directory of the [hipsleek] repository:
   ```
   $ cd examples/working
   $ ../../sleek --inv --dis-eps sleek/infer/app-inv.slk
   <snip>
   !!! **WARNING****sleek.ml#494:[../../prelude.slk,sleek/infer/app-inv.slk] <snip> fixcalc: error while loading shared libraries: libffi.so.6: cannot open shared object file: No such file or directory
   <snip>
   ```
   So symlink to `/usr/lib/libffi.so` with the name `/usr/lib/libffi.so.6`.

## List of dependencies
Name|Type|Location on `loris`
---|---|---
`oc`|binary|`/usr/local/bin/oc`
`fixcalc`|binary|`/usr/local/bin/fixcalc`
`minisat`|binary|`/usr/bin/minisat`
`z3-4.3.2`|binary|`/usr/local/bin/z3-4.3.2`
`z3`|symlink|
`mona_predicates.mona`|mona|`/usr/local/lib/mona_predicates.mona`
`mona_1.4-18`|package|`/usr/local/mona_1.4-18`
`reduce`|package|`/usr/local/etc/reduce`

[hipsleek]: https://github.com/hipsleek/hipsleek
