# flak

An experiment: A small (unfinished!) JIT-compiled (x86) functional language -- just to see how small it can be.

And I'm sure this can remain under 1k lines and still be a real/usable language -- but for now it's just a glorified calculator with extra features. ^^ See `struct TestS` (in `flak.cc`) for built in tests that demonstrate most of what is supported at this stage.

# building

The `makefile` just invokes `./build`; and `build` assumes clang++ is present. Edit as needed. :)

# license

If you make improvements, please share; that would be kind. :) BSD license, as per flak.h and LICENSE.
