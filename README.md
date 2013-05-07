`io_libc` -- fast formatter NIF for Erlang using standard C library
===================

API
---------
Currently, the only working function is `io_libc:fwrite/2`, `io_libc:format/2` is alias for it.

Usage
-------------
To format strings faster, call `io_libc:format` instead of `io_lib:format`, providing C-style fmt string.
Example:

    > io_libc:format("hello, %08.3f, %s %04X!", [3.14, dead, 48879]).
    <<"hello, 0003.140, dead BEEF!">>

Numbers are converted automatically when needed, falling back to zero on error:

    > io_libc:format("float %.1f, int %d, bad %d and %f", [3, 2.7, {eee}]).
    <<"float 3.0, int 2, bad 0 and 0.000000">>

You can pass atom or any valid iolist as string value:

    > io_libc:format("string %s, atom %s, bad '%s' and '%s'", [["abc", [<<"de">>], $f], hello, {foo}]).
    <<"string abcdef, atom hello, bad '' and ''">>

Extra convertion arguments are also supported:

    > io_libc:format("%*.*f; %0*d", [8, 3, 2.7, 6, 12]).                                               
    <<"   2.700; 000012">>
