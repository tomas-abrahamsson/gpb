Short guide to the design of encoding
-------------------------------------

The encoding relies heavily on the fact that binaries are
write-appendable as long as possible. (fixme: more clearly define what
this means).

Each encoding function adds some bytes to the end of the binary.

An alternative approach could have been to generate iolists, and call
iolist_to_binary at the top level, but it didn't save much over the
write-append approach, when I tried.
