# Simple adaptive quadrature for Ocaml

Following a C++ code I wrote doing the same thing I implement here
Gauss-Kronrod adaptive quadrature in Ocaml.

# Warnings

1. This implementation is slower than necessary.
2. This implementation is probably not robust.


# Future Work

1. Would like to parameterize the type and not require it to be floating point.

2. Perhaps more work on module signature allowing a variety of integration methods, not just gauss-kronrod.

3. The adaptation algorithm here is very naive, as it is in my C++ code. This could be improved using more sophisticated logic and cut down on redundant function evaluations.
