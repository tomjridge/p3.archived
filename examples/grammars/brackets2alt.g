D -> ?notlt? ELT
ELT -> S ?notltgt? ELTST
ELTST -> ELT ?notltgt? ELTST | T

S -> "<"
T -> ">"


COMMENT -> "

  Like brackets2.g, but with order of alternatives for ELTST rule
  reversed.

"
