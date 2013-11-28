ST -> S ?ws? ?EOF?

S -> "" | SS

SS -> "x" X | "x"

X -> Y | Y X

Y -> SS "x" | "x"

