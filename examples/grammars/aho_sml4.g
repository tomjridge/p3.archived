ST -> S ?ws? ?EOF?

S -> "" | SS

SS -> "x" X | "x" 

X -> S "x" | S "x" X 

