ST -> S ?ws? ?EOF?

S -> "" | SS

SS -> "x" X | "x" 

X -> SS "x" | SS "x" X 

