# midicompiler
Syntax:
x ranges over variable names matching [a-zA-Z]([a-zA-Z]|[0-9]
)*
t := n | '$'x | note | chord | chordS | '{' (l_i=t_i)^{i in {1..4}} '}' | x = t_0 | 'chord' t_0 | 'chord' t_0 t_1 | 'input' t_0 | 'input' t_0 t_1 | '"'t_0','...','t_l '"' | t_0 '/' t_1 | t_0 '+' | t_0'-' | t_0 '#' |  t_0 'b' | t_0 '_' n |'"'t_0'.'...'.'t_l '"' | 'R' | t_0 '7' | t_0 '9'| t_0 '11' | t_0 '13'
