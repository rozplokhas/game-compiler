let pow:N->N->N = \x:N n:N ->
    new i   in
    new res in
    i := n;
    res := 1;
    while !i do (
        res := !res * x;
        i := !i - 1
    );
    !res
in

pow 2 10
