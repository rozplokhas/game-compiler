let x:N = 2 in
let n:N = 10 in

new i   in
new res in
i := n;
res := 1;
while !i do (
    res := !res * x;
    i := !i - 1
);
!res