let rec fibo1 n n1 n2 = 
    if n = 0 then n
    elif n < 2 then (n1 + n2)
    else fibo1 (n - 1) n2 (n1 + n2)

let rec fibo2 c n = 
    if n < 2 then c n
    else fibo2 (fun x -> fibo2 (fun y -> c (x + y)) (n - 2) ) (n - 1)

let rec bigList n k =     
    let rec f acc lst =
        if acc = 0 then k lst
        else f (acc-1) (1 :: k lst)
    f n []
