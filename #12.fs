let rec upto = function 
 |1->[1]
 |n-> upto(n-1) @ [n]
 
let rec dnto =function 
 |1-> [1]
 |n -> n :: dnto(n-1)
 
let rec evenn = function
 |0->[0]
 |n when (n<0) -> []
 |n when n%2=0-> evenn(n-2) @ [n]
 |n when n%2=1 -> evenn(n-1)
