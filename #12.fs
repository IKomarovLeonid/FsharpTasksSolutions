let rec upto = function  // example:  8 -> [1;2;3;4;5;6;7;8] 
 |1->[1]
 |n-> upto(n-1) @ [n]
 
let rec dnto =function  // example: 9-> [9;8;7;6;5;4;3;2;1]
 |1-> [1]
 |n -> n :: dnto(n-1)
 
let rec evenn = function  // example: 10 -> [0;2;4;6;8;10]
 |1->[0]
 |2->evenn(1) @ [2]
 |3->evenn(2) @ [4]
 |n when n<1->[]
 |n->evenn(n-1) @ [2*(n-1)]
