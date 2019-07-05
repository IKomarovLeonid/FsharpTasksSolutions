// 39.1
let rec rmodd = function   // example: [0;2;3;6;8;9;5;3] -> [2;6;9;3] 
 |[]->[]
 |[x]->[]
 |head :: tail -> [tail.Head] @ rmodd tail.Tail

// 39.2              
let rec del_even = function                   // example: [0;1;2;3;4;5;6;7;8;9] -> [1;3;5;7;9]
 |[]->[]
 |head::tail when head%2=0-> del_even tail
 |head::tail when head%2<>0->[head]@ del_even tail

// 39.3
let rec multiplicity x xs  =                    // example: 4 [0;1;2;4;4;5;4;7;4;4] -> 5 
 match x,xs with
 |x,[] ->0
 |x,head::tail when x=head -> 1+multiplicity x tail
 |x,head::tail when x<>head -> multiplicity x tail
 
 // 39.4
let rec split =                              // example: [0;1;2;3;4;5] -> ([0;2;4],[1;3;5])
  let rec even = function
  | [ x ] -> [ x ]
  | head :: (_ :: tail) -> head :: even tail
  | _ -> []

  let rec odd = function
  | [ _ ] -> []
  | _ :: (head2 :: tail) -> head2 :: odd tail
  | _ -> []

  fun xs -> (even xs, odd xs)
 
 //39.5 
let rec zip (xs1,xs2)=                          // example [1;2;3;4] [4;3;7;5] -> [(1,4);(2,3);(3,7);(4,5)]
  match xs1,xs2 with
  |[],[]->[]
  |head::tail,head1::tail1 when xs1.Length=xs2.Length-> [(head,head1)] @ zip (tail,tail1)
  |head::tail,head1::tail1 when xs1.Length<>xs2.Length-> failwith "Need equal length of lists"
  |_->[]
