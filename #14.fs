// 39.1
let rec rmodd = function
 |[]->[]
 |[x]->[]
 |head :: tail -> [tail.Head] @ rmodd tail.Tail

// 39.2              
let rec del_even = function
 |[]->[]
 |head::tail when head%2=0-> del_even tail
 |head::tail when head%2<>0->[head]@ del_even tail

// 39.3
let rec multiplicity x xs  = 
 match x,xs with
 |x,[] ->0
 |x,head::tail when x=head -> 1+multiplicity x tail
 |x,head::tail when x<>head -> multiplicity x tail
 
 // 39.4
let rec split =
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
let rec zip (xs1,xs2)= 
  match xs1,xs2 with
  |[],[]->[]
  |head::tail,head1::tail1 when xs1.Length=xs2.Length-> [(head,head1)] @ zip (tail,tail1)
  |head::tail,head1::tail1 when xs1.Length<>xs2.Length-> failwith "Need equal length of lists"
  |_->[]
