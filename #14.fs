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
