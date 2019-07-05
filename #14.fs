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
