let rec rmodd = function
 |[]->[]
 |[x]->[]
 |head :: tail -> [tail.Head] @ rmodd tail.Tail
