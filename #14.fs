// 40.2.1   
let rec count (xs, n)= 
 match xs,n with
 |[],n->0
 |head::tail,n when head<n -> 0+count(tail,n)
 |head::tail,n when head=n -> 1+count(tail,n)
 |head::tail,n when head>n -> 0
 
 // 40.3.1 
let rec smallest = function
|[]-> failwith "List is empty"
|head::tail-> List.min(head::tail)

  // 40.3.2
let rec delete (n, xs) = 
 match n,xs with
 |n,[]->[]
 |n,head::tail when n<>head-> [head] @ delete(n,tail)
 |n,head::tail when n=head->tail
 
   // 40.3.3
let rec sort = function
 |head::tail-> List.sort(head::tail)
 |_-> []
