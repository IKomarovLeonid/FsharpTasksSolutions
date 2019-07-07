// 40.2.1   
let rec count (xs, n)= 
 match xs,n with
 |[],n->0
 |head::tail,n when head<n -> 0+count(tail,n)
 |head::tail,n when head=n -> 1+count(tail,n)
 |head::tail,n when head>n -> 0
 
 // 40.2.2
 let rec insert (xs, n) =
 match xs,n with 
|head::tail,n when head>n -> [n] @ head::tail
|head::tail,n when head<=n -> [head] @ insert(tail,n)
|[],n->[n]
|_,n->[]

// 40.2.4
let rec plus (xs1, xs2) = 
 List.append xs1 xs2 
 
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
