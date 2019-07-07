   // 40.1
let rec sum (p, xs) =
    match xs with
    | [] -> 0
    | [ x ] when p x -> x
    | head :: tail when p head -> head + sum (p, tail)
    | _ :: tail -> 0 + sum (p, tail)
    | _ -> 0


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

// 40.2.3
let rec intersect (xs1: list<int>, xs2: list<int>) =
    match (xs1, xs2) with
    | (head1 :: tail1, head2 :: _) when head1 < head2 -> intersect (tail1, xs2)
    | (head1 :: _, head2 :: tail2) when head1 > head2 -> intersect (xs1, tail2)
    | (head1 :: tail1, head2 :: tail2) when head1 = head2 -> head1 :: intersect (tail1, tail2)
    | ([ x ], [ y ]) when x = y -> [ x ]
    | _ -> []

// 40.2.4
let rec plus (xs1, xs2) = 
  List.append xs1 xs2 
 
 // 40.2.5
let rec minus (xs1: list<int>, xs2: list<int>) =
    match (xs1, xs2) with
    | (head1 :: tail1, head2 :: _) when head1 < head2 -> head1 :: minus (tail1, xs2)
    | (head1 :: _, head2 :: _) when head1 > head2 -> [ head1 ]
    | (head1 :: tail1, head2 :: tail2) when head1 = head2 -> minus (tail1, tail2)
    | (xs1, []) -> xs1
    | _ -> []
 
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
    // 40.4
let rec revrev = fun (xs: list<list<int>>) ->
   match xs with
   | [] -> []
   | [ x ] -> [ List.rev x ]
   | head :: tail -> revrev tail @ [ List.rev head ]
