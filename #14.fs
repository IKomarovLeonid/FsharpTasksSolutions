// 40.2.1   
let rec count (xs, n)= 
 match xs,n with
 |[],n->0
 |head::tail,n when head<n -> 0+count(tail,n)
 |head::tail,n when head=n -> 1+count(tail,n)
 |head::tail,n when head>n -> 0
