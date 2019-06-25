let vat n x =
 let r=(float)n/100.0
 let add=x*r
 x+add

let unvat n x =
 x / (1.0 + float n / 100.0)
   
let rec min f = 
 let rec find n=
  match f n with
  |0-> n
  |f-> find(n+1)
 let rez=find 0  
 rez
