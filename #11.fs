type F = 
  | AM
  | PM

type TimeOfDay = { hours : int; minutes : int; f: F }   // Time data 

let (.>.) x y =    // example: 10 53 AM 8 45 PM -> false 
 match x.f,y.f with
 |AM,PM->  false
 |PM,AM-> true
 |_-> x>y
