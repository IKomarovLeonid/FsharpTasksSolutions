type TimeOfDay = { hours: int; minutes: int; f: string }  // Time data

let (.>.) x y =                           // example: 4 00 PM 11:43 AM -> true
  let f1=x.f
  let f2=y.f
  if(f1="AM" && f2="PM") then false;
  else if(f1="PM" && f2="AM") then true
  else x>y
