type TimeOfDay = { hours: int; minutes: int; f: string }

let (.>.) x y = 
  let f1=x.f
  let f2=y.f
  if(f1="AM" && f2="PM") then false;
  else if(f1="PM" && f2="AM") then true
  else x>y
