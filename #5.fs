let notDivisible(n,m)  = m % n  = 0
let rec prime x=
      seq {
      for i=2 to x/2 do
      if x%i=0 then yield i
      }
      |>Seq.isEmpty
