let rec pow(s:string,n)=
 match n with
|0->""
|n->s+pow(s,n-1)
   
let rec isIthChar(s : string,n,c)=
 match n with
|n when n<0 ->false
|n when n>s.Length-1 ->false
|n->s.[n]=c
   
let rec occFromIth(s : string,n,c)=
 let amount=0;
 let rec counter(s: string,n,c,amount)=
  match n with 
  |n when n<0 -> amount
  |n when n=s.Length -> amount
  |n when s.[n]=c -> counter(s,n+1,c,amount+1)
  |n when s.[n]<>c -> counter(s,n+1,c,amount)
 let rez=counter(s,n,c,amount);
 rez
