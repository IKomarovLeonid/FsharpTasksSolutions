let days_in_month = function 
|n when n<1 || n>12 ->0
|2                  ->28  
|4|6|9|11           ->30
|_                  ->31
