open Printf 

let prime p =
  if p <= 3 then true
  else
    let rec loop pr c = 
      if c < pr then(
        if (pr mod c) = 0 then false
        else
          loop pr (c+1) 
      )
      else
        true
      
    in
      loop p 2
      

let rec nextprime prev = 
    if prime (prev+1) then 
      prev + 1
    else
      nextprime (prev+1)

let prime_list n = 
  if n > 100 || n < 1 then
    exit 1
  else
    let rec work cnt prime =
      if cnt < n then(
        prime :: work (cnt+1) (nextprime (prime))
        )
      else []
    in 
      work 0 2;


