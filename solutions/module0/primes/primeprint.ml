open Printf 
(*this functions is going to return codes depending on whether the integer is ok or not*)
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

let print_prime n = 
  if n > 100 || n < 1 then
    exit 1
  else
    let rec work cnt prime =
      if cnt < n then(
        printf "%d\n" prime;
        work (cnt+1) (nextprime (prime))
        )
      else ()
    in 
      work 0 2;


    (*how to do primes in ocaml? I want this function to essentially return lets do a recursive function that has the number we are counting to
    and the number we already did as input and also the last prime we computed?
    
    how would you writ this in c?
    void print_prime(int n){
    if n is wrong
      return 1;

    I have a number, I can check if the number is prime. if it is then print it and also add 1 to my counter or whatever if not try the next number. 
    
      how do I check if a number is prime?
        I try to divide it by all number starting from two, so I got cnt = 2 while cnt < prime if prime mod cnt = flase else loop cnt+1 prime
    }

    *)
