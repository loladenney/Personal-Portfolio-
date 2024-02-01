(* Question 1 *)

(* TODO: Write a good set of tests for {!q1a_nat_of_int}. *)
let q1a_nat_of_int_tests : (int * nat) list = 
  [(3, S (S (S Z)));
   (0,Z);
   (2, S (S Z));
   (1, (S Z) );
  ]

(* TODO:  Implement {!q1a_nat_of_int} using a tail-recursive helper. *)
(* maybe make outer function recurisive if this doesnt work*)
let q1a_nat_of_int (n : int) : nat = 
  let rec q1a_helper (n : int) (m : nat) =
    if n = 1 then m 
    else
      q1a_helper (n-1) (S (m))
      
  in 
  if n = 0 then Z 
  else 
    q1a_helper n (S Z)
  

(* TODO: Write a good set of tests for {!q1b_int_of_nat}. *)
let q1b_int_of_nat_tests : (nat * int) list = 
  [(S (S (S Z)), 3);
   (Z, 0);
   ( S (S Z), 2);
   ((S Z), 1);
  ]


(* TODO:  Implement {!q1b_int_of_nat} using a tail-recursive helper. *)
let q1b_int_of_nat (n : nat) : int = 
  let rec q1b_helper ( n : nat) ( m : int) =
    if q1a_nat_of_int m = n then m
    else
      q1b_helper n (m + 1)
  in 
  q1b_helper n 0 
  

(* TODO: Write a good set of tests for {!q1c_add}. *)
let q1c_add_tests : ((nat * nat) * nat) list = [
  ((S (S (S Z)), (S (S (S Z)))), (S(S (S (S (S (S Z))))))); 
  (((S Z), Z), (S Z));
  ((Z, Z), Z);
  ((Z, (S (S Z))), (S (S Z)));
]

(* TODO: Implement {!q1c_add}. *)
let rec q1c_add (n : nat) (m : nat) : nat = 
  if m = Z then n (* base case 1*)
  else if n = Z then m (* base case 2*) 
  else 
    q1c_add (n) (S (m))
      
let rec q1c_add (n : nat) (m : nat) : nat = 
  match n with
  | Z -> m
  | (S Z) -> S(m)
  | S(x) -> q1c_add x (S(m) )
    
  


(* Question 2 *)

(* TODO: Implement {!q2a_neg}. *)
let q2a_neg (e : exp) : exp = 
  Times( e, Const(-1.0) )

(* TODO: Implement {!q2b_minus}. *)
let q2b_minus (e1 : exp) (e2 : exp) : exp = 
  Plus( e1 , q2a_neg e2 ) 

(* TODO: Implement {!q2c_pow}. *)
let q2c_pow (e1 : exp) (p : nat) : exp = 
  let rec pow_helper e1 p tot = 
    match p with
    | Z -> tot
    | S(x) -> pow_helper e1 x (Times(tot , e1))
  in
  pow_helper e1 p (Const(1.0))
    
    
    


(* Question 3 *)

(* TODO: Write a good set of tests for {!eval}. *) 
let eval_tests : ((float * exp) * float) list = [
  ((1.0 , Times(Var, Const(5.0))), 5.0 );
  ((2.0 , Plus (
       Plus (
         Times (
           Const 2.0,
           Var
         ),
         Times (
           Const (-1.0),
           Div (Var, Const 3.0)
         )
       ),
       Const 10.0
     )), 13.3333 );
  
]

(* TODO: Implement {!eval}. *)
let rec eval (a : float) (e : exp) : float = 
  match e with 
  | Var -> a
  | Const(x) -> x
  | Plus(x, y) -> (eval a x) +. (eval a y)
  | Times(x, y) -> (eval a x) *. (eval a y)
  | Div(x, y) -> (eval a x ) /. (eval a y) 




(* Question 4 *)

(* TODO: Write a good set of tests for {!diff_tests}. *)
let diff_tests : (exp * exp) list = [
  ( Var , Const(1.0));
  ( Times(Var, Const(2.0)) , (Plus (Times (Const 1., Const 2.), Times (Const 0., Var)))) ;
  (Plus(Var, Div(Var, Const(10.0))),  (Plus (Const 1.,
                                             Div
                                               (Plus (Times (Const 1., Const 10.),
                                                      Times (Times (Const 0., Var), Const (-1.))),
                                                Times (Const 10., Const 10.)))));
  
  
]

(* TODO: Implement {!diff}. *)
let rec diff (e : exp) : exp = 
  match e with
  | Var -> Const(1.0)
  | Const(_) -> Const(0.)
  | Plus(x, y) -> Plus( (diff x), (diff y))
  | Times(x, y) -> Plus((Times((diff x), y)), Times((diff y), x))
  | Div(x, y) -> Div( ( q2b_minus (Times((diff x), y)) (Times((diff y), x)) ) , (Times(y, y)))
      
    
    






