(* Question 1: Manhattan Distance *)
(* TODO: Write a good set of tests for distance. *)
let distance_tests = [
  (
    ((0, 0), (0, 0)), (* input: two inputs, each a pair, so we have a pair of pairs *)
    0                 (* output: the distance between (0,0) and (0,0) is 0 *)
  );                    (* end each case with a semicolon *)
  (
    ((0 ,0 ), (1 ,1 )),
    2             
  );
  (
    (( -1,0 ), ( 1,0 )),
    2            
  );
  (
    ((0 ,-2 ), ( 1, 2)),
    5             
  );
  (
    ((-2, 0), ( 1, 2)),
    5                
  );
  (
    ((-1 , -2), ( -2, -1)),
    2               
  );(* Your test cases go here *)
]
;;

(* TODO: Correct this implementation so that it compiles and returns
         the correct answers. DONE
*)
let distance (x1, y1) (x2, y2) = 
  abs(x1 - x2) + abs(y1 - y2) 


(* Question 2: Binomial *)
(* TODO: Write your own tests for the binomial function.
         See the provided test for how to write test cases.
         Remember that we assume that  n >= k >= 0; you should not write test cases where this assumption is violated.
*)
let binomial_tests = [
  (* Your test cases go here. Correct this incorrect test case for the function. *)
  ((0, 0), 1);
  (( 1, 0),  1);
  (( 5, 5),  1);
  (( 5, 4),  5);
  (( 5, 3), 10 );

]

(* TODO: Correct this implementation so that it compiles and returns
         the correct answers.
*)
let binomial n k =
  let rec factorial m =
    if m = 0 then
      1 
    else
      factorial(m-1) * m; (*infinite loop here somewhere?*)
  in
  factorial n / (factorial k * factorial(n-k))



(* Question 3: Lucas Numbers *)

(* TODO: Write a good set of tests for lucas_tests. *)
let lucas_tests = [
  ( 0,2 );
  (1 ,1 );
  ( 4, 7);
  ( 5, 11);
]

(* TODO: Implement a tail-recursive helper lucas_helper. 
let rec lucas_helper params = *)
let rec lucas_helper n ln1 ln2 =
  if n = 2 then (ln1+ln2) else
    lucas_helper (n-1) (ln1+ln2) ln1
    
  

(* TODO: Implement lucas by calling lucas_helper.  
  let lucas n = *)
    
    
let lucas n = 
  if n = 0 then 2
  else if n = 1 then 1 
  else 
    lucas_helper n 1 2
    
    
    
    
    
    
    
    
