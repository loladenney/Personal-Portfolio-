(* Hi everyone. All of these problems are generally "one-liners" and have slick solutions. They're quite cute to think
   about but are certainly confusing without the appropriate time and experience that you devote towards reasoning about
   this style. Good luck! :-) *)

(* For example, if you wanted to use the encoding of five in your test cases, you could define: *)
let five : 'b church = fun s z -> s (s (s (s (s z)))) 
let six : 'b church = fun s z -> s(s (s (s (s (s z)))) )
let four : 'b church = fun s z -> s (s (s (s z)))
let three : 'b church = fun s z -> s (s (s z))
let two : 'b church = fun s z -> s (s z)
(* and use 'five' like a constant. You could also just use
   'fun z s -> s (s (s (s (s z))))' directly in the test cases too. *)

(* If you define a personal helper function like int_to_church, use it for your test cases, and see things break, you should
   suspect it and consider hard coding the input cases instead *)

(*---------------------------------------------------------------*)
(* QUESTION 1 *)

(* Question 1a: Church numeral to integer *)
(* TODO: Test cases *)
let to_int_tests : (int church * int) list = [
  (five, 5);
  (one, 1);
  (four, 4);
  (zero, 0);
]

(* TODO: Implement:
   Although the input n is of type int church, please do not be confused. This is due to typechecking reasons, and for
   your purposes, you could pretend n is of type 'b church just like in the other problems. *)
    
   (* let to_int (n : int church) : int = 
       let f i = i+1
       in
       (n f 1 )-1 
*)

let to_int (n : int church) : int = 
  n (fun i -> i+1) 0 
  
  
(* Question 1b: Determine if a church numeral is zero *)
(* TODO: Test cases *)
let is_zero_tests : ('b church * bool) list = [
  (five, false);
  (one, false);
  (zero, true);
  ((fun s z -> z), true)
]
  

(* TODO: Implement *)
let is_zero (n : 'b church) : bool = 
  n (fun s -> false) true 
  
  

(* Question 1c: Add two church numerals *)
(* TODO: Test cases *)
let add_tests : (('b church * 'b church) * 'b church) list = [
  ((zero, one), one);
  ((four, one), five);
  ((two, two), four);
  ((zero, zero), zero);
  ((four, zero), four);

]

(* TODO: Implement *)
let add (n1 : 'b church) (n2 : 'b church) : 'b church = 
  fun s z -> n1 s (n2 s z)
  
      
  

(*---------------------------------------------------------------*)
(* QUESTION 2 *)

(* Question 2a: Multiply two church numerals *)
(* TODO: Test cases *)
let mult_tests : (('b church * 'b church) * 'b church) list = [
  ((zero, zero) , zero);
  ((four, zero), zero);
  ((two, two), four);
  ((two, three), six);
  ((four, one), four);
]

(* TODO: Implement *)
let mult (n1 : 'b church) (n2 : 'b church) : 'b church = 
  fun s z -> n1 (n2 s) z
  

(* Question 2b: Compute the power of a church numeral given an int as the power *)
(* TODO: Test cases *)
let int_pow_church_tests : ((int * 'b church) * int) list = [
  ((5, zero) , 1);
  ((2, two), 4);
  ((3, one), 3);
  ((0, three), 0); 
]
    
(* TODO: Implement *)
let int_pow_church (x : int) (n : 'b church) : int = 
  n (fun s -> s * x) 1 
  
  
  
  
  
  
