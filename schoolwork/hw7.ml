(* SECTION 1 *)

(*  Question 1.1 *)
let rec repeat (x : 'a) : 'a stream = 
  {
    head = x;
    tail = Susp (fun () -> repeat x);
  } 


(* Question 1.2 *)
let rec filter (f : 'a -> bool) (s : 'a stream) : 'a stream = 
  if f s.head then
    {
      head = s.head;
      tail = Susp (fun () -> filter f (force s.tail));
    }
  else filter f (force s.tail)
    

(* Question 1.3 *)
let rec lucas1 =
  { 
    head = 2;
    tail = Susp (fun () -> lucas2);
  }

and lucas2 =
  { 
    head = 1;
    tail = Susp (fun () -> zip_with ( + ) lucas1 lucas2);
  } 

(* Question 1.4 *)
let unfold (f : 'a -> 'b * 'a) (seed : 'a) : 'b stream = 
  map fst (iterate (fun (x,y) -> f y ) (f seed))
  

(* Question 1.5 *)
let unfold_lucas : int stream = 
  unfold (fun (a,b) -> (a, (b, (a + b)))) (2, 1)
  
  
  

(* SECTION 2 *)

(* Question 2.1 *)
let scale (s1 : int stream) (n : int) : int stream = 
  map (fun (x : int) -> n * x ) s1
    
    

(* Question 2.2 *)
let rec s = 
  {
    head = 1;
    tail = Susp ( fun () ->  
        merge (merge (scale s 2) ( scale s 3)) (scale s 5)
      );
  } 

