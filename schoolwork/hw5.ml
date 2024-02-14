
let xyz_truth_asgn1 : truth_assignment
  =  Variable_map.empty
     |> Variable_map.add       "x" true
     |> Variable_map.add       "y" false
     |> Variable_map.add       "z" false
       
let xyz_truth_asgn2 : truth_assignment
  =  Variable_map.empty
     |> Variable_map.add       "x" true
     |> Variable_map.add       "y" false
     |> Variable_map.add       "z" false
       
let xyz_truth_asgn3 : truth_assignment
  =  Variable_map.empty
     |> Variable_map.add       "x" false
     |> Variable_map.add       "y" false
     |> Variable_map.add       "z" true


(* TODO: Add test cases for both Question 1 and 2. *)
let find_sat_assignment_tests : (formula * truth_assignment option) list = [ 
  (parse_formula "x & ~y | z", Some xyz_truth_asgn1);
  (parse_formula "~x & ~y | ~z", Some xyz_truth_asgn2);
  (parse_formula "~x & ~y & z", Some xyz_truth_asgn3);
  (parse_formula "~x & z & ~z", None);
]


(* Question 1 *)
(*----------------------------------------*)

(* TODO: Implement the function. *)
let find_sat_assignment_exc (formula : formula) : truth_assignment = 
  (* initialize all variables to true*)
  let rec change_map (vars: string list) (map : truth_assignment): truth_assignment =
    match vars with
    | [] -> (match eval map formula with
        | true -> map
        | false -> raise Unsatisfiable_formula) 
    | x :: xs -> try change_map xs (Variable_map.add x true map) with
      | Unsatisfiable_formula -> change_map xs (Variable_map.add x false map)
  in 
  change_map (collect_variables formula) Variable_map.empty
    

(* Question 2 *)
(*----------------------------------------*)

(* TODO: Implement the function. *)
let find_sat_assignment_cps (formula : formula)
    (return : truth_assignment -> 'r) (fail : unit -> 'r) : 'r =
  let rec change_map (vars: string list) (map : truth_assignment) return fail =
    match vars with
    | [] -> (match eval map formula with
        | true -> return map
        | false -> fail ()) 
    | x :: xs -> change_map xs (Variable_map.add x true map) 
                   (return ) 
                   (fun () ->  change_map xs (Variable_map.add x false map) return fail) 
  in 
  change_map (collect_variables formula) Variable_map.empty return fail

    



