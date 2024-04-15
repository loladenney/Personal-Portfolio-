(** Part 1: Parsing *)
let parse_exp_tests : (string * exp option) list = [
  ("", None);
  ("if true then 2 else 3",  Some(If(ConstB true,ConstI 2,ConstI 3)));
  ("true", Some(ConstB(true)));
]

let rec exp_parser i =
  let open Parser in
  (** Use [identifier] and [keyword] in your implementation,
      not [identifier_except] and [keyword_among] directly *)
  let identifier, keyword =
    let keywords = ["true"; "false"; "let"; "in"; "end"; "if"; "then"; "else"; "fn"; "rec"] in
    identifier_except keywords, keyword_among keywords
  in
  (** You may need to define helper parsers depending on [exp_parser] here *) 
  let atomic_exp_parser : exp t =
    first_of [ 
      const_map (ConstB(true)) (keyword "true");
      const_map (ConstB(false)) (keyword "false");
      map (fun i  -> ConstI i) int_digits; 
      map (fun s  -> Var s) identifier; 
      between (skip (symbol "(")) (skip (symbol ")")) exp_parser; 
    ] 
  in
  let applicative_exp_parser : exp t = 
    left_assoc_op  
      (symbol "") 
      atomic_exp_parser
      (fun a _ b -> Apply(a,b)) 
        
  in
  let let_comma_parser = 
    keyword "let" |>> symbol "(" |>> identifier |*> fun x -> 
        symbol "," |>> identifier |*> fun y -> 
            symbol ")" |>> symbol "=" |>> exp_parser |*> fun e1 ->
                keyword "in" |>> exp_parser |*> fun e2 ->
                    keyword"end" |>>
                    of_value (LetComma(x,y,e1,e2)) 
  in
  let let_parser = 
    keyword "let" |>> identifier |*> fun x -> 
        symbol "=" |>> exp_parser |*> fun y -> 
            keyword "in" |>> exp_parser |*> fun z -> 
                keyword "end" |>>
                of_value (Let(x,y,z))
  in
  let if_parser =
    keyword "if" |>> exp_parser |*> fun e1 ->
        keyword "then"|>> exp_parser |*> fun e2 ->
            keyword "else" |>>  exp_parser |*> fun e3 ->
                of_value (If(e1,e2,e3))
  in 
  let fn_parser =
    keyword "fn" |>> identifier |*> fun x -> 
        optional (symbol ":" |>> typ_parser ) |*> fun o ->
          symbol "=>" |>> exp_parser |*> fun y -> 
              of_value (Fn (x,o,y)) 
  in
  let rec_parser = 
    keyword "rec" |>> identifier |*> fun x -> 
        optional (symbol ":" |>> typ_parser ) |*> fun o ->
          symbol "=>" |>> exp_parser |*> fun y -> 
              of_value (Rec(x,o,y)) 
  in
  let negatable_exp_parser =
    first_of [ 
      let_comma_parser; 
      let_parser; 
      if_parser;
      fn_parser; 
      rec_parser; 
      applicative_exp_parser; 
    ] 
      
  in
  let negation_exp_parser : exp t = 
    prefix_op (symbol "-") negatable_exp_parser (fun _ b -> PrimUop(Negate,b)) 
    (*the following works, but i want to try to implement it in a simpler way
    (optional (symbol "-") ) |*> function
      | Some _ -> negatable_exp_parser |*> fun y -> of_value (PrimUop(Negate, y))
      | None -> negatable_exp_parser  
*)
        
  in
  let multiplicative_exp_parser =
    left_assoc_op 
      (symbol "*") 
      negation_exp_parser 
      (fun a _ b -> PrimBop(a, Times, b))
  in
  let additive_exp_parser =
    left_assoc_op 
      (first_of_2 (symbol "+"|> const_map Plus) (symbol "-"|> const_map Minus))
      multiplicative_exp_parser
      (fun a sep b -> PrimBop(a, sep, b))
  
  in
  let comparative_exp_parser =
    non_assoc_op 
      (first_of_2 (symbol "="|> const_map Equals) (symbol "<"|> const_map LessThan))
      additive_exp_parser
      (fun a sep b -> PrimBop(a, sep, b)) 
  
  in
  let exp_parser_impl =
    non_assoc_op
      (symbol ",") 
      comparative_exp_parser
      (fun a _ b -> Comma(a,b))
      
  in
  exp_parser_impl i

(** DO NOT Change This Definition *)
let parse_exp : string -> exp option =
  let open Parser in
  run (between spaces eof exp_parser)

(** Part 2: Type Inference *)
let typ_infer_test_helper_tests : ((Context.t * exp) * typ option) list = [
  ((Context.empty, ConstB true), Some Bool)
]

let rec typ_infer (ctx : Context.t) (e : exp) : typ =
  match e with
  | ConstI _ -> Int
  | PrimBop (e1, bop, e2) ->
      let ((t1, t2), t3) = bop_type bop in
      if typ_infer ctx e1 = t1 && typ_infer ctx e2 = t2
      then t3
      else raise TypeInferenceError
  | PrimUop (uop, e') ->
      let (t1, t2) = uop_type uop in
      if typ_infer ctx e' = t1
      then t2
      else raise TypeInferenceError

  | ConstB _ -> Bool
  | If (e', e1, e2) -> 
      (match typ_infer ctx e' with
       | Bool ->
           let te1 = typ_infer ctx e1 in
           if te1 = typ_infer ctx e2 then te1
           else raise TypeInferenceError
       | _ -> raise TypeInferenceError
      )

  | Comma (e1, e2) -> Pair (typ_infer ctx e1, typ_infer ctx e2)
  | LetComma (x, y, e1, e2) -> 
      (match typ_infer ctx e1 with
       | Pair (te1, te2) ->
           let ctx1 = Context.extend ctx (x, te1) in 
           let ctx2 = Context.extend ctx1 (y, te2) in
           typ_infer ctx2 e2
       | _ -> raise TypeInferenceError
      )

  | Fn (x, Some t, e') -> 
      let ctx1 = Context.extend ctx (x, t) in
      let end_result_typ = typ_infer ctx1 e' in
      Arrow (t, end_result_typ)
  | Apply (e1, e2) -> 
      let te2 = typ_infer ctx e2 in
      (match typ_infer ctx e1 with
       | Arrow (funt1, funt2) ->
           if funt1 = te2 then funt2 else raise TypeInferenceError
       | _ -> raise TypeInferenceError )

      (* rec f : t => e or rec f => e *)
  | Rec (f, Some t, e') -> 
      
      let ctx1 = Context.extend ctx (f, t) in
      let end_result_typ = typ_infer ctx1 e' in
      if end_result_typ = t then t else raise TypeInferenceError
          

  | Let (x, e1, e2) -> 
      let te1 = typ_infer ctx e1 in
      let ctx1 = Context.extend ctx (x, te1) in
      typ_infer ctx1 e2
                         
  | Var x ->
      begin
        match Context.lookup ctx x with
        | Some t -> t
        | None -> raise TypeInferenceError
      end

  (** You can ignore these cases for Part 2 *)
  | Fn (_, None, _) -> raise IgnoredInPart2
  | Rec (_, None, _) -> raise IgnoredInPart2

(** DO NOT Change This Definition *)
let typ_infer_test_helper ctx e =
  try
    Some (typ_infer ctx e)
  with
  | TypeInferenceError -> None

(** Part 3: Substitution & Evaluation *)
let free_vars_test_helper_tests : (exp * ident list) list = [
  (ConstI 5, []);
  (Var "x", ["x"])
]

let rec free_vars (e : exp) : IdentSet.t =
  match e with
  | ConstI _ -> IdentSet.empty
  | PrimBop (e1, _, e2) -> IdentSet.union (free_vars e1) (free_vars e2)  (*this was already here*)
  | PrimUop (_, e') -> free_vars e' (*this was already here*)

  | ConstB _ -> IdentSet.empty (*this was already here*)
  | If (e', e1, e2) -> IdentSet.union (free_vars e2 ) (IdentSet.union (free_vars e') (free_vars e1))

  | Comma (e1, e2) -> IdentSet.union (free_vars e1) (free_vars e2) (*this was already here*)
  | LetComma (x, y, e1, e2) -> IdentSet.union (free_vars e1) 
                                 ((free_vars e2) |> IdentSet.remove x |> 
                                  IdentSet.remove y )

  | Fn (x, tOpt, e') ->  (free_vars e') |> IdentSet.remove x 
  | Apply (e1, e2) -> IdentSet.union (free_vars e1) (free_vars e2) 

  | Rec (f, tOpt, e') -> (free_vars e') |> IdentSet.remove f (*not correct?*)

  | Let (x, e1, e2) -> IdentSet.union (free_vars e1) 
                         ((free_vars e2) |> IdentSet.remove x )
  | Var x -> IdentSet.singleton x (*this was already here*)

(** DO NOT Change This Definition *)
let free_vars_test_helper e = IdentSet.elements (free_vars e)

let subst_tests : (((exp * ident) * exp) * exp) list = [
  (((ConstI 5, "x"), PrimBop (ConstI 2, Plus, Var "x")), PrimBop (ConstI 2, Plus, ConstI 5)); 
]

let rec subst ((d, z) : exp * ident) (e : exp) : exp =
  (** [rename (x, e)] replace [x] appears in [e] with a fresh identifier
and returns the fresh identifier and updated expression *)
  let rename ((x, e) : ident * exp) : ident * exp =
    let x' = fresh_ident x in
    (x', subst (Var x', x) e)
  in
  match e with
  | ConstI _ -> e (*this was already here*)
  | PrimBop (e1, bop, e2) -> PrimBop (subst (d, z) e1, bop, subst (d, z) e2) (*this was already here*)
  | PrimUop (uop, e') -> PrimUop (uop, subst (d, z) e') (*this was already here*)

  | ConstB _ -> e (*this was already here*)
  | If (e', e1, e2) -> If ( subst (d, z) e',subst (d, z) e1 , subst (d, z) e2)(*this is correct*)

  | Comma (e1, e2) -> Comma (subst (d, z) e1, subst (d, z) e2) (*this was already here*)
  | LetComma (x, y, e1, e2) ->  (*this is correct*)
      rename (x, e2) |> fun (a,e2') ->
      rename (y, e2') |> fun (b, e2'') ->
      LetComma( a, b, 
                subst (d, z) e1, 
                subst (d,z) e2'') 

  | Fn (x, tOpt, e') -> rename (x, e') |> fun (a,e'') -> (*this is correct*)
                        Fn ( a , tOpt , subst(d,z) e'')
  | Apply (e1, e2) -> Apply (subst (d, z) e1, subst (d, z) e2) (*this is correct*)

  | Rec (f, tOpt, e') -> rename (f, e') |> fun (a,e'') -> (*this is correct*)
                         Rec ( a , tOpt , subst(d,z) e'')

  | Let (x, e1, e2) ->   (*this is correct*)
      rename (x, e2) |> fun (a,e2') ->
      Let( a, subst (d, z) e1, subst (d,z) e2')
        
  | Var x -> (*this was already here*)
      if x = z
      then d
      else e

let eval_test_helper_tests : (exp * exp option) list = [
  (Var "x", None);
  (ConstI 5, Some (ConstI 5));
  (PrimBop (ConstI 5, Minus, ConstI 5), Some (ConstI 0))
]

let rec eval (e : exp) : exp =
  match e with
  | ConstI _ -> e(*this was already here*)
  | PrimBop (e1, bop, e2) ->(*this was already here*)
      begin
        match eval e1, eval e2 with
        | ConstI n1, ConstI n2 ->(*this was already here*)
            begin
              match bop with
              | Equals -> ConstB (n1 = n2)
              | LessThan -> ConstB (n1 < n2)
              | Plus -> ConstI (n1 + n2)
              | Minus -> ConstI (n1 - n2)
              | Times -> ConstI (n1 * n2)
            end
        | _ -> raise EvaluationStuck
      end
  | PrimUop (_, e) ->(*this was already here*)
      begin
        match eval e with
        | ConstI n -> ConstI (- n)
        | _ -> raise EvaluationStuck
      end

  | ConstB _ -> e (*this was already here*)
  | If (e', e1, e2) -> (*this is correct ? *)
      begin 
        match eval e' with
        | ConstB true -> eval e1
        | ConstB false -> eval e2
        | _ -> raise EvaluationStuck
      end 

  | Comma (e1, e2) -> Comma (eval e1, eval e2) (*this was already here*)
  | LetComma (x, y, e1, e2) -> (*this is failing because its a function idk *)
      eval e1 |> begin function
        | Comma(vx, vy) ->
            eval (subst (vx, x) (subst (vy, y) e2 )) 
        | _ -> raise EvaluationStuck
      end 
  | Fn (x, tOpt, e') -> (*this is correct *)
      Fn (x, tOpt, e') 
  | Apply (e1, e2) ->   (*this is correct*)
      begin
        match eval e1 with
        | Fn (x, _ , e1') -> eval e2 |> fun v2 -> eval (subst (v2, x) e1') 
        | _ -> raise EvaluationStuck
      end 
  | Rec (f, tOpt, e') -> (*this is correct*)  
      eval (subst (Rec (f, tOpt, e'), f) e')  
  | Let (x, e1, e2) -> (*this is correct*) 
      eval e1 |> fun v1 -> 
      eval (subst (v1, x) e2)
  | Var _ -> raise EvaluationStuck (*this was already here*)

(** DO NOT Change This Definition *)
let eval_test_helper e =
  try
    Some (eval e)
  with
  | EvaluationStuck -> None

(** Part 4: Unification & Advanced Type Inference *)
let unify_test_case1 () =
  let x = new_tvar () in
  let y = new_tvar () in
  y := Some Int;
  (TVar x, TVar y)

let unify_test_case2 () =
  let x = new_tvar () in
  (TVar x, Arrow (TVar x, TVar x))
  
let unify_test_case3 () =
  let x = new_tvar () in
  let y = new_tvar () in
  (Pair (TVar x, Int), Pair (Int, TVar y))
  
let unify_test_case4 () =
  let x = new_tvar () in
  let y = new_tvar () in
  (Pair (TVar x, TVar y), Pair (Int, Int))
  
let unify_test_case5 () = 
  let x = new_tvar () in
  let y = new_tvar () in
  (Arrow (TVar x, TVar y), Arrow (Int, Int))
  
let unify_test_case6 () =
  let x = new_tvar () in
  let y = new_tvar () in
  x := Some Int;
  y := Some Int;
  (TVar x, TVar y)

let unify_test_helper_tests : ((unit -> typ * typ) * bool) list = [
  ((fun () -> (Int, Int)), true);
  ((fun () -> (Int, Bool)), false);
  (unify_test_case1, true);
  (unify_test_case2, false);
  (unify_test_case3, true);
  (unify_test_case4, true);
  (unify_test_case5, true);
  (unify_test_case6, true);
]

let rec unify : typ -> typ -> unit =
  let rec occurs_check (x : typ option ref) (t : typ) : bool =
    let t = rec_follow_tvar t in
    match t with
    | Int -> false
    | Bool -> false
    | Pair (t1, t2) -> (occurs_check x t1 || occurs_check x t2)
    | Arrow (t1, t2) -> (occurs_check x t1 || occurs_check x t2)
    | TVar y -> is_same_tvar x y
  in
  fun ta tb ->
    let ta = rec_follow_tvar ta in
    let tb = rec_follow_tvar tb in
    match ta, tb with
    | Int, Int -> ()
    | Bool, Bool -> ()
    | Pair (ta1, ta2), Pair (tb1, tb2) -> 
        if (unify ta1 tb1) = () then unify ta2 tb2
    | Arrow (ta1, ta2), Arrow (tb1, tb2) -> (*unify ta2 tb2*) (*changing this order changes points?*)
        if (unify ta1 tb1) = () then unify ta2 tb2
    | TVar xa, TVar xb when is_same_tvar xa xb -> ()
    | TVar xa, _ -> 
        if occurs_check xa tb then 
          raise UnificationFailure
        else 
          
          xa := Some tb; () (*here's the issue*)
    | _, TVar xb -> unify tb ta
    | _, _ -> raise UnificationFailure

(** DO NOT Change This Definition *)
let unify_test_helper f =
  let ta, tb = f () in
  try
    unify ta tb; true
  with
  | UnificationFailure -> false
  | OccursCheckFailure -> false

let adv_typ_infer_test_case1 =
  let x = new_tvar () in
  ((Context.empty, Fn ("y", None, Var "y")), Some (Arrow (TVar x, TVar x)))

let adv_typ_infer_test_helper_tests : ((Context.t * exp) * typ option) list = [
  adv_typ_infer_test_case1;
  ((Context.empty, (Rec ("f", None, Fn ("x", None, Apply (Var "f", ConstI 5))))), Some (Arrow(Int, TVar (new_tvar ()))))
]

let rec adv_typ_infer (ctx : Context.t) (e : exp) : typ =
  match e with
  
  | ConstI n -> Int
    
  | PrimBop (e1, bop, e2) -> let ((t1, t2), t3) = bop_type bop in
      if (unify (adv_typ_infer ctx e1) t1) = () && (unify (adv_typ_infer ctx e2) t2) = ()
      then t3
      else raise TypeInferenceError
          
  | PrimUop (uop, e') -> let (t1, t2) = uop_type uop in
      if (unify (adv_typ_infer ctx e') t1) = ()
      then t2
      else raise TypeInferenceError

  | ConstB b -> Bool
    
  | If (e', e1, e2) -> 
      if (unify (adv_typ_infer ctx e') Bool) = ()
      then 
        let te1 = adv_typ_infer ctx e1 in
        if (unify te1 (adv_typ_infer ctx e2)) = () then te1
        else raise TypeInferenceError
      else raise TypeInferenceError
  
  | Comma (e1, e2) -> Pair (adv_typ_infer ctx e1, adv_typ_infer ctx e2)
                        
  | LetComma (x, y, e1, e2) -> 
  
      (match adv_typ_infer ctx e1 with
       | Pair (te1, te2) ->
           let ctx1 = Context.extend ctx (x, te1) in 
           let ctx2 = Context.extend ctx1 (y, te2) in
           adv_typ_infer ctx2 e2
       | TVar t -> 
           (
             match t.contents with
             | Some Pair (te1, te2) -> 
                 let ctx1 = Context.extend ctx (x, te1) in 
                 let ctx2 = Context.extend ctx1 (y, te2) in
                 adv_typ_infer ctx2 e2
             | _ -> raise TypeInferenceError
           )
       | _ -> raise TypeInferenceError
      )

  | Fn (x, Some t, e') -> 
      
      let ctx1 = Context.extend ctx (x, t) in
      let end_result_typ = adv_typ_infer ctx1 e' in
      Arrow (t, end_result_typ)
        
  | Fn (x, None, e') -> 
      let t_var_input = TVar (new_tvar ()) in 
      let ctx1 = Context.extend ctx (x, t_var_input) in 
      Arrow (t_var_input, adv_typ_infer ctx1 e')
      
                          
  | Apply (e1, e2) -> let te2 = adv_typ_infer ctx e2 in
  
      (match adv_typ_infer ctx e1 with
       | Arrow (funt1, funt2) ->
           if (unify funt1 te2) = () then funt2 else raise TypeInferenceError
       | TVar t -> 
           (
             match t.contents with
             | Some Arrow (funt1, funt2) -> 
                 if (unify funt1 te2) = () then funt2 else raise TypeInferenceError 
             | _ -> raise TypeInferenceError
           )
       | _ ->  raise TypeInferenceError) 

  | Rec (f, Some t, e') -> 
      let ctx1 = Context.extend ctx (f, t) in
      let end_result_typ = adv_typ_infer ctx1 e' in
      if (unify end_result_typ t) = () then t else raise TypeInferenceError
              
  | Rec (f, None, e') -> 
      (match e' with 
       | Fn (_, _, _) -> 
           let input_tvar = TVar (new_tvar()) in
           let output_tvar = TVar (new_tvar()) in
           let ctx1 = Context.extend ctx (f, Arrow (input_tvar, output_tvar)) in
           let end_result_typ = adv_typ_infer ctx1 e' in 
           if (unify end_result_typ (Arrow(input_tvar, output_tvar))) = () then end_result_typ else raise TypeInferenceError
  
       | _-> 
           let t_var_input = TVar (new_tvar ()) in 
           let ctx1 = Context.extend ctx (f, t_var_input) in
           let end_result_typ = adv_typ_infer ctx1 e' in 
           if (unify end_result_typ t_var_input) = () then end_result_typ else raise TypeInferenceError
      )
  
  | Let (x, e1, e2) -> let te1 = adv_typ_infer ctx e1 in
      let ctx1 = Context.extend ctx (x, te1) in
      adv_typ_infer ctx1 e2
      
        
  | Var x -> begin
      match Context.lookup ctx x with
      | Some t -> t
      | None -> raise TypeInferenceError
    end

(** DO NOT Change This Definition *)
let adv_typ_infer_test_helper ctx e =
  try
    Some (adv_typ_infer ctx e)
  with
  | UnificationFailure -> None
  | OccursCheckFailure -> None
  | TypeInferenceError -> None

(**
 ************************************************************
 You Don't Need to Modify Anything After This Line
 ************************************************************

 Following definitions are the helper entrypoints
 so that you can do some experiments in the top-level.
 Once you implement [exp_parser], [typ_infer], and [eval],
 you can test them with [infer_main] in the top-level.
 Likewise, once you implement [exp_parser], [adv_typ_infer], and [eval],
 you can test them with [adv_infer_main] in the top-level.
 *)
let infer_main exp_str =
  match parse_exp exp_str with
  | None -> raise ParserFailure
  | Some e ->
      print_string "input expression       : "; print_exp e; print_newline ();
      let t = typ_infer Context.empty e in
      print_string "type of the expression : "; print_typ t; print_newline ();
      print_string "evaluation result      : "; print_exp (eval e); print_newline ()

let adv_infer_main exp_str =
  match parse_exp exp_str with
  | None -> raise ParserFailure
  | Some e ->
      print_string "input expression       : "; print_exp e; print_newline ();
      let t = adv_typ_infer Context.empty e in
      print_string "type of the expression : "; print_typ t; print_newline ();
      print_string "evaluation result      : "; print_exp (eval e); print_newline ()
