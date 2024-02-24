let open_account (pass : password) : bank_account =
  
  let wrong_pass_counter = ref 0 in 
  let balance = ref 0 in 
  
  let can_access p = fun () ->  
    if (!wrong_pass_counter >= 3) then raise account_locked; 
    if p <> pass then (wrong_pass_counter := !wrong_pass_counter + 1; raise wrong_pass); 
  in 
  
  let deposit p amt = 
    can_access p ();
    wrong_pass_counter := 0;
    if amt < 0 then raise negative_amount
    else
      balance := !balance + amt 
  in

  let show_balance p = 
    can_access p ();
    wrong_pass_counter := 0;
    !balance 
  in
  
  let withdraw p (amt : int) = 
    can_access p ();
    wrong_pass_counter := 0;
    if amt < 0 then raise negative_amount
    else if amt > !balance then raise not_enough_balance
    else
      balance := !balance - amt 
  in
  
  {
    deposit = deposit;
    show_balance = show_balance;
    withdraw = withdraw;
  }
  

