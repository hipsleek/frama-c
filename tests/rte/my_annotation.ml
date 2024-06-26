let print () =
  File.pretty_ast ();
  Kernel.log "================================"

let print_status () =
  Kernel.log "printing status";
  let _, _, get_signedOv_status = RteGen.Api.get_signedOv_status () in
  Globals.Functions.iter
    (fun kf ->
       Kernel.log "kf = %s rte_gen_status = %b\n"
         (Kernel_function.get_name kf)
         (get_signedOv_status kf))

let main () =
  Dynamic.Parameter.Bool.set "-rte-mem" true;
  Dynamic.Parameter.Bool.set "-rte-pointer-call" true;
  Dynamic.Parameter.Bool.set "-rte-float-to-int" true;
  Dynamic.Parameter.Bool.set "-rte-div" true;
  Kernel.SignedDowncast.on ();
  Kernel.SignedOverflow.on ();

  if not(Ast.is_computed ()) then Ast.compute () ;
  print ();

  Globals.Functions.iter (fun kf -> RteGen.Api.annotate_kf kf);
  print () ;
  print_status ();

  Kernel.log "Removing some rte annotations" ;
  let _, set_signed, _ = RteGen.Api.get_signedOv_status () in
  let emitter = Dynamic.get ~plugin:"RteGen" "emitter" Emitter.ty in
  let filter = function
    | Alarms.Overflow _ -> true
    | _ -> false
  in
  Alarms.remove ~filter emitter;
  print ();
  print_status ();

  (*  Dynamic.Parameter.Bool.set "-rte-all" true;*)
  let one_on_two = ref true in
  Globals.Functions.iter
    (fun kf ->
       if !one_on_two then begin
         set_signed kf false;
         RteGen.Api.annotate_kf kf
       end;
       one_on_two := not !one_on_two);
  print ()  ;
  print_status ()

let () = Boot.Main.extend main
