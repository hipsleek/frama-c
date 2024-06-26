let print () =
  File.pretty_ast ();
  Kernel.log "================================"

let print_status () =
  Kernel.log "printing status";
  let  _, _, get_signedOv_status = RteGen.Api.get_signedOv_status () in
  Globals.Functions.iter
    (fun kf ->
       Kernel.log "kf = %s rte_gen_status = %b\n"
         (Kernel_function.get_name kf)
         (get_signedOv_status kf))

let main () =
  Dynamic.Parameter.Bool.set "-rte-mem" false;
  Dynamic.Parameter.Bool.set "-rte-pointer-call" false;
  Dynamic.Parameter.Bool.set "-rte-float-to-int" false;
  Dynamic.Parameter.Bool.set "-rte-div" false;
  Kernel.SignedOverflow.off ();
  Kernel.SignedDowncast.off ();
  print ();
  print_status ();

  Kernel.log "computing rte-div annotations" ;
  Dynamic.Parameter.Bool.set "-rte-div" true ;
  RteGen.Api.compute () ;
  print ();
  print_status ();

  Kernel.log "removing rte-div alarms" ;
  let emitter = Dynamic.get ~plugin:"RteGen" "emitter" Emitter.ty in
  let filter = function
    | Alarms.Division_by_zero _ -> true
    | _ -> false
  in
  Alarms.remove ~filter emitter;
  RteGen.Api.compute () ;
  print ();
  print_status ()

let () = Boot.Main.extend main
