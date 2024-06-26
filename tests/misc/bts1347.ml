let emitter =
  Emitter.create "emitter" ~correctness:[] ~tuning:[]
    [ Emitter.Code_annot; Emitter.Property_status ]

let run () =
  Globals.Functions.iter
    (fun kf ->
       if not (Cil_builtins.has_fc_builtin_attr (Kernel_function.get_vi kf)) then begin
         Globals.set_entry_point (Kernel_function.get_name kf) true;
         Eva.Analysis.compute();
         let hyps =
           Alarms.fold
             (fun _ kf' s ~rank:_ _ a l ->
                if Kernel_function.equal kf kf' then
                  Property.ip_of_code_annot_single kf s a :: l
                else
                  l)
             []
         in
         let s = Kernel_function.find_return kf in
         let ca = Logic_parse_string.code_annot kf s "assert 32.5>=10.;" in
         Annotations.add_code_annot emitter ~kf s ca;
         let ip = Property.ip_of_code_annot_single kf s ca in
         Property_status.emit emitter ~hyps ip Property_status.True
       end)

let () =
  Boot.Main.extend run
