open Cil_types

let run () =
  Dynamic.Parameter.Bool.set "-eva-context-valid-pointers" true;
  Eva.Analysis.compute ();
  Globals.Functions.iter
    (fun kf ->
       let kf_name = Kernel_function.get_name kf in
       Populate_spec.populate_funspec kf [`Assigns];
       let spec = Annotations.funspec kf in
       let ip = Property.ip_of_spec kf Kglobal ~active:[] spec in
       List.iter
         (fun ip ->
            let bname = match Property.get_behavior ip with
              | None -> "?"
              | Some b -> b.b_name
            in
            let function_name = kf_name ^ ": behavior " ^ bname in
            let status = Property_status.get ip in
            Kernel.result "@[%s@ @[%a@]@]"
              function_name Property_status.pretty status)
         ip)

let () = Boot.Main.extend run
