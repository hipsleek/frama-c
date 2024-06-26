
let main _ =
  let all = Cil_datatype.Fundec.Set.empty in
  let new_proj = Constant_Propagation.Api.get all ~cast_intro:true in
  Project.on
    new_proj
    (fun () ->
       Kernel.CodeOutput.output
         (fun fmt -> Format.fprintf fmt "After Constant propagation :@."))
    ();
  File.pretty_ast ~prj:new_proj ();;

let () = Boot.Main.extend main
