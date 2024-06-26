class add_skip = object(this)
  inherit Visitor.frama_c_inplace

  method! vfunc f =
    File.must_recompute_cfg f ;
    Cil.DoChildren

  method! vstmt s =
    let open Cil_types in
    begin match s.skind with
      | If(_) ->
        this#queueInstr([Skip(Current_loc.get())])
      | _ -> ()
    end ;
    Cil.DoChildren

  method! vinst _ =
    let open Cil_types in
    this#queueInstr([Skip(Current_loc.get())]) ;
    Cil.DoChildren
end

let run () =
  Visitor.visitFramacFileSameGlobals (new add_skip) (Ast.get())

let () =
  Boot.Main.extend run
