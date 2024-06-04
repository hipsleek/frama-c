(**************************************************************************)
(*                                                                        *)
(*  This file is part of the Frama-C's E-ACSL plug-in.                    *)
(*                                                                        *)
(*  Copyright (C) 2012-2024                                               *)
(*    CEA (Commissariat à l'énergie atomique et aux énergies              *)
(*         alternatives)                                                  *)
(*                                                                        *)
(*  you can redistribute it and/or modify it under the terms of the GNU   *)
(*  Lesser General Public License as published by the Free Software       *)
(*  Foundation, version 2.1.                                              *)
(*                                                                        *)
(*  It is distributed in the hope that it will be useful,                 *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         *)
(*  GNU Lesser General Public License for more details.                   *)
(*                                                                        *)
(*  See the GNU Lesser General Public License version 2.1                 *)
(*  for more details (enclosed in the file licenses/LGPLv2.1).            *)
(*                                                                        *)
(**************************************************************************)

(* return true if the global g has been generated by E-ACSL *)
let is_generated (g: Cil_datatype.Global.t) =
  let name =
    match g with
    | GType (ti, _) -> ti.tname
    | GCompTagDecl (ci, _) | GCompTag(ci, _) -> ci.cname
    | GEnumTagDecl (ei, _) | GEnumTag (ei, _) -> ei.ename
    | GVarDecl (vi, _) | GVar(vi, _, _) -> vi.vname
    | GFun (fundec, _) -> fundec.svar.vname
    | GFunDecl (_, vi, _) -> vi.vname
    | _ -> ""
  in
  E_ACSL.Functions.RTL.is_generated_name name

module Printer_extension(X: Printer.PrinterClass) = struct

  class printer = object
    inherit X.printer as super

    method! global fmt g =
      let loc, _ = Cil_datatype.Global.loc g in
      let file = loc.Filepath.pos_path in
      if file = Datatype.Filepath.dummy ||
         List.exists (fun s -> s = file) (Kernel.Files.get ()) ||
         is_generated g
      then super#global fmt g

  end

end

let () = Printer.update_printer (module Printer_extension)
