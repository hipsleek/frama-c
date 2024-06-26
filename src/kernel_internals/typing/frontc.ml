(****************************************************************************)
(*                                                                          *)
(*  Copyright (C) 2001-2003                                                 *)
(*   George C. Necula    <necula@cs.berkeley.edu>                           *)
(*   Scott McPeak        <smcpeak@cs.berkeley.edu>                          *)
(*   Wes Weimer          <weimer@cs.berkeley.edu>                           *)
(*   Ben Liblit          <liblit@cs.berkeley.edu>                           *)
(*  All rights reserved.                                                    *)
(*                                                                          *)
(*  Redistribution and use in source and binary forms, with or without      *)
(*  modification, are permitted provided that the following conditions      *)
(*  are met:                                                                *)
(*                                                                          *)
(*  1. Redistributions of source code must retain the above copyright       *)
(*  notice, this list of conditions and the following disclaimer.           *)
(*                                                                          *)
(*  2. Redistributions in binary form must reproduce the above copyright    *)
(*  notice, this list of conditions and the following disclaimer in the     *)
(*  documentation and/or other materials provided with the distribution.    *)
(*                                                                          *)
(*  3. The names of the contributors may not be used to endorse or          *)
(*  promote products derived from this software without specific prior      *)
(*  written permission.                                                     *)
(*                                                                          *)
(*  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS     *)
(*  "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT       *)
(*  LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS       *)
(*  FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE          *)
(*  COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,     *)
(*  INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,    *)
(*  BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;        *)
(*  LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER        *)
(*  CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT      *)
(*  LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN       *)
(*  ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE         *)
(*  POSSIBILITY OF SUCH DAMAGE.                                             *)
(*                                                                          *)
(*  File modified by CEA (Commissariat à l'énergie atomique et aux          *)
(*                        énergies alternatives)                            *)
(*               and INRIA (Institut National de Recherche en Informatique  *)
(*                          et Automatique).                                *)
(*                                                                          *)
(****************************************************************************)

let parse_to_cabs (path : Datatype.Filepath.t) =
  try
    Kernel.feedback ~level:2 "Parsing %a" Datatype.Filepath.pretty path;
    Errorloc.clear_errors () ;
    let lexbuf, lexer =
      Clexer.init ~filename:(path :> string) Clexer.initial in
    (* The pwd during preprocessing might have changed, e.g. if a JCDB has
       been used; we may need to adjust Errorloc's working directory to
       compensate for it, otherwise relative line directives previously added
       by the preprocessor will be invalid. *)
    let pwd =
      match Parse_env.get_workdir path with
      | None -> (Filepath.pwd () :> string)
      | Some workdir -> workdir
    in
    Errorloc.setCurrentWorkingDirectory pwd;
    let cabs = Cparser.file lexer lexbuf in
    (* Cprint.print_defs cabs;*)
    Clexer.finish ();
    if Errorloc.had_errors () then begin
      Kernel.abort "There were parsing errors in %a"
        Datatype.Filepath.pretty path
    end;

    (path, cabs)
  with
  | Sys_error msg ->
    Clexer.finish () ;
    Kernel.abort "Cannot open %a : %s" Datatype.Filepath.pretty path msg ;
  | Parsing.Parse_error ->
    Errorloc.parse_error "syntax error"

module Syntactic_transformations = Hook.Fold(struct type t = Cabs.file end)
let add_syntactic_transformation = Syntactic_transformations.extend

let parse path =
  Kernel.feedback ~level:2 "Parsing %a to Cabs" Datatype.Filepath.pretty path;
  let cabs = parse_to_cabs path in
  let cabs = Syntactic_transformations.apply cabs in
  (* Now (return a function that will) convert to CIL *)
  fun _ ->
    Kernel.feedback ~level:2 "Converting %a from Cabs to CIL"
      Datatype.Filepath.pretty path;
    let cil = Cabs2cil.convFile cabs in
    cil,cabs
