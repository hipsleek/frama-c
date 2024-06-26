let print_json () =
  Kernel.feedback
    "Value of -json-compilation-database in %s is %a"
    (Project.get_name (Project.current()))
    Filepath.Normalized.pretty (Kernel.JsonCompilationDatabase.get())

let run () =
  print_json ();
  Ast.compute();
  let prj = Project.create_by_copy ~last:true "copy" in
  Project.on prj print_json ()

let () = Boot.Main.extend run
