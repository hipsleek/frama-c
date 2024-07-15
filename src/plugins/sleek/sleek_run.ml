let cwd = Sys.getcwd() ^ "/src/plugins/sleek"

let slk_suffix = [".ch"]

let run () =
	List.iter
		(fun suf -> File.new_file_type suf Parser.custom_parser) slk_suffix;
	()

let () =
  Dynamic.register 
    ~plugin:"SLEEK" 
    "run" 
    (Datatype.func Datatype.unit Datatype.unit) 
    run
    ();
