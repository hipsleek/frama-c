open Cil_types
open Cabs

let cwd = Sys.getcwd() ^ "/src/plugins/sleek"

let get_slk_dir dir = 
	let get_config_value filename key =
		let ic = open_in filename in
		let rec find_key () =
			match input_line ic with
			| exception End_of_file -> close_in ic; None
			| line ->
				if String.length line > 0 && line.[0] <> '#' then
					let parts = String.split_on_char '=' line in
					match parts with
					| [k; v] when String.trim k = key -> close_in ic; Some (String.trim v)
					| _ -> find_key ()
				else
					find_key ()
		in
		find_key ()
	in
		match get_config_value (cwd ^ "/sleek.conf") "hipsleek_dir" with
		| Some value -> dir := value
		| None -> ()

(*dummy implemtation of parser*)
let custom_parser f = 
	let empty_cil : Cil_types.file = {
		fileName = Filepath.Normalized.of_string f;
		globals = [];
		globinit = None;
		globinitcalled = false;
	} in
	let filepath : Datatype.Filepath.t = Datatype.Filepath.of_string f in
	let empty_definitions : (bool * definition) list = [] in
	let empty_cabs : Cabs.file = (filepath , empty_definitions) in
	(empty_cil, empty_cabs)

let slk_suffix = [".cslk"]

let run () =
	let () = List.iter
		(fun suf -> File.new_file_type suf custom_parser) slk_suffix in
	let slk_dir = 
		ref "" 
	in get_slk_dir slk_dir;
	let command = Printf.sprintf "%s/sleek %s/input.slk > %s/sleek_analysis.out" !slk_dir cwd cwd in
	ignore (Unix.system command)

let () = 
  Dynamic.register 
    ~plugin:"SLEEK" 
    "run" 
    (Datatype.func Datatype.unit Datatype.unit) 
    run
    ()