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

let slk_suffix = [".ch"]

let run () =
	let () = List.iter
		(fun suf -> File.new_file_type suf Parser.custom_parser) slk_suffix in
	let slk_dir = 
		ref "" 
	in get_slk_dir slk_dir;
	let command = Printf.sprintf "%s/sleek %s/input.slk > %s/sleek_analysis.out" !slk_dir cwd cwd in
	ignore (Sys.command command)
	(* ignore ((Sleekmain.parse_file Nativefront.list_parse) (Printf.sprintf "%s./input.slk" cwd)) *)

let () = 
  Dynamic.register 
    ~plugin:"SLEEK" 
    "run" 
    (Datatype.func Datatype.unit Datatype.unit) 
    run
    ()