let slk_dir = "./~/hipsleek"

let run () =
	if Sleek_options.Enabled.get() then
		let command = Printf.sprintf "%s/sleek input.slk > sleek_analysis.out" slk_dir in
		ignore(Unix.system command);
		Sleek_print.output "Analysis has been completed"

let () = Boot.Main.extend run



