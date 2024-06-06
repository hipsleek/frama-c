let help_msg = "Call external prover SLEEK"

module Self = Plugin.Register
	(struct
		let name = "SLEEK"
		let shortname = "sleek"
		let help = help_msg
	end)

module Enabled = Self.False
	(struct
		let option_name = "-sleek"
		let help = "when on (off by default), " ^ help_msg
	end)

module Output_file = Self.String
	(struct
		let option_name = "-sleek-output"
		let default = "-"
		let arg_name = "output-file"
		let help = 
			"file where the message is output (default: output to the console)"
	end)


