let run () =
  if Hello_options.Enabled.get() then
    Hello_print.output "Hello, world!"

let () = Boot.Main.extend run
