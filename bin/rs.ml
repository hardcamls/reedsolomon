open Core

let () =
  Command_unix.run
    (Command.group
       ~summary:"Hardcaml Reed-solomon tooling"
       [ "simulate", Simulate.command
       ; "generate", Generate.command
       ; "model", Model.command
       ])
;;
