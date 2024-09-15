# Cron

Cron data structure, Angstrom parser and schedule explorer for OCaml.

```ocaml
(** create a cron schedule and test again current timestamp *)
let schedule = Option.get_ok @@ Cron.Parser.parse_cron_schedule "*/2 * * * *"
let now = Ptime_clock.now ()
if Cron.Schedule.matches schedule now
    then
        print_endline "schedule matches"
        (** run your functions/tasks here *)
    else
        (** your cron schedule not matches against current time! DO Nothing *)
        print_endline "schedule not matches"
```