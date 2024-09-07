val cron_schedule_p : Types.t Angstrom.t
(** Angstrom parser for a cron schedule. Compiles fully with the standard
    cron format. Also includes the following shorthand formats which cron also
    support: @yearly, @monthly, @weekly, @daily, @hourly. Note that this parser
    will fail if there is extraneous input. This is to prevent things
    like extra fields. If you want a more lax parser, use `cron_schedule_loose`,
    which is fine with extra input. *)

val cron_schedule_loose : Types.t Angstrom.t
(** Same as `cron_schedule_p`, but does not fail on extraneous input. *)

val parse_cron_schedule : string -> (Types.t, string) result
(** Convenience function to parse a cron schedule string *)
