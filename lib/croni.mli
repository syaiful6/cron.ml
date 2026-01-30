module Element : sig
  type t =
    | Star
    | Specified of int
    | Range of int * int

  val equal : t -> t -> bool
  val to_string : t -> string
  val pp : Format.formatter -> t -> unit
  val valid : t -> int -> int -> bool
end

module Field : sig
  type t =
    | Field of Element.t
    | List of Element.t list
    | Step of Element.t * int

  val equal : t -> t -> bool
  val to_string : t -> string
  val pp : Format.formatter -> t -> unit
  val is_star : t -> bool
  val restricted : t -> bool
end

type t =
  { minute : Field.t
  ; hour : Field.t
  ; day_of_month : Field.t
  ; month : Field.t
  ; day_of_week : Field.t
  }

val equal : t -> t -> bool
(** equal x y is true if and only x = y *)

val to_string : t -> string
(** Convert t to string representation *)

val every_minute : t
(** Shorthand for an expression that always matches. Parsed with `* * * * *` *)

val hourly : t
(** Shorthand for every hour on the hour. Parsed with `@hourly`, `0 * * * *` *)

val daily : t
(** Shorthand for every day at midnight. Parsed with `@daily`, `0 0 * * *` *)

val weekly : t
(** Shorthand for every sunday at midnight. Parsed with `@weekly`, `0 0 * * 0` *)

val monthly : t
(** Shorthand for every 1st of the month at midnight. Parsed with `@monthly`, `0 0 1 * *` *)

val yearly : t
(** Shorthand for every January 1st at midnight. Parsed with `@yearly`, `0 0 1 1 *` *)

val pp : Format.formatter -> t -> unit
(** [pp pf t] prints the given cron [t] on [ppf] *)

module Parser : sig
  val cron_schedule_p : t Angstrom.t
  (** Angstrom parser for a cron schedule. Compiles fully with the standard
          cron format. Also includes the following shorthand formats which cron also
          support: @yearly, @monthly, @weekly, @daily, @hourly. Note that this parser
          will fail if there is extraneous input. This is to prevent things
          like extra fields. If you want a more lax parser, use `cron_schedule_loose`,
          which is fine with extra input. *)

  val cron_schedule_loose : t Angstrom.t
  (** Same as `cron_schedule_p`, but does not fail on extraneous input. *)

  val parse_cron_schedule : string -> (t, string) result
  (** Convenience function to parse a cron schedule string *)
end

module Schedule : sig
  val matches : t -> Ptime.t -> bool
  (** Does the given cron schedule match for the given timestamp? *)

  val next : t -> Ptime.t -> Ptime.t option
  (** [next schedule time] returns the next time from the given starting point
      where the schedule will match. Returns [None] if the schedule will never
      match (within a 5 year search window). Note that this function is not
      inclusive of the given time: the result will always be at least 1 minute
      beyond the given time. *)
end
