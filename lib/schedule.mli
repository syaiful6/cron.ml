val matches : Types.t -> Ptime.t -> bool
(** Does the given cron schedule match for the given timestamp? *)

val next : Types.t -> Ptime.t -> Ptime.t option
(** [next schedule time] returns the next time from the given starting point
    where the schedule will match. Returns [None] if the schedule will never
    match (within a 5 year search window). Note that this function is not
    inclusive of the given time: the result will always be at least 1 minute
    beyond the given time. *)
