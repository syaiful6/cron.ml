let ( <.> ) f g x = f (g x)
let in_range (min, max) x = min <= x && x <= max

module Element = struct
  type t = Star | Specified of int | Range of int * int

  let equal a b =
    match (a, b) with
    | Star, Star -> true
    | Specified a, Specified b -> a = b
    | Range (a, b), Range (c, d) -> a = c && b = d
    | _, _ -> false

  let to_string = function
    | Star -> "*"
    | Specified a -> Int.to_string a
    | Range (a, b) -> Int.to_string a ^ "-" ^ Int.to_string b

  let pp ppf elem =
    match elem with
    | Star -> Format.fprintf ppf "*"
    | Specified a -> Format.fprintf ppf "%d" a
    | Range (a, b) -> Format.fprintf ppf "%d-%d" a b

  let create_range x y = if x <= y then Some (Range (x, y)) else None

  let create_specified = function
    | n when n >= 0 -> Some (Specified n)
    | _ -> None

  let valid elem min max =
    match elem with
    | Star -> true
    | Specified n -> in_range (min, max) n
    | Range (x, y) -> in_range (min, max) x && in_range (min, max) y
end

module Field = struct
  type t =
    | Field of Element.t
    | List of Element.t list
    | Step of Element.t * int

  let equal a b =
    match (a, b) with
    | Field a, Field b -> Element.equal a b
    | Field a, List (b :: []) -> Element.equal a b
    | List (a :: []), Field b -> Element.equal a b
    | List xs, List ys -> List.equal Element.equal xs ys
    | Step (el, a), Step (dl, b) -> Element.equal el dl && a = b
    | _, _ -> false

  let to_string = function
    | Field elem -> Element.to_string elem
    | List xs -> String.concat "-" (List.map Element.to_string xs)
    | Step (elem, step) -> Element.to_string elem ^ "/" ^ Int.to_string step

  let pp_dash ppf () = Format.fprintf ppf "-"

  let pp ppf field =
    match field with
    | Field elem -> Format.fprintf ppf "%a" Element.pp elem
    | List xs ->
        Format.fprintf ppf "%a"
          Format.(pp_print_list ~pp_sep:pp_dash Element.pp)
          xs
    | Step (elem, step) -> Format.fprintf ppf "%a/%d" Element.pp elem step

  let valid_cron_field field min max =
    match field with
    | Field elem -> Element.valid elem min max
    | List xs -> List.for_all (fun x -> Element.valid x min max) xs
    | Step (elem, step) ->
        Element.valid elem min max && in_range (min, max) step

  let minute_optional cf = if valid_cron_field cf 0 59 then Some cf else None
  let minute = Option.get <.> minute_optional
  let hour_optional cf = if valid_cron_field cf 0 23 then Some cf else None
  let hour = Option.get <.> hour_optional

  let day_of_month_optional cf =
    if valid_cron_field cf 1 31 then Some cf else None

  let day_of_month = Option.get <.> day_of_month_optional
  let month_optional cf = if valid_cron_field cf 1 12 then Some cf else None
  let month = Option.get <.> month_optional

  let day_of_week_optional cf =
    if valid_cron_field cf 0 7 then Some cf else None

  let day_of_week = Option.get <.> day_of_week_optional
  let make_step_field elem n = if n > 0 then Some (Step (elem, n)) else None

  let is_star = function
    | Field Element.Star -> true
    | List xs -> List.exists (Element.equal Element.Star) xs
    | Step (Element.Star, step) -> step = 1
    | _ -> false

  let restricted = not <.> is_star
end

type t = {
  minute : Field.t;
  hour : Field.t;
  day_of_month : Field.t;
  month : Field.t;
  day_of_week : Field.t;
}

let equal a b =
  List.equal Field.equal
    [ a.minute; a.hour; a.day_of_month; a.month; a.day_of_week ]
    [ b.minute; b.hour; b.day_of_month; b.month; b.day_of_week ]

let to_string t =
  String.concat " "
    [
      Field.to_string t.minute;
      Field.to_string t.hour;
      Field.to_string t.day_of_month;
      Field.to_string t.month;
      Field.to_string t.day_of_week;
    ]

(** Shorthand for an expression that always matches. Parsed with `* * * * *` *)
let every_minute =
  {
    minute = Field.Field Element.Star;
    hour = Field.Field Element.Star;
    day_of_month = Field.Field Element.Star;
    month = Field.Field Element.Star;
    day_of_week = Field.Field Element.Star;
  }

(** Shorthand for every hour on the hour. Parsed with `@hourly`, `0 * * * *` *)
let hourly = { every_minute with minute = Field.Field (Element.Specified 0) }

(** Shorthand for every day at midnight. Parsed with `@daily`, `0 0 * * *` *)
let daily = { hourly with hour = Field.Field (Element.Specified 0) }

(** Shorthand for every sunday at midnight. Parsed with `@weekly`, `0 0 * * 0` *)
let weekly = { daily with day_of_week = Field.Field (Element.Specified 0) }

(** Shorthand for every 1st of the month at midnight. Parsed with `@monthly`, `0 0 1 * *` *)
let monthly = { daily with day_of_month = Field.Field (Element.Specified 1) }

(** Shorthand for every January 1st at midnight. Parsed with `@yearly`, `0 0 1 1 *` *)
let yearly = { monthly with month = Field.Field (Element.Specified 1) }

let pp ppf t =
  Format.fprintf ppf "%a"
    Format.(pp_print_list ~pp_sep:Format.pp_print_space Field.pp)
    [ t.minute; t.hour; t.day_of_month; t.month; t.day_of_week ]
