module Utils = struct
  let non_empty = function [] -> None | xs -> Some xs

  let rec elem_by eq y ls =
    match ls with [] -> false | x :: xs -> eq x y || elem_by eq y xs

  let nub eq ls =
    let rec aux = function
      | [], _ -> []
      | y :: ys, xs when elem_by eq y xs -> aux (ys, xs)
      | y :: ys, xs -> y :: aux (ys, y :: xs)
    in
    aux (ls, [])

  let option_apply fa fm =
    match fa, fm with Some f, Some b -> Some (f b) | _, _ -> None

  let lift_option2 f fa fb = option_apply (Option.map f fa) fb

  let traverse_option f xs =
    List.fold_right
      (fun x ys -> lift_option2 List.cons (f x) ys)
      xs
      (Option.some [])

  let minimum cmp xs =
    let min acc x =
      match acc with
      | None -> Some x
      | Some y -> Some (if cmp x y < 0 then x else y)
    in
    List.fold_left min None xs
end

type expanded =
  { min : field
  ; hour : field
  ; dom : field
  ; month : field
  ; dow : field
  }

and field = int list

let ( -- ) i j =
  let rec aux n acc = if n < i then acc else aux (n - 1) (n :: acc) in
  aux j []

let ( <.> ) f g x = f (g x)

let fill_to (start, finish) step =
  let nums = Seq.unfold (fun x -> Option.some (start + (step * x), x + 1)) 0 in
  if step <= 0
  then []
  else if finish < start
  then []
  else List.of_seq @@ Seq.take_while (( >= ) finish) nums

let expand_element (lo, hi) element =
  let open Types.Element in
  match element with
  | Star -> Some (lo -- hi)
  | Specified x -> Some [ x ]
  | Range (start, finish) -> Some (start -- finish)

let rec expand_element_stepped range element step =
  let open Types.Element in
  match element with
  | Star -> Utils.non_empty @@ fill_to range step
  | Range (a, b) ->
    let finish = min b (snd range) in
    Utils.non_empty @@ fill_to (a, finish) step
  | Specified x -> expand_element_stepped (x, snd range) Star step

let expand_field range field =
  let open Types.Field in
  match field with
  | Field elem -> expand_element range elem
  | List xs ->
    Option.map
      (Utils.nub Int.equal <.> List.concat)
      (Utils.traverse_option (expand_element range) xs)
  | Step (elem, step) -> expand_element_stepped range elem step

let has_valid_for_month day days =
  let minimum xs =
    match Utils.minimum Int.compare xs with None -> 0 | Some x -> x
  in
  match day with
  | 1 -> minimum days <= 31
  | 2 -> minimum days <= 29
  | 3 -> minimum days <= 31
  | 4 -> minimum days <= 30
  | 5 -> minimum days <= 31
  | 6 -> minimum days <= 30
  | 7 -> minimum days <= 31
  | 8 -> minimum days <= 31
  | 9 -> minimum days <= 30
  | 10 -> minimum days <= 31
  | 11 -> minimum days <= 30
  | 12 -> minimum days <= 31
  | _ -> false

let expand (cron : Types.t) =
  let min_f = expand_field (0, 59) cron.minute in
  let hour_f = expand_field (0, 23) cron.hour in
  let dom_f = expand_field (1, 31) cron.day_of_month in
  let month_f = expand_field (1, 12) cron.month in
  let remap_sunday lst =
    match List.partition (fun x -> x = 0 || x = 7) lst with
    | [], _ -> lst
    | _, not_sundays -> List.cons 0 not_sundays
  in
  let dow_f = Option.map remap_sunday @@ expand_field (0, 7) cron.day_of_week in
  let dom_restricted = Types.Field.restricted cron.day_of_month in
  let dow_restricted = Types.Field.restricted cron.day_of_week in
  let satisfiable expanded =
    (dom_restricted && dow_restricted)
    || List.exists (fun m -> has_valid_for_month m expanded.dom) expanded.month
  in
  let ( <$> ) = Option.map in
  let ( <*> ) fa fm =
    match fa, fm with Some f, Some m -> Some (f m) | _, _ -> None
  in
  let create_expanded min hour dom month dow = { min; hour; dom; month; dow } in
  let expanded =
    create_expanded <$> min_f <*> hour_f <*> dom_f <*> month_f <*> dow_f
  in
  Option.bind expanded @@ fun exp -> if satisfiable exp then Some exp else None

(** Does the given cron schedule match for the given timestamp? *)
let matches cron ptime =
  let date, time = Ptime.to_date_time ptime in
  let _, mth, dom = date in
  let hr, mn, _ = fst time in
  let week_day = Ptime.weekday_num ptime in
  let elem a = List.exists (Int.equal a) in
  match expand cron with
  | None -> false
  | Some expanded ->
    let open Types in
    let check_dom_and_dow =
      if Field.restricted cron.day_of_month && Field.restricted cron.day_of_week
      then elem dom expanded.dom || elem week_day expanded.dow
      else elem dom expanded.dom && elem week_day expanded.dow
    in
    List.for_all
      Fun.id
      [ elem mn expanded.min
      ; elem hr expanded.hour
      ; elem mth expanded.month
      ; check_dom_and_dow
      ]

let is_leap_year year =
  (year mod 4 = 0 && year mod 100 <> 0) || year mod 400 = 0

let days_in_month year month =
  match month with
  | 1 -> 31
  | 2 -> if is_leap_year year then 29 else 28
  | 3 -> 31
  | 4 -> 30
  | 5 -> 31
  | 6 -> 30
  | 7 -> 31
  | 8 -> 31
  | 9 -> 30
  | 10 -> 31
  | 11 -> 30
  | 12 -> 31
  | _ -> 0

let ptime_min a b = if Ptime.compare a b <= 0 then a else b

let next_match expanded time =
  let (start_year, start_month, start_day), ((start_hour, start_minute, _), _) =
    Ptime.to_date_time time
  in
  let find_ge x lst =
    let sorted = List.sort Int.compare lst in
    List.find_opt (fun v -> v >= x) sorted
  in
  let rec search_year year =
    if year > start_year + 5
    then None
    else
      let first_month = if year = start_year then start_month else 1 in
      match search_month year first_month with
      | Some _ as result -> result
      | None -> search_year (year + 1)
  and search_month year month =
    match find_ge month expanded.month with
    | None -> None
    | Some m ->
      let first_day =
        if year = start_year && m = start_month then start_day else 1
      in
      (match search_day year m first_day with
      | Some _ as result -> result
      | None -> search_month year (m + 1))
  and search_day year month day =
    let max_day = days_in_month year month in
    if day > max_day || day > 31
    then None
    else
      match Ptime.of_date_time ((year, month, day), ((0, 0, 0), 0)) with
      | None -> search_day year month (day + 1)
      | Some ptime ->
        let dow = Ptime.weekday_num ptime in
        let dom_valid = List.exists (Int.equal day) expanded.dom in
        let dow_valid = List.exists (Int.equal dow) expanded.dow in
        if dom_valid && dow_valid
        then
          let first_hour =
            if year = start_year && month = start_month && day = start_day
            then start_hour
            else 0
          in
          match search_hour year month day first_hour with
          | Some _ as result -> result
          | None -> search_day year month (day + 1)
        else search_day year month (day + 1)
  and search_hour year month day hour =
    match find_ge hour expanded.hour with
    | None -> None
    | Some h ->
      let first_minute =
        if
          year = start_year
          && month = start_month
          && day = start_day
          && h = start_hour
        then start_minute
        else 0
      in
      (match search_minute year month day h first_minute with
      | Some _ as result -> result
      | None -> search_hour year month day (h + 1))
  and search_minute year month day hour minute =
    match find_ge minute expanded.min with
    | None -> None
    | Some m -> Ptime.of_date_time ((year, month, day), ((hour, m, 0), 0))
  in
  search_year start_year

let star_field = Types.Field.Field Types.Element.Star

let next (cron : Types.t) start =
  let dom_restricted = Types.Field.restricted cron.day_of_month in
  let dow_restricted = Types.Field.restricted cron.day_of_week in
  match Ptime.add_span start (Ptime.Span.of_int_s 60) with
  | None -> None
  | Some time ->
    if dom_restricted && dow_restricted
    then
      (* Trick from Python's croniter: run with DOM=* and DOW=*, take earlier *)
      let dom_star_result =
        let cron' = { cron with day_of_month = star_field } in
        Option.bind (expand cron') (fun exp -> next_match exp time)
      in
      let dow_star_result =
        let cron' = { cron with day_of_week = star_field } in
        Option.bind (expand cron') (fun exp -> next_match exp time)
      in
      match dom_star_result, dow_star_result with
      | Some a, Some b -> Some (ptime_min a b)
      | Some a, None -> Some a
      | None, Some b -> Some b
      | None, None -> None
    else Option.bind (expand cron) (fun exp -> next_match exp time)
