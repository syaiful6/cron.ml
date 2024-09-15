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
    match (fa, fm) with Some f, Some b -> Some (f b) | _, _ -> None

  let lift_option2 f fa fb = option_apply (Option.map f fa) fb

  let traverse_option f xs =
    List.fold_right
      (fun x ys -> lift_option2 List.cons (f x) ys)
      xs (Option.some [])

  (* let lift_list2 f xs ys =
     List.concat_map (fun x -> List.concat_map (fun y -> [ f x y ]) ys) xs *)

  let minimum cmp xs =
    let min acc x =
      match acc with
      | None -> Some x
      | Some y -> Some (if cmp x y < 0 then x else y)
    in
    List.fold_left min None xs
end

type expanded = {
  min : field;
  hour : field;
  dom : field;
  month : field;
  dow : field;
}

and field = int list

let ( -- ) i j =
  let rec aux n acc = if n < i then acc else aux (n - 1) (n :: acc) in
  aux j []

let ( <.> ) f g x = f (g x)

let fill_to (start, finish) step =
  let nums = Seq.unfold (fun x -> Option.some (start + (step * x), x + 1)) 0 in
  if step <= 0 then []
  else if finish < start then []
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

(* let tod_ptime_span hour minute =
   Ptime.Span.of_int_s @@ ((hour * 60 * 60) + (minute * 60)) *)

(* let valid_tods hrs mns =
   let minutes = List.sort Int.compare mns in
   let hours = List.sort Int.compare hrs in
   Utils.lift_list2 tod_ptime_span hours minutes *)

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
    match (fa, fm) with Some f, Some m -> Some (f m) | _, _ -> None
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
        if
          Field.restricted cron.day_of_month
          && Field.restricted cron.day_of_week
        then elem dom expanded.dom || elem week_day expanded.dow
        else elem dom expanded.dom && elem week_day expanded.dow
      in
      List.for_all Fun.id
        [
          elem mn expanded.min;
          elem hr expanded.hour;
          elem mth expanded.month;
          check_dom_and_dow;
        ]
