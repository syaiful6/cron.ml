open Angstrom
open Types

let ( <.> ) f g x = f (g x)

let ( -- ) i j =
  let rec aux n acc = if n < i then acc else aux (n - 1) (n :: acc) in
  aux j []

let const_str_p i str = Fun.const i <$> string str

let parse_day =
  choice
  @@ List.map2 const_str_p (1 -- 7)
       [ "mon"; "tue"; "wed"; "thu"; "fri"; "sat"; "sun" ]

let parse_month =
  choice
  @@ List.map2 const_str_p (1 -- 12)
       [
         "jan";
         "feb";
         "mar";
         "apr";
         "may";
         "jun";
         "jul";
         "aug";
         "sep";
         "oct";
         "nov";
         "dec";
       ]

let parse_int =
  take_while1 (function '0' .. '9' -> true | _ -> false) >>| int_of_string

type str_support = MonthlyString | DayString | NoString

let support_parser = function
  | MonthlyString -> choice [ parse_month; parse_int ]
  | DayString -> choice [ parse_day; parse_int ]
  | NoString -> parse_int

let m_parse f msg = Option.fold ~none:(fail msg) ~some:return <.> f

let range_element_p ss =
  support_parser ss >>= fun start ->
  char '-'
  *> ( support_parser ss >>= fun ends ->
       m_parse
         (Element.create_range start)
         "start of range must be less than or equal to end" ends )

let specified_element_p ss =
  support_parser ss
  >>= m_parse Element.create_specified "specified field value out of range"

let element_p ss =
  let star_p = char '*' *> return Element.Star in
  range_element_p ss <|> star_p <|> specified_element_p ss

let step_field_p ss =
  element_p ss >>= fun elem ->
  char '/'
  *> (parse_int >>= m_parse (Field.make_step_field elem) "invalid stepping")

let list_field_p p = (fun xs -> Field.List xs) <$> sep_by1 (char ',') p

let cron_field_p ss =
  step_field_p ss
  <|> list_field_p (element_p ss)
  <|> ((fun x -> Field.Field x) <$> element_p ss)

let yearly_p = string "@yearly" *> return yearly
let monthly_p = string "@monthly" *> return monthly
let weekly_p = string "@weekly" *> return weekly
let daily_p = string "@daily" *> return daily
let hourly_p = string "@hourly" *> return hourly

let minutes_p =
  cron_field_p NoString >>= m_parse Field.minute_optional "minutes out of range"

let hours_p =
  cron_field_p NoString >>= m_parse Field.hour_optional "hours out of range"

let day_of_month_p =
  cron_field_p NoString
  >>= m_parse Field.day_of_month_optional "day of month out of range"

let month_p =
  cron_field_p MonthlyString
  >>= m_parse Field.month_optional "month out of range"

let day_of_week_p =
  cron_field_p DayString
  >>= m_parse Field.day_of_week_optional "day of week out of range"

let classic_p =
  let mk_schedule minute hour day_of_month month day_of_week =
    { minute; hour; day_of_month; month; day_of_week }
  in
  let space = char ' ' in
  mk_schedule <$> (minutes_p <* space) <*> (hours_p <* space)
  <*> (day_of_month_p <* space) <*> (month_p <* space) <*> day_of_week_p

let cron_schedule_loose =
  yearly_p <|> monthly_p <|> weekly_p <|> daily_p <|> hourly_p <|> classic_p

let cron_schedule_p = cron_schedule_loose <* end_of_input

let parse_cron_schedule =
  parse_string ~consume:All cron_schedule_p <.> String.lowercase_ascii
