let schedule = Alcotest.testable Cron.pp Cron.equal

let assert_parser p testa s expected =
  let res = Angstrom.parse_string ~consume:All p s in
  Alcotest.(check (result testa string)) "same parsed" res expected

let assert_not_parsed parser s =
  let res = Angstrom.parse_string ~consume:All parser s in
  Alcotest.(check bool)
    "expected parser return Error" (Result.is_error res) true

let assert_cron_parser = assert_parser Cron.Parser.cron_schedule_p schedule

let test_parse_schedule_hourly () =
  assert_cron_parser "@hourly" (Ok Cron.hourly)

let test_parse_schedule_daily () = assert_cron_parser "@daily" (Ok Cron.daily)

let test_parse_schedule_weekly () =
  assert_cron_parser "@weekly" (Ok Cron.weekly)

let test_parse_schedule_monthly () =
  assert_cron_parser "@monthly" (Ok Cron.monthly)

let test_parse_schedule_yearly () =
  assert_cron_parser "@yearly" (Ok Cron.yearly)

let test_parse_all_stars () =
  assert_cron_parser "* * * * *" (Ok Cron.every_minute)

let test_parse_specific_values () =
  let open Cron in
  let expected =
    {
      every_minute with
      minute = Field.Field (Element.Specified 1);
      hour = Field.Field (Element.Specified 2);
      day_of_month = Field.Field (Element.Specified 3);
    }
  in
  assert_cron_parser "1 2 3 * *" (Ok expected)

let test_parse_list_values () =
  let open Cron in
  let expected =
    {
      every_minute with
      day_of_month = Field.List [ Element.Specified 3; Element.Specified 4 ];
    }
  in
  assert_cron_parser "* * 3,4 * *" (Ok expected)

let test_parse_range_values () =
  let open Cron in
  let expected =
    { every_minute with day_of_month = Field.Field (Element.Range (3, 4)) }
  in
  assert_cron_parser "* * 3-4 * *" (Ok expected)

let test_parse_step_values () =
  let open Cron in
  let expected =
    {
      every_minute with
      minute = Field.Step (Element.Star, 2);
      day_of_month = Field.Step (Element.Range (2, 10), 4);
    }
  in
  assert_cron_parser "*/2 * 2-10/4 * *" (Ok expected)

let test_refuse_recursive_steps () =
  assert_not_parsed Cron.Parser.cron_schedule_p "*/2/3 * * * *"

let test_refuse_sparse_lists () =
  assert_not_parsed Cron.Parser.cron_schedule_p "1,,2 * * * *"

let test_too_many_fields () =
  assert_not_parsed Cron.Parser.cron_schedule_p "* * * * * *"

let test_refuse_extraneous_input () =
  assert_not_parsed Cron.Parser.cron_schedule_p "* * * * *  extra input"

let test_parse_ranges_at_last_field () =
  let open Cron in
  let expected =
    { every_minute with day_of_week = Field.Field (Element.Range (3, 4)) }
  in
  assert_cron_parser "* * * * 3-4" (Ok expected)

let test_parse_list_at_last_field () =
  let open Cron in
  let expected =
    {
      every_minute with
      day_of_week = Field.List [ Element.Specified 3; Element.Specified 4 ];
    }
  in
  assert_cron_parser "* * * * 3,4" (Ok expected)

let test_parse_steps_at_last_field () =
  let open Cron in
  let expected =
    { every_minute with day_of_week = Field.Step (Element.Star, 4) }
  in
  assert_cron_parser "* * * * */4" (Ok expected)

let test_parse_sunday_as_7 () =
  let open Cron in
  let expected =
    { every_minute with day_of_week = Field.Field (Element.Specified 7) }
  in
  assert_cron_parser "* * * * 7" (Ok expected)

let test_parse_sunday_as_0 () =
  let open Cron in
  let expected =
    { every_minute with day_of_week = Field.Field (Element.Specified 0) }
  in
  assert_cron_parser "* * * * 0" (Ok expected)

let test_parse_example () =
  let open Cron in
  let expected =
    { every_minute with minute = Field.Step (Element.Range (1, 59), 2) }
  in
  assert_cron_parser "1-59/2 * * * *" (Ok expected)

let from_datetime ~year:y ~month:m ~day:d ~hour:h ~minute:mn =
  let date = (y, m, d) in
  let time = ((h, mn, 0), 0) in
  Option.get @@ Ptime.of_date_time (date, time)

let test_schedule_matches_star () =
  let matches =
    Cron.Schedule.matches Cron.every_minute
      (from_datetime ~year:2024 ~month:5 ~day:24 ~hour:1 ~minute:2)
  in
  Alcotest.(check bool) "cron expected matches" true matches

let test_schedule_matches_specific_field () =
  let open Cron in
  (* at every minute past hour 1 *)
  let schedule =
    { every_minute with hour = Field.Field (Element.Specified 1) }
  in
  let matches =
    Schedule.matches schedule
      (from_datetime ~year:2024 ~month:5 ~day:25 ~hour:1 ~minute:2)
  in
  Alcotest.(check bool) "cron expected matches" true matches

let test_schedule_matches_a_range () =
  let open Cron in
  (* at every minute on every day-of-month from 3 through 5 *)
  let schedule =
    { every_minute with day_of_month = Field.Field (Element.Range (3, 5)) }
  in
  let matches =
    Schedule.matches schedule
      (from_datetime ~year:2024 ~month:5 ~day:4 ~hour:1 ~minute:2)
  in
  Alcotest.(check bool) "cron expected matches" true matches

let test_schedule_matches_a_list () =
  let open Cron in
  (* at every minute in Jan, Feb and March *)
  let schedule =
    {
      every_minute with
      month =
        Field.List
          [ Element.Specified 1; Element.Specified 2; Element.Specified 3 ];
    }
  in
  let matches =
    Schedule.matches schedule
      (from_datetime ~year:2024 ~month:2 ~day:1 ~hour:1 ~minute:2)
  in
  Alcotest.(check bool) "cron expected matches" true matches

let test_schedule_matches_a_step () =
  let open Cron in
  (* at every minute on every 2nd day-of-month from 10 through 16 *)
  let schedule =
    { every_minute with day_of_month = Field.Step (Element.Range (10, 16), 2) }
  in
  let matches =
    Schedule.matches schedule
      (from_datetime ~year:2024 ~month:5 ~day:12 ~hour:1 ~minute:2)
  in
  Alcotest.(check bool) "cron expected matches" true matches

let test_schedule_not_matches_a_step () =
  let open Cron in
  (* at every minute on every 2nd day-of-month from 10 through 16 *)
  let schedule =
    { every_minute with day_of_month = Field.Step (Element.Range (10, 16), 2) }
  in
  let matches =
    (* day-of-month 13 should not match *)
    Schedule.matches schedule
      (from_datetime ~year:2024 ~month:5 ~day:13 ~hour:1 ~minute:2)
  in
  Alcotest.(check bool) "cron expected not matches" false matches

let test_schedule_matches_starred_stepped_field () =
  let open Cron in
  (* at every 2nd minute *)
  let schedule = { every_minute with minute = Field.Step (Element.Star, 2) } in
  let matches =
    Schedule.matches schedule
      (from_datetime ~year:2024 ~month:5 ~day:13 ~hour:1 ~minute:2)
  in
  Alcotest.(check bool) "cron expected matches" true matches

let test_schedule_not_matches_starred_stepped_field () =
  let open Cron in
  (* at every 2nd minute *)
  let schedule = { every_minute with minute = Field.Step (Element.Star, 2) } in
  let matches =
    (* 5th minute should not match *)
    Schedule.matches schedule
      (from_datetime ~year:2024 ~month:5 ~day:13 ~hour:1 ~minute:5)
  in
  Alcotest.(check bool) "cron expected not matches" false matches

let () =
  Alcotest.run "Cron"
    [
      ( "parser",
        [
          Alcotest.test_case "parse @hourly" `Quick test_parse_schedule_hourly;
          Alcotest.test_case "parse @daily" `Quick test_parse_schedule_daily;
          Alcotest.test_case "parse @weekly" `Quick test_parse_schedule_weekly;
          Alcotest.test_case "parse monthly" `Quick test_parse_schedule_monthly;
          Alcotest.test_case "parse @yearly" `Quick test_parse_schedule_yearly;
          Alcotest.test_case "parse all stars" `Quick test_parse_all_stars;
          Alcotest.test_case "parse specified value" `Quick
            test_parse_specific_values;
          Alcotest.test_case "parse list values" `Quick test_parse_list_values;
          Alcotest.test_case "parse range values" `Quick test_parse_range_values;
          Alcotest.test_case "parse step values" `Quick test_parse_step_values;
          Alcotest.test_case "refuses to parse recursive steps" `Quick
            test_refuse_recursive_steps;
          Alcotest.test_case "refuses to parse sparse list" `Quick
            test_refuse_sparse_lists;
          Alcotest.test_case "refuses too many fields" `Quick
            test_too_many_fields;
          Alcotest.test_case "refuses extraneous input" `Quick
            test_refuse_extraneous_input;
          Alcotest.test_case "parse ranges at the field" `Quick
            test_parse_ranges_at_last_field;
          Alcotest.test_case "parse lists at the last field" `Quick
            test_parse_list_at_last_field;
          Alcotest.test_case "parse steps at the last field" `Quick
            test_parse_steps_at_last_field;
          Alcotest.test_case "parses a sunday as 7" `Quick
            test_parse_sunday_as_7;
          Alcotest.test_case "parses a sunday as 0" `Quick
            test_parse_sunday_as_0;
          Alcotest.test_case "parse example" `Quick test_parse_example;
        ] );
      ( "schedule matches",
        [
          Alcotest.test_case "matches a catch-all" `Quick
            test_schedule_matches_star;
          Alcotest.test_case "matches specific field" `Quick
            test_schedule_matches_specific_field;
          Alcotest.test_case "matches a range" `Quick
            test_schedule_matches_a_range;
          Alcotest.test_case "matches a list" `Quick
            test_schedule_matches_a_list;
          Alcotest.test_case "match a step" `Quick test_schedule_matches_a_step;
          Alcotest.test_case "does not match, something missing step field"
            `Quick test_schedule_not_matches_a_step;
          Alcotest.test_case "matches starred stepped fields" `Quick
            test_schedule_matches_starred_stepped_field;
          Alcotest.test_case
            "does not match fields that miss starred stepped fields" `Quick
            test_schedule_not_matches_starred_stepped_field;
        ] );
    ]
