let schedule = Alcotest.testable Croni.pp Croni.equal

let assert_parser p testa s expected =
  let res = Angstrom.parse_string ~consume:All p s in
  Alcotest.(check (result testa string)) "same parsed" res expected

let assert_not_parsed parser s =
  let res = Angstrom.parse_string ~consume:All parser s in
  Alcotest.(check bool)
    "expected parser return Error"
    (Result.is_error res)
    true

let assert_cron_parser = assert_parser Croni.Parser.cron_schedule_p schedule

let test_parse_schedule_hourly () =
  assert_cron_parser "@hourly" (Ok Croni.hourly)

let test_parse_schedule_daily () = assert_cron_parser "@daily" (Ok Croni.daily)

let test_parse_schedule_weekly () =
  assert_cron_parser "@weekly" (Ok Croni.weekly)

let test_parse_schedule_monthly () =
  assert_cron_parser "@monthly" (Ok Croni.monthly)

let test_parse_schedule_yearly () =
  assert_cron_parser "@yearly" (Ok Croni.yearly)

let test_parse_all_stars () =
  assert_cron_parser "* * * * *" (Ok Croni.every_minute)

let test_parse_specific_values () =
  let expected =
    Croni.
      { every_minute with
        minute = Field.Field (Element.Specified 1)
      ; hour = Field.Field (Element.Specified 2)
      ; day_of_month = Field.Field (Element.Specified 3)
      }
  in
  assert_cron_parser "1 2 3 * *" (Ok expected)

let test_parse_list_values () =
  let expected =
    Croni.
      { every_minute with
        day_of_month = Field.List [ Element.Specified 3; Element.Specified 4 ]
      }
  in
  assert_cron_parser "* * 3,4 * *" (Ok expected)

let test_parse_range_values () =
  let expected =
    Croni.
      { every_minute with day_of_month = Field.Field (Element.Range (3, 4)) }
  in
  assert_cron_parser "* * 3-4 * *" (Ok expected)

let test_parse_step_values () =
  let expected =
    Croni.
      { every_minute with
        minute = Field.Step (Element.Star, 2)
      ; day_of_month = Field.Step (Element.Range (2, 10), 4)
      }
  in
  assert_cron_parser "*/2 * 2-10/4 * *" (Ok expected)

let test_refuse_recursive_steps () =
  assert_not_parsed Croni.Parser.cron_schedule_p "*/2/3 * * * *"

let test_refuse_sparse_lists () =
  assert_not_parsed Croni.Parser.cron_schedule_p "1,,2 * * * *"

let test_too_many_fields () =
  assert_not_parsed Croni.Parser.cron_schedule_p "* * * * * *"

let test_refuse_extraneous_input () =
  assert_not_parsed Croni.Parser.cron_schedule_p "* * * * *  extra input"

let test_parse_ranges_at_last_field () =
  let expected =
    Croni.{ every_minute with day_of_week = Field.Field (Element.Range (3, 4)) }
  in
  assert_cron_parser "* * * * 3-4" (Ok expected)

let test_parse_list_at_last_field () =
  let expected =
    Croni.
      { every_minute with
        day_of_week = Field.List [ Element.Specified 3; Element.Specified 4 ]
      }
  in
  assert_cron_parser "* * * * 3,4" (Ok expected)

let test_parse_steps_at_last_field () =
  let expected =
    Croni.{ every_minute with day_of_week = Field.Step (Element.Star, 4) }
  in
  assert_cron_parser "* * * * */4" (Ok expected)

let test_parse_sunday_as_7 () =
  let expected =
    Croni.{ every_minute with day_of_week = Field.Field (Element.Specified 7) }
  in
  assert_cron_parser "* * * * 7" (Ok expected)

let test_parse_sunday_as_0 () =
  let expected =
    Croni.{ every_minute with day_of_week = Field.Field (Element.Specified 0) }
  in
  assert_cron_parser "* * * * 0" (Ok expected)

let test_parse_example () =
  let expected =
    Croni.{ every_minute with minute = Field.Step (Element.Range (1, 59), 2) }
  in
  assert_cron_parser "1-59/2 * * * *" (Ok expected)

let from_datetime ~year:y ~month:m ~day:d ~hour:h ~minute:mn =
  let date = y, m, d in
  let time = (h, mn, 0), 0 in
  Option.get @@ Ptime.of_date_time (date, time)

let test_schedule_matches_star () =
  let matches =
    Croni.Schedule.matches
      Croni.every_minute
      (from_datetime ~year:2024 ~month:5 ~day:24 ~hour:1 ~minute:2)
  in
  Alcotest.(check bool) "cron expected matches" true matches

let test_schedule_matches_specific_field () =
  (* at every minute past hour 1 *)
  let schedule =
    Croni.{ every_minute with hour = Field.Field (Element.Specified 1) }
  in
  let matches =
    Croni.Schedule.matches
      schedule
      (from_datetime ~year:2024 ~month:5 ~day:25 ~hour:1 ~minute:2)
  in
  Alcotest.(check bool) "cron expected matches" true matches

let test_schedule_matches_a_range () =
  (* at every minute on every day-of-month from 3 through 5 *)
  let schedule =
    Croni.
      { every_minute with day_of_month = Field.Field (Element.Range (3, 5)) }
  in
  let matches =
    Croni.Schedule.matches
      schedule
      (from_datetime ~year:2024 ~month:5 ~day:4 ~hour:1 ~minute:2)
  in
  Alcotest.(check bool) "cron expected matches" true matches

let test_schedule_matches_a_list () =
  (* at every minute in Jan, Feb and March *)
  let schedule =
    Croni.
      { every_minute with
        month =
          Field.List
            [ Element.Specified 1; Element.Specified 2; Element.Specified 3 ]
      }
  in
  let matches =
    Croni.Schedule.matches
      schedule
      (from_datetime ~year:2024 ~month:2 ~day:1 ~hour:1 ~minute:2)
  in
  Alcotest.(check bool) "cron expected matches" true matches

let test_schedule_matches_a_step () =
  (* at every minute on every 2nd day-of-month from 10 through 16 *)
  let schedule =
    Croni.
      { every_minute with
        day_of_month = Field.Step (Element.Range (10, 16), 2)
      }
  in
  let matches =
    Croni.Schedule.matches
      schedule
      (from_datetime ~year:2024 ~month:5 ~day:12 ~hour:1 ~minute:2)
  in
  Alcotest.(check bool) "cron expected matches" true matches

let test_schedule_not_matches_a_step () =
  (* at every minute on every 2nd day-of-month from 10 through 16 *)
  let schedule =
    Croni.
      { every_minute with
        day_of_month = Field.Step (Element.Range (10, 16), 2)
      }
  in
  let matches =
    (* day-of-month 13 should not match *)
    Croni.Schedule.matches
      schedule
      (from_datetime ~year:2024 ~month:5 ~day:13 ~hour:1 ~minute:2)
  in
  Alcotest.(check bool) "cron expected not matches" false matches

let test_schedule_matches_starred_stepped_field () =
  (* at every 2nd minute *)
  let schedule =
    Croni.{ every_minute with minute = Field.Step (Element.Star, 2) }
  in
  let matches =
    Croni.Schedule.matches
      schedule
      (from_datetime ~year:2024 ~month:5 ~day:13 ~hour:1 ~minute:2)
  in
  Alcotest.(check bool) "cron expected matches" true matches

let test_schedule_not_matches_starred_stepped_field () =
  (* at every 2nd minute *)
  let schedule =
    Croni.{ every_minute with minute = Field.Step (Element.Star, 2) }
  in
  let matches =
    (* 5th minute should not match *)
    Croni.Schedule.matches
      schedule
      (from_datetime ~year:2024 ~month:5 ~day:13 ~hour:1 ~minute:5)
  in
  Alcotest.(check bool) "cron expected not matches" false matches

let ptime = Alcotest.testable (Ptime.pp_human ()) Ptime.equal

let test_next_every_minute () =
  (* at every minute *)
  let start = from_datetime ~year:2024 ~month:5 ~day:24 ~hour:10 ~minute:25 in
  let expected =
    from_datetime ~year:2024 ~month:5 ~day:24 ~hour:10 ~minute:26
  in
  let result = Croni.Schedule.next Croni.every_minute start in
  Alcotest.(check (option ptime)) "next minute" (Some expected) result

let test_next_not_inclusive () =
  (* schedule matches at minute 30, starting at minute 30 should return next
     hour's 30 *)
  let schedule =
    Croni.{ every_minute with minute = Field.Field (Element.Specified 30) }
  in
  let start = from_datetime ~year:2024 ~month:5 ~day:24 ~hour:10 ~minute:30 in
  let expected =
    from_datetime ~year:2024 ~month:5 ~day:24 ~hour:11 ~minute:30
  in
  let result = Croni.Schedule.next schedule start in
  Alcotest.(check (option ptime)) "not inclusive" (Some expected) result

let test_next_same_hour () =
  (* at minute 30, starting at minute 25 *)
  let schedule =
    Croni.{ every_minute with minute = Field.Field (Element.Specified 30) }
  in
  let start = from_datetime ~year:2024 ~month:5 ~day:24 ~hour:10 ~minute:25 in
  let expected =
    from_datetime ~year:2024 ~month:5 ~day:24 ~hour:10 ~minute:30
  in
  let result = Croni.Schedule.next schedule start in
  Alcotest.(check (option ptime)) "same hour" (Some expected) result

let test_next_hour_rollover () =
  (* at minute 15, starting at minute 45 should go to next hour *)
  let schedule =
    Croni.{ every_minute with minute = Field.Field (Element.Specified 15) }
  in
  let start = from_datetime ~year:2024 ~month:5 ~day:24 ~hour:10 ~minute:45 in
  let expected =
    from_datetime ~year:2024 ~month:5 ~day:24 ~hour:11 ~minute:15
  in
  let result = Croni.Schedule.next schedule start in
  Alcotest.(check (option ptime)) "hour rollover" (Some expected) result

let test_next_day_rollover () =
  (* at 00:00, starting at 23:30 should go to next day *)
  let schedule = Croni.daily in
  let start = from_datetime ~year:2024 ~month:5 ~day:24 ~hour:23 ~minute:30 in
  let expected = from_datetime ~year:2024 ~month:5 ~day:25 ~hour:0 ~minute:0 in
  let result = Croni.Schedule.next schedule start in
  Alcotest.(check (option ptime)) "day rollover" (Some expected) result

let test_next_month_rollover () =
  (* at 00:00 on day 1, starting at end of month *)
  let schedule = Croni.monthly in
  let start = from_datetime ~year:2024 ~month:5 ~day:31 ~hour:23 ~minute:30 in
  let expected = from_datetime ~year:2024 ~month:6 ~day:1 ~hour:0 ~minute:0 in
  let result = Croni.Schedule.next schedule start in
  Alcotest.(check (option ptime)) "month rollover" (Some expected) result

let test_next_year_rollover () =
  (* at Jan 1 00:00, starting at end of year *)
  let schedule = Croni.yearly in
  let start = from_datetime ~year:2024 ~month:12 ~day:31 ~hour:23 ~minute:30 in
  let expected = from_datetime ~year:2025 ~month:1 ~day:1 ~hour:0 ~minute:0 in
  let result = Croni.Schedule.next schedule start in
  Alcotest.(check (option ptime)) "year rollover" (Some expected) result

let test_next_step_field () =
  (* at every 15th minute *)
  let schedule =
    Croni.{ every_minute with minute = Field.Step (Element.Star, 15) }
  in
  let start = from_datetime ~year:2024 ~month:5 ~day:24 ~hour:10 ~minute:25 in
  let expected =
    from_datetime ~year:2024 ~month:5 ~day:24 ~hour:10 ~minute:30
  in
  let result = Croni.Schedule.next schedule start in
  Alcotest.(check (option ptime)) "step field" (Some expected) result

let test_next_specific_day_of_week () =
  (* every Sunday at 00:00 - 2024-05-24 is a Friday, next Sunday is
     2024-05-26 *)
  let schedule = Croni.weekly in
  let start = from_datetime ~year:2024 ~month:5 ~day:24 ~hour:10 ~minute:25 in
  let expected = from_datetime ~year:2024 ~month:5 ~day:26 ~hour:0 ~minute:0 in
  let result = Croni.Schedule.next schedule start in
  Alcotest.(check (option ptime)) "specific day of week" (Some expected) result

let test_next_leap_year () =
  (* at Feb 29 00:00 in a leap year *)
  let schedule =
    Croni.
      { daily with
        month = Field.Field (Element.Specified 2)
      ; day_of_month = Field.Field (Element.Specified 29)
      }
  in
  let start = from_datetime ~year:2024 ~month:2 ~day:28 ~hour:10 ~minute:0 in
  let expected = from_datetime ~year:2024 ~month:2 ~day:29 ~hour:0 ~minute:0 in
  let result = Croni.Schedule.next schedule start in
  Alcotest.(check (option ptime)) "leap year feb 29" (Some expected) result

let test_next_dom_and_dow_restricted () =
  (* at 00:00 on day 15 and on Monday (OR logic) 2024-05-24 is Friday, next is
     either Monday 2024-05-27 or day 15 (June 15) Monday 2024-05-27 comes
     first *)
  let schedule =
    Croni.
      { daily with
        day_of_month = Field.Field (Element.Specified 15)
      ; day_of_week = Field.Field (Element.Specified 1) (* Monday *)
      }
  in
  let start = from_datetime ~year:2024 ~month:5 ~day:24 ~hour:10 ~minute:0 in
  let expected = from_datetime ~year:2024 ~month:5 ~day:27 ~hour:0 ~minute:0 in
  let result = Croni.Schedule.next schedule start in
  Alcotest.(check (option ptime))
    "dom and dow restricted"
    (Some expected)
    result

let () =
  Alcotest.run
    "Cron"
    [ ( "parser"
      , [ Alcotest.test_case "parse @hourly" `Quick test_parse_schedule_hourly
        ; Alcotest.test_case "parse @daily" `Quick test_parse_schedule_daily
        ; Alcotest.test_case "parse @weekly" `Quick test_parse_schedule_weekly
        ; Alcotest.test_case "parse monthly" `Quick test_parse_schedule_monthly
        ; Alcotest.test_case "parse @yearly" `Quick test_parse_schedule_yearly
        ; Alcotest.test_case "parse all stars" `Quick test_parse_all_stars
        ; Alcotest.test_case
            "parse specified value"
            `Quick
            test_parse_specific_values
        ; Alcotest.test_case "parse list values" `Quick test_parse_list_values
        ; Alcotest.test_case "parse range values" `Quick test_parse_range_values
        ; Alcotest.test_case "parse step values" `Quick test_parse_step_values
        ; Alcotest.test_case
            "refuses to parse recursive steps"
            `Quick
            test_refuse_recursive_steps
        ; Alcotest.test_case
            "refuses to parse sparse list"
            `Quick
            test_refuse_sparse_lists
        ; Alcotest.test_case
            "refuses too many fields"
            `Quick
            test_too_many_fields
        ; Alcotest.test_case
            "refuses extraneous input"
            `Quick
            test_refuse_extraneous_input
        ; Alcotest.test_case
            "parse ranges at the field"
            `Quick
            test_parse_ranges_at_last_field
        ; Alcotest.test_case
            "parse lists at the last field"
            `Quick
            test_parse_list_at_last_field
        ; Alcotest.test_case
            "parse steps at the last field"
            `Quick
            test_parse_steps_at_last_field
        ; Alcotest.test_case
            "parses a sunday as 7"
            `Quick
            test_parse_sunday_as_7
        ; Alcotest.test_case
            "parses a sunday as 0"
            `Quick
            test_parse_sunday_as_0
        ; Alcotest.test_case "parse example" `Quick test_parse_example
        ] )
    ; ( "schedule matches"
      , [ Alcotest.test_case
            "matches a catch-all"
            `Quick
            test_schedule_matches_star
        ; Alcotest.test_case
            "matches specific field"
            `Quick
            test_schedule_matches_specific_field
        ; Alcotest.test_case
            "matches a range"
            `Quick
            test_schedule_matches_a_range
        ; Alcotest.test_case
            "matches a list"
            `Quick
            test_schedule_matches_a_list
        ; Alcotest.test_case "match a step" `Quick test_schedule_matches_a_step
        ; Alcotest.test_case
            "does not match, something missing step field"
            `Quick
            test_schedule_not_matches_a_step
        ; Alcotest.test_case
            "matches starred stepped fields"
            `Quick
            test_schedule_matches_starred_stepped_field
        ; Alcotest.test_case
            "does not match fields that miss starred stepped fields"
            `Quick
            test_schedule_not_matches_starred_stepped_field
        ] )
    ; ( "schedule next"
      , [ Alcotest.test_case "next every minute" `Quick test_next_every_minute
        ; Alcotest.test_case "not inclusive" `Quick test_next_not_inclusive
        ; Alcotest.test_case "same hour" `Quick test_next_same_hour
        ; Alcotest.test_case "hour rollover" `Quick test_next_hour_rollover
        ; Alcotest.test_case "day rollover" `Quick test_next_day_rollover
        ; Alcotest.test_case "month rollover" `Quick test_next_month_rollover
        ; Alcotest.test_case "year rollover" `Quick test_next_year_rollover
        ; Alcotest.test_case "step field" `Quick test_next_step_field
        ; Alcotest.test_case
            "specific day of week"
            `Quick
            test_next_specific_day_of_week
        ; Alcotest.test_case "leap year" `Quick test_next_leap_year
        ; Alcotest.test_case
            "dom and dow restricted"
            `Quick
            test_next_dom_and_dow_restricted
        ] )
    ]
