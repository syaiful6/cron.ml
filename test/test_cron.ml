open Cron

let schedule = Alcotest.testable Types.pp Types.equal

let assert_parser p testa s expected =
  let res = Angstrom.parse_string ~consume:All p s in
  Alcotest.(check (result testa string)) "same parsed" res expected

let assert_not_parsed parser s =
  let res = Angstrom.parse_string ~consume:All parser s in
  Alcotest.(check bool)
    "expected parser return Error" (Result.is_error res) true

let assert_cron_parser = assert_parser Parser.cron_schedule_p schedule

let test_parse_schedule_hourly () =
  assert_cron_parser "@hourly" (Ok Types.hourly)

let test_parse_schedule_daily () = assert_cron_parser "@daily" (Ok Types.daily)

let test_parse_schedule_weekly () =
  assert_cron_parser "@weekly" (Ok Types.weekly)

let test_parse_schedule_monthly () =
  assert_cron_parser "@monthly" (Ok Types.monthly)

let test_parse_schedule_yearly () =
  assert_cron_parser "@yearly" (Ok Types.yearly)

let test_parse_all_stars () =
  assert_cron_parser "* * * * *" (Ok Types.every_minute)

let test_parse_specific_values () =
  let open Types in
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
  let open Types in
  let expected =
    {
      every_minute with
      day_of_month = Field.List [ Element.Specified 3; Element.Specified 4 ];
    }
  in
  assert_cron_parser "* * 3,4 * *" (Ok expected)

let test_parse_range_values () =
  let open Types in
  let expected =
    { every_minute with day_of_month = Field.Field (Element.Range (3, 4)) }
  in
  assert_cron_parser "* * 3-4 * *" (Ok expected)

let test_parse_step_values () =
  let open Types in
  let expected =
    {
      every_minute with
      minute = Field.Step (Element.Star, 2);
      day_of_month = Field.Step (Element.Range (2, 10), 4);
    }
  in
  assert_cron_parser "*/2 * 2-10/4 * *" (Ok expected)

let test_refuse_recursive_steps () =
  assert_not_parsed Parser.cron_schedule_p "*/2/3 * * * *"

let test_refuse_sparse_lists () =
  assert_not_parsed Parser.cron_schedule_p "1,,2 * * * *"

let test_too_many_fields () =
  assert_not_parsed Parser.cron_schedule_p "* * * * * *"

let test_refuse_extraneous_input () =
  assert_not_parsed Parser.cron_schedule_p "* * * * *  extra input"

let test_parse_ranges_at_last_field () =
  let open Types in
  let expected =
    { every_minute with day_of_week = Field.Field (Element.Range (3, 4)) }
  in
  assert_cron_parser "* * * * 3-4" (Ok expected)

let test_parse_list_at_last_field () =
  let open Types in
  let expected =
    {
      every_minute with
      day_of_week = Field.List [ Element.Specified 3; Element.Specified 4 ];
    }
  in
  assert_cron_parser "* * * * 3,4" (Ok expected)

let test_parse_steps_at_last_field () =
  let open Types in
  let expected =
    { every_minute with day_of_week = Field.Step (Element.Star, 4) }
  in
  assert_cron_parser "* * * * */4" (Ok expected)

let test_parse_sunday_as_7 () =
  let open Types in
  let expected =
    { every_minute with day_of_week = Field.Field (Element.Specified 7) }
  in
  assert_cron_parser "* * * * 7" (Ok expected)

let test_parse_sunday_as_0 () =
  let open Types in
  let expected =
    { every_minute with day_of_week = Field.Field (Element.Specified 0) }
  in
  assert_cron_parser "* * * * 0" (Ok expected)

let test_parse_example () =
  let open Types in
  let expected =
    { every_minute with minute = Field.Step (Element.Range (1, 59), 2) }
  in
  assert_cron_parser "1-59/2 * * * *" (Ok expected)

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
    ]
