open Cron

let schedule = Alcotest.testable Types.pp Types.equal

let test_parse_schedule_yearly () =
  let s = Parser.parse_cron_schedule "@yearly" in
  Alcotest.(check (result schedule string)) "same schedule" (Ok Types.yearly) s

let test_parse_schedule_hourly () =
  let s = Parser.parse_cron_schedule "@hourly" in
  Alcotest.(check (result schedule string)) "same schedule" (Ok Types.hourly) s

let () =
  Alcotest.run "Cron"
    [
      ( "parser",
        [
          Alcotest.test_case "parse @yearly" `Quick test_parse_schedule_yearly;
          Alcotest.test_case "parse @hourly" `Quick test_parse_schedule_hourly;
        ] );
    ]
