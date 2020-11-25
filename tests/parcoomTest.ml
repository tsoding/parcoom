open Parcoom

let test_fail () =
  print_endline "test_fail...";
  let input = "whatever input" in
  let message = "khello" in
  let expected = Error { desc = message; pos = 0 } in
  let actual = run (fail message) input in
  assert (compare expected actual == 0)

let test_wrap () =
  print_endline "test_wrap...";
  let input = "whatever input" in
  let result = 69 in
  let expected = Ok result in
  let actual = run (wrap result) input in
  assert (compare expected actual == 0)

let test_map () =
  print_endline "test_map...";
  let input = "whatever input" in
  let ok_map () =
    let result = 68 in
    let expected = Ok (result + 1) in
    let actual = run (result |> wrap |> map (fun x -> x + 1)) input in
    assert (compare expected actual == 0)
  in
  let error_map () =
    ()
  in
  ok_map ();
  error_map ()

let () =
  test_fail ();
  test_wrap ();
  test_map ()
