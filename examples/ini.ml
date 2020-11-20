type key = string
type value = string
type pair = key * value
type section =
  { name: string;
    pairs: pair list;
  }

let show_pair ((key, value): pair): string =
  Printf.sprintf "(%s, %s)" key value

let show_pairs (pairs: pair list): string =
  pairs
  |> List.map show_pair
  |> String.concat ","
  |> Printf.sprintf "[%s]"

let show_section (sec: section): string =
  Printf.sprintf "{ name = %s; pairs = %s }"
    sec.name
    (show_pairs sec.pairs)

let show_sections (sections: section list): string =
  sections
  |> List.map show_section
  |> String.concat ","
  |> Printf.sprintf "[%s]"

let ini: section list Parcoom.parser =
  Parcoom.fail
    { desc = "Not implemented yet";
      pos = 0;
    }

let read_whole_file (file_path: string): string =
  let ch = open_in file_path in
  let n = in_channel_length ch in
  let s = really_input_string ch n in
  close_in ch;
  s

let () =
  let result = "./test.ini"
               |> read_whole_file
               |> Parcoom.make_input
               |> ini.run
  in
  match result with
  | Ok (_, sections) -> sections
                        |> show_sections
                        |> print_endline
  | Error error -> Printf.printf
                     "Error happend at %d: %s\n"
                     error.pos
                     error.desc
