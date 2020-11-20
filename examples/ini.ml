open Parcoom

type key_t = string
type value_t = string
type pair_t = key_t * value_t
type section_t =
  { name: string;
    pairs: pair_t list;
  }

let read_whole_file (file_path: string): string =
  let ch = open_in file_path in
  let n = in_channel_length ch in
  let s = really_input_string ch n in
  close_in ch;
  s

let section_name: string Parcoom.parser =
  prefix "[" *> parse_while (fun x -> x != ']') <* prefix "]"

let is_space (x: char) = x == ' ' || x == '\n'

let wss: string Parcoom.parser =
  parse_while is_space

let pair: pair_t Parcoom.parser =
  let name = parse_while (fun x -> not (is_space x) && x != '=') in
  (wss *> name <* wss <* prefix "=" <* wss) <*> (name <* wss)

let section: section_t Parcoom.parser =
  section_name <*> many pair
  |> map (fun (name, pairs) -> { name = name; pairs = pairs; })

let ini: section_t list Parcoom.parser =
  many section
