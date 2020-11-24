type input =
  { text: string;
    pos: int;
  }

let input_sub (start: int) (len: int) (s: input): input =
  { text = String.sub (s.text) start len;
    pos = s.pos + start;
  }

let make_input (s: string): input =
  { text = s; pos = 0 }

type error =
  { desc: string;
    pos: int
  }

type 'a parser =
  { run : input -> input * ('a, string) result
  }

let fail (e: string) = { run = fun input -> input, Error e }
let wrap (x: 'a) = { run = fun input -> input, Ok x }

let map (f: 'a -> 'b) (p: 'a parser): 'b parser =
  { run = fun input ->
          match p.run input with
          | input', Ok x        -> input', Ok (f x)
          | input', Error error -> input', Error error
  }

let bind (f: 'a -> 'b parser) (p: 'a parser): 'b parser =
  { run = fun input ->
          match p.run input with
          | input', Ok x -> (f x).run input'
          | input', Error error -> input', Error error
  }

let parse_while (p: char -> bool): string parser =
  { run = fun input ->
          let n = String.length input.text in
          let i = ref 0 in
          while !i < n && (String.get input.text !i |> p) do
            incr i
          done;
          input_sub !i (n - !i) input, Ok (String.sub input.text 0 !i)
  }

let prefix (prefix_str: string): string parser =
  { run = fun input ->
          let unexpected_prefix_error =
            Printf.sprintf "expected `%s`" prefix_str
          in
          try
            let prefix_size = String.length prefix_str in
            let input_size = String.length input.text in
            let prefix_input = input |> input_sub 0 prefix_size in
            if String.equal prefix_input.text prefix_str then
              let rest = input |> input_sub prefix_size (input_size - prefix_size) in
              rest, Ok prefix_str
            else
              input, Error unexpected_prefix_error
          with
            Invalid_argument _ -> input, Error unexpected_prefix_error
  }

let ( *> ) (p1: 'a parser) (p2: 'b parser): 'b parser =
  { run = fun input ->
          let input', result = p1.run input in
          match result  with
          | Ok _  -> p2.run input'
          | Error e -> input', Error e
  }

let ( <* ) (p1: 'a parser) (p2: 'b parser): 'a parser =
  { run = fun input ->
          let input', result = p1.run input in
          match result with
          | Ok x ->
             let input'', result' = p2.run input' in
             (match result' with
              | Ok _  -> input'', Ok x
              | Error e -> input'', Error e)
          | Error e -> input', Error e
  }

let ( <*> ) (p1: 'a parser) (p2: 'b parser): ('a * 'b) parser =
  { run = fun input ->
          let input', result = p1.run input in
          match result with
          | Ok x ->
             let input'', result' = p2.run input' in
             (match result' with
              | Ok y  -> input'', Ok (x, y)
              | Error e -> input'', Error e)
          | Error e -> input', Error e
  }

let ( <|> ) (p1: 'a parser) (p2: 'a parser): 'a parser =
  { run = fun input ->
          let input', result = p1.run input in
          match result with
          | Ok x -> input', Ok x
          | Error left_error -> p2.run input
  }

let optional (p: 'a parser): 'a option parser =
  { run = fun input ->
          let input', result = p.run input in
          match result with
          | Ok x    -> input', Ok (Some x)
          | Error _ -> input', Ok None
  }

let many_exact (n: int) (p: 'a parser): 'a list parser =
  { run = fun input ->
          let rec loop i xs input' =
            if i < n then
              let input'', result = p.run input' in
              match result with
              | Ok x    -> loop (i + 1) (x :: xs) input''
              | Error e -> input'', Error e
            else
              input', Ok (List.rev xs)
          in loop 0 [] input
  }

let many (p: 'a parser): 'a list parser =
  { run = fun input ->
          let xs = ref [] in
          let rec loop input =
            let input', result = p.run input in
            match result with
            | Ok x ->
               xs := x :: !xs;
               loop input'
            | Error _ ->
               input
          in
          let input' = loop input in
          input', Ok (!xs |> List.rev)
  }

let any_char: char parser =
  { run = fun input ->
          let n = String.length input.text in
          try
            input_sub 1 (n - 1) input, Ok (String.get input.text 0)
          with
            Invalid_argument _ -> input, Error "expected any character"
  }

let run (p: 'a parser) (s: string): ('a, error) result =
  match s |> make_input |> p.run with
  | _     , Ok x    -> Ok x
  | input', Error desc -> Error {pos = input'.pos; desc = desc; }
