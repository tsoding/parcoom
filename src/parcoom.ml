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
  { run : input -> (input * 'a, error) result
  }

let fail (e: error) = { run = fun _ -> Error e }
let wrap (x: 'a) = { run = fun input -> Ok (input, x) }

let map (f: 'a -> 'b) (p: 'a parser): 'b parser =
  { run = fun input ->
          match p.run input with
          | Ok (input', x) -> Ok (input', f x)
          | Error error -> Error error
  }

let bind (f: 'a -> 'b parser) (p: 'a parser): 'b parser =
  { run = fun input ->
          match p.run input with
          | Ok (input', x) -> (f x).run input'
          | Error error -> Error error
  }

let prefix (prefix_str: string): string parser =
  { run = fun input ->
          let unexpected_prefix_error =
            { pos = input.pos;
              desc = Printf.sprintf "Expected `%s`" prefix_str
            }
          in
          try
            let prefix_size = String.length prefix_str in
            let input_size = String.length input.text in
            let prefix_input = input |> input_sub 0 prefix_size in
            if String.equal prefix_input.text prefix_str then
              let rest = input |> input_sub prefix_size (input_size - prefix_size) in
              Ok (rest, prefix_str)
            else
              Error unexpected_prefix_error
          with
            Invalid_argument _ -> Error unexpected_prefix_error
  }
