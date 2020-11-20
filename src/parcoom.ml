type input =
  { text: string;
    pos: int
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
