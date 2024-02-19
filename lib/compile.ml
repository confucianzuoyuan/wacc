open Batteries

let compile src_file =
  let source_lines = File.lines_of src_file in
  let source =
    Enum.reduce (fun line1 line2 -> line1 ^ "\n" ^ line2) source_lines
  in
  let tokens = Lex.lex source in
  List.iter (Printf.printf "%s\n") (List.map Tokens.show tokens)
