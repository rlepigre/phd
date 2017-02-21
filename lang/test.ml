open Lang_parser

let _ =
  try while true do
    let s = input_line stdin in
    let _ = parse_form s in
    Printf.printf "Parsed [%s]\n" s
  done with End_of_file -> ()
