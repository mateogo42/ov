open Ov

let rec loop t e =
  let e = Editor.update t e in
  Editor.render t e;
  loop t e

let main () =
  let term, editor =
    if Array.length Sys.argv = 1 then Editor.init ()
    else
      let filename = Sys.argv.(1) in
      Editor.init_with_file filename
  in
  loop term editor

let () = main ()
