open Ov

let rec loop t e =
  let e = Editor.update t e in
  Editor.render t e;
  loop t e

let main () =
  let term, editor = Editor.init () in
  loop term editor

let () = main ()
