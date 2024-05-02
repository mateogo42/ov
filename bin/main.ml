open Minttea
open Ov

let init _model = Command.Seq [ Enter_alt_screen; Hide_cursor ]
let update event (model : Editor.t) = Editor.update event model
let view (model : Editor.t) = Editor.render model

let main () =
  let initial_model = Editor.init () in
  let app = Minttea.app ~init ~update ~view () in
  Minttea.start app ~initial_model

let () = main ()
