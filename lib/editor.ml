module Editor = struct
  include Minttea
  include Leaves

  type modes = Normal | Insert
  type t = { cx : int; cy : int; mode : modes; nrows : int; ncols : int }

  let init () =
    let rows, cols =
      match Terminal.Size.get_dimensions () with
      | Some { rows; columns } -> (rows, columns)
      | None -> (0, 0)
    in
    { cx = 0; cy = 0; mode = Normal; nrows = rows; ncols = cols }

  let render editor =
    let text_style = Spices.(default |> build) in
    let result = ref "" in
    for j = 0 to editor.nrows - 1 do
      for i = 0 to editor.ncols - 1 do
        let txt =
          if i = editor.cx && j = editor.cy then
            Cursor.view (Cursor.make ()) ~text_style:Spices.default " "
          else text_style "%s" " "
        in
        result := !result ^ txt
      done
    done;
    text_style "%s" !result

  let update event editor =
    match event with
    | Event.KeyDown (Key "q") -> (editor, Command.Quit)
    | Event.KeyDown Right -> ({ editor with cx = editor.cx + 1 }, Command.Noop)
    | Event.KeyDown Left -> ({ editor with cx = editor.cx - 1 }, Command.Noop)
    | Event.KeyDown Up -> ({ editor with cy = editor.cy - 1 }, Command.Noop)
    | Event.KeyDown Down -> ({ editor with cy = editor.cy + 1 }, Command.Noop)
    | _ -> (editor, Command.Noop)
end
