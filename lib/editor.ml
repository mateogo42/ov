module Editor = struct
  include Notty
  include Notty_unix

  type mode = Normal | Insert

  type action =
    | Quit
    | MoveUp
    | MoveDown
    | MoveLeft
    | MoveRight
    | EnterMode of mode
    | InsertChar of char
    | InsertNewLine

  type t = {
    cx : int;
    cy : int;
    mode : mode;
    nrows : int;
    ncols : int;
    buf : Buffer.t;
    filename : string option;
    lines : string array;
  }

  let init () =
    let term = Term.create () in
    let rows, cols =
      match Notty_unix.winsize Unix.stdout with
      | Some (cols, rows) -> (rows, cols)
      | None -> (0, 0)
    in
    ( term,
      {
        cx = 0;
        cy = 0;
        mode = Normal;
        nrows = rows - 1;
        ncols = cols;
        buf = Buffer.create 0;
        filename = None;
        lines = [||];
      } )

  let init_with_file filename =
    let term = Term.create () in
    let rows, cols =
      match Notty_unix.winsize Unix.stdout with
      | Some (cols, rows) -> (rows, cols)
      | None -> (0, 0)
    in
    ( term,
      {
        cx = 0;
        cy = 0;
        mode = Normal;
        nrows = rows - 1;
        ncols = cols;
        buf = Buffer.create 0;
        filename = Some filename;
        lines = [||];
      } )

  let handle_normal_event term =
    match Term.event term with
    | `Key (`Arrow `Right, [] | `ASCII 'l', []) -> Some MoveRight
    | `Key (`Arrow `Left, [] | `ASCII 'h', []) -> Some MoveLeft
    | `Key (`Arrow `Up, [] | `ASCII 'k', []) -> Some MoveUp
    | `Key (`Arrow `Down, [] | `ASCII 'j', []) -> Some MoveDown
    | `Key (`ASCII 'q', []) -> Some Quit
    | `Key (`ASCII 'i', []) -> Some (EnterMode Insert)
    | _ -> None

  let handle_insert_event term =
    match Term.event term with
    | `Key (`Arrow `Right, []) -> Some MoveRight
    | `Key (`Arrow `Left, []) -> Some MoveLeft
    | `Key (`Arrow `Up, []) -> Some MoveUp
    | `Key (`Arrow `Down, []) -> Some MoveDown
    | `Key (`Escape, []) -> Some (EnterMode Normal)
    | `Key (`ASCII c, []) -> Some (InsertChar c)
    | `Key (`Enter, []) -> Some InsertNewLine
    | _ -> None

  let handle_event term editor =
    match editor.mode with
    | Normal -> handle_normal_event term
    | Insert -> handle_insert_event term

  let handle_insert_char editor c =
    let c = String.make 1 c in
    if Array.length editor.lines = 0 then
      { editor with lines = [| c |]; cx = succ editor.cx }
    else
      let line = editor.lines.(editor.cy) ^ c in
      editor.lines.(editor.cy) <- line;
      { editor with cx = succ editor.cx }

  let update term editor =
    let action = handle_event term editor in
    match action with
    | Some Quit -> exit 0
    | Some MoveRight -> { editor with cx = editor.cx + 1 }
    | Some MoveLeft -> { editor with cx = Int.max 0 (editor.cx - 1) }
    | Some MoveUp -> { editor with cy = Int.max 0 (editor.cy - 1) }
    | Some MoveDown -> { editor with cy = editor.cy + 1 }
    | Some (InsertChar c) -> handle_insert_char editor c
    | Some InsertNewLine -> { editor with cy = succ editor.cy }
    | Some (EnterMode mode) -> { editor with mode }
    | _ -> editor

  let draw_lines editor =
    List.mapi
      (fun i _ ->
        if i < Array.length editor.lines then
          let line = editor.lines.(i) in
          I.string A.empty line
        else I.string A.empty "~")
      (List.init editor.nrows (fun n -> n))

  let draw_statusbar editor =
    let mode =
      match editor.mode with Normal -> "  NORMAL  " | Insert -> "  INSERT  "
    in
    let mode_img = I.string A.(bg magenta ++ fg black) mode in
    let filename = Option.value editor.filename ~default:"[No Name]" in
    let filename_img = I.string A.(fg yellow) filename in
    I.(mode_img <|> void 2 0 <|> filename_img)

  let render term editor =
    Term.cursor term (Some (editor.cx, editor.cy));
    Term.image term (I.vcat (draw_lines editor @ [ draw_statusbar editor ]))
end
