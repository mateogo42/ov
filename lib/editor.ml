module Editor = struct
  include Notty_unix

  type mode = Normal | Insert

  type action =
    | Quit
    | MoveUp
    | MoveDown
    | MoveLeft
    | MoveRight
    | EnterMode of mode
    | InsertChar of string

  type t = {
    cx : int;
    cy : int;
    mode : mode;
    nrows : int;
    ncols : int;
    buf : Buffer.t;
    filename : string option;
  }

  let init () =
    let term = Term.create () in
    let rows, cols =
      match Notty_unix.winsize Unix.stdout with
      | Some (rows, columns) -> (rows, columns)
      | None -> (0, 0)
    in
    ( term,
      {
        cx = 0;
        cy = 1;
        mode = Normal;
        nrows = rows;
        ncols = cols;
        buf = Buffer.create 0;
        filename = None;
      } )

  let init_with_file filename =
    let term = Term.create () in
    let rows, cols =
      match Notty_unix.winsize Unix.stdout with
      | Some (rows, columns) -> (rows, columns)
      | None -> (0, 0)
    in
    ( term,
      {
        cx = 0;
        cy = 1;
        mode = Normal;
        nrows = rows;
        ncols = cols;
        buf = Buffer.create 0;
        filename = Some filename;
      } )

  let render term editor = Term.cursor term (Some (editor.cx, editor.cy))

  let handle_normal_event term =
    match Term.event term with
    | `Key (`Arrow `Right, [] | `ASCII 'l', []) -> Some MoveRight
    | `Key (`Arrow `Left, [] | `ASCII 'h', []) -> Some MoveLeft
    | `Key (`Arrow `Up, [] | `ASCII 'k', []) -> Some MoveUp
    | `Key (`Arrow `Down, [] | `ASCII 'j', []) -> Some MoveDown
    | `Key (`ASCII 'q', []) -> Some Quit
    | `Key (`ASCII 'i', []) -> Some (EnterMode Normal)
    | _ -> None

  let handle_insert_event term =
    match Term.event term with
    | `Key (`Escape, []) -> Some (EnterMode Normal)
    | _ -> None

  let handle_event term editor =
    match editor.mode with
    | Normal -> handle_normal_event term
    | Insert -> handle_insert_event term

  let update term editor =
    let action = handle_event term editor in
    match action with
    | Some Quit -> exit 0
    | Some MoveRight -> { editor with cx = editor.cx + 1 }
    | Some MoveLeft -> { editor with cx = Int.max 0 (editor.cx - 1) }
    | Some MoveUp -> { editor with cy = Int.max 0 (editor.cy - 1) }
    | Some MoveDown -> { editor with cy = editor.cy + 1 }
    | Some (EnterMode mode) -> { editor with mode }
    | _ -> editor
end
