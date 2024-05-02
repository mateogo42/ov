module Editor = struct
  open Notty
  open Notty_unix

  type mode = Normal | Insert

  type action =
    | Quit
    | MoveUp
    | MoveDown
    | MoveLeft
    | MoveRight
    | EnterMode of mode
    | InsertChar of char
    | DeleteChar
    | InsertNewLine

  type t = {
    cx : int;
    cy : int;
    ox : int;
    oy : int;
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
        ox = 0;
        oy = 0;
        mode = Normal;
        nrows = rows - 1;
        ncols = cols;
        buf = Buffer.create 0;
        filename = None;
        lines = [| "" |];
      } )

  let init_with_file filename =
    let term = Term.create () in
    let rows, cols =
      match Notty_unix.winsize Unix.stdout with
      | Some (cols, rows) -> (rows, cols)
      | None -> (0, 0)
    in
    let oc = open_in filename in
    let lines = In_channel.input_lines oc in
    ( term,
      {
        cx = 0;
        cy = 0;
        ox = 0;
        oy = 0;
        mode = Normal;
        nrows = rows - 1;
        ncols = cols;
        buf = Buffer.create 0;
        filename = Some filename;
        lines = Array.of_list lines;
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
    | `Key (`Backspace, []) -> Some DeleteChar
    | _ -> None

  let handle_event term editor =
    match editor.mode with
    | Normal -> handle_normal_event term
    | Insert -> handle_insert_event term

  let scroll_y editor cy =
    let oy = Int.min editor.oy cy in
    if cy >= oy + editor.nrows then cy - editor.nrows + 1 else oy

  let clip_cursor editor =
    let row = try editor.lines.(editor.cy) with _ -> "" in
    let row_len = Int.max (String.length row) 0 in
    { editor with cx = Int.min row_len editor.cx }

  let move_cursor editor cx cy =
    let editor = { editor with cx; cy; oy = scroll_y editor cy } in
    clip_cursor editor

  let handle_insert_char editor c =
    let c = String.make 1 c in
    if Array.length editor.lines = 0 then
      { editor with lines = [| c |]; cx = succ editor.cx }
    else
      let line = editor.lines.(editor.cy) in
      let left = String.sub line 0 editor.cx in
      let right = String.sub line editor.cx (String.length line - editor.cx) in
      editor.lines.(editor.cy) <- left ^ c ^ right;
      { editor with cx = succ editor.cx }

  let handle_newline editor =
    let before = Array.sub editor.lines 0 editor.cy in
    let after =
      Array.sub editor.lines (editor.cy + 1)
        (Array.length editor.lines - editor.cy - 1)
    in
    let line = editor.lines.(editor.cy) in
    let prev_line = String.sub line 0 editor.cx in
    let new_line =
      String.sub editor.lines.(editor.cy) editor.cx
        (String.length line - editor.cx)
    in
    let editor =
      {
        editor with
        lines = Array.concat [ before; [| prev_line; new_line |]; after ];
      }
    in
    move_cursor editor 0 (succ editor.cy)

  let handle_delete_char editor =
    let editor =
      if editor.cx = 0 && editor.cy = 0 then editor
      else if editor.cx = 0 then
        let line = editor.lines.(editor.cy) in
        let prev_line = editor.lines.(editor.cy - 1) in
        let before = Array.sub editor.lines 0 (editor.cy - 1) in
        let after =
          try
            Array.sub editor.lines (editor.cy + 1)
              (Array.length editor.lines - editor.cy - 1)
          with _ -> [||]
        in
        {
          editor with
          lines = Array.concat [ before; [| prev_line ^ line |]; after ];
          cy = pred editor.cy;
          cx = String.length prev_line;
        }
      else
        let line = editor.lines.(editor.cy) in
        let before = String.sub line 0 (editor.cx - 1) in
        let after =
          try String.sub line editor.cx (String.length line - editor.cx)
          with _ -> ""
        in
        editor.lines.(editor.cy) <- before ^ after;
        { editor with cx = pred editor.cx }
    in
    move_cursor editor editor.cx editor.cy

  let update term editor =
    let action = handle_event term editor in
    match action with
    | Some Quit -> exit 0
    | Some MoveRight -> move_cursor editor (succ editor.cx) editor.cy
    | Some MoveLeft -> move_cursor editor (Int.max 0 (editor.cx - 1)) editor.cy
    | Some MoveUp -> move_cursor editor editor.cx (Int.max 0 (pred editor.cy))
    | Some MoveDown ->
        move_cursor editor editor.cx
          (Int.min (Array.length editor.lines - 1) (succ editor.cy))
    | Some (InsertChar c) -> handle_insert_char editor c
    | Some DeleteChar -> handle_delete_char editor
    | Some InsertNewLine -> handle_newline editor
    | Some (EnterMode mode) -> { editor with mode }
    | _ -> editor

  let draw_lines editor =
    List.map
      (fun i ->
        if i < Array.length editor.lines then
          let line = editor.lines.(i) in
          I.string A.empty line
        else I.string A.empty "~")
      (List.init editor.nrows (fun n -> n + editor.oy))

  let draw_statusbar editor =
    let mode =
      match editor.mode with Normal -> "  NORMAL  " | Insert -> "  INSERT  "
    in
    let mode_img = I.string A.(bg magenta ++ fg black) mode in
    let filename = Option.value editor.filename ~default:"[No Name]" in
    let filename_img = I.string A.(fg yellow) filename in
    let editor_pos =
      Format.sprintf "%5d:%d     " (editor.cy + 1) (Array.length editor.lines)
    in
    let editor_pos_img = I.string A.(bg blue ++ fg black) editor_pos in

    let space =
      editor.ncols - String.length filename - String.length mode
      - String.length editor_pos
    in
    I.(
      mode_img <|> void 2 0 <|> filename_img <|> void space 0 <|> editor_pos_img)

  let render term editor =
    Term.cursor term (Some (editor.cx - editor.ox, editor.cy - editor.oy));
    Term.image term (I.vcat (draw_lines editor @ [ draw_statusbar editor ]))
end
