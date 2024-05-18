module Editor = struct
  open Notty
  open Notty_unix

  type mode = Normal | Insert | Command
  type subaction = G

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
    | InsertLineAbove
    | InsertLineBelow
    | MoveCursorForward
    | MoveToEndOfLine of mode
    | MoveToStartOfFile
    | MoveToEndOfFile
    | SetSubAction of subaction
    | ExecCmd

  type t = {
    cx : int;
    cy : int;
    ox : int;
    oy : int;
    cmd : Buffer.t;
    mode : mode;
    nrows : int;
    ncols : int;
    buf : Buffer.t;
    filename : string option;
    lines : string array;
    subaction : subaction option;
    logger : Out_channel.t;
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
        cmd = Buffer.create 0;
        mode = Normal;
        nrows = rows - 1;
        ncols = cols;
        buf = Buffer.create 0;
        filename = None;
        lines = [| "" |];
        subaction = None;
        logger = open_out "ov.log";
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
        cmd = Buffer.create 0;
        mode = Normal;
        nrows = rows - 1;
        ncols = cols;
        buf = Buffer.create 0;
        filename = Some filename;
        lines = Array.of_list lines;
        subaction = None;
        logger = open_out "ov.log";
      } )

  let handle_normal_event editor term =
    match Term.event term with
    | `Key (`Arrow `Right, [] | `ASCII 'l', []) -> Some MoveRight
    | `Key (`Arrow `Left, [] | `ASCII 'h', []) -> Some MoveLeft
    | `Key (`Arrow `Up, [] | `ASCII 'k', []) -> Some MoveUp
    | `Key (`Arrow `Down, [] | `ASCII 'j', []) -> Some MoveDown
    | `Key (`ASCII 'i', []) -> Some (EnterMode Insert)
    | `Key (`ASCII ':', []) -> Some (EnterMode Command)
    | `Key (`ASCII 'w', []) -> Some MoveCursorForward
    | `Key (`ASCII 'A', []) -> Some (MoveToEndOfLine Insert)
    | `Key (`ASCII 'o', []) -> Some InsertLineBelow
    | `Key (`ASCII 'O', []) -> Some InsertLineAbove
    | `Key (`ASCII '$', []) -> Some (MoveToEndOfLine Normal)
    | `Key (`ASCII 'G', []) -> Some MoveToEndOfFile
    | `Key (`ASCII 'g', []) when editor.subaction = Some G ->
        Some MoveToStartOfFile
    | `Key (`ASCII 'g', []) -> Some (SetSubAction G)
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

  let get_action_from_command editor =
    let cmd = Buffer.contents editor.cmd in
    match cmd with
    | _ when String.starts_with ~prefix:"q" cmd -> Some Quit
    | _ -> None

  let handle_command editor term =
    match Term.event term with
    | `Key (`Backspace, []) ->
        if Buffer.length editor.cmd > 0 then
          Buffer.truncate editor.cmd (Buffer.length editor.cmd - 1);
        None
    | `Key (`Escape, []) ->
        Buffer.reset editor.cmd;
        Some (EnterMode Normal)
    | `Key (`Enter, []) -> get_action_from_command editor
    | `Key (`ASCII c, []) ->
        Buffer.add_char editor.cmd c;
        None
    | _ -> None

  let handle_event term editor =
    match editor.mode with
    | Normal -> handle_normal_event editor term
    | Insert -> handle_insert_event term
    | Command -> handle_command editor term

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

  let handle_motion_forward editor =
    let s = Str.regexp {|[a-zA-Z0-9]* +\([a-zA-Z0-9]+\)|} in
    let line = editor.lines.(editor.cy) in
    let _ = Str.string_match s line editor.cx in
    let pos = try Str.group_beginning 1 with _ -> editor.cx in
    { editor with cx = pos }

  let handle_move_to_end_of_line mode editor =
    let line = editor.lines.(editor.cy) in
    let len = String.length line in
    { editor with cx = len; mode }

  let handle_move_to_end_of_file editor =
    let len = Array.length editor.lines in
    match len with 0 -> editor | _ -> { editor with cy = len - 1 }

  let handle_insert_line editor off =
    match Array.length editor.lines with
    | 0 ->
        let lines = Array.append editor.lines [| "" |] in
        { editor with lines; cy = editor.cy + off; cx = 0 }
    | _ ->
        let above = Array.sub editor.lines 0 (editor.cy + off) in
        let below =
          Array.sub editor.lines (editor.cy + off)
            (Array.length editor.lines - editor.cy - off)
        in
        let lines = Array.concat [ above; [| "" |]; below ] in
        { editor with lines; cx = 0; cy = editor.cy + off; mode = Insert }

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
    | Some MoveCursorForward -> handle_motion_forward editor
    | Some (MoveToEndOfLine mode) -> handle_move_to_end_of_line mode editor
    | Some MoveToEndOfFile -> handle_move_to_end_of_file editor
    | Some MoveToStartOfFile -> { editor with cx = 0; cy = 0; subaction = None }
    | Some (SetSubAction a) -> { editor with subaction = Some a }
    | Some InsertLineAbove -> handle_insert_line editor 0
    | Some InsertLineBelow -> handle_insert_line editor 1
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
      match editor.mode with
      | Normal -> "  NORMAL  "
      | Insert -> "  INSERT  "
      | _ -> ":" ^ Buffer.contents editor.cmd
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
