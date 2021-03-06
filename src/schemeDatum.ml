open! Base
module Format = Caml.Format

module With_position = struct
  type 'a t =
    { value : 'a
    ; start : Lexing.position
    ; end_ : Lexing.position
    }

  let string_of_position { start; end_; value = _ } =
    let f1 = start.pos_fname in
    let f2 = end_.pos_fname in
    let l1 = start.pos_lnum in
    let l2 = end_.pos_lnum in
    let c1 = start.pos_cnum - start.pos_bol + 1 in
    let c2 = end_.pos_cnum - end_.pos_bol + 1 in
    match String.equal f1 f2, l1 = l2, c1 = c2 with
    | false, _, _ ->
      Printf.sprintf "%s:%d.%d-%s:%d.%d" f1 l1 c1 f2 l2 c2
    | _, false, _ ->
      Printf.sprintf "%s:%d.%d-%d.%d" f1 l1 c1 l2 c2
    | _, _, false ->
      Printf.sprintf "%s:%d.%d-%d" f1 l1 c1 c2
    | _, _, true ->
      Printf.sprintf "%s:%d.%d" f1 l1 c1

  let pp ppv f v =
    Format.fprintf f "@[<v>%a@ (* %s *)@]" ppv v.value (string_of_position v)

  let show ppv v =
    Format.asprintf "%a" (pp ppv) v

  let dummy value =
    { value; start = Lexing.dummy_pos; end_ = Lexing.dummy_pos }
end

type lexeme_datum =
  [ `Boolean of bool
  | `Integer2 of string
  | `Integer8 of string
  | `Integer10 of string
  | `Integer16 of string
  | `Number of string
  | `Char of Uchar.t
  | `String of string
  | `Symbol of string
  ]
[@@deriving show]

type token =
  [ lexeme_datum
  | `NamedChar of string
  | `LineComment of string
  | `BlockComment of string
  | `DatumComment
  | `Whitespace of string
  | `OpenBv
  | `OpenL
  | `OpenV
  | `Close
  | `Dot
  | `Quote
  | `Quasiquote
  | `Unquote
  | `UnquoteSplicing
  | `Eof
  ]
[@@deriving show]

type atom_ =
  [ lexeme_datum
  | `Bytevector of bytes
  ]
[@@deriving show]

type 't atom =
  [ atom_
  | `Vector of 't array
  ]
[@@deriving show]

type ('t, 'tl) t_ =
  [ 't atom
  | `List of 't list
  | `DottedList of 't list * 'tl
  ]
[@@deriving show]

type t = (t, t atom) t_
[@@deriving show]

type positioned = (t_with_position, t_with_position atom With_position.t) t_
and t_with_position = positioned With_position.t

type patom = t_with_position atom

type atomosphere =
  [ `LineComment of string
  | `BlockComment of string
  | `DatumComment of t_with_position
  | `Whitespace of string
  ] With_position.t

type ss =
  [ positioned
  | `Eof
  | `Close
  | `Dot
  ] With_position.t

type lexical_error = [`LexicalError of string With_position.t]
[@@deriving show]

let make_lexical_error ?start ~lexbuf msg : [> lexical_error] =
  let b_, end_ = Sedlexing.lexing_positions lexbuf in
  let start = Option.value ~default:b_ start in
  `LexicalError { value = msg; start; end_ }

let fail_lexical_error ?start ~lexbuf msg =
  make_lexical_error ?start ~lexbuf msg
  |> Result.fail

let fail_lexical_errorf ?start ~lexbuf fmt =
  Printf.ksprintf (fail_lexical_error ?start ~lexbuf) fmt

type parse_error = [`ParseError of string With_position.t]
[@@deriving show]

let make_parse_error ~start ~end_ msg : [> parse_error] =
  `ParseError { With_position.value = msg; start; end_ }

let make_parse_errorf ~start ~end_ fmt =
  Printf.ksprintf (make_parse_error ~start ~end_) fmt

let fail_parse_error ~start ~end_ msg =
  Result.fail @@ make_parse_error ~start ~end_ msg

let fail_parse_errorf ~start ~end_ fmt =
  Printf.ksprintf (fail_parse_error ~start ~end_) fmt

type eof = [`Eof of string With_position.t]
[@@deriving show]

let make_eof ~start ~end_ msg : [> eof] =
  `Eof { With_position.value = msg; start; end_ }

let fail_eof ~start ~end_ msg =
  Result.fail @@ make_eof ~start ~end_ msg

let uchar_alarm = Uchar.of_char '\x07'
let uchar_backspace = Uchar.of_char '\x08'
let uchar_delete = Uchar.of_char '\x7F'
let uchar_escape = Uchar.of_char '\x1B'
let uchar_newline = Uchar.of_char '\x0A'
let uchar_null = Uchar.of_char '\x00'
let uchar_return = Uchar.of_char '\x0D'
let uchar_space = Uchar.of_char '\x07'
let uchar_tab = Uchar.of_char '\x09'

let name_chars =
  [ "alarm", uchar_alarm
  ; "backspace", uchar_backspace
  ; "delete", uchar_delete
  ; "escape", uchar_escape
  ; "newline", uchar_newline
  ; "null", uchar_null
  ; "return", uchar_return
  ; "space", uchar_space
  ; "tab", uchar_tab
  ]

let char_names = List.Assoc.inverse name_chars

let mnemonic_chars =
  [ "a", uchar_alarm
  ; "b", uchar_backspace
  ; "t", uchar_tab
  ; "n", uchar_newline
  ; "r", uchar_return
  ]

let char_mnemonics = List.Assoc.inverse mnemonic_chars

let uchar_of_hex ~start ~lexbuf s =
  let ss = "0x" ^ s in
  match Int.of_string ss |> Uchar.of_scalar with
  | Some v -> Result.return v
  | None ->
    fail_lexical_errorf ~start ~lexbuf "invalid unicode scalar value: %s" ss

module Lex = Sedlexing.Utf8

let add_position ?start ~lexbuf value =
  let b_, end_ = Sedlexing.lexing_positions lexbuf in
  let start = Option.value ~default:b_ start in
  { With_position.value; start; end_ }

let tap f x =
  let () = f x in
  x

let _a = [%sedlex.regexp? Chars "Aa"]
let _b = [%sedlex.regexp? Chars "Bb"]
let _c = [%sedlex.regexp? Chars "Cc"]
let _d = [%sedlex.regexp? Chars "Dd"]
let _e = [%sedlex.regexp? Chars "Ee"]
let _f = [%sedlex.regexp? Chars "Ff"]
let _g = [%sedlex.regexp? Chars "Gg"]
let _h = [%sedlex.regexp? Chars "Hh"]
let _i = [%sedlex.regexp? Chars "Ii"]
let _j = [%sedlex.regexp? Chars "Jj"]
let _k = [%sedlex.regexp? Chars "Kk"]
let _l = [%sedlex.regexp? Chars "Ll"]
let _m = [%sedlex.regexp? Chars "Mm"]
let _n = [%sedlex.regexp? Chars "Nn"]
let _o = [%sedlex.regexp? Chars "Oo"]
let _p = [%sedlex.regexp? Chars "Pp"]
let _q = [%sedlex.regexp? Chars "Qq"]
let _r = [%sedlex.regexp? Chars "Rr"]
let _s = [%sedlex.regexp? Chars "Ss"]
let _t = [%sedlex.regexp? Chars "Tt"]
let _u = [%sedlex.regexp? Chars "Uu"]
let _v = [%sedlex.regexp? Chars "Vv"]
let _w = [%sedlex.regexp? Chars "Ww"]
let _x = [%sedlex.regexp? Chars "Xx"]
let _y = [%sedlex.regexp? Chars "Yy"]
let _z = [%sedlex.regexp? Chars "Zz"]

let digit2 = [%sedlex.regexp? '0' .. '1']
let digit8 = [%sedlex.regexp? '0' .. '7']
let digit10 = [%sedlex.regexp? '0' .. '9']
let digit16 = [%sedlex.regexp? digit10 | _a | _b | _c | _d | _e | _f]

let explicit_sign = [%sedlex.regexp? "-" | "+"]
let sign = [%sedlex.regexp? Opt explicit_sign]
let exp = [%sedlex.regexp? _e, Plus digit10]

let intraline_whitespace = [%sedlex.regexp? Chars " \t"]
let line_ending = [%sedlex.regexp? "\n" | "\r\n" | "\r"]
let white_space = [%sedlex.regexp? intraline_whitespace | line_ending]

let hex_digit = [%sedlex.regexp? digit16]
let hex_escape = [%sedlex.regexp? "\\", _x, Plus hex_digit, ";"]
let mnemonic_escape = [%sedlex.regexp? "\\", Chars "abtnr"]
let escaped_space = [%sedlex.regexp?
    ("\\", Star intraline_whitespace, line_ending, Star intraline_whitespace)]

let digit = [%sedlex.regexp? digit10]
let letter = [%sedlex.regexp? 'a' .. 'z' | 'A' .. 'Z']
let special_initial = [%sedlex.regexp? Chars "!$%&*/:<=>?^_~"]
let initial = [%sedlex.regexp? letter | special_initial]
let special_subsequent = [%sedlex.regexp? explicit_sign | "." | "@"]
let subsequent = [%sedlex.regexp? initial | digit | special_subsequent]
let sign_subsequent = [%sedlex.regexp? initial | explicit_sign | "@"]
let dot_subsequent = [%sedlex.regexp? sign_subsequent | "."]
let peculiar_identifier = [%sedlex.regexp?
    ( explicit_sign
    | explicit_sign, sign_subsequent, Star subsequent
    | explicit_sign, ".", dot_subsequent, Star subsequent
    | ".", dot_subsequent, Star subsequent
    )]

let flonum = [%sedlex.regexp?
    ( Plus digit10, exp
    | Star digit10, ".", Plus digit10, Opt exp
    | Plus digit10, ".", Star digit10, Opt exp
    )]

let delimiter = [%sedlex.regexp? white_space | "|" | "(" | ")" | "\"" | ";"]

let uchar_dquote = Uchar.of_char '"'

let uchar_bar = Uchar.of_char '|'

let write_quoted ~quote buf v =
  let puts s = Buffer.add_string buf s in
  let putc c = Caml.Buffer.add_utf_8_uchar buf c in
  let putf fmt = Printf.ksprintf puts fmt in
  let lexbuf = Sedlexing.Utf8.from_string v in
  let is_control c = match Uucp.Gc.general_category c with `Cc -> true | _ -> false in
  let rec loop () =
    match %sedlex lexbuf with
    | eof -> ()
    | any ->
      let c = Sedlexing.lexeme_char lexbuf 0 in
      if is_control c then
        begin match List.Assoc.find ~equal:Uchar.equal char_mnemonics c with
          | Some s ->
            puts "\\";
            puts s
          | None ->
            putf "\\x%02x;" @@ Uchar.to_scalar c
        end
      else begin
        if Uchar.equal c quote then begin
          puts "\\"
        end;
        putc c;
      end;
      loop ()
    | _ -> assert false
  in
  putc quote;
  loop ();
  putc quote

let write_symbol buf v =
  let puts s = Buffer.add_string buf s in
  let lexbuf = Sedlexing.Utf8.from_string v in
  match %sedlex lexbuf with
  | initial, Star subsequent, eof
  | peculiar_identifier, eof ->
    puts v
  | _ ->
    write_quoted ~quote:uchar_bar buf v

let string_of_token : token -> string = function
  | `Boolean true ->
    "#t"
  | `Boolean false ->
    "#f"
  | `Integer2 s ->
     "#b" ^ s
  | `Integer8 s ->
     "#o" ^ s
  | `Integer10 s ->
     s
  | `Integer16 s ->
     "#x" ^ s
  | `Number s ->
     s
  | `Char c ->
    begin match List.Assoc.find ~equal:Uchar.equal char_names c with
    | Some s ->
      Printf.sprintf "#\\%s" s
    | None ->
      Printf.sprintf "#\\x%x" @@ Uchar.to_scalar c
    end
  | `String s ->
    let buf = Buffer.create 16 in
    write_quoted ~quote:uchar_dquote buf s;
    Buffer.contents buf
  | `Symbol s ->
    let buf = Buffer.create 16 in
    write_symbol buf s;
    Buffer.contents buf
  | `NamedChar s ->
    Printf.sprintf "#\\%s" s
  | `LineComment s ->
    ";" ^ s
  | `BlockComment s ->
    Printf.sprintf "#|%s|#" s
  | `DatumComment ->
    "#;"
  | `Whitespace s ->
    s
  | `OpenBv ->
    "#u8("
  | `OpenL ->
    "("
  | `OpenV ->
    "#("
  | `Close ->
    ")"
  | `Dot ->
    "."
  | `Quote ->
    "'"
  | `Quasiquote ->
    "`"
  | `Unquote ->
    ","
  | `UnquoteSplicing ->
    ",@"
  | `Eof -> ""

let rec write buf (t : t) =
  let puts s = Buffer.add_string buf s in
  let putt t = puts @@ string_of_token t in
  let putsp () = puts " " in
  match t with
  | #lexeme_datum as v ->
    putt v
  | `Bytevector bytes ->
    putt `OpenBv;
    bytes |> Caml.Bytes.iteri (fun i c ->
        if i > 0 then begin
          putsp ()
        end;
        puts @@ Printf.sprintf "%d" @@ Char.to_int c
      );
    putt `Close
  | `Vector vs ->
    putt `OpenV;
    vs |> Array.iteri ~f:(fun i v ->
        if i > 0 then begin
          putsp ()
        end;
        write buf v
      );
    putt `Close
  | `List vs ->
    putt `OpenL;
    vs |> List.iteri ~f:(fun i v ->
        if i > 0 then begin
          putsp ()
        end;
        write buf v
      );
    putt `Close
  | `DottedList (vs, t) ->
    putt `OpenL;
    vs |> List.iteri ~f:(fun i v ->
        if i > 0 then begin
          putsp ()
        end;
        write buf v
      );
    putsp ();
    putt `Dot;
    putsp ();
    write buf (t :> t);
    putt `Close

let write_to_string t =
  let buf = Buffer.create 128 in
  write buf t;
  Buffer.contents buf

let rec strip (x : t_with_position) : t =
  let strip_atom = function
    | { With_position.value = #atom_ as v; _ } ->
      v
    | { With_position.value = `Vector arr; _ } ->
      `Vector (Array.map ~f:strip arr)
  in
  match x with
  | { With_position.value = #patom; _ } as v ->
    (strip_atom v :> t)
  | { value = `List vs; _ } ->
    `List (List.map ~f:strip vs)
  | { value = `DottedList (vs, t); _ } ->
    `DottedList (List.map ~f:strip vs, strip_atom t)

let require_delimiter ~what lexbuf res =
  let open Result.Let_syntax in
  Sedlexing.start lexbuf;
  let r =
    let%bind res = res in
    match %sedlex lexbuf with
      | delimiter | eof ->
        return res
      | _ ->
        fail_lexical_errorf ~lexbuf
          "%s should be terminated by a delimiter" what
  in
  Sedlexing.rollback lexbuf;
  r

let rec recover_lexical_state f lexbuf =
  match f lexbuf with
  | Ok _ ->
    ()
  | Error _ ->
    recover_lexical_state f lexbuf

let quoted what cont start buf lexbuf =
  let open Result.Let_syntax in
  match %sedlex lexbuf with
  | "\\", _x ->
    let xstart, _ = Sedlexing.lexing_positions lexbuf in
    begin match %sedlex lexbuf with
      | Plus hex_digit ->
        let h = Lex.lexeme lexbuf in
        begin match %sedlex lexbuf with
          | ";" ->
            let%bind uc =
              uchar_of_hex h ~start:xstart ~lexbuf
              |> tap (Result.iter_error ~f:(fun _ ->
                  recover_lexical_state (cont start buf) lexbuf))
            in
            Caml.Buffer.add_utf_8_uchar buf uc;
            cont start buf lexbuf
          | _ ->
            let r = fail_lexical_errorf ~start:xstart ~lexbuf
                "unterminated hex_escape in %s" what in
            recover_lexical_state (cont start buf) lexbuf;
            r
        end
      | _ ->
        let r = fail_lexical_errorf ~start ~lexbuf
            "invalid hex_escape in %s" what in
        recover_lexical_state (cont start buf) lexbuf;
        r
    end
  | mnemonic_escape ->
    Lex.sub_lexeme lexbuf 1 1
    |> List.Assoc.find_exn ~equal:String.equal mnemonic_chars
    |> Caml.Buffer.add_utf_8_uchar buf;
    cont start buf lexbuf
  | escaped_space ->
    cont start buf lexbuf
  | "\\", any ->
    let r = fail_lexical_errorf ~start ~lexbuf
        "unrecognized escape sequence in %s: %s" what (Lex.lexeme lexbuf)
    in
    recover_lexical_state (cont start buf) lexbuf;
    r
  | eof ->
    fail_lexical_errorf ~start ~lexbuf "unclosed %s" what
  | any ->
    let r = fail_lexical_errorf ~start ~lexbuf
        "unexpected char in %s: %s" what (Lex.lexeme lexbuf)
    in
    recover_lexical_state (cont start buf) lexbuf;
    r
  | _ -> assert false

let rec string start buf lexbuf =
  let open Result.Let_syntax in
  match %sedlex lexbuf with
  | "\"" ->
    `String (Buffer.contents buf)
    |> add_position ~start ~lexbuf
    |> return
  | "\\\"" ->
    Buffer.add_string buf "\"";
    string start buf lexbuf
  | Compl (Chars "\"\\") ->
    Lex.lexeme lexbuf
    |> Buffer.add_string buf;
    string start buf lexbuf
  | _ ->
    quoted "string" string start buf lexbuf

let rec symbol start buf lexbuf =
  let open Result.Let_syntax in
  match %sedlex lexbuf with
  | "|" ->
    `Symbol (Buffer.contents buf)
    |> add_position ~start ~lexbuf
    |> return
  | "\\|" ->
    Buffer.add_string buf "|";
    symbol start buf lexbuf
  | Compl (Chars "|\\") ->
    Lex.lexeme lexbuf
    |> Buffer.add_string buf;
    symbol start buf lexbuf
  | _ ->
    quoted "symbol" symbol start buf lexbuf

let rec comment start buf lexbuf level =
  let open Result.Let_syntax in
  match %sedlex lexbuf with
  | "#|" ->
    Lex.lexeme lexbuf
    |> Buffer.add_string buf;
    comment start buf lexbuf @@ level + 1
  | "|#" ->
    if level = 1 then
      `BlockComment (Buffer.contents buf)
      |> add_position ~start ~lexbuf
      |> return
    else begin
      Lex.lexeme lexbuf
      |> Buffer.add_string buf;
      comment start buf lexbuf @@ level - 1
    end
  | any ->
    Lex.lexeme lexbuf
    |> Buffer.add_string buf;
    comment start buf lexbuf level
  | eof ->
    fail_lexical_error ~start ~lexbuf "unclosed comment"
  | _ ->
    assert false

let read_token lexbuf : (token With_position.t, _) Result.t =
  let open Result.Let_syntax in
  match %sedlex lexbuf with
  (* boolean *)
  | "#", _t, Opt (_r, _u, _e) ->
    `Boolean true
    |> add_position ~lexbuf
    |> return
    |> require_delimiter ~what:"booleans" lexbuf
  | "#", _f, Opt (_a, _l, _s, _e) ->
    `Boolean false
    |> add_position ~lexbuf
    |> return
    |> require_delimiter ~what:"booleans" lexbuf
  (* number *)
  | "#", _b ->
    let start, _ = Sedlexing.lexing_positions lexbuf in
    begin match %sedlex lexbuf with
      | sign, Plus digit2 ->
        `Integer2 (Lex.lexeme lexbuf)
        |> add_position ~start ~lexbuf
        |> return
        |> require_delimiter ~what:"numbers" lexbuf
      | _ ->
        fail_lexical_error ~start ~lexbuf "unexpected input while reading `Integer2"
    end
  | "#", _o ->
    let start, _ = Sedlexing.lexing_positions lexbuf in
    begin match %sedlex lexbuf with
      | sign, Plus digit8 ->
        `Integer8 (Lex.lexeme lexbuf)
        |> add_position ~start ~lexbuf
        |> return
        |> require_delimiter ~what:"numbers" lexbuf
      | _ ->
        fail_lexical_error ~start ~lexbuf "unexpected input while reading `Integer8"
    end
  | "#", _x ->
    let start, _ = Sedlexing.lexing_positions lexbuf in
    begin match %sedlex lexbuf with
      | sign, Plus digit16 ->
        `Integer16 (Lex.lexeme lexbuf)
        |> add_position ~start ~lexbuf
        |> return
        |> require_delimiter ~what:"numbers" lexbuf
      | _ ->
        fail_lexical_error ~start ~lexbuf "unexpected input while reading `Integer16"
    end
  | "#", _d ->
    let start, _ = Sedlexing.lexing_positions lexbuf in
    begin match %sedlex lexbuf with
      | sign, Plus digit10 ->
        `Integer10 (Lex.lexeme lexbuf)
        |> add_position ~start ~lexbuf
        |> return
        |> require_delimiter ~what:"numbers" lexbuf
      | sign, flonum ->
        `Number (Lex.lexeme lexbuf)
        |> add_position ~start ~lexbuf
        |> return
        |> require_delimiter ~what:"numbers" lexbuf
      | _ ->
        fail_lexical_error ~start ~lexbuf "unexpected input while reading `Number"
    end
  | sign, Plus digit10 ->
    `Integer10 (Lex.lexeme lexbuf)
    |> add_position ~lexbuf
    |> return
    |> require_delimiter ~what:"numbers" lexbuf
  | sign, flonum ->
    `Number (Lex.lexeme lexbuf)
    |> add_position ~lexbuf
    |> return
    |> require_delimiter ~what:"numbers" lexbuf
  (* character *)
  | "#\\" ->
    let start, _ = Sedlexing.lexing_positions lexbuf in
    begin match %sedlex lexbuf with
      | _x, Plus hex_digit ->
        let x = Lex.sub_lexeme lexbuf 1 (Sedlexing.lexeme_length lexbuf - 1) in
        uchar_of_hex ~start ~lexbuf x
        |> Result.map ~f:(fun c -> add_position ~start ~lexbuf @@ `Char c)
        |> require_delimiter ~what:"characters" lexbuf
      | initial, Plus subsequent ->
        `NamedChar (Lex.lexeme lexbuf )
        |> add_position ~start ~lexbuf
        |> return
        |> require_delimiter ~what:"characters" lexbuf
      | any ->
        let ch = Sedlexing.lexeme_char lexbuf 0 in
        `Char ch
        |> add_position ~start ~lexbuf
        |> return
        |> require_delimiter ~what:"characters" lexbuf
      | _ -> assert false
    end
  (* string *)
  | "\"" ->
    let buf = Buffer.create 16 in
    let start, _ = Sedlexing.lexing_positions lexbuf in
    string start buf lexbuf
  (* symbol *)
  | "|" ->
    let buf = Buffer.create 16 in
    let start, _ = Sedlexing.lexing_positions lexbuf in
    symbol start buf lexbuf
  | initial, Star subsequent ->
    `Symbol (Lex.lexeme lexbuf)
    |> add_position ~lexbuf
    |> return
    |> require_delimiter ~what:"identifiers" lexbuf
  | peculiar_identifier ->
    `Symbol (Lex.lexeme lexbuf)
    |> add_position ~lexbuf
    |> return
    |> require_delimiter ~what:"identifiers" lexbuf
  (* comment*)
  | "#|" ->
    let buf = Buffer.create 16 in
    let start, _ = Sedlexing.lexing_positions lexbuf in
    comment start buf lexbuf 1
  | ";", Star (Compl (Chars "\r\n")), (line_ending | eof) ->
    let c = Lex.sub_lexeme lexbuf 1 (Sedlexing.lexeme_length lexbuf - 1) in
    (* NB: preserve newlines to distinguish comments at eof and others*)
    `LineComment c
    |> add_position ~lexbuf
    |> return
  | "#;" ->
    `DatumComment
    |> add_position ~lexbuf
    |> return
  (* bytevector *)
  | "#u8(" ->
    `OpenBv
    |> add_position ~lexbuf
    |> return
  (* compound datum *)
  | "(" ->
    `OpenL
    |> add_position ~lexbuf
    |> return
  | "#(" ->
    `OpenV
    |> add_position ~lexbuf
    |> return
  | ")" ->
    `Close
    |> add_position ~lexbuf
    |> return
  | "." ->
    `Dot
    |> add_position ~lexbuf
    |> return
    |> require_delimiter ~what:"dot" lexbuf
  (* abbreviation *)
  | "'" ->
    `Quote
    |> add_position ~lexbuf
    |> return
  | "`" ->
    `Quasiquote
    |> add_position ~lexbuf
    |> return
  | "," ->
    `Unquote
    |> add_position ~lexbuf
    |> return
  | ",@" ->
    `UnquoteSplicing
    |> add_position ~lexbuf
    |> return
  (* whitespace *)
  | Plus white_space ->
    `Whitespace (Lex.lexeme lexbuf)
    |> add_position ~lexbuf
    |> return
  | eof ->
    `Eof
    |> add_position ~lexbuf
    |> return
  | any ->
    fail_lexical_errorf ~lexbuf "unexpected input: %s" (Lex.lexeme lexbuf)
  | _ ->
    assert false

type 'a tokenizer = unit -> (token With_position.t, 'a) Result.t

let parse_bytevector_element (v : t_with_position) =
  let f ~start ~end_ v s =
    match Char.of_int @@ Int.of_string s with
    | Some v -> Result.return v
    | None ->
      fail_parse_errorf ~start ~end_ "integer out of byte range: %s" (write_to_string @@ strip v)
    | exception Failure _ ->
      fail_parse_errorf ~start ~end_ "integer out of byte range: %s" (write_to_string @@ strip v)
  in
  match v with
  | { With_position.value = `Integer2 s; start; end_ } ->
    f ~start ~end_ v ("0b" ^ s)
  | { value = `Integer8 s; start; end_ } ->
    f ~start ~end_ v ("0o" ^ s)
  | { value = `Integer10 s; start; end_ } ->
    f ~start ~end_ v s
  | { value = `Integer16 s; start; end_ } ->
    f ~start ~end_ v ("0x" ^ s)
  | { value = _; start; end_ } ->
    fail_parse_errorf ~start ~end_ "not a byte: %s" (write_to_string @@ strip v)

let list_to_bytevector ~start ~end_ vs =
  let open Result.Let_syntax in
  let buf = Buffer.create @@ List.length vs in
  let ns = vs |> List.map ~f:parse_bytevector_element in
  let%map ns = Result.all ns in
  List.iter ~f:(Buffer.add_char buf) ns;
  let value = `Bytevector (Buffer.contents_bytes buf) in
  { With_position.value; start; end_ }

let rec parse_tokens ~left (tokenize : _ tokenizer) =
  let open Result.Let_syntax in
  match%bind parse0 ~left tokenize with
  | { value = `Close; start; end_ } ->
    fail_parse_error ~start ~end_ "unexpected close parenthesis"
  | { value = `Dot; start; end_ } ->
    fail_parse_error ~start ~end_ "unexpected dot"
  | { With_position.value = `Eof; start; end_ } ->
    fail_eof ~start ~end_ "eof"
  | { value = #positioned; _ } as x ->
    return x
and parse0 ?(left : atomosphere list = []) tokenize : (ss, _) Result.t =
  let open Result.Let_syntax in
  match%bind tokenize () with
  | { With_position.value = #lexeme_datum; _ } as v ->
    return v
  | { value = `NamedChar c; start; end_ } ->
    let%map c =
      List.Assoc.find ~equal:String.equal name_chars c
      |> Result.of_option
        ~error:(make_parse_errorf ~start ~end_ "unknown character named %s" c)
    in
    { With_position.value = `Char c; start; end_ }
  | { value = `LineComment _; _ } as v ->
    parse0 ~left:(v::left) tokenize
  | { value = `BlockComment _; _ } as v ->
    parse0 ~left:(v::left) tokenize
  | { value = `Whitespace _; _ } as v ->
    parse0 ~left:(v::left) tokenize
  | { value = `DatumComment; start; _ } ->
    let%bind v =
      parse_tokens ~left:[] tokenize
      |> Result.map_error ~f:(function
          | `Eof { With_position.start; end_; _ } ->
            make_parse_error ~start ~end_ "eof after #;"
          | v -> v)
    in
    let c = { With_position.value = `DatumComment v; start; end_ = v.end_ } in
    parse0 ~left:(c::left) tokenize
  | { value = `OpenL; start; _ } ->
    begin match%map list ~what:"list" ~allows_dot:true ~start [] tokenize with
      | vs, None, end_ ->
        let value = `List vs in
        { With_position.value; start; end_ }
      | vs, Some { With_position.value = `List ys; _ }, end_ ->
        let value = `List (vs @ ys) in
        { With_position.value; start; end_ }
      | vs, Some { With_position.value = `DottedList(ys, t); _ }, end_ ->
        let value = `DottedList (vs @ ys, t) in
        { With_position.value; start; end_ }
      | vs, Some ({ With_position.value = #patom; _} as t), end_ ->
        let value = `DottedList (vs, t) in
        { With_position.value; start; end_ }
    end
  | { value = `OpenV; start; _ } ->
    begin match%bind list ~what:"vector" ~allows_dot:false ~start [] tokenize with
      | _, Some _, _end_ ->
        (* ~allows_dot:false *)
        assert false
      | vs, None, end_ ->
        let value = `Vector (Array.of_list vs) in
        return { With_position.value; start; end_ }
    end
  | { value = `OpenBv; start; _ } ->
    begin match%bind list ~what:"bytevector" ~allows_dot:false ~start [] tokenize with
      | _, Some _, _end_ ->
        (* ~allows_dot:false *)
        assert false
      | vs, None, end_ ->
        list_to_bytevector ~start ~end_ vs
    end
  | { value = `Quote; start; _ } ->
    abbr ~start "quote" tokenize
  | { value = `Quasiquote; start; _ } ->
    abbr ~start "quasiquote" tokenize
  | { value = `Unquote; start; _ } ->
    abbr ~start "unquote" tokenize
  | { value = `UnquoteSplicing; start; _ } ->
    abbr ~start "unquote-splicing" tokenize
  | { value = (`Eof | `Dot | `Close); _ } as v ->
    return v
and list ~what ~start ~allows_dot elems ?tail tokenize =
  let open Result.Let_syntax in
  match%bind parse0 tokenize with
  | { value = `Close; end_ } ->
    return (List.rev elems, tail, end_)
  | { value = `Eof; start = _; end_ } ->
    fail_parse_errorf ~start ~end_ "unclosed %s" what
  | { value = `Dot; start; end_ } ->
    begin match allows_dot, elems, tail with
      | false, _, _ ->
        fail_parse_errorf ~start ~end_ "%s cannot contain `.'" what
      | true, _, Some _ ->
        fail_parse_error ~start ~end_ "multiple dots"
      | true, [], None ->
        fail_parse_error ~start ~end_ "unexpected dot"
      | true, (_::_), None ->
        begin match%bind parse0 ~left:[] tokenize with
         | { value = `Eof; start; end_ } ->
             fail_parse_errorf ~start ~end_ "unclosed %s" what
         | { value = `Close; start; end_ } ->
             fail_parse_errorf ~start ~end_ "unexpected close parenthesis"
         | { value = `Dot; start; end_ } ->
             fail_parse_errorf ~start ~end_ "multiple dots"
         | { value = #positioned; start = _; end_ = _ } as tail ->
             list ~what ~allows_dot ~start elems ~tail tokenize
        end
    end
  | { value = #positioned; start; end_ } as v ->
    begin match tail with
      | Some tl ->
        fail_parse_errorf ~start ~end_ "multiple elements after dot: %s %s"
          (write_to_string @@ strip tl)
          (write_to_string @@ strip v)
      | None ->
        list ~what ~allows_dot ~start (v :: elems) ?tail tokenize
    end
and abbr ~start name tokenize =
  let open Result.Let_syntax in
  let%map v =
    parse_tokens ~left:[] tokenize
    |> Result.map_error ~f:(function
        | `Eof { With_position.start; end_; _ } ->
          make_parse_errorf ~start ~end_ "eof after %s" name
        | v -> v)
  in
  let value = `List [With_position.dummy @@ `Symbol name ; v] in
  { With_position.value; start; end_ = v.end_ }

let read_with_position lexbuf =
  let tokenize () = read_token lexbuf in
  parse_tokens ~left:[] tokenize

let read lexbuf =
  read_with_position lexbuf
  |> Result.map ~f:strip
