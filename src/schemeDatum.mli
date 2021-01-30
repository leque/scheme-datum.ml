module With_position : sig
  type 'a t =
    { value : 'a
    ; start : Lexing.position
    ; end_ : Lexing.position
    }
  [@@deriving show]
end

(** {1 Tokens and AST} *)

type lexeme_datum =
    [ `Boolean of bool
    (** #t -> [`Boolean true], #f -> [`Boolean false] *)
    | `Char of Uchar.t
    (** #\a -> [`Char U+0041], #\x0020 -> [`Char U+0020] *)
    | `Integer2 of string
    (** #b001 -> [`Integer2 "001"] *)
    | `Integer8 of string
    (** #o644 -> [`Integer2 "644"] *)
    | `Integer10 of string
    (** 1024 -> [`Integer10 "1024"], #d4096 -> [`Integer10 "4096"] *)
    | `Integer16 of string
    (** #xdeadbeef -> [`Integer16 "deadbeef"] *)
    | `Number of string
    (** 3.14e0 -> [`Number "3.14e0"] *)
    | `String of string
    | `Symbol of string
    ]
  [@@deriving show]

(** {2 Tokens} *)

type token =
    [ lexeme_datum
    | `Eof
    (** end of file *)
    | `NamedChar of string
    (**
       #\space -> [`NamedChar "space"],
       #\Backspace -> [`NamedChar "Backspace"] *)
    | `BlockComment of string
    (** #|...|# -> [`BlockComment "..."] *)
    | `DatumComment
    (** #; *)
    | `LineComment of string
    (** ;... -> [`LineComment "..."] *)
    | `Dot
    (** . *)
    | `OpenBv
    (** #u8( *)
    | `OpenL
    (** ( *)
    | `OpenV
    (** #( *)
    | `Close
    (** ) *)
    | `Quasiquote
    (** ` *)
    | `Quote
    (** ' *)
    | `Unquote
    (** , *)
    | `UnquoteSplicing
    (** ,@ *)
    | `Whitespace of string
    ]
  [@@deriving show]

(** {2 AST} *)

type 't atom =
  [ lexeme_datum
  | `Bytevector of bytes
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

(** {1 Errors} *)

type lexical_error = [ `LexicalError of string With_position.t ]
[@@deriving show]

type parse_error = [ `ParseError of string With_position.t ]
[@@deriving show]

type eof = [ `Eof of string With_position.t ]
[@@deriving show]

(** {1 Reader and Writer} *)

val read_token :
  Sedlexing.lexbuf ->
  (token With_position.t, [> lexical_error ]) result

val string_of_token : token -> string

val read_with_position :
  Sedlexing.lexbuf ->
  (t_with_position, [> eof | lexical_error | parse_error ]) result

val strip : t_with_position -> t

val read :
  Sedlexing.lexbuf ->
  (t, [> eof | lexical_error | parse_error ]) result

val write : Buffer.t -> t -> unit

val write_to_string : t -> string
