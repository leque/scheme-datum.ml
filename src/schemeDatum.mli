module With_position : sig
  type 'a t =
    { value : 'a
    ; start : Lexing.position
    ; end_ : Lexing.position
    }

  val pp :
    (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a t -> unit

  val show : (Format.formatter -> 'a -> unit) -> 'a t -> string
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

val pp_lexeme_datum :
  Format.formatter -> lexeme_datum -> unit

val show_lexeme_datum : lexeme_datum -> string

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

val pp_token : Format.formatter -> token -> unit

val show_token : token -> string

(** {2 AST} *)

type t =
    [ lexeme_datum
    | `Bytevector of Bytes.t
    | `DottedList of t list * t
    | `List of t list
    | `Vector of t array
    ]

val pp : Format.formatter -> t -> unit

val show : t -> string

type t_with_position =
    ([ lexeme_datum
     | `Bytevector of Bytes.t
     | `DottedList of 'a With_position.t list * 'a With_position.t
     | `List of 'a With_position.t list
     | `Vector of 'a With_position.t array
     ] as 'a) With_position.t

(** {1 Errors} *)

type lexical_error = [ `LexicalError of string With_position.t ]

val pp_lexical_error :
  Format.formatter -> lexical_error -> unit

val show_lexical_error : lexical_error -> string

type parse_error = [ `ParseError of string With_position.t ]

val pp_parse_error :
  Format.formatter -> parse_error -> unit

val show_parse_error : parse_error -> string

type eof = [ `Eof of string With_position.t ]

(** {1 Reader and Writer} *)

val read_token :
  Sedlexing.lexbuf ->
  (token With_position.t, [> lexical_error ]) result

val write_token : token -> string

val read_with_position :
  Sedlexing.lexbuf ->
  (t_with_position, [> eof | lexical_error | parse_error ]) result

val strip : t_with_position -> t

val read :
  Sedlexing.lexbuf ->
  (t, [> eof | lexical_error | parse_error ]) result

val write : Buffer.t -> t -> unit

val write_to_string : t -> string
