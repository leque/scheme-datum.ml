(* -*- mode: caml -*- *)
(* generated automatically. DO NOT EDIT *)
(* To update this file, you should run `dune runtest; dune promote`. *)

(* test.scm *)
`List ([`Symbol ("define"); `List ([`Symbol ("fact"); `Symbol ("n")]);
         `List ([`Symbol ("if");
                  `List ([`Symbol ("="); `Symbol ("n"); `Integer10 ("0")]);
                  `Integer10 ("1");
                  `List ([`Symbol ("*"); `Symbol ("n");
                           `List ([`Symbol ("fact");
                                    `List ([`Symbol ("-"); `Symbol ("n");
                                             `Integer10 ("1")])
                                    ])
                           ])
                  ])
         ])

(* list.scm *)
`List ([`Integer10 ("1"); `Integer10 ("2"); `Integer10 ("3")])
`List ([`Integer10 ("1"); `Integer10 ("2"); `Integer10 ("3")])
`DottedList (([`Integer10 ("1"); `Integer10 ("2")], `Integer10 ("3")))
`DottedList (([`Integer10 ("1"); `Integer10 ("2"); `Integer10 ("3")],
              `Integer10 ("4")))
`ParseError ("unexpected dot"
             (* list.scm:5.2-3 *))
`Integer10 ("1")
`ParseError ("extra close parenthesis"
             (* list.scm:5.5-6 *))
`ParseError ("extra close parenthesis"
             (* list.scm:6.5-6 *))
`ParseError ("multiple elements after dot: 2 3"
             (* list.scm:7.8-9 *))
`ParseError ("extra close parenthesis"
             (* list.scm:7.9-10 *))
`ParseError ("bare ."
             (* list.scm:8.6-7 *))
`ParseError ("extra close parenthesis"
             (* list.scm:8.7-8 *))
`ParseError ("multiple dots"
             (* list.scm:9.8-9 *))
`ParseError ("extra close parenthesis"
             (* list.scm:9.9-10 *))
`ParseError ("unclosed list"
             (* list.scm:10.2-11.1 *))

(* vector.scm *)
`ParseError ("vector cannot contain `.'"
             (* vector.scm:1.5-6 *))
`Symbol ("b")
`ParseError ("extra close parenthesis"
             (* vector.scm:1.8-9 *))
`ParseError ("vector cannot contain `.'"
             (* vector.scm:2.3-4 *))
`Symbol ("b")
`ParseError ("extra close parenthesis"
             (* vector.scm:2.6-7 *))
`ParseError ("vector cannot contain `.'"
             (* vector.scm:3.5-6 *))
`ParseError ("extra close parenthesis"
             (* vector.scm:3.6-7 *))
`ParseError ("unclosed vector"
             (* vector.scm:4.1-5.1 *))

(* bytevector.scm *)
`Bytevector ("\001")
`ParseError ("integer out of byte range: #x100"
             (* bytevector.scm:2.5-10 *))
`ParseError ("not a byte: a"
             (* bytevector.scm:3.5-6 *))
`ParseError ("bytevector cannot contain `.'"
             (* bytevector.scm:4.5-6 *))
`Integer10 ("1")
`ParseError ("extra close parenthesis"
             (* bytevector.scm:4.8-9 *))
`ParseError ("bytevector cannot contain `.'"
             (* bytevector.scm:5.7-8 *))
`ParseError ("extra close parenthesis"
             (* bytevector.scm:5.8-9 *))
`ParseError ("unclosed bytevector"
             (* bytevector.scm:6.1-7.1 *))
