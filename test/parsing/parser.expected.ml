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