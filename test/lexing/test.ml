open Base

let () =
  Stdio.print_endline "(* -*- mode: caml -*- *)";
  Stdio.print_endline "(* generated automatically. DO NOT EDIT *)";
  Stdio.print_endline "(* To update this file, you should run `dune runtest; dune promote`. *)";
  Sys.get_argv ()
  |> Array.to_list
  |> List.tl
  |> Option.value ~default:[]
  |> List.iter ~f:begin fun fn ->
    try
      Stdio.In_channel.with_file fn
        ~f:(fun in_ch ->
            let lexbuf = Sedlexing.Utf8.from_channel in_ch in
            Sedlexing.set_filename lexbuf fn;
            Stdio.printf "\n(* %s *)\n" fn;
            let rec loop () =
              match SchemeDatum.read_token lexbuf with
              | Ok { value = `Eof; _ } ->
                ()
              | Ok { value = `Whitespace _; _ } ->
                loop ()
              | Ok t ->
                t
                |> SchemeDatum.With_position.show SchemeDatum.pp_token
                |> Stdio.print_endline;
                loop ()
              | Error (#SchemeDatum.lexical_error as e) ->
                e
                |> SchemeDatum.show_lexical_error
                |> Stdio.print_endline;
                loop ()
            in loop ()
          )
    with
    | e ->
      Stdio.eprintf "%s: parse error: %s\n" (Sys.get_argv ()).(0) @@ Exn.to_string e;
      Caml.exit 1
  end
