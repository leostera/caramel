open Printf

let dir =
  if Array.length Sys.argv > 1 then
    Sys.argv.(1)
  else
    Filename.concat (Sys.getcwd ()) "tests/cow"

let slurp filename =
  let file = open_in filename in
  let size = in_channel_length file in
  let buf = Bytes.create size in
  really_input file buf 0 size;
  Bytes.unsafe_to_string buf

let remove_blank s =
  let b = Buffer.create (String.length s) in
    for i = 0 to String.length s - 1 do
      match s.[i] with
        | ' ' | '\t' | '\n' -> ()
        | c -> Buffer.add_char b c
    done;
    Buffer.contents b

let process successes failures file =
  let html = (Filename.chop_extension file) ^ ".html" in
  if not (Sys.file_exists html) then (
    Printf.eprintf "File %s does not exist.\n" html;
    exit 2;
  );
  let expected = slurp html in
  let md, observed =
   try
     let md = Omd.of_string (slurp file) in
     md, Omd.to_html md
   with e -> [], Printexc.to_string e
  in
  (* Make sure a round trip produces identical results *)
  let round_trip = Omd.of_string(Omd.to_markdown md) in
  if expected <> observed && remove_blank expected <> remove_blank observed
  then (
    eprintf "FAILURE: %s\n" file;
    eprintf "  expected = %S\n" (expected);
    eprintf "  observed = %S\n" (observed);
    incr failures
  )
  else if Omd_representation.(
    loose_compare md round_trip <> 0
    && loose_compare (normalise_md md) (normalise_md round_trip) <> 0
  )
  then (
    eprintf "FAILURE: %s\n" file;
    eprintf "  Omd.of_string(Omd.to_markdown md) <> md\n";
    eprintf "Expected =%S\n  Result =%S\n"
      (Omd_backend.sexpr_of_md (Omd_representation.normalise_md md))
      (Omd_backend.sexpr_of_md (Omd_representation.normalise_md round_trip));
    incr failures
  )
  else (
    eprintf "SUCCESS: %s\n" file;
    incr successes
  )

let () =
  prerr_endline ("Reading directory " ^ dir);
  let files = Array.to_list (Sys.readdir dir) in
  let files = List.map (fun f -> Filename.concat dir f) files in
  let md_files = List.filter (fun f -> Filename.check_suffix f ".md") files in
  let md_files = List.sort String.compare md_files in
  let successes = ref 0
  and failures = ref 0 in
  List.iter (process successes failures) md_files;
  eprintf "%i test passed;  %i tests failed.\n" !successes !failures
