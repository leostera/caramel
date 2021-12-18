let write ~unit ~ext pp t =
  let filename = Compilation_unit.source_file unit ^ ext in
  Logs.debug (fun f -> f "Writing %s" filename);
  let oc = open_out_bin filename in
  (try
     let ppf = Format.formatter_of_out_channel oc in
     Format.fprintf ppf "%a" pp t
   with _ -> Sys.remove filename);
  close_out oc;
  Logs.debug (fun f -> f "OK")

let write_many ~unit ~ext pp ts =
  List.iteri
    (fun idx t -> write ~unit ~ext:(ext ^ "_" ^ Int.to_string idx) pp t)
    ts
