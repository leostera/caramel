open Caramel_misc

(* Exposed modules *)
module B = B
module IR = Ir
module Translation_unit = Translation_unit

let translate Translation_unit.({ print_time; dump_pass; _ } as unit) =
  let tunit = Ir_0.ir_of_unit unit in
  let Translation_unit.{ ir; comp_unit = cunit; _ } = tunit in

  let pass idx name p ir =
    let t0 = Mtime_clock.now () in
    let ir' = p ir in
    let t1 = Mtime_clock.now () in
    if dump_pass == idx then
      Output.write ~unit:cunit ~ext:(".ir_" ^ Int.to_string idx) Ir.pp ir';
    let t2 = Mtime_clock.now () in

    if print_time then
      Logs.debug (fun f ->
          let pass_time = Mtime.span t1 t0 in
          let print_time = Mtime.span t2 t1 in
          f "Pass #%d: %s, took %a (+write=%a)" idx name Mtime.Span.pp pass_time
            Mtime.Span.pp print_time);

    ir'
  in

  if dump_pass != -1 then Output.write ~unit:cunit ~ext:".ir_0" Ir.pp ir;

  let ir =
    ir
    |> pass 1 "rename modules" Pass_rename_modules.run
    |> pass 1 "build metadata table" Pass_metadata_table.build_table
    |> pass 2 "rewrite external calls" Pass_ext_calls.run
    |> pass 3 "mark exported symbols" (Pass_mark_exported.run ~tunit)
    |> pass 4 "flatten all modules" Pass_flatten_modules.run
    |> pass 5 "rewrite recursive calls" Pass_rewrite_recursive_apply.run
  in

  { tunit with ir }

let to_b_lang ir = B_builder.of_ir ir
