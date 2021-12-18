open Caramel_misc

(* Exposed modules *)
module B = B
module IR = Ir
module Translation_unit = Translation_unit

let translate Translation_unit.({ dump_pass; _ } as unit) =
  let tunit = Ir_0.ir_of_unit unit in
  let Translation_unit.{ ir; comp_unit = cunit; _ } = tunit in

  let pass idx p ir =
    let ir' = p ir in
    if dump_pass == idx then
      Output.write ~unit:cunit ~ext:(".ir_" ^ Int.to_string idx) Ir.pp ir';
    ir'
  in

  if dump_pass != -1 then Output.write ~unit:cunit ~ext:".ir_0" Ir.pp ir;

  let ir =
    ir
    |> pass 1 Pass_rename_modules.run
    |> pass 2 Pass_ext_calls.run
    |> pass 3 Pass_flatten_modules.run
  in

  { tunit with ir }

let to_b_lang ir = B_builder.of_ir ir
