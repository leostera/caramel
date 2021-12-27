open Lambda

let rec of_lambda_const ~unit c =
  match c with
  | Const_base (c, metadata) -> of_const_base ~unit ~metadata c
  | Const_block (_size, parts, metadata) -> of_const_block ~unit ~metadata parts
  | Const_float_array _ -> Error.todo "Const_float_array"
  | Const_immstring str -> Literal.string str |> Ir.lit

and of_const_base ~unit:_ ~metadata c =
  match metadata with
  | Some (Const_variant { label }) ->
      Ir.lit (Literal.atom label)
  | Some (Const_construct { name = "()"; _ }) ->
      Ir.lit (Literal.atom "unit")
  | Some (Const_construct { name = "[]"; _ }) ->
      Ir.nil
  | Some (Const_construct { name; _ }) ->
      Ir.lit (Literal.atom (String.lowercase_ascii name))
  | None -> Literal.of_const c |> Ir.lit

and of_const_block ~unit ~metadata fields =
  match (metadata, fields) with
  | Some (Block_variant { label }), _tag :: fields ->
      let fields = List.map (of_lambda_const ~unit) fields in
      let tag = Ir.lit (Literal.atom (String.lowercase_ascii label)) in
      Ir.tuple ~parts:(tag :: fields)
  | Some (Block_variant { label = _ }), [] ->
      Error.panic "variant without parts that isn't a constant?!"
  | Some Block_tuple, _ ->
      Ir.tuple ~parts:(List.map (of_lambda_const ~unit) fields)
  | Some (Block_construct { name = "::"; _ }), fields -> (
      let fields = List.map (of_lambda_const ~unit) fields in
      match fields with
      | [] -> Ir.nil
      | [ h; t ] -> Ir.cons h t
      | _ -> Error.panic "found an improper list!")
  | Some (Block_construct { name; _ }), _ ->
      let fields = List.map (of_lambda_const ~unit) fields in
      let tag = Ir.lit (Literal.atom (String.lowercase_ascii name)) in
      Ir.tuple ~parts:(tag :: fields)
  | Some (Block_record { fields = record_fields; representation = _ }), _ ->
      let fields =
        List.mapi
          (fun idx v ->
            let open Lambda in
            let field = record_fields.(idx) in
            let k = Ir.lit (Literal.string field.brf_name) in
            (k, of_lambda_const ~unit v))
          fields
      in
      Ir.record ~fields
  | None, fields ->
      let fields = List.map (of_lambda_const ~unit) fields in
      Ir.tuple ~parts:fields
