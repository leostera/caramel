let start () = Dummy.start ()
let start_link mod_name init_arg opts = Ok (Process.spawn start)

module type S = sig
  type error
  type state
  type opts
  val init : opts -> (state, error) result
end
