let start_link mod_name init_arg opts = Ok (Process.make_pid ())

module type S = sig
  type error
  type state
  type opts
  val init : opts -> (state, error) result
end
