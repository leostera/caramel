  $ caramel compile --sugarcane --dump-ast functors.ml
  caramel: [DEBUG] Running Sugarcane compiler on sources: 
  ((sources (functors.ml))
    (stdlib
      (/Users/ostera/repos/github.com/AbstractMachinesLab/sugarcane/_build/install/default/bin/../lib/caramel/stdlib))
    (dump_ast true))
  
  caramel: [DEBUG] Compiling implementation unit: ((source_file functors.ml)
                                                    (source_kind impl))
  caramel: [DEBUG] Calling typedtree handler for unit: ((source_file
                                                          functors.ml)
                                                         (source_kind impl))
  caramel: [DEBUG] Translating functors.ml (module Functors)
  caramel: [DEBUG] Creating module: Caramel.Functors
  Oops! This function has not been implemented yet: Tmod_apply
  [1]
  $ cat Caramel.Functors.core Caramel.Functors.core.ast
  cat: Caramel.Functors.core: No such file or directory
  cat: Caramel.Functors.core.ast: No such file or directory
  [1]
