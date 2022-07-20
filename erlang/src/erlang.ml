module Core = struct
  module Ast = Core_ast
  module Lexer = Core_lexer
  (* module Parser = Core_parser *)
  module Printer = Core_printer
end

module Ast = Erl_ast
module Ast_helper = Erl_ast_helper
module Lexer = Erl_lexer
module Parse = Erl_parse
module Parser = Erl_parser
module Printer = Erl_printer
