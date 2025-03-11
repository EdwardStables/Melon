module ::= KW_module module_name control_ports signal_ports module_body
module_name ::= VL_variable

control_ports ::= SS_open_bracket control_port SS_close_bracket |
                  SS_open_bracket control_port SS_comma control_port SS_close_bracket |
                  ""
control_port ::= OP_add VL_variable | OP_subtract VL_variable

signal_ports ::= SS_open_parenthesis signal_port_list SS_close_parenthesis
signal_port_list ::= signal_port SS_comma signal_port_list |
                     signal_port | ""
signal_port ::= direction width signal_name
direction ::= input | output
width ::= SS_open_bracket VL_integer_literal SS_close_bracket | ""
signal_name ::= VL_variable

module_body ::= SS_open_brace module_body_list SS_close_brace
module_body_list ::= module_body_item module_body_list | ""
module_body_item ::= declaration | block | instantiation

declaration ::= KW_signal width signal_name_list SS_semi_colon
signal_name_list ::= signal_name SS_comma signal_name_list | signal_name

block ::= block_type block_body
block_type ::= KW_proc | KW_comb
block_body ::= SS_open_brace statement_list SS_close_brace |
               statement
statement_list ::= statement statement_list | ""

statement ::= assignment_statement SS_semi_colon |
              if_statement
if_statement ::= KW_if expr SS_open_brace statement_list SS_close_brace
assignment_statement ::= VL_variable SS_assign expr SS_semi_colon

expr ::= atomic_expr atom_follow expr_tail |
         unary_op expr expr_tail
expr_tail ::= binary_op expr expr_tail | ""

atom_follow ::= SS_open_bracket expr SS_close_bracket | ""
atomic_expr ::= SS_open_parenthesis expr SS_close_parenthesis |
                VL_integer_literal |
                VL_variable |
unary_op ::= OP_and | OP_or | OP_negate | OP_xor 
binary_op ::= OP_add expr | OP_subtract | OP_and |
             OP_or | OP_xor | OP_lsl |
             OP_lsr | OP_concat | OP_equals

instantiation ::= VL_variable VL_varialbe SS_open_parenthesis signal_connection_list SS_close_parenthesis SS_semi_colon
signal_connection_list ::= signal_connection SS_comma signal_connection_list | ""
signal_connection ::= OP_dot VL_variable SS_open_parenthesis VL_variable SS_close_parenthesis
