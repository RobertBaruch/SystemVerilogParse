grammar SystemVerilog;

//
// A.1.2 SystemVerilog source text
//

source_text : timeunits_decl? description* ;

// TODO: add more
description :
	module_decl
	| package_decl
	| attribute_instance* package_item
	;

// TODO: define non-ANSI header
module_decl : 
	module_ansi_header timeunits_decl? non_port_module_item* 
	ENDMODULE (':' module_identifier)?
	| EXTERN module_ansi_header
	;

module_ansi_header :
	attribute_instance* module_keyword lifetime? module_identifier
		package_import_decl* parameter_port_list? list_of_port_decls? ';'
	;

module_keyword : MODULE | MACROMODULE ;

package_decl :
	attribute_instance* PACKAGE lifetime? package_identifier ';'
	timeunits_decl? (attribute_instance* package_item)?
	ENDPACKAGE (':' package_identifier)
	;

timeunits_decl :
	TIMEUNIT time_literal (SLASH time_literal)? ';'
	| TIMEPRECISION time_literal ';'
	| TIMEUNIT time_literal ';' TIMEPRECISION time_literal ';'
	| TIMEPRECISION time_literal ';' TIMEUNIT time_literal ';'
	;

//
// A.1.3 Module parameters and ports
//

parameter_port_list :
	'#' '(' list_of_param_assignments (',' parameter_port_decl)* ')'
	| '#' '(' parameter_port_decl (',' parameter_port_decl)* ')'
	| '#' '(' ')'
	;

parameter_port_decl : parameter_decl | local_parameter_decl
	| data_type list_of_param_assignments | TYPE list_of_type_assignments
	;

list_of_ports : '(' port (',' port)* ')' ;

list_of_port_decls : 
	'(' ( attribute_instance* ansi_port_decl
		(',' attribute_instance* ansi_port_decl)* )? ')' ;

port : port_expr? | '.' port_identifier '(' port_expr? ')' ;

port_expr : port_reference | '(' port_reference (',' port_reference)* ')' ;

port_reference : port_identifier constant_select ;

port_direction : INPUT | OUTPUT | INOUT | REF ;

net_port_header : port_direction? net_port_type ;

variable_port_header : port_direction? variable_port_type ;

interface_port_header : interface_identifier ('.' modport_identifier)?
	| INTERFACE ('.' modport_identifier)?
	;

// A further pass must validate net/interface port headers and unpacked dimensions
// vs variable port headers and variable dimensions.
// Also between interface identifiers and net type identifiers.
//
// I had to split up ansi_port_decl and hoist some definitions because there 
// were multiple ways the header alternatives could result in empty. Namely,
// implicit_data_type could be empty.

ansi_port_decl : ansi_port_decl_typed | ansi_port_decl_dotted_identifiers ;

ansi_port_decl_typed :
	port_direction? 
	ansi_port_decl_type
	port_identifier 
	variable_dimension* 
	(EQUALS constant_expr)?
	;

ansi_port_decl_type :
	net_type data_type
	| net_type implicit_data_type
	| VAR data_type
	| VAR implicit_data_type
	| identifier ('.' modport_identifier)?
	| INTERCONNECT implicit_data_type 
	| data_type 
	| implicit_data_type
	| INTERFACE ('.' modport_identifier)?
	;

ansi_port_decl_dotted_identifiers : 
	port_direction? '.' port_identifier '(' expr? ')' ;

//
// A.1.4 Module items
//

// TODO: more module common items
module_common_item :
	initial_construct
	| module_or_generate_item_declaration
	| continuous_assign
	| final_construct
	| always_construct
	;

// TODO: more
module_or_generate_item :
	attribute_instance* module_common_item
	;

module_or_generate_item_declaration :
	package_or_generate_item_decl
	| genvar_decl
	| clocking_decl
	| DEFAULT CLOCKING clocking_identifier ';'
	| DEFAULT DISABLE IFF expr_or_dist ';'
	;

// TODO: more nonport module items
non_port_module_item :
	generate_region
	| module_or_generate_item
	| module_decl
	| timeunits_decl
	;

//
// A.1.6 Interface items
//

// TODO: more
interface_or_generate_item :
	attribute_instance* module_common_item
	;

//
// A.1.8 Checker items
//

checker_or_generate_item :
	checker_or_generate_item_decl
	| initial_construct
	| always_construct
	| final_construct
	| assertion_item
	| continuous_assign
	| checker_generate_item
	;

// TODO: more
checker_or_generate_item_decl :
	RAND? data_decl
	| assertion_item_decl
	| genvar_decl
	| clocking_decl
	| DEFAULT CLOCKING clocking_identifier ';'
	| DEFAULT DISABLE IFF expr_or_dist ';'
	| ';'
	;

// TODO: more
checker_generate_item :
	generate_region
	;

//
// A.1.9 Class items
//

random_qualifier : RAND | RANDC ;

//
// A.1.10 Constraints
//

constraint_block : '{' constraint_block_item* '}' ;

constraint_block_item :
	SOLVE solve_before_list BEFORE solve_before_list ';'
	| constraint_expr
	;

solve_before_list : constraint_primary (',' constraint_primary)* ;

constraint_primary : 
	(implicit_class_handle '.' | class_scope)? hierarchical_identifier select ;

constraint_expr :
	SOFT? expr_or_dist ';'
	| uniqueness_constraint ';'
	| expr ARROW constraint_set
	| IF '(' expr ')' constraint_set (ELSE constraint_set)?
	| FOREACH '(' ps_or_hierarchical_array_identifier '[' loop_variables ']' ')'
		constraint_set
	| DISABLE SOFT constraint_primary ';'
	;

uniqueness_constraint : UNIQUE '{' open_range_list '}' ;

constraint_set : constraint_expr | '{' constraint_expr* '}' ;

dist_list : dist_item (',' dist_item)* ;

dist_item : value_range dist_weight? ;

dist_weight : (COLON_EQUAL | COLON_SLASH) expr ;

identifier_list : identifier (',' identifier)* ;

// 
// A.1.11 Package items
//

// TODO: add more
package_item :
	package_or_generate_item_decl
	| package_export_decl
	| timeunits_decl
	;

// TODO: add more
package_or_generate_item_decl :
	data_decl
	;

// 
// A.2 Declarations
//

//
// A.2.1 Declaration types
//

//
// A.2.1.1 Module parameter declarations
//

local_parameter_decl :
	LOCALPARAM data_type_or_implicit list_of_param_assignments
	| LOCALPARAM TYPE list_of_type_assignments
	;

parameter_decl :
	PARAMETER data_type_or_implicit list_of_param_assignments
	| PARAMETER TYPE list_of_type_assignments
	;

//
// A.2.1.3 Type declarations
//

data_decl :
	CONST? VAR? lifetime? data_type_or_implicit list_of_variable_decl_assignments ';'
	| type_decl
	| package_import_decl
	| net_type_decl
	;

package_import_decl : IMPORT package_import_item (',' package_import_item)* ';' ;

package_import_item : package_identifier '::' (identifier | STAR) ;

package_export_decl :
	EXPORT STAR '::' STAR ';'
	| EXPORT package_import_item (',' package_import_item)* ';'
	;

genvar_decl : GENVAR list_of_genvar_identifiers ';' ;

type_decl :
	TYPEDEF data_type type_identifier variable_dimension* ';'
	| TYPEDEF interface_instance_identifier 
		constant_bit_select '.' type_identifier type_identifier ';'
	| TYPEDEF (ENUM | STRUCT | UNION | CLASS | INTERFACE CLASS)? type_identifier ';'
	;

net_type_decl :
	NETTYPE data_type net_type_identifier
		(WITH (package_scope | class_scope)? tf_identifier)? ';'
	| NETTYPE (package_scope | class_scope)? net_type_identifier net_type_identifier ';'
	;

lifetime : STATIC | AUTOMATIC ;

//
// A.2.2 Declaration data types
//

//
// A.2.2.1 Net and variable types
//

casting_type : simple_type | constant_primary | signing | STRING | CONST ;

data_type :
	integer_vector_type signing? packed_dimension*
	| integer_atom_type signing?
	| non_integer_type
	| struct_union (PACKED signing?)? '{' struct_union_member+ '}' packed_dimension*
	| ENUM enum_base_type? '{' enum_name_declaration (',' enum_name_declaration)* '}'
		packed_dimension*
	| STRING
	| CHANDLE
	| VIRTUAL INTERFACE? interface_identifier parameter_value_assignment?
		('.' modport_identifier)?
	| (class_scope | package_scope)? type_identifier packed_dimension*
	| class_type
	| EVENT
	| ps_covergroup_identifier
	| type_reference
	;

data_type_or_implicit : data_type | implicit_data_type ;

implicit_data_type : signing? packed_dimension* ;

enum_base_type :
	integer_atom_type signing?
	| integer_vector_type signing? packed_dimension?
	| type_identifier packed_dimension?
	;

enum_name_declaration : enum_identifier
	( '[' integral_number (':' integral_number)? ']' )? (EQUALS constant_expr)?
	;

class_scope : class_type ':' ':' ;

class_type : ps_class_identifier parameter_value_assignment?
	( ':' ':' class_identifier parameter_value_assignment? )*
	;

integer_type : integer_vector_type | integer_atom_type ;

integer_atom_type : BYTE | SHORTINT | INT | LONGINT | INTEGER | TIME ;

integer_vector_type : BIT | LOGIC | REG ;

non_integer_type : SHORTREAL | REAL | REALTIME ;

net_type : SUPPLY0 | SUPPLY1 | TRI | TRIAND | TRIOR | TRIREG | TRI0 | TRI1
	| UWIRE | WIRE | WAND | WOR ;

net_port_type : 
	net_type? data_type_or_implicit 
	| net_type_identifier
	| INTERCONNECT implicit_data_type ;

variable_port_type : var_data_type ;

var_data_type : data_type | VAR data_type_or_implicit ;

signing : SIGNED | UNSIGNED ;

simple_type : 
	integer_type | non_integer_type | ps_type_identifier | ps_parameter_identifer ;

struct_union_member :
	attribute_instance* random_qualifier? data_type_or_void 
	list_of_variable_decl_assignments ;

data_type_or_void : data_type | VOID ;

struct_union : STRUCT | UNION TAGGED? ;

type_reference : TYPE '(' expr ')' | TYPE '(' data_type ')' ;

//
// A.2.2.3 Strengths
//

drive_strength :
	'(' strength0 ',' strength1 ')'
	| '(' strength1 ',' strength0 ')'
	| '(' strength0 ',' HIGHZ1 ')'
	| '(' strength1 ',' HIGHZ0 ')'
	| '(' HIGHZ0 ',' strength1 ')'
	| '(' HIGHZ1 ',' strength0 ')'
	;

strength0 : SUPPLY0 | STRONG0 | PULL0 | WEAK0 ;

strength1: SUPPLY1 | STRONG1 | PULL1 | WEAK1 ;

charge_strength : '(' (SMALL | MEDIUM | LARGE) ')' ;

//
// A.2.2.3 Delays
//

delay3 : 
	DELAY_ZERO 
	| '#' delay_value 
	| '#' '(' mintypmax_expr (',' mintypmax_expr (',' mintypmax_expr)?)? ;

delay2 :
	DELAY_ZERO
	| '#' delay_value
	| '#' '(' mintypmax_expr (',' mintypmax_expr)? ;

delay_value : 
	UNSIGNED_NUMBER | real_number | ps_identifier | time_literal | ONESTEP ;

//
// A.2.3 Declaration lists
//

list_of_genvar_identifiers : genvar_identifier (',' genvar_identifier)* ;

list_of_param_assignments : param_assignment (',' param_assignment)* ;

list_of_type_assignments : type_assignment (',' type_assignment)* ;

list_of_variable_decl_assignments : 
	variable_decl_assignment (',' variable_decl_assignment)* ;

//
// A.2.4 Declaration assignments
//

param_assignment : parameter_identifer unpacked_dimension* ('=' constant_param_expr)? ;

type_assignment : type_identifier (EQUALS data_type)? ;

variable_decl_assignment :
	variable_identifier variable_dimension* (EQUALS expr)?
	| dynamic_array_variable_identifier unsized_dimension variable_dimension*
		(EQUALS dynamic_array_new)?
	| class_variable_identifier (EQUALS class_new)?
	;

class_new : class_scope? NEW ('(' list_of_args ')') ;

dynamic_array_new : NEW '[' expr ']' ('(' expr ')')? ;

//
// A.2.5 Declaration ranges
//

unpacked_dimension : '[' constant_range ']' | '[' constant_expr ']' ;

packed_dimension : '[' constant_range ']' | unsized_dimension ;

associative_dimension : '[' data_type ']' | '[' STAR ']' ;

variable_dimension : unsized_dimension | unpacked_dimension
	| associative_dimension | queue_dimension ;

queue_dimension : '[' '$' (':' constant_expr)? ']' ;

unsized_dimension : '[' ']' ;

//
// A.2.8 Block item declarations
//

// TODO: add more
block_item_decl :
	attribute_instance* data_decl
	| attribute_instance* local_parameter_decl
	| attribute_instance* parameter_decl
	;

//
// A.2.10 Assertion declarations
//

concurrent_assertion_item :
	(block_identifier ':')? concurrent_assertion_statement
	| checker_instantiation
	;

concurrent_assertion_statement :
	assert_property_statement
	| assume_property_statement
	| cover_property_statement
	| cover_sequence_statement
	| restrict_property_statement
	;

assert_property_statement :
	ASSERT PROPERTY '(' property_spec ')' action_block ;

assume_property_statement :
	ASSUME PROPERTY '(' property_spec ')' action_block ;

cover_property_statement :
	COVER PROPERTY '(' property_spec ')' statement_or_null ;

expect_property_statement :
	EXPECT '(' property_spec ')' action_block ;

cover_sequence_statement :
	COVER SEQUENCE '(' clocking_event? (DISABLE IFF '(' expr_or_dist ')')?
		sequence_expr ')' statement_or_null ;

restrict_property_statement :
	RESTRICT PROPERTY '(' property_spec ')' ';' ;

property_actual_arg : property_expr | sequence_actual_arg ;

assertion_item_decl : property_decl | sequence_decl | let_decl ;

property_decl :
	PROPERTY property_identifier ('(' property_port_list? ')')? ';'
		assertion_variable_decl* property_spec ';'?
		ENDPROPERTY (':' property_identifier)? ;

property_port_list : property_port_item (',' property_port_item)* ;

property_port_item :
	attribute_instance* (LOCAL property_lvar_port_direction?)? property_formal_type
		formal_port_identifier variable_dimension* (EQUALS property_actual_arg)? ;

property_lvar_port_direction : INPUT ;

property_formal_type : sequence_formal_type | PROPERTY ;

property_spec : clocking_event? (DISABLE IFF '(' expr_or_dist ')')? property_expr ;

// TODO: add more
property_expr :
	sequence_expr
	;

sequence_decl :
	SEQUENCE sequence_identifier ('(' sequence_port_list? ')')? ';'
		assertion_variable_decl* sequence_expr ';'?
		ENDSEQUENCE (':' sequence_identifier)? ;

sequence_port_list : sequence_port_item (',' sequence_port_item)* ;

sequence_port_item :
	attribute_instance* (LOCAL sequence_lvar_port_direction?)? sequence_formal_type
		formal_port_identifier variable_dimension* (EQUALS sequence_actual_arg)? ;

sequence_lvar_port_direction : INPUT | INOUT | OUTPUT ;

sequence_formal_type : data_type_or_implicit | SEQUENCE | UNTYPED ;

// TODO: add more sequence_exprs
sequence_expr :
	expr_or_dist boolean_abbrev?
	| sequence_expr LIT_AND sequence_expr
	| sequence_expr INTERSECT sequence_expr
	| sequence_expr LIT_OR sequence_expr
	| expr_or_dist THROUGHOUT sequence_expr
	| sequence_expr WITHIN sequence_expr
	;

sequence_method_call : sequence_instance '.' method_identifier ;

sequence_instance : 
	ps_or_hierarchical_sequence_identifier ('(' sequence_list_of_args ')')? ;

sequence_list_of_args :
	sequence_actual_arg? (',' sequence_actual_arg)*
		(',' '.' identifier '(' sequence_actual_arg ')')*
	| '.' identifier '(' sequence_actual_arg ')'
		(',' '.' identifier '(' sequence_actual_arg ')')*
	;

sequence_actual_arg : event_expr | sequence_expr ;

boolean_abbrev : consecutive_repetition | non_consecutive_repetition | goto_repetition ;

sequence_abbrev : consecutive_repetition ;

consecutive_repetition :
	'[' STAR const_or_range_expr ']' | '[' STAR ']' | '[' PLUS ']' ;

non_consecutive_repetition : '[' EQUALS const_or_range_expr ']' ;

goto_repetition : '[' ARROW const_or_range_expr ']' ;

const_or_range_expr : constant_expr | cycle_delay_const_range_expr ;

cycle_delay_const_range_expr : constant_expr ':' ('$' | constant_expr) ;

expr_or_dist : expr (DIST '{' dist_list '}')? ;

assertion_variable_decl : var_data_type list_of_variable_decl_assignments ';' ;

let_decl : LET let_identifier ('(' let_port_list? ')')? EQUALS expr ';' ;

let_port_list : let_port_item (',' let_port_item)* ;

let_port_item :
	attribute_instance* let_formal_type formal_port_identifier
		variable_dimension* (EQUALS expr)? ;

let_formal_type : data_type_or_implicit | UNTYPED ;

let_expr : package_scope? let_identifier ('(' let_list_of_args ')')? ;

let_list_of_args : let_actual_arg? (',' let_actual_arg)*
	(',' '.' identifier '(' let_actual_arg ')')?
	| '.' identifier '(' let_actual_arg ')' 
	(',' '.' identifier '(' let_actual_arg ')')? ;

let_actual_arg : expr ;

//
// A.4.1 Instantiation
//

//
// A.4.1.1 Module instantiation
//

parameter_value_assignment : '#' '(' list_of_param_assignments? ')' ;

name_of_instance : instance_identifier unpacked_dimension* ;

//
// A.4.1.4 Checker instantiation
//

checker_instantiation :
	ps_checker_identifier name_of_instance 
	'(' list_of_checker_port_connections ')' ';'
	;

list_of_checker_port_connections :
	ordered_checker_port_connection (',' ordered_checker_port_connection)*
	| named_checker_port_connection (',' named_checker_port_connection)*
	;

ordered_checker_port_connection : attribute_instance* property_actual_arg? ;

named_checker_port_connection :
	attribute_instance* '.' formal_port_identifier ('(' property_actual_arg? ')')?
	| attribute_instance* DOT_STAR
	;

//
// A.4.2 Generated instantiation
//

generate_region : GENERATE generate_item* ENDGENERATE ;

generate_item :
	module_or_generate_item
	| interface_or_generate_item
	| checker_or_generate_item
	;

//
// A.6 Behavioral statements
//

//
// A.6.1 Continuous assignment and net alias statements
//

continuous_assign :
	ASSIGN drive_strength? delay3? list_of_net_assignments ';'
	| ASSIGN delay_control? list_of_variable_assignments ';'
	;

list_of_net_assignments : net_assignment (',' net_assignment)* ;

list_of_variable_assignments : variable_assignment (',' variable_assignment)* ;

net_assignment : net_lvalue EQUALS expr ;

//
// A.6.2 Procedural blocks and assignments
//

initial_construct : INITIAL statement_or_null ;

always_construct : always_keyword statement ;

always_keyword : ALWAYS | ALWAYS_COMB | ALWAYS_LATCH | ALWAYS_FF ;

final_construct : FINAL function_statement ;

blocking_assignment :
	variable_lvalue EQUALS delay_or_event_control expr
	| nonrange_variable_lvalue EQUALS dynamic_array_new
	| (implicit_class_handle '.' | class_scope | package_scope)?
		hierarchical_variable_identifier select EQUALS class_new
	| operator_assignment
	;

operator_assignment : variable_lvalue assignment_operator expr ;

assignment_operator :
	EQUALS | PLUSEQ | MINUSEQ | MULTEQ | DIVEQ | MODEQ | ANDEQ | OREQ | XOREQ
	| LSLEQ | LSREQ | ASLEQ | ASREQ ;

nonblocking_assignment : variable_lvalue LTE delay_or_event_control? expr ;

procedural_continuous_assignment :
	ASSIGN variable_assignment
	| DEASSIGN variable_lvalue
	| FORCE variable_assignment
	| FORCE net_assignment
	| RELEASE variable_lvalue
	| RELEASE net_lvalue
	;

variable_assignment : variable_lvalue EQUALS expr ;

//
// A.6.3 Parallel and sequential blocks
//

action_block : statement_or_null | statement? ELSE statement_or_null ;

seq_block : 
	BEGIN (':' block_identifier)? block_item_decl* statement_or_null*
	END (':' block_identifier)?
	;

par_block : 
	FORK (':' block_identifier)? block_item_decl* statement_or_null*
	join_keyword (':' block_identifier)?
	;

join_keyword : JOIN | JOIN_ANY | JOIN_NONE ;

//
// A.6.4 Statements
//

statement_or_null : statement | attribute_instance* ';' ;

statement : (block_identifier ':')? attribute_instance* statement_item ;

// TODO: more statements
statement_item :
	blocking_assignment ';'
	| nonblocking_assignment ';'
	| procedural_continuous_assignment ';'
	| case_statement
	| conditional_statement
	| inc_or_dec_expr ';'
	| subroutine_call_statement
	| disable_statement
	| event_trigger
	| loop_statement
	| jump_statement
	| par_block
	| procedural_timing_control_statement
	| seq_block
	| wait_statement
	| procedural_assertion_statement
	| clocking_drive ';'
	| expect_property_statement
	;

function_statement : statement ;

function_statement_or_null : function_statement | attribute_instance* ';' ;

variable_identifier_list : variable_identifier (',' variable_identifier)* ;

//
// A.6.5 Timing control statements
//

procedural_timing_control_statement :
	procedural_timing_control statement_or_null ;

delay_or_event_control : 
	delay_control | event_control | REPEAT '(' expr ')' event_control ;

delay_control : DELAY_ZERO | '#' delay_value | '#' '(' mintypmax_expr ')' ;

event_control :
	'@' hierarchical_event_identifier
	| '@' '(' event_expr ')'
	| '@' STAR
	| '@' '(' STAR ')'
	| '@' ps_or_hierarchical_sequence_identifier
	;

event_expr :
	edge_identifier? expr (IFF expr)?
	| sequence_instance (IFF expr)?
	| event_expr LIT_OR event_expr
	| event_expr ',' event_expr
	| '(' event_expr ')'
	;

procedural_timing_control : delay_control | event_control | cycle_delay ;

jump_statement :
	RETURN expr? ';'
	| BREAK ';'
	| CONTINUE ';'
	;

wait_statement :
	WAIT '(' expr ')' statement_or_null
	| WAIT FORK ';'
	| WAIT_ORDER '(' hierarchical_identifier (',' hierarchical_identifier)*
		')' action_block
	;

event_trigger :
	ARROW hierarchical_event_identifier ';'
	| BIGARROW delay_or_event_control? hierarchical_event_identifier ';'
	;

disable_statement :
	// hierarchical task or block identifier
	DISABLE hierarchical_identifier ';'
	| DISABLE FORK ';'
	;

//
// A.6.6 Conditional statements
//

conditional_statement :
	unique_priority? IF '(' cond_predicate ')' statement_or_null
	(ELSE IF '(' cond_predicate ')' statement_or_null)*
	(ELSE statement_or_null)?
	;

unique_priority : UNIQUE | UNIQUE0 | PRIORITY ;

cond_predicate : expr_or_cond_pattern (TRIPLE_AND expr_or_cond_pattern)* ;

expr_or_cond_pattern : expr | cond_pattern ;

cond_pattern : expr MATCHES pattern ;

//
// A.6.7 Case statements
//

case_statement : 
	unique_priority? case_keyword '(' case_expr ')' case_item+ ENDCASE
	| unique_priority? case_keyword '(' case_expr ')' 
		MATCHES case_pattern_item+ ENDCASE
	| unique_priority? CASE '(' case_expr ')' 
		INSIDE case_inside_item+ ENDCASE
	;

case_keyword : CASE | CASEZ | CASEX ;

case_expr : expr ;

case_item : 
	case_item_expr (',' case_item_expr)* ':' statement_or_null
	| DEFAULT ':' statement_or_null
	;

case_pattern_item :
	pattern (TRIPLE_AND expr)? ':' statement_or_null
	| DEFAULT ':' statement_or_null
	;

case_inside_item :
	open_range_list ':' statement_or_null
	| DEFAULT ':' statement_or_null
	;

case_item_expr : expr ;

open_range_list : open_value_range (',' open_value_range)* ;

open_value_range : value_range ;

//
// A.6.7.1 Patterns
//

pattern :
	'.' variable_identifier
	| '.' STAR
	| constant_expr
	| TAGGED member_identifier pattern?
	| '\'{' pattern (',' pattern)* '}'
	| '\'{' member_identifier ':' pattern (',' member_identifier ':' pattern)* '}'
	;

// A further pass has to validate and distinguish pattern keys.
assignment_pattern :
	'\'{' expr (',' expr)* '}'
	| '\'{' pattern_key ':' expr (',' pattern_key ':' expr) '}'
	| '\'{' constant_expr '{' expr (',' expr)* '}' '}'
	;

pattern_key : member_identifier | constant_expr | assignment_pattern_key ;

assignment_pattern_key : simple_type | DEFAULT ;

// A further pass has to distinguish the identifier
assignment_pattern_expr_type : identifier | integer_atom_type | type_reference ;

assignment_pattern_expr : assignment_pattern_expr_type? assignment_pattern ;

constant_assignment_pattern_expr : assignment_pattern_expr ;

assignment_pattern_net_lvalue :
	'\'{' net_lvalue (',' net_lvalue)* '}' ;

assignment_pattern_variable_lvalue :
	'\'{' variable_lvalue (',' variable_lvalue)* '}' ;

//
// A.6.8 Looping statements
//

loop_statement :
	FOREVER statement_or_null
	| REPEAT '(' expr ')' statement_or_null
	| WHILE '(' expr ')' statement_or_null
	| FOR '(' for_initialization? ';' expr? ';' for_step? ')' statement_or_null
	| DO statement_or_null WHILE '(' expr ')' ';'
	| FOREACH '(' ps_or_hierarchical_array_identifier '[' loop_variables ']' ')'
		statement
	;

for_initialization :
	list_of_variable_assignments
	| for_variable_decl (',' for_variable_decl)*
	;

for_variable_decl :
	VAR? data_type variable_identifier EQUALS expr 
	(',' variable_identifier EQUALS expr)*
	;

for_step : for_step_assignment (',' for_step_assignment)* ;

for_step_assignment : 
	operator_assignment | inc_or_dec_expr | function_subroutine_call ;

loop_variables : index_variable_identifier? (',' index_variable_identifier?)* ;

//
// A.6.9 Subroutine call statements
//

subroutine_call_statement :
	subroutine_call ';'
	| VOID '\'' '(' function_subroutine_call ')' ';'
	;

//
// A.6.10 Assertion statements
//

assertion_item :
	concurrent_assertion_item | deferred_immediate_assertion_item ;

deferred_immediate_assertion_item : 
	(block_identifier ':')? deferred_immediate_assertion_statement ;

procedural_assertion_statement :
	concurrent_assertion_statement
	| immediate_assertion_statement
	| checker_instantiation
	;

immediate_assertion_statement :
	simple_immediate_assertion_statement
	| deferred_immediate_assertion_statement
	;

simple_immediate_assertion_statement :
	simple_immediate_assert_statement
	| simple_immediate_assume_statement
	| simple_immediate_cover_statement
	;

simple_immediate_assert_statement : ASSERT '(' expr ')' action_block ;

simple_immediate_assume_statement : ASSUME '(' expr ')' action_block ;

simple_immediate_cover_statement : COVER '(' expr ')' statement_or_null ;

deferred_immediate_assertion_statement :
	deferred_immediate_assert_statement
	| deferred_immediate_assume_statement
	| deferred_immediate_cover_statement
	;

deferred_immediate_assert_statement :
	ASSERT DELAY_ZERO '(' expr ')' action_block
	| ASSERT FINAL '(' expr ')' action_block
	;

deferred_immediate_assume_statement :
	ASSUME DELAY_ZERO '(' expr ')' action_block
	| ASSUME FINAL '(' expr ')' action_block
	;
	
deferred_immediate_cover_statement :
	COVER DELAY_ZERO '(' expr ')' statement_or_null
	| COVER FINAL '(' expr ')' statement_or_null
	;

//
// A.6.11 Clocking block
//

clocking_decl :
	DEFAULT? CLOCKING clocking_identifier? clocking_event ';'
		clocking_item* ENDCLOCKING (':' clocking_identifier)?
	| GLOBAL CLOCKING clocking_identifier? clocking_event ';'
		ENDCLOCKING (':' clocking_identifier)?
	;

clocking_event : '@' identifier | '@' '(' event_expr ')' ;

clocking_item :
	DEFAULT default_skew ';'
	| clocking_direction list_of_clocking_decl_assign ';'
	| attribute_instance* assertion_item_decl
	;

default_skew :
	INPUT clocking_skew 
	| OUTPUT clocking_skew
	| INPUT clocking_skew OUTPUT clocking_skew
	;

clocking_direction :
	INPUT clocking_skew?
	| OUTPUT clocking_skew?
	| INPUT clocking_skew? OUTPUT clocking_skew?
	| INOUT
	;

list_of_clocking_decl_assign : clocking_decl_assign (',' clocking_decl_assign)* ;

clocking_decl_assign : signal_identifier (EQUALS expr)? ;

clocking_skew :
	edge_identifier delay_control?
	| delay_control
	;

clocking_drive : clockvar_expr LTE cycle_delay? expr ;

cycle_delay :
	'##' integral_number
	| '##' identifier
	| '##' '(' expr ')'
	;

clockvar : hierarchical_identifier ;

clockvar_expr : clockvar select ;

//
// A.7.4 Specify path delays
//

edge_identifier : POSEDGE | NEGEDGE | EDGE ;

//
// A.8 Expressions
//

//
// A.8.1 Concatenations
//

concatenation : '{' expr (',' expr)* '}' ;

constant_concatenation : '{' constant_expr (',' constant_expr)* '}' ;

multiple_concatenation : '{' expr concatenation '}' ;

streaming_concatenation : '{' stream_operator slice_size? stream_concatenation '}' ;

stream_operator : LSL | LSR ;

slice_size : simple_type | constant_expr ;

stream_concatenation : '{' stream_expr (',' stream_expr)* '}' ;

stream_expr : expr (WITH '[' array_range_expr ']')? ;

array_range_expr : expr ( (':' | PLUSCOLON | MINUSCOLON) expr)? ;

empty_queue : '{' '}' ;

//
// A.8.2 Subroutine calls
//

tf_call : ps_or_hierarchical_tf_identifier attribute_instance* ('(' list_of_args ')')? ;

system_tf_call : system_tf_identifier ('(' list_of_args ')')? ;

list_of_args : 
	expr? (',' expr)* (',' '.' identifier '(' expr? ')')*
	| '.' identifier '(' expr? ')' (',' '.' identifier '(' expr? ')')*
	;

subroutine_call : tf_call | system_tf_call | method_call 
	| STD_COLON_COLON randomize_call ;

function_subroutine_call : subroutine_call ;

method_call : method_call_root '.' method_call_body ;

method_call_body : method_identifier attribute_instance* ('(' list_of_args ')')?
	|	built_in_method_call
	;

built_in_method_call : array_manipulation_call | randomize_call ;

array_manipulation_call : 
	array_method_name attribute_instance* ('(' list_of_args ')')? (WITH '(' expr ')')? ;

randomize_call : 
	RANDOMIZE attribute_instance*
	('(' (variable_identifier_list | NULL)? ')')?
	(WITH ('(' identifier_list ')')? constraint_block)? ;

method_call_root : primary | implicit_class_handle ;

array_method_name : method_identifier | UNIQUE | LIT_AND | LIT_OR | LIT_XOR ;

//
// A.8.3 Expressions
//

inc_or_dec_expr :
	inc_or_dec_operator attribute_instance* variable_lvalue
	| variable_lvalue attribute_instance* inc_or_dec_operator
	;

conditional_expression : cond_predicate '?' attribute_instance* expr ':' expr ;

constant_expr :
	constant_primary
	| unary_operator attribute_instance* constant_primary
	| constant_expr binary_operator attribute_instance* constant_expr
	| constant_expr 
		'?' attribute_instance* constant_expr 
		':' constant_expr
	;

constant_mintypmax_expr : constant_expr (':' constant_expr ':' constant_expr)? ;

constant_param_expr : constant_mintypmax_expr | data_type | '$' ;

constant_range_expr : constant_expr | constant_part_select_range ;

constant_part_select_range : constant_range | constant_indexed_range ;

constant_range : constant_expr ':' constant_expr ;

constant_indexed_range : constant_expr (PLUSCOLON | MINUSCOLON) constant_expr ;

// TODO: add more exprs

expr :
	primary
	| unary_operator attribute_instance* primary
	| inc_or_dec_expr
	| '(' operator_assignment ')'
	| expr binary_operator attribute_instance* expr
	// Expand conditional_expression
		// Expand cond_predicate
			// Expand expr_or_cond_pattern to expose left-recursion
			| expr (TRIPLE_AND expr_or_cond_pattern)* 
				'?' attribute_instance* expr ':' expr
			| expr MATCHES pattern (TRIPLE_AND expr_or_cond_pattern)* 
				'?' attribute_instance* expr ':' expr
	;

value_range : expr | '[' expr ':' expr ']' ;

mintypmax_expr : expr ':' expr ':' expr | expr ;

part_select_range : constant_range | indexed_range ;

indexed_range : expr (PLUSCOLON | MINUSCOLON) constant_expr ;

//
// A.8.4 Primaries
//

// A later pass will have to distinguish what each identifier is.
constant_primary : 
	constant_primary2
//	| primary '.' method_call_body
	// Expanding mutual left-recursion with primary
		| primary2 ('.' method_call_body)+
		| constant_primary '\'' '(' expr')'  ('.' method_call_body)+
	| constant_primary '\'' '(' constant_expr ')'
	;

primary : 
	primary2
	//| primary '.' method_call_body
	//| constant_primary '\'' '(' expr ')'
	// Expanding mutual left-recursion with constant_primary
		| primary '.' method_call_body 
			('\'' '(' constant_expr ')')* '\'' '(' expr ')'
		| constant_primary2 
			('\'' '(' constant_expr ')')* '\'' '(' expr ')'
	;

constant_primary2 :
	primary_literal
	| identifier constant_select
	| identifier ('[' constant_range_expr ']')?
	| (package_scope | class_scope)? enum_identifier
	| constant_concatenation ('[' constant_range_expr ']')?
	// Expand subroutine_call
		| tf_call 
		| system_tf_call 
		// Expand method_call
			// Expand method_call_root, move primary's left-recursion
				| implicit_class_handle '.' method_call_body
		| STD_COLON_COLON randomize_call
	| constant_let_expr
	| '(' constant_mintypmax_expr ')'
	// Expand constant_cast
		// Expand casting_type, move left-recursion
			| (simple_type | signing | STRING | CONST)
				'\'' '(' constant_expr ')'
	| constant_assignment_pattern_expr
	| type_reference
	;

primary2 :
	(class_qualifier_nonempty | package_scope)? hierarchical_identifier select
	| primary_literal 
	| empty_queue
	| concatenation ('[' range_expr ']')?
	| multiple_concatenation ('[' range_expr ']')?
	// Expand subroutine_call
		| tf_call 
		| system_tf_call 
		// Expand method_call
			// Expand method_call_root, move left-recursion
				| implicit_class_handle '.' method_call_body
		| STD_COLON_COLON randomize_call
	| let_expr
	| '(' mintypmax_expr ')'
	// Expand cast
		// Expand casting_type, move constant_primary left-recursion
			| (simple_type | signing | STRING | CONST) '\'' '(' expr ')'
	| assignment_pattern_expr
	| streaming_concatenation
	| sequence_method_call
	| THIS
	| '$'
	| NULL
	;


class_qualifier : LOCAL_COLON_COLON? (implicit_class_handle '.' | class_scope)? ;

class_qualifier_nonempty : 
	LOCAL_COLON_COLON (implicit_class_handle '.' | class_scope)?
	| implicit_class_handle '.' 
	| class_scope
	;

range_expr : expr | part_select_range ;

primary_literal : number | time_literal | unbased_unsized_literal | string_literal ;

time_literal : unsigned_number time_unit | fixed_point_number time_unit ;

time_unit : SECOND | MILLISECOND | MICROSECOND | NANOSECOND | PICOSECOND | FEMTOSECOND ;

implicit_class_handle : THIS | SUPER | THIS '.' SUPER ;

bit_select : ('[' expr ']')* ;

select :
	( ('.' member_identifier bit_select)* '.' member_identifier)? bit_select
	('[' part_select_range ']')? ;

nonrange_select :
	( ('.' member_identifier bit_select)* '.' member_identifier)? bit_select
	;

constant_bit_select : ('[' constant_expr ']')* ;

constant_select :
	( ('.' member_identifier constant_bit_select)* member_identifier)? constant_bit_select
	('[' constant_part_select_range ']')? ;

constant_cast : casting_type '\'' '(' constant_expr ')' ;

constant_let_expr : let_expr ;

cast : casting_type '\'' '(' expr ')' ;

//
// A.8.5 Expression left-side values
//

net_lvalue :
	ps_or_hierarchical_net_identifier constant_select
	| '{' net_lvalue (',' net_lvalue)* '}'
	| assignment_pattern_expr_type? assignment_pattern_net_lvalue
	;

variable_lvalue :
	(implicit_class_handle '.' | package_scope)?
	hierarchical_variable_identifier select
	| '{' variable_lvalue (',' variable_lvalue)* '}'
	| assignment_pattern_expr_type? assignment_pattern_variable_lvalue
	| stream_concatenation
	;

nonrange_variable_lvalue :
	(implicit_class_handle '.' | package_scope)?
	hierarchical_variable_identifier nonrange_select
	;

//
// A.8.6 Operators. Also see table 11-1 for their meanings.
//

unary_operator : PLUS | MINUS | NOT | TILDE | AND | NAND | OR | NOR | XOR | NXOR ;

binary_operator : PLUS | MINUS | STAR | SLASH | PERCENT | EQUALITY | INEQUALITY
	| CASE_EQUALITY | CASE_INEQUALITY | WILDCARD_EQUALITY | WILDCARD_INEQUALITY
	| LOGICAL_AND | LOGICAL_OR | POWER | LT | LTE | GT | GTE | AND | OR | XOR
	| NXOR | LSR | LSL | ASR | ASL | ARROW | EQUIVALENCE ;

inc_or_dec_operator : INC | DEC ;

unary_module_path_operator : NOT | TILDE | AND | NAND | OR | NOR | XOR | NXOR ;

binary_module_path_operator : EQUALITY | INEQUALITY | LOGICAL_AND | LOGICAL_OR
	| AND | OR | XOR | NXOR ;

//
// A.8.7 Numbers
//

// Section 5.7.1: Integer literal constants.
// These are either a bare decimal number, or an optional size followed by a 
// base spec, followed by a number in that base. Underscores can be used
// as visual separators.
// Examples: 123 1'b0 'b1000_0101 'sb110 (a signed binary number)
// x = unknown digit, z or ? = high-impedance digit
// Spaces may separate the base spec from the other parts, for example:
// -3 'b 010 is a 2's complement size-3 binary number.
// -3 'sb 010 is the same, except it is treated as signed everywhere in the .sv file.
number : integral_number | real_number ;

integral_number : octal_number 
	| binary_number 
	| hex_number
	| decimal_number
	;

octal_number : OCTAL_NUMBER ;
binary_number : BINARY_NUMBER ;
hex_number : HEX_NUMBER ;
decimal_number : DECIMAL_NUMBER ;
real_number : (PLUS | MINUS)? (FIXED_POINT_NUMBER | SCI_NUMBER);
unsigned_number : UNSIGNED_NUMBER ;
fixed_point_number : FIXED_POINT_NUMBER ;

unbased_unsized_literal : UNBASED_ZERO | UNBASED_ONE | UNBASED_Z_OR_X ;

//
// A.8.8 Strings
//

string_literal : STRING_LITERAL ;

//
// A.9.1 Attributes
//

attribute_instance : OPEN_STAR attr_spec (',' attr_spec)* CLOSE_STAR ;

attr_spec : attr_name (EQUALS constant_expr)? ;

attr_name : identifier ;

//
// A.9.3 Identifiers
//

identifier : SIMPLE_IDENTIFIER | ESCAPED_IDENTIFIER ;

array_identifier : identifier ;
block_identifier : identifier ;
bin_identifier : identifier ;
cell_identifier : identifier ;
checker_identifier : identifier ;
class_identifier : identifier ;
class_variable_identifier : identifier ;
clocking_identifier : identifier ;
covergroup_identifier : identifier ;
dynamic_array_variable_identifier : variable_identifier ;
enum_identifier : identifier ;
formal_port_identifier : identifier ;
generate_block_identifier : identifier ;
genvar_identifier : identifier ;
hierarchical_identifier : 
	(DOLLAR_ROOT '.')? (identifier constant_bit_select '.')* identifier ;
hierarchical_array_identifier : hierarchical_identifier ;
hierarchical_event_identifier : hierarchical_identifier ;
hierarchical_net_identifier : hierarchical_identifier ;
hierarchical_sequence_identifier : hierarchical_identifier ;
hierarchical_tf_identifier : hierarchical_identifier ;
hierarchical_variable_identifier : hierarchical_identifier ;
index_variable_identifier : identifier ;
instance_identifier : identifier ;
interface_identifier : identifier ;
interface_instance_identifier : identifier ;
let_identifier : identifier ;
member_identifier : identifier ;
method_identifier : identifier ;
modport_identifier : identifier ;
module_identifier : identifier ;
net_identifier : identifier ;
net_type_identifier : identifier ;
package_scope : package_identifier '::' | DOLLAR_UNIT '::' ;
package_identifier : identifier ;
parameter_identifer : identifier ;
port_identifier : identifier ;
property_identifier : identifier ;
ps_checker_identifier : package_scope? checker_identifier ;
ps_class_identifier : package_scope? class_identifier ;
ps_covergroup_identifier : package_scope? covergroup_identifier ;
ps_identifier : package_scope? identifier ;
ps_or_hierarchical_array_identifier :
	(implicit_class_handle '.' | class_scope | package_scope)? 
	hierarchical_array_identifier ;
ps_or_hierarchical_net_identifier :
	package_scope? net_identifier | hierarchical_net_identifier ;
ps_or_hierarchical_sequence_identifier : 
	package_scope? sequence_identifier | hierarchical_sequence_identifier ;
ps_or_hierarchical_tf_identifier : 
	package_scope? tf_identifier | hierarchical_tf_identifier ;
ps_parameter_identifer : 
	(package_scope | class_scope)? parameter_identifer
	| (generate_block_identifier ('[' constant_expr ']')? '.' )* parameter_identifer
	;
ps_type_identifier : (LOCAL_COLON_COLON | package_scope)? type_identifier ;
sequence_identifier : identifier ;
signal_identifier : identifier ;
specparam_identifier : identifier ;
system_tf_identifier : SYSTEM_TF_IDENTIFIER ;
tf_identifier : identifier ;
type_identifier : identifier ;
variable_identifier : identifier ;


// Lexer rules must be unambiguous (within any given mode)

//
// Section 22: Compiler directives
//

// These are, strictly speaking, not part of the grammar. They can generally appear
// anywhere in the source, so we're going to treat them as lexer tokens that
// get skipped (but processed by the compiler).

INCLUDE_COMPILER_DIRECTIVE :
	('`include' [ \t\n]* '"' .*? '"'
	| '`include' [ \t\n]* '<' .*? '>') -> skip ;

OCTAL_NUMBER : SIGN? NON_ZERO_UNSIGNED_NUMBER? OCTAL_BASE OCTAL_VALUE ;
BINARY_NUMBER : SIGN? NON_ZERO_UNSIGNED_NUMBER? BINARY_BASE BINARY_VALUE ;
HEX_NUMBER : SIGN? NON_ZERO_UNSIGNED_NUMBER? HEX_BASE HEX_VALUE ;
DECIMAL_NUMBER : SIGN? NON_ZERO_UNSIGNED_NUMBER? DECIMAL_BASE DECIMAL_VALUE 
	| SIGN? UNSIGNED_NUMBER ;
FIXED_POINT_NUMBER : UNSIGNED_NUMBER '.' UNSIGNED_NUMBER ;
SCI_NUMBER : UNSIGNED_NUMBER ('.' UNSIGNED_NUMBER)? [eE] SIGN? UNSIGNED_NUMBER ;
UNSIGNED_NUMBER : DECIMAL_DIGIT ('_' | DECIMAL_DIGIT)* ;

STRING_LITERAL : '"' (ESC|.)*? '"' ;

// four-symbol groups

ASLEQ : '<<<=' ;
ASREQ : '>>>=' ;

// three-symbol groups

ASL : '<<<' ;
ASR : '>>>' ;
BIGARROW : '->>' ;
CASE_EQUALITY : '===' ;
CASE_INEQUALITY : '!==' ;
EQUIVALENCE : '<->' ;
LSLEQ : '<<=' ;
LSREQ : '>>=' ;
TRIPLE_AND : '&&&' ;
WILDCARD_EQUALITY : '==?' ;
WILDCARD_INEQUALITY : '!=?' ;

// two-symbol groups

ANDEQ : '&=' ;
CLOSE_STAR : '*)' ;
COLON_EQUAL : ':=' ;
COLON_SLASH : ':/' ;
COND : '?:' ;
DEC : '--' ;
DELAY_ZERO : '#0' ; // Must include with any other '#' unsigned_integer!
DIVEQ : '/=' ;
DOT_STAR : '.*' ;
EQUALITY : '==' ;
ARROW : '->' ;
INC : '++' ;
INEQUALITY : '!=' ;
GTE : '>=' ;
LOGICAL_AND : '&&' ;
LOGICAL_OR : '||' ;
LSL : '<<' ;
LSR : '>>' ;
LTE : '<=' ;
MINUSCOLON : '-:' ;
MINUSEQ : '-=' ;
MODEQ : '%=' ;
MULTEQ : '*=' ;
NAND : '~&' ;
NOR : '~|' ;
NXOR : '~^' | '^~' ;
OPEN_STAR : '(*' ;
OREQ : '|=' ;
PLUSCOLON : '+:' ;
PLUSEQ : '+=' ;
POWER : '**' ;
UNBASED_ONE : '\'1' ;
UNBASED_ZERO : '\'0' ;
UNBASED_Z_OR_X : '\'' [xXzZ] ;
XOREQ : '^=' ;

// one-symbol groups

AND : '&' ;
EQUALS : '=' ;
GT : '>' ;
LT : '<' ;
NOT : '!' ;
OR : '|' ;
PERCENT : '%' ;
PLUS : '+' ;
MINUS : '-' ;
SLASH : '/' ;
STAR : '*' ;
TILDE : '~' ;
XOR : '^' ;

// Reserved keywords

DOLLAR_ROOT : '$root' ;
DOLLAR_UNIT : '$unit' ;
ONESTEP : '1step' ;
ALWAYS_COMB : 'always_comb' ;
ALWAYS_FF : 'always_ff' ;
ALWAYS_LATCH : 'always_latch' ;
ALWAYS : 'always' ; // Last so keywords prefixed with 'always' lex first
ASSERT : 'assert' ;
ASSUME : 'assume' ;
LIT_AND : 'and' ;
ASSIGN : 'assign' ;
AUTOMATIC : 'automatic' ;
BEFORE : 'before' ;
BEGIN : 'begin' ;
BIT : 'bit' ;
BREAK : 'break' ;
BYTE : 'byte' ;
CASEX : 'casex' ;
CASEZ : 'casez' ;
CASE : 'case' ; // Last so keywords prefixed with 'case' lex first
CHANDLE : 'chandle' ;
CLASS : 'class' ;
CLOCKING : 'clocking' ;
CONST : 'const' ;
CONTINUE : 'continue' ;
COVER : 'cover' ;
DEASSIGN : 'deassign' ;
DEFAULT : 'default' ;
DISABLE : 'disable' ;
DIST : 'dist' ;
DO : 'do' ;
EDGE : 'edge' ;
ELSE : 'else' ;
ENDCASE : 'endcase' ;
ENDCLOCKING : 'endclocking' ;
ENDGENERATE : 'endgenerate' ;
ENDMODULE : 'endmodule' ;
ENDPACKAGE : 'endpackage' ;
ENDPROPERTY : 'endproperty' ;
ENDSEQUENCE : 'endsequence' ;
END : 'end' ; // Last so keywords prefixed with 'end' lex first
ENUM : 'enum' ;
EVENT : 'event' ;
EXPECT : 'expect' ;
EXPORT : 'export' ;
EXTERN : 'extern' ;
FEMTOSECOND : 'fs' ;
FINAL : 'final' ;
FORCE : 'force' ;
FOREACH : 'foreach' ;
FOREVER : 'forever' ;
FORK : 'fork' ;
FOR : 'for' ; // Last so keywords prefixed with 'for' lex first
GENERATE : 'generate' ;
GENVAR : 'genvar' ;
GLOBAL : 'global' ;
HIGHZ0 : 'highz0' ;
HIGHZ1 : 'highz1' ;
IFF : 'iff' ;
IF : 'if' ; // Last so keywords prefixed with 'if' lex first
IMPORT : 'import' ;
INITIAL : 'initial' ;
INOUT : 'inout' ;
INPUT : 'input' ;
INSIDE : 'inside' ;
INTEGER : 'integer' ;
INTERCONNECT : 'interconnect' ;
INTERFACE : 'interface' ;
INTERSECT : 'intersect' ;
INT : 'int' ; // Last so keywords prefixed with 'int' lex first
JOIN_ANY : 'join_any' ;
JOIN_NONE : 'join_none' ;
JOIN : 'join' ; // Last so keywords prefixed with 'join' lex first
LARGE : 'large' ;
LET : 'let' ;
LOCAL_COLON_COLON : 'local::' ;
LOCALPARAM : 'localparam' ;
LOCAL : 'local' ; // Last so keywords prefixed with 'local' lex first
LOGIC : 'logic' ;
LONGINT : 'longint' ;
MACROMODULE : 'macromodule' ;
MATCHES : 'matches' ;
MEDIUM : 'medium' ;
MICROSECOND : 'us' ;
MILLISECOND : 'ms' ;
MODULE : 'module' ;
NANOSECOND : 'ns' ;
NEGEDGE : 'negedge' ;
NETTYPE : 'nettype' ;
NEW : 'new' ;
NULL : 'null' ;
LIT_OR : 'or' ;
OUTPUT : 'output' ;
PACKAGE : 'package' ;
PACKED : 'packed' ;
PARAMETER : 'parameter' ;
PICOSECOND : 'ps' ;
POSEDGE : 'posedge' ;
PRIORITY : 'priority' ;
PROPERTY : 'property' ;
PULL0 : 'pull0' ;
PULL1 : 'pull1' ;
RANDOMIZE : 'randomize' ;
RANDC : 'randc' ;
RAND : 'rand'; // Last so keywords prefixed with 'rand' lex first
REALTIME : 'realtime' ;
REAL : 'real' ; // Last so keywords prefixed with 'real' lex first
REF : 'ref' ;
REG : 'reg' ;
RELEASE : 'release' ;
REPEAT : 'repeat' ;
RESTRICT : 'restrict' ;
RETURN : 'return' ;
SEQUENCE : 'sequence' ;
SHORTINT : 'shortint' ;
SHORTREAL : 'shortreal' ;
SIGNED : 'signed' ;
SMALL : 'small' ;
SOFT : 'soft' ;
SOLVE : 'solve' ;
STATIC : 'static' ;
STD_COLON_COLON : 'std::' ;
STRING : 'string' ;
STRONG0 : 'strong0' ;
STRONG1 : 'strong1' ;
STRUCT : 'struct' ;
SUPER : 'super' ;
SUPPLY0 : 'supply0' ;
SUPPLY1 : 'supply1' ;
SECOND : 's' ; // Last so keywords prefixed with 's' lex first
TAGGED : 'tagged' ;
THIS : 'this' ;
THROUGHOUT : 'throughout' ;
TIMEPRECISION : 'timeprecision' ;
TIMEUNIT : 'timeunit' ;
TIME : 'time' ; // Last so keywords prefixed with 'time' lex first
TRI0 : 'tri0' ;
TRI1 : 'tri1' ;
TRIAND : 'triand' ;
TRIOR : 'trior' ;
TRIREG : 'trireg' ;
TRI : 'tri' ; // Last so keywords prefixed with 'tri' lex first
TYPEDEF : 'typedef' ;
TYPE : 'type' ; // Last so keywords prefixed with 'type' lex first
UNION : 'union' ;
UNIQUE0 : 'unique0' ;
UNIQUE : 'unique' ; // Last so keywords prefixed with 'unique' lex first
UNSIGNED : 'unsigned' ;
UNTYPED : 'untyped' ;
UWIRE : 'uwire' ;
VAR : 'var' ;
VIRTUAL : 'virtual' ;
VOID : 'void' ;
WAIT_ORDER : 'wait_order' ;
WAIT : 'wait' ; // Last so keywords prefixed with 'wait' lex first
WAND : 'wand' ;
WEAK0 : 'weak0' ;
WEAK1 : 'weak1' ;
WHILE : 'while' ;
WITHIN : 'within' ;
WITH : 'with' ; // Last so keywords prefixed with 'with' lex first
WIRE : 'wire' ;
WOR : 'wor' ;
LIT_XOR : 'xor' ;

SIMPLE_IDENTIFIER : [a-zA-Z_] [a-zA-Z0-9_$]* ;
ESCAPED_IDENTIFIER : '\\' .*? WS ;
SYSTEM_TF_IDENTIFIER : '$' [a-zA-Z0-9_$]+ ;

// Fragments don't have to be unambiguous: they are expanded in the
// lexer rule they're used in.

fragment ESC : '\\"' | '\\\\' ;
fragment SIGN : PLUS | MINUS ;
fragment DECIMAL_BASE : WS? '\'' [sS]? [dD] WS? ;
fragment OCTAL_BASE : WS? '\'' [sS]? [oO] WS? ;
fragment BINARY_BASE : WS? '\'' [sS]? [bB] WS? ;
fragment HEX_BASE : WS? '\'' [sS]? [hH] WS? ;
fragment NON_ZERO_UNSIGNED_NUMBER : NON_ZERO_DECIMAL_DIGIT ('_' | DECIMAL_DIGIT)* ;
fragment BINARY_VALUE : BINARY_DIGIT ('_' | BINARY_DIGIT)* ;
fragment OCTAL_VALUE : OCTAL_DIGIT ('_' | OCTAL_DIGIT)* ;
fragment HEX_VALUE : HEX_DIGIT ('_' | HEX_DIGIT)* ;
fragment DECIMAL_VALUE : UNSIGNED_NUMBER
	| X_DIGIT '_'*
	| Z_DIGIT '_'*
	;
fragment NON_ZERO_DECIMAL_DIGIT : [1-9] ;
fragment DECIMAL_DIGIT : [0-9] ;
fragment BINARY_DIGIT : X_DIGIT | Z_DIGIT | [01] ;
fragment OCTAL_DIGIT : X_DIGIT | Z_DIGIT | [0-7] ;
fragment HEX_DIGIT : X_DIGIT | Z_DIGIT | [0-9A-Fa-f] ;
fragment X_DIGIT : [xX] ;
fragment Z_DIGIT : [zZ?] ;

WS : [ \t\n]+ -> skip ;
