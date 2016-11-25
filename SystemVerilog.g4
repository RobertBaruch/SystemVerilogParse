grammar SystemVerilog;

//
// A.1.2 SystemVerilog source text
//

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

// TODO: more nonport module items
non_port_module_item :
	module_decl
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

package_import_decl : IMPORT package_import_item (',' package_import_item)* ';' ;

package_import_item : package_identifier '::' (identifier | STAR) ;

type_declaration :
	TYPEDEF data_type type_identifier variable_dimension* ';'
	| TYPEDEF interface_instance_identifier 
		constant_bit_select '.' type_identifier type_identifier ';'
	| TYPEDEF (ENUM | STRUCT | UNION | CLASS | INTERFACE CLASS)? type_identifier ';'
	;

lifetime : STATIC | AUTOMATIC ;

//
// A.2.2 Declaration data types
//

//
// A.2.2.1: Net and variable types
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
// A.2.3: Declaration lists
//

list_of_param_assignments : param_assignment (',' param_assignment)* ;

list_of_type_assignments : type_assignment (',' type_assignment)* ;

list_of_variable_decl_assignments : 
	variable_decl_assignment (',' variable_decl_assignment)* ;

//
// A.2.4: Declaration assignments
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
// A.2.5: Declaration ranges
//

unpacked_dimension : '[' constant_range ']' | '[' constant_expr ']' ;

packed_dimension : '[' constant_range ']' | unsized_dimension ;

associative_dimension : '[' data_type ']' | '[' STAR ']' ;

variable_dimension : unsized_dimension | unpacked_dimension
	| associative_dimension | queue_dimension ;

queue_dimension : '[' '$' (':' constant_expr)? ']' ;

unsized_dimension : '[' ']' ;

//
// A.2.10 Assertion declarations
//

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

//
// A.6.4 Statements
//

variable_identifier_list : variable_identifier (',' variable_identifier)* ;

//
// A.6.5 Timing control statements
//

event_expr :
	edge_identifier? expr (IFF expr)?
	| sequence_instance (IFF expr)?
	| event_expr LIT_OR event_expr
	| event_expr ',' event_expr
	| '(' event_expr ')'
	;

//
// A.6.7 Case statements
//

open_range_list : open_value_range (',' open_value_range)* ;

open_value_range : value_range ;

//
// A.6.7.1 Patterns
//

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

//
// A.6.8 Looping statements
//

loop_variables : index_variable_identifier? (',' index_variable_identifier?)* ;

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
	| expr binary_operator attribute_instance* expr
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
	primary_literal
	| identifier constant_select
	| identifier ('[' constant_range_expr ']')?
	| (package_scope | class_scope)? enum_identifier
	| constant_concatenation ('[' constant_range_expr ']')?
	// Expand subroutine_call
		| tf_call 
		| system_tf_call 
		// Expand method_call
			// Expand method_call_root
				| primary '.' method_call_body
				| implicit_class_handle '.' method_call_body
		| STD_COLON_COLON randomize_call
	| constant_let_expr
	| '(' constant_mintypmax_expr ')'
	// Expand constant_cast
		// Expand casting_type, expose left-recursion
		| (simple_type | signing | STRING | CONST)
			'\'' '(' constant_expr ')'
		| constant_primary '\'' '(' constant_expr ')'
	| constant_assignment_pattern_expr
	| type_reference
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

primary : 
	(class_qualifier_nonempty | package_scope)? hierarchical_identifier select
	| primary_literal 
	| empty_queue
	| concatenation ('[' range_expr ']')?
	| multiple_concatenation ('[' range_expr ']')?
	// Expand subroutine_call
		| tf_call 
		| system_tf_call 
		// Expand method_call
			// Expand method_call_root, expose left-recursion
				| primary '.' method_call_body
				| implicit_class_handle '.' method_call_body
		| STD_COLON_COLON randomize_call
	| let_expr
	| '(' mintypmax_expr ')'
	// Expand cast
		// Expand casting_type
		| (simple_type | signing | STRING | CONST) '\'' '(' expr ')'
		| constant_primary '\'' '(' expr ')'
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

constant_bit_select : ('[' constant_expr ']')* ;

constant_select :
	( ('.' member_identifier constant_bit_select)* member_identifier)? constant_bit_select
	('[' constant_part_select_range ']')? ;

constant_cast : casting_type '\'' '(' constant_expr ')' ;

constant_let_expr : let_expr ;

cast : casting_type '\'' '(' expr ')' ;

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
generate_block_identifier : identifier ;
genvar_identifier : identifier ;
hierarchical_identifier : 
	(DOLLAR_ROOT '.')? (identifier constant_bit_select '.')* identifier ;
hierarchical_array_identifier : hierarchical_identifier ;
hierarchical_sequence_identifier : hierarchical_identifier ;
hierarchical_tf_identifier : hierarchical_identifier ;
index_variable_identifier : identifier ;
interface_identifier : identifier ;
interface_instance_identifier : identifier ;
let_identifier : identifier ;
member_identifier : identifier ;
method_identifier : identifier ;
modport_identifier : identifier ;
module_identifier : identifier ;
net_type_identifier : identifier ;
package_scope : package_identifier '::' | DOLLAR_UNIT '::' ;
package_identifier : identifier ;
parameter_identifer : identifier ;
port_identifier : identifier ;
ps_class_identifier : package_scope? class_identifier ;
ps_covergroup_identifier : package_scope? covergroup_identifier ;
ps_or_hierarchical_array_identifier :
	(implicit_class_handle '.' | class_scope | package_scope)? 
	hierarchical_array_identifier ;
ps_or_hierarchical_sequence_identifier : package_scope? sequence_identifier
	| hierarchical_sequence_identifier ;
ps_or_hierarchical_tf_identifier : package_scope? tf_identifier
	| hierarchical_tf_identifier ;
ps_parameter_identifer : 
	(package_scope | class_scope)? parameter_identifer
	| (generate_block_identifier ('[' constant_expr ']')? '.' )* parameter_identifer
	;
ps_type_identifier : (LOCAL_COLON_COLON | package_scope)? type_identifier ;
sequence_identifier : identifier ;
specparam_identifier : identifier ;
system_tf_identifier : SYSTEM_TF_IDENTIFIER ;
tf_identifier : identifier ;
type_identifier : identifier ;
variable_identifier : identifier ;


// Lexer rules must be unambiguous (within any given mode)

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
CASE_EQUALITY : '===' ;
CASE_INEQUALITY : '!==' ;
EQUIVALENCE : '<->' ;
LSLEQ : '<<=' ;
LSREQ : '>>=' ;
WILDCARD_EQUALITY : '==?' ;
WILDCARD_INEQUALITY : '!=?' ;

// two-symbol groups

ANDEQ : '&=' ;
CLOSE_STAR : '*)' ;
COLON_EQUAL : ':=' ;
COLON_SLASH : ':/' ;
COND : '?:' ;
DEC : '--' ;
DIVEQ : '/=' ;
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
LIT_AND : 'and' ;
AUTOMATIC : 'automatic' ;
BEFORE : 'before' ;
BIT : 'bit' ;
BYTE : 'byte' ;
CHANDLE : 'chandle' ;
CLASS : 'class' ;
CONST : 'const' ;
DEFAULT : 'default' ;
DISABLE : 'disable' ;
DIST : 'dist' ;
EDGE : 'edge' ;
ELSE : 'else' ;
ENDMODULE : 'endmodule' ;
ENUM : 'enum' ;
EVENT : 'event' ;
EXTERN : 'extern' ;
FEMTOSECOND : 'fs' ;
FOREACH : 'foreach' ;
IFF : 'iff' ;
IF : 'if' ; // Last so keywords prefixed with 'if' lex first
IMPORT : 'import' ;
INOUT : 'inout' ;
INPUT : 'input' ;
INSIDE : 'inside' ;
INTEGER : 'integer' ;
INTERCONNECT : 'interconnect' ;
INTERFACE : 'interface' ;
INTERSECT : 'intersect' ;
INT : 'int' ; // Last so keywords prefixed with 'int' lex first
LOCAL_COLON_COLON : 'local::' ;
LOCALPARAM : 'localparam' ;
LOGIC : 'logic' ;
LONGINT : 'longint' ;
MACROMODULE : 'macromodule' ;
MICROSECOND : 'us' ;
MILLISECOND : 'ms' ;
MODULE : 'module' ;
NANOSECOND : 'ns' ;
NEGEDGE : 'negedge' ;
NEW : 'new' ;
NULL : 'null' ;
LIT_OR : 'or' ;
OUTPUT : 'output' ;
PACKED : 'packed' ;
PARAMETER : 'parameter' ;
PICOSECOND : 'ps' ;
POSEDGE : 'posedge' ;
RANDOMIZE : 'randomize' ;
RANDC : 'randc' ;
RAND : 'rand'; // Last so keywords prefixed with 'rand' lex first
REALTIME : 'realtime' ;
REAL : 'real' ; // Last so keywords prefixed with 'real' lex first
REF : 'ref' ;
REG : 'reg' ;
SHORTINT : 'shortint' ;
SHORTREAL : 'shortreal' ;
SIGNED : 'signed' ;
SOFT : 'soft' ;
SOLVE : 'solve' ;
STATIC : 'static' ;
STD_COLON_COLON : 'std::' ;
STRING : 'string' ;
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
UNIQUE : 'unique' ;
UNSIGNED : 'unsigned' ;
UWIRE : 'uwire' ;
VAR : 'var' ;
VIRTUAL : 'virtual' ;
VOID : 'void' ;
WAND : 'wand' ;
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
