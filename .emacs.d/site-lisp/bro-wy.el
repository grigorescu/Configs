;;; bro-wy.el --- Generated parser support file

;; Copyright (C) 2015 Vlad Grigorescu

;; Author: Vlad Grigorescu <vlad@broala.com>
;; Created: 2015-03-23 01:00:16-0400
;; Keywords: syntax
;; X-RCS: $Id$

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of
;; the License, or (at your option) any later version.

;; This software is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; PLEASE DO NOT MANUALLY EDIT THIS FILE!  It is automatically
;; generated from the grammar file bro.wy.

;;; History:
;;

;;; Code:

(require 'semantic/lex)
(eval-when-compile (require 'semantic/bovine))

;;; Prologue
;;

;;; Declarations
;;
(defconst bro-wy--keyword-table
  (semantic-lex-make-keyword-table 'nil 'nil)
  "Table of language keywords.")

(defconst bro-wy--token-table
  (semantic-lex-make-type-table
   '(("<no-type>"
      (TOK_CCE)
      (TOK_CCL)
      (TOK_NUMBER)
      (TOK_CHAR)))
   'nil)
  "Table of lexical tokens.")

(defconst bro-wy--parse-table
  (progn
    (eval-when-compile
      (require 'semantic/wisent/comp))
    (wisent-compile-grammar
     '((TOK_CHAR TOK_NUMBER TOK_CCL TOK_CCE)
       nil
       (flexrule
	((re))
	((error)))
       (re
	((re 124 series))
	((series))
	(nil))
       (series
	((series singleton))
	((singleton)))
       (singleton
	((singleton 42))
	((singleton 43))
	((singleton 63))
	((singleton 123 TOK_NUMBER 44 TOK_NUMBER 125))
	((singleton 123 TOK_NUMBER 44 125))
	((singleton 123 TOK_NUMBER 125))
	((46))
	((full_ccl))
	((TOK_CCL))
	((34 string 34))
	((40 re 41))
	((TOK_CHAR))
	((94))
	((36)))
       (full_ccl
	((91 ccl 93))
	((91 94 ccl 93)))
       (ccl
	((ccl TOK_CHAR 45 TOK_CHAR))
	((ccl TOK_CHAR))
	((ccl ccl_expr))
	(nil))
       (ccl_expr
	((TOK_CCE)))
       (string
	((string TOK_CHAR))
	(nil)))
     'nil))
  "Parser table.")

shcon(defun bro-wy--install-parser ()
  "Setup the Semantic Parser."
  (semantic-install-function-overrides
   '((parse-stream . wisent-parse-stream)))
  (setq semantic-parser-name "LALR"
	semantic--parse-table bro-wy--parse-table
	semantic-debug-parser-source "bro.wy"
	semantic-flex-keywords-obarray bro-wy--keyword-table
	semantic-lex-types-obarray bro-wy--token-table)
  ;; Collect unmatched syntax lexical tokens
  (semantic-make-local-hook 'wisent-discarding-token-functions)
  (add-hook 'wisent-discarding-token-functions
	    'wisent-collect-unmatched-syntax nil t))


;;; Analyzers
;;

;;; Epilogue
;;
;; %token TOK_ADD TOK_ADD_TO TOK_ADDR TOK_ANY
;; %token TOK_ATENDIF TOK_ATELSE TOK_ATIF TOK_ATIFDEF TOK_ATIFNDEF
;; %token TOK_BOOL TOK_BREAK TOK_CASE TOK_CONST
;; %token TOK_CONSTANT TOK_COPY TOK_COUNT TOK_COUNTER TOK_DEFAULT TOK_DELETE
;; %token TOK_DOUBLE TOK_ELSE TOK_ENUM TOK_EVENT TOK_EXPORT TOK_FALLTHROUGH
;; %token TOK_FILE TOK_FOR TOK_FUNCTION TOK_GLOBAL TOK_HOOK TOK_ID TOK_IF TOK_INT
;; %token TOK_INTERVAL TOK_LIST TOK_LOCAL TOK_MODULE
;; %token TOK_NEXT TOK_OF TOK_OPAQUE TOK_PATTERN TOK_PATTERN_TEXT
;; %token TOK_PORT TOK_PRINT TOK_RECORD TOK_REDEF
;; %token TOK_REMOVE_FROM TOK_RETURN TOK_SCHEDULE TOK_SET
;; %token TOK_STRING TOK_SUBNET TOK_SWITCH TOK_TABLE
;; %token TOK_TIME TOK_TIMEOUT TOK_TIMER TOK_TYPE TOK_UNION TOK_VECTOR TOK_WHEN
;; %token TOK_WHILE

;; %token TOK_ATTR_ADD_FUNC TOK_ATTR_ENCRYPT TOK_ATTR_DEFAULT
;; %token TOK_ATTR_OPTIONAL TOK_ATTR_REDEF TOK_ATTR_ROTATE_INTERVAL
;; %token TOK_ATTR_ROTATE_SIZE TOK_ATTR_DEL_FUNC TOK_ATTR_EXPIRE_FUNC
;; %token TOK_ATTR_EXPIRE_CREATE TOK_ATTR_EXPIRE_READ TOK_ATTR_EXPIRE_WRITE
;; %token TOK_ATTR_PERSISTENT TOK_ATTR_SYNCHRONIZED
;; %token TOK_ATTR_RAW_OUTPUT TOK_ATTR_MERGEABLE
;; %token TOK_ATTR_PRIORITY TOK_ATTR_LOG TOK_ATTR_ERROR_HANDLER
;; %token TOK_ATTR_TYPE_COLUMN TOK_ATTR_DEPRECATED

;; %token TOK_DEBUG

;; %token TOK_NO_TEST

;; %nonassoc TOK_HOOK
;; %left ',' '|'
;; %right '=' TOK_ADD_TO TOK_REMOVE_FROM
;; %right '?' ':'
;; %left TOK_OR
;; %left TOK_AND
;; %nonassoc '<' '>' TOK_LE TOK_GE TOK_EQ TOK_NE
;; %left TOK_IN TOK_NOT_IN
;; %left '+' '-'
;; %left '*' '/' '%'
;; %left TOK_INCR TOK_DECR
;; %right '!'
;; %left '$' '[' ']' '(' ')' TOK_HAS_FIELD TOK_HAS_ATTR
;; %%

;; bro:
;; 		decl_list stmt_list

;; 	|
;; 		Silly way of allowing the debugger to call yyparse()
;; 		on an expr rather than a file.

;; 		TOK_DEBUG
;; 		expr

;; 	;

;; decl_list:
;; 		decl_list decl
;; 	|
;; 	;

;; opt_expr:
;; 		expr

;; 	|

;; 	;

;; expr:
;; 		'(' expr ')'


;; 	|	TOK_COPY '(' expr ')'


;; 	|	TOK_INCR expr


;; 	|	TOK_DECR expr


;; 	|	'!' expr


;; 	|	'-' expr	%prec '!'


;; 	|	'+' expr	%prec '!'


;; 	|	expr '+' expr


;; 	|	expr TOK_ADD_TO expr


;; 	|	expr '-' expr


;; 	|	expr TOK_REMOVE_FROM expr


;; 	|	expr '*' expr


;; 	|	expr '/' expr


;; 	|	expr '%' expr


;; 	|	expr TOK_AND expr


;; 	|	expr TOK_OR expr


;; 	|	expr TOK_EQ expr


;; 	|	expr TOK_NE expr


;; 	|	expr '<' expr


;; 	|	expr TOK_LE expr


;; 	|	expr '>' expr


;; 	|	expr TOK_GE expr


;; 	|	expr '?' expr ':' expr


;; 	|	expr '=' expr


;; 	|	TOK_LOCAL local_id '=' expr


;; 	|	expr '[' expr_list ']'


;; 	|	expr '[' opt_expr ':' opt_expr ']'


;; 	|	expr '$' TOK_ID


;; 	|	'$' TOK_ID '=' expr


;; 	|       '$' TOK_ID func_params '='

;; 		 func_body


;; 	|	expr TOK_IN expr


;; 	|	expr TOK_NOT_IN expr


;; 	|	'[' expr_list ']'


;; 	|	'[' ']'



;; 	|	TOK_RECORD '(' expr_list ')'


;; 	|	TOK_TABLE '('
;; 	opt_expr_list ')'
;; 		opt_attr


;; 	|	TOK_SET '(' opt_expr_list ')' opt_attr


;; 	|	TOK_VECTOR '(' opt_expr_list ')'


;; 	|	expr '('


;; 		opt_expr_list


;; 		')'


;; 	|	TOK_HOOK
;; 	expr


;; 	|	expr TOK_HAS_FIELD TOK_ID


;; 	|	anonymous_function

;; 	|	TOK_SCHEDULE expr '{' event '}'


;; 	|	TOK_ID


;; 	|	TOK_CONSTANT


;; 	|	pattern


;; 	|       '|' expr '|'

;; 	;

;; expr_list:
;; 		expr_list ',' expr


;; 	|	expr

;; 	;

;; opt_expr_list:
;; 		expr_list
;; 	|

;; 	;

;; pattern:
;; 		pattern '|' single_pattern


;; 	|	single_pattern

;; 	;

;; single_pattern:
;; 		'/'
;; 		TOK_PATTERN_TEXT
;; 		'/'

;; 	;

;; enum_body:
;; 		enum_body_list


;; 	|	enum_body_list ','

;; 	;

;; enum_body_list:
;; 		enum_body_elem

;; 	|	enum_body_list ',' enum_body_elem
;; 	;

;; enum_body_elem:
;; 		TODO: We could also define this as TOK_ID '=' expr, (or
;; 		TOK_ID '=' = TOK_ID) so that we can return more descriptive
;; 		error messages if someboy tries to use constant variables as
;; 		enumerator.

;; 		TOK_ID '=' TOK_CONSTANT TOK_ATTR_DEPRECATED


;; 	|	TOK_ID '=' TOK_CONSTANT


;; 	|	TOK_ID '=' '-' TOK_CONSTANT


;; 	|	TOK_ID TOK_ATTR_DEPRECATED


;; 	|	TOK_ID

;; 	;

;; type:
;; 		TOK_BOOL

;; 	|	TOK_INT

;; 	|	TOK_COUNT

;; 	|	TOK_COUNTER

;; 	|	TOK_DOUBLE

;; 	|	TOK_TIME

;; 	|	TOK_INTERVAL

;; 	|	TOK_STRING

;; 	|	TOK_PATTERN

;; 	|	TOK_TIMER

;; 	|	TOK_PORT

;; 	|	TOK_ADDR

;; 	|	TOK_SUBNET

;; 	|	TOK_ANY

;; 	|	TOK_TABLE '[' type_list ']' TOK_OF type


;; 	|	TOK_SET '[' type_list ']'


;; 	|	TOK_RECORD '{'

;; 		type_decl_list

;; 		'}'


;; 	|	TOK_UNION '{' type_list '}'


;; 	|	TOK_ENUM '{'
;; 	enum_body '}'


;; 	|	TOK_LIST


;; 	|	TOK_LIST TOK_OF type


;; 	|	TOK_VECTOR TOK_OF type


;; 	|	TOK_FUNCTION func_params


;; 	|	TOK_EVENT '(' formal_args ')'


;; 	|	TOK_HOOK '(' formal_args ')'


;; 	|	TOK_FILE TOK_OF type


;; 	|	TOK_FILE


;; 	|	TOK_OPAQUE TOK_OF TOK_ID


;; 	|	resolve_id

;; 	;

;; type_list:
;; 		type_list ',' type

;; 	|	type

;; 	;

;; type_decl_list:
;; 		type_decl_list type_decl

;; 	|

;; 	;

;; type_decl:
;; 		TOK_ID ':' type opt_attr ';'

;; 	;

;; formal_args:
;; 		formal_args_decl_list

;; 	|	formal_args_decl_list ';'

;; 	|

;; 	;

;; formal_args_decl_list:
;; 		formal_args_decl_list ';' formal_args_decl

;; 	|	formal_args_decl_list ',' formal_args_decl

;; 	|	formal_args_decl

;; 	;

;; formal_args_decl:
;; 		TOK_ID ':' type opt_attr

;; 	;

;; decl:
;; 		TOK_MODULE TOK_ID ';'


;; 	|	TOK_EXPORT '{'
;; 	decl_list '}'


;; 	|	TOK_GLOBAL def_global_id opt_type init_class opt_init opt_attr ';'


;; 	|	TOK_CONST def_global_id opt_type init_class opt_init opt_attr ';'


;; 	|	TOK_REDEF global_id opt_type init_class opt_init opt_attr ';'


;; 	|	TOK_REDEF TOK_ENUM global_id TOK_ADD_TO '{'

;; 		enum_body '}' ';'


;; 	|	TOK_REDEF TOK_RECORD global_id

;; 		TOK_ADD_TO '{'

;; 		type_decl_list

;; 		'}' opt_attr ';'


;; 	|	TOK_TYPE global_id ':'

;; 		type opt_attr ';'


;; 	|	func_hdr func_body


;; 	|	conditional
;; 	;

;; conditional:
;; 		TOK_ATIF '(' expr ')'

;; 	|	TOK_ATIFDEF '(' TOK_ID ')'

;; 	|	TOK_ATIFNDEF '(' TOK_ID ')'

;; 	|	TOK_ATENDIF

;; 	|	TOK_ATELSE

;; 	;

;; func_hdr:
;; 		TOK_FUNCTION def_global_id func_params

;; 	|	TOK_EVENT event_id func_params

;; 	|	TOK_HOOK def_global_id func_params

;; 	|	TOK_REDEF TOK_EVENT event_id func_params

;; 	;

;; func_body:
;; 		opt_attr '{'


;; 		stmt_list


;; 		'}'

;; 	;

;; anonymous_function:
;; 		TOK_FUNCTION begin_func func_body

;; 	;

;; begin_func:
;; 		func_params

;; 	;

;; func_params:
;; 		'(' formal_args ')' ':' type

;; 	|	'(' formal_args ')'

;; 	;

;; opt_type:
;; 		':' type

;; 	|

;; 	;

;; init_class:

;; 	|	'='
;; 	|	TOK_ADD_TO
;; 	|	TOK_REMOVE_FROM
;; 	;

;; opt_init:

;; init

;; 	|

;; 	;

;; init:
;; 		'{' opt_expr_list '}'

;; 	|	'{' expr_list ',' '}'

;; 	|	expr
;; 	;

;; opt_attr:
;; 		attr_list
;; 	|

;; 	;

;; attr_list:
;; 		attr_list attr

;; 	|	attr

;; 	;

;; attr:
;; 		TOK_ATTR_DEFAULT '=' expr

;; 	|	TOK_ATTR_OPTIONAL

;; 	|	TOK_ATTR_REDEF

;; 	|	TOK_ATTR_ROTATE_INTERVAL '=' expr

;; 	|	TOK_ATTR_ROTATE_SIZE '=' expr

;; 	|	TOK_ATTR_ADD_FUNC '=' expr

;; 	|	TOK_ATTR_DEL_FUNC '=' expr

;; 	|	TOK_ATTR_EXPIRE_FUNC '=' expr

;; 	|	TOK_ATTR_EXPIRE_CREATE '=' expr

;; 	|	TOK_ATTR_EXPIRE_READ '=' expr

;; 	|	TOK_ATTR_EXPIRE_WRITE '=' expr

;; 	|	TOK_ATTR_PERSISTENT

;; 	|	TOK_ATTR_SYNCHRONIZED

;; 	|	TOK_ATTR_ENCRYPT

;; 	|	TOK_ATTR_ENCRYPT '=' expr

;; 	|	TOK_ATTR_RAW_OUTPUT

;; 	|	TOK_ATTR_MERGEABLE

;; 	|	TOK_ATTR_PRIORITY '=' expr

;; 	|	TOK_ATTR_TYPE_COLUMN '=' expr

;; 	|	TOK_ATTR_LOG

;; 	|	TOK_ATTR_ERROR_HANDLER

;; 	|	TOK_ATTR_DEPRECATED

;; 	;

;; stmt:
;; 		'{' TOK_NO_TEST stmt_list '}'


;; 	|	'{' stmt_list '}'


;; 	|	TOK_PRINT expr_list ';' opt_no_test


;; 	|	TOK_EVENT event ';' opt_no_test


;; 	|	TOK_IF '(' expr ')' stmt


;; 	|	TOK_IF '(' expr ')' stmt TOK_ELSE stmt


;; 	|	TOK_SWITCH expr '{' case_list '}'


;; 	|	for_head stmt


;; 	|	TOK_WHILE '(' expr ')' stmt


;; 	|	TOK_NEXT ';' opt_no_test


;; 	|	TOK_BREAK ';' opt_no_test


;; 	|	TOK_FALLTHROUGH ';' opt_no_test


;; 	|	TOK_RETURN ';' opt_no_test


;; 	|	TOK_RETURN expr ';' opt_no_test


;; 	|	TOK_ADD expr ';' opt_no_test


;; 	|	TOK_DELETE expr ';' opt_no_test


;; 	|	TOK_LOCAL local_id opt_type init_class opt_init opt_attr ';' opt_no_test


;; 	|	TOK_CONST local_id opt_type init_class opt_init opt_attr ';' opt_no_test


;; 	|	TOK_WHEN '(' expr ')' stmt


;; 	|	TOK_WHEN '(' expr ')' stmt TOK_TIMEOUT expr '{' opt_no_test_block stmt_list '}'



;; 	|	TOK_RETURN TOK_WHEN '(' expr ')' stmt


;; 	|	TOK_RETURN TOK_WHEN '(' expr ')' stmt TOK_TIMEOUT expr '{' opt_no_test_block stmt_list '}'


;; 	|	expr ';' opt_no_test


;; 	|	';'


;; 	|	conditional

;; 	;

;; stmt_list:
;; 		stmt_list stmt

;; 	|

;; 	;

;; event:
;; 		TOK_ID '(' opt_expr_list ')'

;; 	;

;; case_list:
;; 		case_list case

;; 	|

;; 	;

;; case:
;; 		TOK_CASE expr_list ':' stmt_list

;; 	|
;; 		TOK_DEFAULT ':' stmt_list

;; 	;

;; for_head:
;; 		TOK_FOR '(' TOK_ID TOK_IN expr ')'

;; 	|
;; 		TOK_FOR '(' '[' local_id_list ']' TOK_IN expr ')'

;; 		;

;; local_id_list:
;; 		local_id_list ',' local_id

;; 	|	local_id

;; 	;

;; local_id:
;; 		TOK_ID

;; 	;

;; global_id:

;; global_or_event_id

;; 	;

;; def_global_id:

;; global_id

;; 	;

;; event_id:

;; global_or_event_id

;; 	;

;; global_or_event_id:
;; 		TOK_ID

;; 	;


;; resolve_id:
;; 		TOK_ID

;; 	;

;; opt_no_test:
;; 		TOK_NO_TEST

;; 	|


;; %%

(provide 'bro-wy)

;; Local Variables:
;; version-control: never
;; no-update-autoloads: t
;; End:

;;; bro-wy.el ends here
