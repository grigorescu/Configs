;;; wisent-bro-wy.el --- Generated parser support file

;; Copyright (C) 2015 Vlad Grigorescu

;; Author: Vlad Grigorescu <vlad@broala.com>
;; Created: 2015-03-30 19:02:03-0400
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
(defconst wisent-bro-wy--keyword-table
  (semantic-lex-make-keyword-table
   '(("add" . ADD)
     ("addr" . ADDR)
     ("any" . ANY)
     ("bool" . BOOL)
     ("break" . BREAK)
     ("case" . CASE)
     ("const" . CONST)
     ("copy" . COPY)
     ("count" . COUNT)
     ("counter" . COUNTER)
     ("default" . DEFAULT)
     ("delete" . DELETE)
     ("double" . DOUBLE)
     ("else" . ELSE)
     ("enum" . ENUM)
     ("event" . EVENT)
     ("export" . EXPORT)
     ("fallthrough" . FALLTHROUGH)
     ("file" . FILE)
     ("for" . FOR)
     ("while" . WHILE)
     ("function" . FUNCTION)
     ("global" . GLOBAL)
     ("?$" . HAS_FIELD)
     ("hook" . HOOK)
     ("if" . IF)
     ("in" . IN)
     ("int" . INT)
     ("interval" . INTERVAL)
     ("list" . LIST)
     ("local" . LOCAL)
     ("module" . MODULE)
     ("next" . NEXT)
     ("of" . OF)
     ("opaque" . OPAQUE)
     ("pattern" . PATTERN)
     ("port" . PORT)
     ("print" . PRINT)
     ("record" . RECORD)
     ("redef" . REDEF)
     ("return" . RETURN)
     ("schedule" . SCHEDULE)
     ("set" . SET)
     ("string" . STRING)
     ("subnet" . SUBNET)
     ("switch" . SWITCH)
     ("table" . TABLE)
     ("time" . TIME)
     ("timeout" . TIMEOUT)
     ("timer" . TIMER)
     ("type" . TYPE)
     ("union" . UNION)
     ("vector" . VECTOR)
     ("when" . WHEN))
   'nil)
  "Table of language keywords.")

(defconst wisent-bro-wy--token-table
  (semantic-lex-make-type-table
   '(("number"
      (NUMBER_LITERAL))
     ("string"
      (STRING_LITERAL))
     ("punctuation"
      (TOK_DOLLAR_SIGN . "$")
      (TOK_TILDE . "~")
      (TOK_QUESTION_MARK . "?")
      (TOK_GT . ">")
      (TOK_EQUAL . "=")
      (TOK_LT . "<")
      (TOK_SEMICOLON . ";")
      (TOK_COLON . ":")
      (TOK_COMMA . ",")
      (TOK_MINUS . "-")
      (TOK_PLUS . "+")
      (TOK_FWD_SLASH . "/")
      (TOK_ASTERISK . "*")
      (TOK_PERCENT . "%")
      (TOK_NOT . "!")
      (TOK_OR . "||")
      (TOK_AND . "&&")
      (TOK_LE . "<=")
      (TOK_GE . ">=")
      (TOK_NE . "!=")
      (TOK_EQ . "==")
      (TOK_REMOVE_FROM . "-=")
      (TOK_ADD_TO . "+=")
      (TOK_INCR . "++")
      (TOK_DECR . "--"))
     ("close-paren"
      (RBRACK . "]")
      (RBRACE . "}")
      (RPAREN . ")"))
     ("open-paren"
      (LBRACK . "[")
      (LBRACE . "{")
      (LPAREN . "("))
     ("block"
      (BRACK_BLOCK . "(LBRACK RBRACK)")
      (BRACE_BLOCK . "(LBRACE RBRACE)")
      (PAREN_BLOCK . "(LPAREN RPAREN)")))
   '(("keyword" :declared t)
     ("number" :declared t)
     ("string" :declared t)
     ("punctuation" :declared t)
     ("block" :declared t)))
  "Table of lexical tokens.")

(defconst wisent-bro-wy--parse-table
  (progn
    (eval-when-compile
      (require 'semantic/wisent/comp))
    (wisent-compile-grammar
     '((PAREN_BLOCK BRACE_BLOCK BRACK_BLOCK LPAREN RPAREN LBRACE RBRACE LBRACK RBRACK TOK_DECR TOK_INCR TOK_ADD_TO TOK_REMOVE_FROM TOK_EQ TOK_NE TOK_GE TOK_LE TOK_AND TOK_OR TOK_NOT TOK_PERCENT TOK_ASTERISK TOK_FWD_SLASH TOK_PLUS TOK_MINUS TOK_COMMA TOK_COLON TOK_SEMICOLON TOK_LT TOK_EQUAL TOK_GT TOK_QUESTION_MARK TOK_TILDE TOK_DOLLAR_SIGN STRING_LITERAL NUMBER_LITERAL ADD ADDR ANY BOOL BREAK CASE CONST COPY COUNT COUNTER DEFAULT DELETE DOUBLE ELSE ENUM EVENT EXPORT FALLTHROUGH FILE FOR WHILE FUNCTION GLOBAL HAS_FIELD HOOK IF IN INT INTERVAL LIST LOCAL MODULE NEXT OF OPAQUE PATTERN PORT PRINT RECORD REDEF RETURN SCHEDULE SET STRING SUBNET SWITCH TABLE TIME TIMEOUT TIMER TYPE UNION VECTOR WHEN)
       nil
       (INITIAL
	((bro)))
       (bro
	((NUMBER_LITERAL))
	((STRING_LITERAL))))
     '(INITIAL)))
  "Parser table.")

(defun wisent-bro-wy--install-parser ()
  "Setup the Semantic Parser."
  (semantic-install-function-overrides
   '((parse-stream . wisent-parse-stream)))
  (setq semantic-parser-name "LALR"
	semantic--parse-table wisent-bro-wy--parse-table
	semantic-debug-parser-source "bro.wy"
	semantic-flex-keywords-obarray wisent-bro-wy--keyword-table
	semantic-lex-types-obarray wisent-bro-wy--token-table)
  ;; Collect unmatched syntax lexical tokens
  (semantic-make-local-hook 'wisent-discarding-token-functions)
  (add-hook 'wisent-discarding-token-functions
	    'wisent-collect-unmatched-syntax nil t))


;;; Analyzers
;;
(define-lex-block-type-analyzer wisent-bro-wy--<block>-block-analyzer
  "block analyzer for <block> tokens."
  "\\s(\\|\\s)"
  '((("(" LPAREN PAREN_BLOCK)
     ("{" LBRACE BRACE_BLOCK)
     ("[" LBRACK BRACK_BLOCK))
    (")" RPAREN)
    ("}" RBRACE)
    ("]" RBRACK))
  )

(define-lex-string-type-analyzer wisent-bro-wy--<punctuation>-string-analyzer
  "string analyzer for <punctuation> tokens."
  "\\(\\s.\\|\\s$\\|\\s'\\)+"
  '((TOK_DOLLAR_SIGN . "$")
    (TOK_TILDE . "~")
    (TOK_QUESTION_MARK . "?")
    (TOK_GT . ">")
    (TOK_EQUAL . "=")
    (TOK_LT . "<")
    (TOK_SEMICOLON . ";")
    (TOK_COLON . ":")
    (TOK_COMMA . ",")
    (TOK_MINUS . "-")
    (TOK_PLUS . "+")
    (TOK_FWD_SLASH . "/")
    (TOK_ASTERISK . "*")
    (TOK_PERCENT . "%")
    (TOK_NOT . "!")
    (TOK_OR . "||")
    (TOK_AND . "&&")
    (TOK_LE . "<=")
    (TOK_GE . ">=")
    (TOK_NE . "!=")
    (TOK_EQ . "==")
    (TOK_REMOVE_FROM . "-=")
    (TOK_ADD_TO . "+=")
    (TOK_INCR . "++")
    (TOK_DECR . "--"))
  'punctuation)

(define-lex-regex-type-analyzer wisent-bro-wy--<number>-regexp-analyzer
  "regexp analyzer for <number> tokens."
  semantic-lex-number-expression
  nil
  'NUMBER_LITERAL)

(define-lex-sexp-type-analyzer wisent-bro-wy--<string>-sexp-analyzer
  "sexp analyzer for <string> tokens."
  "\\s\""
  'STRING_LITERAL)

(define-lex-keyword-type-analyzer wisent-bro-wy--<keyword>-keyword-analyzer
  "keyword analyzer for <keyword> tokens."
  "\\(\\sw\\|\\s_\\)+")


;;; Epilogue
;;
;; Define the lexer for this grammar
(define-lex wisent-bro-lexer
  "Lexical analyzer that handles Bro buffers.
It ignores whitespaces, newlines and comments."
  semantic-lex-ignore-whitespace
  semantic-lex-ignore-newline
  semantic-lex-ignore-comments
  ;;;; Auto-generated analyzers.
  wisent-bro-wy--<number>-regexp-analyzer
  wisent-bro-wy--<string>-sexp-analyzer
  ;; Must detect keywords before other symbols
  wisent-bro-wy--<keyword>-keyword-analyzer
;;  wisent-bro-wy--<symbol>-regexp-analyzer
  wisent-bro-wy--<punctuation>-string-analyzer
  wisent-bro-wy--<block>-block-analyzer
  ;;;;
  semantic-lex-default-action)

(provide 'wisent-bro-wy)

;; Local Variables:
;; version-control: never
;; no-update-autoloads: t
;; End:

;;; wisent-bro-wy.el ends here
