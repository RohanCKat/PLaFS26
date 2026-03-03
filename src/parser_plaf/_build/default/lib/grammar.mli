
(* The type of tokens. *)

type token = 
  | UNPAIR
  | UNITTYPE
  | UNIONSET
  | TYPE
  | TREETYPE
  | TRANSPARENT
  | TOPQ
  | TOP
  | TL
  | TIMES
  | THEN
  | TAKE
  | SUPER
  | SUM
  | STACKTYPE
  | SND
  | SIZE
  | SINGLE_RIGHT_QUOTATION_MARK
  | SETTYPE
  | SETREF
  | SET
  | SEND
  | SEMICOLON
  | SELF
  | RRANGLE
  | RPAREN
  | REMOVEQ
  | REMOVEHTBL
  | REFTYPE
  | RBRACKET
  | RBRACE
  | RANGLE
  | QUEUETYPE
  | PUSH
  | PROD
  | PROC
  | POP
  | PLUS
  | PIPE
  | PAIR
  | OPEN
  | OPAQUE
  | OF
  | NODE
  | NEWREF
  | NEW
  | MODULE
  | MKSET
  | MKLIST
  | MINUS
  | MIN
  | METHOD
  | MAXL
  | LPAREN
  | LOOKUPHTBL
  | LLANGLE
  | LISTTYPE
  | LETREC
  | LET
  | LBRACKET
  | LBRACE
  | LANGLE
  | ISZERO
  | ISSUBSET
  | ISNUMBER
  | ISMEMBER
  | ISEMPTY
  | INTTYPE
  | INTERFACE
  | INT of (int)
  | INSTANCEOF
  | INSERTSET
  | INSERTHTBL
  | IN
  | IMPLEMENTS
  | IF
  | ID of (string)
  | HTBLTYPE
  | HD
  | FST
  | FROM
  | FIELD
  | EXTENDS
  | EQUALSMUTABLE
  | EQUALS
  | EOF
  | END
  | EMPTYTREE
  | EMPTYSTACK
  | EMPTYSET
  | EMPTYQUEUE
  | EMPTYLIST
  | EMPTYHTBL
  | ELSE
  | DOT
  | DIVIDED
  | DEREF
  | DEBUG
  | CONSTRUCTOR of (string)
  | CONS
  | COMMA
  | COLON
  | CLASS
  | CAST
  | CASET
  | CASE
  | BOOLTYPE
  | BODY
  | BEGIN
  | AVG
  | ARROW
  | ADDQ
  | ABS

(* This exception is raised by the monolithic API functions. *)

exception Error

(* The monolithic API. *)

val prog: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Ast.prog)
