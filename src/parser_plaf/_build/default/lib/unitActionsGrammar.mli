
(* The type of tokens. *)

type token = Grammar.token

(* This exception is raised by the monolithic API functions. *)

exception Error

(* The monolithic API. *)

val prog: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (unit)

module MenhirInterpreter : sig
  
  (* The incremental API. *)
  
  include MenhirLib.IncrementalEngine.INCREMENTAL_ENGINE
    with type token = token
  
end

(* The entry point(s) to the incremental API. *)

module Incremental : sig
  
  val prog: Lexing.position -> (unit) MenhirInterpreter.checkpoint
  
end

(* The parse tables. *)

(* Warning: this submodule is undocumented. In the future,
   its type could change, or it could disappear altogether. *)

module Tables : MenhirLib.TableFormat.TABLES
  with type token = token
