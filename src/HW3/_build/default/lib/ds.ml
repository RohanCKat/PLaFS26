(* This file defines expressed values and environments *)

(* expressed values and environments are defined mutually recursively *)


type exp_val =
  | NumVal of int
  | BoolVal of bool
  | ListVal of exp_val list
  | TupleVal of exp_val list
type env =
  | EmptyEnv
  | ExtendEnv of string*exp_val*env


(* Environment Abstracted Result *)

type 'a result = Ok of 'a | Error of string

type 'a ea_result = env -> 'a result
  
let return : 'a -> 'a ea_result =
  fun v ->
  fun _env ->
  Ok v

let error : string -> 'a ea_result = fun s ->
  fun _env -> 
  Error s

let (>>=) : 'a ea_result -> ('a -> 'b ea_result) -> 'b ea_result =
  fun c f ->
  fun env ->
  match c env with
  | Error err -> Error err
  | Ok v -> f v env

let (>>+) : env ea_result -> 'a ea_result -> 'a ea_result =
  fun c d ->
  fun env ->
  match c env with
  | Error err -> Error err
  | Ok newenv -> d newenv

let run : 'a ea_result -> 'a result =
  fun c ->
  c EmptyEnv

let lookup : env ea_result = fun env ->
  Ok env

let rec sequence : 'a ea_result list -> 'a list ea_result =
  fun l ->
  match l with
  | [] -> return []
  | h::t ->
    h >>= fun ev ->
    sequence t >>= fun evs ->
    return (ev::evs)
      
(* Operations on environments *)

let empty_env : unit -> env ea_result = fun () ->
  return EmptyEnv

let extend_env : string -> exp_val -> env ea_result = fun id v env ->
  Ok (ExtendEnv(id,v,env))

let rec extend_env_list_helper =
  fun ids evs en ->
  match ids,evs with
  | [],[] -> en
  | id::idt,ev::evt ->
    ExtendEnv(id,ev,extend_env_list_helper idt evt en)
  | _,_ -> failwith
             "extend_env_list_helper: ids and evs have different sizes"
  
let extend_env_list =
  fun ids evs ->
  fun en ->
  Ok (extend_env_list_helper ids evs en)
    
let rec apply_env : string -> exp_val ea_result = fun id env ->
  match env with
  | EmptyEnv -> Error (id^" not found!")
  | ExtendEnv(v,ev,tail) ->
    if id=v
    then Ok ev
    else apply_env id tail



(* operations on expressed values *)

let int_of_numVal : exp_val -> int ea_result =  function
  |  NumVal n -> return n
  | _ -> error "Expected a number!"

let bool_of_boolVal : exp_val -> bool ea_result =  function
  |  BoolVal b -> return b
  | _ -> error "Expected a boolean!"

let list_of_listVal : exp_val -> exp_val list ea_result = function
  | ListVal l -> return l
  | _ -> error "Expected a list!"

let tuple_of_tupelVal : exp_val -> exp_val list ea_result = function
  | TupleVal l -> return l
  | _ -> error "Expected a tuple!"

