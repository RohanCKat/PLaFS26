open Parser_plaf.Ast
open Parser_plaf.Parser
open Ds
    
(** [eval_expr e] evaluates expression [e] *)
let rec eval_expr : expr -> exp_val ea_result =
  fun e ->
  match e with
  | Int(n) ->
    return (NumVal n)
  | Var(id) ->
    apply_env id
  | Add(e1,e2) ->
    eval_expr e1 >>=
    int_of_numVal >>= fun n1 ->
    eval_expr e2 >>=
    int_of_numVal >>= fun n2 ->
    return (NumVal (n1+n2))
  | Sub(e1,e2) ->
    eval_expr e1 >>=
    int_of_numVal >>= fun n1 ->
    eval_expr e2 >>=
    int_of_numVal >>= fun n2 ->
    return (NumVal (n1-n2))
  | Mul(e1,e2) ->
    eval_expr e1 >>=
    int_of_numVal >>= fun n1 ->
    eval_expr e2 >>=
    int_of_numVal >>= fun n2 ->
    return (NumVal (n1*n2))
  | Div(e1,e2) ->
    eval_expr e1 >>=
    int_of_numVal >>= fun n1 ->
    eval_expr e2 >>=
    int_of_numVal >>= fun n2 ->
    if n2==0
    then error "Division by zero"
    else return (NumVal (n1/n2))
  | Let(id,def,body) ->
    eval_expr def >>= 
    extend_env id >>+
    eval_expr body 
  | ITE(e1,e2,e3) ->
    eval_expr e1 >>=
    bool_of_boolVal >>= fun b ->
    if b 
    then eval_expr e2
    else eval_expr e3
  | IsZero(e) ->
    eval_expr e >>=
    int_of_numVal >>= fun n ->
    return (BoolVal (n = 0))
  | Cons(e1,e2) ->
    eval_expr e1 >>= fun n ->
    eval_expr e2 >>= 
    list_of_listVal >>= fun l ->
    return (ListVal (n :: l))
  | Hd(e) ->
    eval_expr e >>= 
    list_of_listVal >>= fun n ->
    if n = [] 
    then error "List is empty!"
    else return ((List.hd n))

  | Tl(e) ->
    eval_expr e >>= 
    list_of_listVal >>= fun n ->
    if n = [] 
    then error "List is empty!"
    else return (ListVal (List.tl n))

  | IsEmpty(e) ->
    eval_expr e >>= 
    list_of_listVal >>= fun n ->
    return (BoolVal (n = []))
  | EmptyList(_t) ->
    return (ListVal [])
  | Tuple(es) -> 
    eval_exprs es >>= fun l ->
    return (TupleVal l)
  | Untuple(ids,e1,e2) -> 
    eval_expr e1 >>=
    tuple_of_tupelVal >>= fun vals ->
    extend_env_list ids vals >>+
    eval_expr e2
  | _ -> failwith "Not implemented yet!"
and
  eval_exprs : expr list -> ( exp_val list ) ea_result =
  fun es ->
  match es with
  | [] -> return []
  | h :: t -> eval_expr h >>= fun i ->
    eval_exprs t >>= fun l ->
    return ( i :: l )


(** [eval_prog e] evaluates program [e] *)
let eval_prog (AProg(_,e)) =
  eval_expr e

(** [interp s] parses [s] and then evaluates it *)
let interp (e:string) : exp_val result =
  let c = e |> parse |> eval_prog
  in run c
  


