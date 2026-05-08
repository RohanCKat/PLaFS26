open ReM
open Dst
open Parser_plaf.Ast
open Parser_plaf.Parser
       
let rec chk_expr : expr -> texpr tea_result = function 
  | Int _n -> return IntType
  | Var id -> apply_tenv id
  | IsZero(e) ->
    chk_expr e >>= fun t ->
    if t=IntType
    then return BoolType
    else error "isZero: expected argument of type int"
  | Add(e1,e2) | Sub(e1,e2) | Mul(e1,e2)| Div(e1,e2) ->
    chk_expr e1 >>= fun t1 ->
    chk_expr e2 >>= fun t2 ->
    if (t1=IntType && t2=IntType)
    then return IntType
    else error "arith: arguments must be ints"
  | ITE(e1,e2,e3) ->
    chk_expr e1 >>= fun t1 ->
    chk_expr e2 >>= fun t2 ->
    chk_expr e3 >>= fun t3 ->
    if (t1=BoolType && t2=t3)
    then return t2
    else error "ITE: condition not boolean or types of then and else do not match"
  | Let(id,e,body) ->
    chk_expr e >>= fun t ->
    extend_tenv id t >>+
    chk_expr body
  | Proc(var,Some t1,e) ->
    extend_tenv var t1 >>+
    chk_expr e >>= fun t2 ->
    return @@ FuncType(t1,t2)
  | Proc(_var,None,_e) ->
    error "proc: type declaration missing"
  | App(e1,e2) ->
    chk_expr e1 >>=
    pair_of_funcType "app: " >>= fun (t1,t2) ->
    chk_expr e2 >>= fun t3 ->
    if t1=t3
    then return t2
    else error "app: type of argument incorrect"
  | Letrec([(_id,_param,None,_,_body)],_target) | Letrec([(_id,_param,_,None,_body)],_target) ->
    error "letrec: type declaration missing"
  | Letrec([(id,param,Some tParam,Some tRes,body)],target) ->
    extend_tenv id (FuncType(tParam,tRes)) >>+
    (extend_tenv param tParam >>+
     chk_expr body >>= fun t ->
     if t=tRes 
     then chk_expr target
     else error
         "LetRec: Type of recursive function does not match
declaration")
  | Pair(e1, e2) ->
    chk_expr e1 >>= fun a ->
    chk_expr e2 >>= fun b ->
    return (PairType(a,b))
  | Unpair(id1, id2, e1, e2) ->
    chk_expr e1 >>= fun pair ->
    (match pair with
    | PairType(a,b) -> extend_tenv id1 a >>+ extend_tenv id2 b >>+ chk_expr e2
    | _ -> error "e1 not a pair") 
  | NewRef(e) ->
    chk_expr e >>= fun a -> return @@ (RefType a)
  | DeRef(e) ->
    chk_expr e >>= fun a ->
    (match a with
    | RefType x -> return @@ x
    | _ -> error "Not a RefType")
  | SetRef(e1, e2) ->
    chk_expr e1 >>= fun r ->
    chk_expr e2 >>= fun n ->
    (match r with
    | RefType x -> if x=n then return @@ UnitType else error "not the same type"
    | _ -> error "Not a RefType")
  | BeginEnd([]) -> return @@ UnitType
  | BeginEnd(es) ->
    chk_expr (List.hd (List.rev es)) >>= fun a -> return @@ a
  | EmptyList(Some t) ->
    return @@ (ListType t)
  | EmptyList(None) ->
    error "No type specified"
  | Cons(e1, e2) ->
    chk_expr e1 >>= fun a ->
    chk_expr e2 >>= fun b ->
    (match b with
    | ListType t -> if t=a then return @@ (ListType a) else error "first and second elements dont have the same type"
    | _ -> error "Not a list")
  | Hd(e) ->
    chk_expr e >>= fun a ->
    (match a with
    | ListType t -> return @@ t
    | _ -> error "Not a list")
  | Tl(e) ->
    chk_expr e >>= fun a ->
    (match a with
    | ListType t -> return @@ (ListType t)
    | _ -> error "Not a list")
  | IsEmpty(e) -> 
    chk_expr e >>= fun a ->
    (match a with
    | ListType _ -> return @@ (BoolType)
    | TreeType _ -> return @@ (BoolType)
    | _ -> error "Not a list")
  | EmptyTree(Some t) ->
    return @@ (TreeType t)
  | EmptyTree(None) ->
    error "No type specified"
  | Node (e1, e2, e3) ->
    chk_expr e1 >>= fun a ->
    chk_expr e2 >>= fun b ->
    chk_expr e3 >>= fun c ->
    (match (b, c) with
    | (TreeType t, TreeType s) -> if t=s && t=a then return @@ (TreeType t) else error "Not same tree type"
    | _ -> error "Not a tree type")
  | CaseT(target, emptycase, id1, id2, id3, nodecase) ->
    chk_expr target >>= fun a ->
    (match a with
    | TreeType t -> chk_expr emptycase >>= fun b ->
      extend_tenv id1 t >>+
      extend_tenv id2 (TreeType t) >>+
      extend_tenv id3 (TreeType t) >>+
      chk_expr nodecase >>= fun c ->
      if b=c then return @@ c else error "return types dont match"
    | _ -> error "Not a tree type")
  | Debug(_e) ->
    string_of_tenv >>= fun str ->
    print_endline str;
    error "Debug: reached breakpoint"
  | _ -> failwith "chk_expr: implement"    
and
  chk_prog (AProg(_,e)) =
  chk_expr e

(* Type-check an expression *)
let chk (e:string) : texpr result =
  let c = e |> parse |> chk_prog
  in run_teac c

let chkpp (e:string) : string result =
  let c = e |> parse |> chk_prog
  in run_teac (c >>= fun t -> return @@ string_of_texpr t)



