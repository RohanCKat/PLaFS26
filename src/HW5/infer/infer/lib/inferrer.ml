open Parser_plaf.Ast
open Parser_plaf.Parser
open ReM
open Unif

(*
Name: Rohan Kathiari
Pledge: I Pledge My Honor That I Have Abided By The Stevens Honor System
*)
let agreement_equations ctx1 ctx2 ctx3 =
  let ctx_arr = [|ctx1;ctx2;ctx3|]
  in
  (* place largest at the beginning of the array *)
  Array.sort (fun c1 c2 -> SubsMap.cardinal c2-SubsMap.cardinal c1)
    ctx_arr;
  (* now compute conflict equations *)
  SubsMap.fold
    (fun id ty ac ->
       match SubsMap.find_opt id ctx_arr.(1),SubsMap.find_opt id ctx_arr.(2)  with
       | Some ty',Some ty'' -> EqSet.(add (ty',ty'') (add (ty,ty') ac))
       | Some ty',None -> EqSet.add (ty,ty') ac
       | None,Some ty'-> EqSet.add (ty,ty') ac
       | None,None -> ac)
    ctx_arr.(0)
    EqSet.empty

let rec infer_expr : expr -> (texpr SubsMap.t*texpr) result =
  fun e ->
  match e with
  | Int(_) -> 
    returnE (SubsMap.empty, IntType)
    
  | Var(id) -> 
    let s = fresh_string id in
    returnE ((SubsMap.singleton id (TypeVar s)), TypeVar s)
  | IsZero(e) ->
    infer_expr e >>>= fun (ctx1, ty1) ->
    let eqs = EqSet.singleton (ty1,IntType)
    in unify eqs >>>= fun mgu ->
    let ctx = (apply_tsubs_to_context ctx1 mgu)
    in returnE (ctx, BoolType)
  | Add(e1,e2) | Sub(e1,e2) | Mul(e1,e2) | Div(e1,e2) ->
    infer_expr e1 >>>= fun (ctx1,ty1) ->
    infer_expr e2 >>>= fun (ctx2,ty2) ->
    let eqs = (EqSet.union (agreement_equations ctx1 ctx2 SubsMap.empty) (EqSet.of_list [(ty1, IntType); (ty2, IntType)]))
    in unify eqs >>>= fun mgu ->
    let ctx = (subs_union "" (apply_tsubs_to_context ctx1 mgu) (apply_tsubs_to_context ctx2 mgu))
    in returnE (ctx, IntType)

  | ITE(e1, e2, e3) ->
    infer_expr e1 >>>= fun (ctx1,ty1) ->
    infer_expr e2 >>>= fun (ctx2,ty2) ->
    infer_expr e3 >>>= fun (ctx3,ty3) ->
    let eqs = (EqSet.union (agreement_equations ctx1 ctx2 ctx3) (EqSet.of_list [(ty2, ty3); (ty1, BoolType)]))
    in unify eqs >>>= fun mgu ->
    let ctx = (subs_union "" (subs_union "" (apply_tsubs_to_context ctx1 mgu) (apply_tsubs_to_context ctx2 mgu)) (apply_tsubs_to_context ctx3 mgu))
    in 
    returnE (ctx, (apply_tsubs_to_type ty2 mgu))

  | App(e1, e2) ->
    infer_expr e1 >>>= fun (ctx1,ty1) ->
    infer_expr e2 >>>= fun (ctx2,ty2) ->
    let str = TypeVar (fresh_string "j")
    in let eqs = (EqSet.union (agreement_equations ctx1 ctx2 SubsMap.empty) (EqSet.of_list [(ty1, FuncType(ty2, str))]))
    in unify eqs >>>= fun mgu ->
    let ctx = (subs_union "" (apply_tsubs_to_context ctx1 mgu) (apply_tsubs_to_context ctx2 mgu))
    in returnE (ctx, (apply_tsubs_to_type str mgu))

  | Let(id, e1, e2) ->
    infer_expr e1 >>>= fun (ctx1,ty1) ->
    infer_expr e2 >>>= fun (ctx2,ty2) ->
    let str = find_or_fresh id ctx2
    in let eqs = (EqSet.union (agreement_equations ctx1 ctx2 SubsMap.empty) (EqSet.of_list [(str, ty1)]))
    in unify eqs >>>= fun mgu ->
    let ctx = (subs_union "" (apply_tsubs_to_context ctx1 mgu) (SubsMap.remove id (apply_tsubs_to_context ctx2 mgu)))
    in returnE (ctx, (apply_tsubs_to_type ty2 mgu))

  | Proc(id, None, e) -> 
    infer_expr e >>>= fun (ctx1,ty1) ->
    let str = find_or_fresh id ctx1 
    in returnE((SubsMap.remove id ctx1), FuncType(str, ty1))
    
  | Proc(id, Some tPar, e) -> 
    infer_expr e >>>= fun (ctx1,ty1) ->
    let str = find_or_fresh id ctx1 
    in let eqs = EqSet.singleton (str, tPar)
    in unify eqs >>>= fun mgu ->
    returnE((SubsMap.remove id ctx1), (apply_tsubs_to_type (FuncType(str, ty1)) mgu)) 
  | _ -> failwith @@ "infer_expr: not implemented yet: "^string_of_expr e

let infer_prog (AProg(_,e)) : (texpr SubsMap.t*texpr) result =
  infer_expr e

let infer : string -> (texpr SubsMap.t*texpr) result =
  fun e ->
  e |> parse |> infer_prog

let string_of_infer e =
  match infer e with
  | Error s -> "error: "^s
  | Ok (ctx,typ) -> "("^string_of_texpr typ^","^string_of_subs ctx^")"
