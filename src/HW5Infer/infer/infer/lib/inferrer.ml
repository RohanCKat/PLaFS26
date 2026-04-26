open Parser_plaf.Ast
open Parser_plaf.Parser
open ReM
open Unif

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
