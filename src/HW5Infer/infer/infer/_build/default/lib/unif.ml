open Parser_plaf.Ast
open ReM

(** ------------------------------ *)
(** equations and set of equations *)
(** ------------------------------ *)

module EqPairs = struct
  type t = texpr * texpr
  let compare (x0, y0) (x1, y1) =
    match Stdlib.compare x0 x1 with
    | 0 -> Stdlib.compare y0 y1
    | c -> c
end

module EqSet = Set.Make(EqPairs)

(** examples of sets of equations *)

(* {(int->a) -> (a->d) .=. c -> (b->b) -> c} *)
let eqs_ex1  =
  EqSet.singleton
    (FuncType(FuncType(IntType,TypeVar "a"), FuncType(TypeVar "a",TypeVar "d")),
   FuncType(TypeVar "c",FuncType(FuncType(TypeVar
                                            "b", TypeVar "b"),TypeVar "c")))

(* {a -> (b->a) .=. b -> ((a->int) -> a)} *)
let eqs_ex2 =
  EqSet.singleton
    (FuncType(TypeVar "a",FuncType(TypeVar "b", TypeVar "a")),
          FuncType(TypeVar "b",FuncType(FuncType(TypeVar "a",IntType),
                                        TypeVar "a")))
(* {a -> int .=. int -> b} *)
let eqs_ex3  =
  EqSet.singleton
    (FuncType(TypeVar "a",IntType),
     FuncType(IntType,TypeVar "b"))
    
let string_of_eq (x,y) =
  "<"^string_of_texpr x^ " .=. " ^string_of_texpr y^">"

(** ------------------------------ *)
(** substitutions                  *)
(** ------------------------------ *)
  
(** Note: the type [texpr SubsMap.t], representing maps from [String] to
    [texpr], is used to used to model both 
    1. Type substitutions  (maps from type variables to types); and
    2. Typing contexts     (maps from term variables to types).
*)  
module SubsMap = Map.Make(String)

let fresh_string =
  let c= ref 0
  in fun s ->
    c:=!c+1;
    s^string_of_int !c

let map_to_list subs =
  SubsMap.fold (fun k v l -> (k,v)::l) subs []
    
let string_of_subs subs =
  "["^
  (String.concat ", " @@
  List.map (fun (x,y) -> (x^" / "^string_of_texpr y)) @@
   map_to_list subs)
  ^"]"
      
let find_or_fresh id subs =
  match SubsMap.find_opt id subs with
   | None -> TypeVar (fresh_string "_")
   | Some ty -> ty

let subs_union s subs1 subs2 =
  SubsMap.(union (fun k v1 v2 -> if v1=v2 then Some v1
                                   else failwith @@ "subs_union: "^s^k^": "
                                        ^string_of_texpr v1^" and "
                                        ^string_of_texpr v2)
             subs1 subs2)
    
let rec apply_tsubs_to_type : texpr -> texpr SubsMap.t -> texpr =
  fun ty subs ->
  match ty with
  | TypeVar(id) ->
    (match SubsMap.find_opt id subs with
     | None -> ty
     | Some t -> t)
  | UserType(_) | IntType | BoolType | UnitType ->
    ty
  | FuncType(t1,t2) ->
    FuncType(apply_tsubs_to_type t1 subs,apply_tsubs_to_type t2 subs)
  | PairType(t1,t2) ->
    PairType(apply_tsubs_to_type t1 subs,apply_tsubs_to_type t2 subs)
  | RefType(t) ->
    RefType(apply_tsubs_to_type t subs)
  | ListType(t) ->
    ListType(apply_tsubs_to_type t subs)
  | TreeType(t) ->
    TreeType(apply_tsubs_to_type t subs)
  | StackType(t) ->
    StackType(apply_tsubs_to_type t subs)
  | SetType(t) ->
    SetType(apply_tsubs_to_type t subs)
  | QueueType(t) ->
    QueueType(apply_tsubs_to_type t subs)
  | HtblType(t1,t2) ->
    HtblType(apply_tsubs_to_type t1 subs,apply_tsubs_to_type t2 subs)
  | RecordType(fs) ->
    let (ids,tys) = List.split fs
    in let ts = List.map (Fun.flip apply_tsubs_to_type subs) tys 
    in RecordType (List.combine ids ts)
  | _ -> failwith "apply_tsubs_to_type: not implemented"

let apply_tsubs_to_context ctxt subs = 
  SubsMap.filter_map
       (fun _key v_ty ->
          Some (apply_tsubs_to_type v_ty subs))
       ctxt

let apply_tsubs_to_eqs eqs subs =
   EqSet.map (fun (x,y) ->
      apply_tsubs_to_type x subs,apply_tsubs_to_type y subs) eqs
     
let compose_tsubs subs1 subs2 = 
  SubsMap.union
    (fun _k v1 v2 ->
       if v1=v2 then Some v1
       else failwith "compose: mismatch")   
    (SubsMap.filter_map
       (fun _key v_ty ->
          Some (apply_tsubs_to_type v_ty subs2))
       subs1
    )
    subs2

(** [occurs id ty] returns true if [TypeVar id] occurs in the type [ty] *)
(* and false otherwise *)
let rec occurs : string -> texpr -> bool =
  fun id ty ->
  match ty with   
  | _ -> failwith "occurs: not implemented"

let rec unify_ : EqSet.t -> texpr SubsMap.t -> texpr SubsMap.t result =
  fun eqs ->
  failwith "implement me"

let unify : EqSet.t -> texpr SubsMap.t result =
  fun eqs -> 
  unify_ eqs SubsMap.empty

let string_of_unify eqs =
  match unify_ eqs SubsMap.empty with
  | Error s -> "error: "^s
  | Ok type_subs -> string_of_subs type_subs




