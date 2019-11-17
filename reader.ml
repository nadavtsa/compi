
#use "pc.ml";;
open PC;;

exception X_not_yet_implemented;;
exception X_this_should_not_happen;;
  
type number =
  | Int of int
  | Float of float;;
  
type sexpr =
  | Bool of bool
  | Nil
  | Number of number
  | Char of char
  | String of string
  | Symbol of string
  | Pair of sexpr * sexpr
  | TaggedSexpr of string * sexpr
  | TagRef of string;;

let rec sexpr_eq s1 s2 =
  match s1, s2 with
  | Bool(b1), Bool(b2) -> b1 = b2
  | Nil, Nil -> true
  | Number(Float f1), Number(Float f2) -> abs_float(f1 -. f2) < 0.001
  | Number(Int n1), Number(Int n2) -> n1 = n2
  | Char(c1), Char(c2) -> c1 = c2
  | String(s1), String(s2) -> s1 = s2
  | Symbol(s1), Symbol(s2) -> s1 = s2
  | Pair(car1, cdr1), Pair(car2, cdr2) -> (sexpr_eq car1 car2) && (sexpr_eq cdr1 cdr2)
  | TaggedSexpr(name1, expr1), TaggedSexpr(name2, expr2) -> (name1 = name2) && (sexpr_eq expr1 expr2) 
  | TagRef(name1), TagRef(name2) -> name1 = name2
  | _ -> false;;

module Reader = struct
(*module Reader: sig
  val read_sexpr : string -> sexpr
  val read_sexprs : string -> sexpr list
  val is_boolean : string -> bool
  val const2 : ('a -> bool) -> 'a list -> 'a * 'a list 
  val nt_a : char list -> char * char list
  val nt_c : char list -> char * char list
  val nt_whitespace : char list -> char * char list
  val make_nt_a_b : (char list -> 'a * char list) -> char list -> 'a * char list
end
= struct
open PC;;*)
let normalize_scheme_symbol str =
  let s = string_to_list str in
  if (andmap
	(fun ch -> (ch = (lowercase_ascii ch)))
	s) then str
  else Printf.sprintf "|%s|" str;;


(*my functions*)
let is_boolean str = 
  match str with
  | "#T" -> true
  | "#t" -> true
  | "#F" -> true
  | "#f" -> true
  | _ -> false

let make_paired nt_left nt_right nt =
  let nt = caten nt_left nt in
  let nt = pack nt (function (_, e) -> e) in
  let nt = caten nt nt_right in
  let nt = pack nt (function (e, _) -> e) in
  nt;;

let const2 pred =
  function 
  | [] -> raise X_no_match
  | e :: s ->
     if (pred e) then (e, s)
     else raise X_no_match;;

let nt_a = const2 (fun ch -> ch == 'a');;

let nt_b = const2 (fun ch -> ch == 'b');;

let nt_c = const2 (fun ch -> ch == 'c');;

let nt_whitespace = const2 (fun ch -> ch <= ' ');;

let make_nt_a_b nt = make_paired (nt_a) (nt_b) nt;;

(*let my_nt_base_12 = 
  let make_nt ch_from ch_to dis = 
    let nt = const2 (fun ch -> ch_from <= ch && ch <= ch_to) in
    let nt = pack nt (let delta = (Char.code ch_from) - dis in
                                           fun ch -> (Char.code ch) - delta) in
    nt in
let nt = disj (make_nt '0' '9' 0) (make_nt 'a' 'b' 10) in
let nt = disj nt (make_nt 'A' 'F' 10)*)

let nt_boolean = 
  let nt_hash = char '#' in
  let nt_f_t = disj (char 'f') (char 't') in
  let nt = caten (nt_hash) (nt_f_t) in
  let last_nt s = (match (nt s) with
          | (('#', 'f'), _) -> Bool(false)
          | (('#', 't'), _) -> Bool(true)
          | _ -> raise X_no_match) in 
  last_nt;;

(*let nt_boolean = 
  let nt_hash = char '#' in
  let nt_f = char 'f' in
  let nt_t = char 't' in
  let nt = caten (caten (star nt_whitespace) (disj nt_f nt_t)) (star nt_whitespace) in
  let last_nt s = match (nt s) with *)







(*end of my functions*)
let read_sexpr string = raise X_not_yet_implemented ;;

let read_sexprs string = raise X_not_yet_implemented;;
  
end;; (* struct Reader *)
