(*******************************************************************
    LL(1) parser generator, and parse tree builder for an extended
    calculator language.

    Author: Michael L. Scott

    This file can be "#use"-ed (or compiled and then "#load"-ed)
    into the top-level interpreter,
 *******************************************************************)

open List;;
(* The List library includes a large collection of useful functions.
   In the provided code, I've used assoc, exists, filter, find,
   fold_left, hd, length, map, and rev
*)

open Str;;      (* for split *)
(* The Str library provides a few extra string-processing routines.
   In particular, it provides "split", which I use to break program
   input into whitespace-separated words.  Note, however, that this
   library is not automatically available.  If you are using the
   top-level interpreter, you have to say
        #load "str.cma";;
   If you are generating an executable fron the shell, you have to say
        ocamlc str.cma interpreter.ml
*)

(* Surprisingly, compose isn't built in.  It's included in various
   widely used commercial packages, but not in the core libraries. *)
let compose f g x = f (g x);;

type symbol_productions = (string * string list list);;
type grammar = symbol_productions list;;
type parse_table = (string * (string list * string list) list) list;;
(*                  nt        predict_set   rhs *)

let calc_gram : grammar =
  [ ("P",  [["SL"; "$$"]])
  ; ("SL", [["S"; "SL"]; []])
  ; ("S",  [ ["id"; ":="; "E"]; ["read"; "id"]; ["write"; "E"]])
  ; ("E",  [["T"; "TT"]])
  ; ("T",  [["F"; "FT"]])
  ; ("TT", [["ao"; "T"; "TT"]; []])
  ; ("FT", [["mo"; "F"; "FT"]; []])
  ; ("ao", [["+"]; ["-"]])
  ; ("mo", [["*"]; ["/"]])
  ; ("F",  [["id"]; ["num"]; ["("; "E"; ")"]])
  ];;

let ecg : grammar =
  [ ("P",  [["SL"; "$$"]])
  ; ("SL", [["S"; "SL"]; []])
  ; ("S",  [ ["id"; ":="; "E"]; ["read"; "id"]; ["write"; "E"]
           ; ["if"; "R"; "SL"; "fi"]; ["do"; "SL"; "od"]; ["check"; "R"]
           ])
  ; ("R",  [["E"; "ET"]])
  ; ("E",  [["T"; "TT"]])
  ; ("T",  [["F"; "FT"]])
  ; ("F",  [["id"]; ["num"]; ["("; "E"; ")"]])
  ; ("ET", [["ro"; "E"]; []])
  ; ("TT", [["ao"; "T"; "TT"]; []])
  ; ("FT", [["mo"; "F"; "FT"]; []])
  ; ("ro", [["=="]; ["<>"]; ["<"]; [">"]; ["<="]; [">="]])
  ; ("ao", [["+"]; ["-"]])
  ; ("mo", [["*"]; ["/"]])
  ];;

(* is e a member of list l? *)
let member e l = exists ((=) e) l;;

(* OCaml has a built-in quicksort; this was just an exercise. *)
let rec sort l =
  let rec partition pivot l left right =
    match l with
      | []        -> (left, right)
      | c :: rest -> if (compare c pivot) < 0
          then partition pivot rest (c :: left) right
          else partition pivot rest left (c :: right) in
    match l with
      | []        -> l
      | h :: []   -> l
      | h :: rest -> let (left, right) = partition h rest [] [] in
            (sort left) @ [h] @ (sort right);;

(* leave only one of any consecutive identical elements in list *)
let rec unique l =
  match l with
    | []             -> l
    | h :: []        -> l
    | a :: b :: rest -> if a = b (* structural equivalence *)
        then unique (b :: rest)
        else a :: unique (b :: rest);;

let unique_sort l = unique (sort l);;

(* Return all individual productions in grammar. *)
let productions gram : (string * string list) list =
  let prods (lhs, rhss) = map (fun rhs -> (lhs, rhs)) rhss in
    fold_left (@) [] (map prods gram);;

(* Return all symbols in grammar. *)
let gsymbols gram : string list =
  unique_sort
    (fold_left (@) [] (map (compose (fold_left (@) []) snd) gram));;

(* Return all elements of l that are not in excluded.
   Assume that both lists are sorted *)
let list_minus l excluded =
  let rec helper rest te rtn =
    match rest with
      | []     -> rtn
      | h :: t -> match te with
        | []       -> (rev rest) @ rtn
        | h2 :: t2 -> match compare h h2 with
          | -1 -> helper t te (h :: rtn)
          |  0 -> helper t t2 rtn
          |  _ -> helper rest t2 rtn in
    rev (helper l excluded []);;

(* Return just the nonterminals. *)
let nonterminals gram : string list = map fst gram;;

(* Return just the terminals. *)
let terminals gram : string list =
  list_minus (gsymbols gram) (unique_sort (nonterminals gram));;

(* Return the start symbol.  Throw exception if grammar is empty. *)
let start_symbol gram : string = fst (hd gram);;

let is_nonterminal e gram = member e (nonterminals gram);;

let is_terminal e gram = member e (terminals gram);;

let union s1 s2 = unique_sort (s1 @ s2);;

(* return suffix of lst that begins with first occurrence of sym
   (or [] if there is no such occurrence) *)
let rec suffix sym lst = 
  match lst with
    | [] -> []
    | h :: t -> if h = sym (* structural equivalence *)
        then lst else suffix sym t;;

(* Return a list of pairs.
   Each pair consists of a symbol A and a list of symbols beta
   such that for some alpha, A -> alpha B beta. *)
type right_context = (string * string list) list;;
let get_right_context (b:string) gram : right_context =
  fold_left (@) []
    (map (fun prod ->
            let a = fst prod in
            let rec helper accum rhs =
              let b_beta = suffix b rhs in
                match b_beta with
                  | [] -> accum
                  | x :: beta  -> (* assert x = b *)
                      helper ((a, beta) :: accum) beta in
              helper [] (snd prod))
        (productions gram));;

(* parser generator starts here *)

type symbol_knowledge = string * bool * string list * string list;;
type knowledge = symbol_knowledge list;;
let symbol_field (s, e, fi, fo) = s;;
let eps_field (s, e, fi, fo) = e;;
let first_field (s, e, fi, fo) = fi;;
let follow_field (s, e, fi, fo) = fo;;

let initial_knowledge gram : knowledge =
  map (fun a -> (a, false, [], [])) (nonterminals gram);;

let get_symbol_knowledge (a:string) (kdg:knowledge) : symbol_knowledge =
  find (fun (s, e, fi, fo) -> s = a) kdg;;

(* Can word list w generate epsilon based on current estimates?
   if w is an empty list, yes
   if w is a single terminal, no
   if w is a single nonterminal, look it up
   if w is a non-empty list, "iterate" over elements *)
let rec generates_epsilon (w:string list) (kdg:knowledge) gram : bool =
  match w with
    | [] -> true
    | h :: t -> if is_terminal h gram then false
        else eps_field (get_symbol_knowledge h kdg)
             && generates_epsilon t kdg gram;;

(* Return FIRST(w), based on current estimates.
   if w is an empty list, return []  [empty set]
   if w is a single terminal, return [w]
   if w is a single nonterminal, look it up
   if w is a non-empty list, "iterate" over elements *)
let rec first (w:string list) (kdg:knowledge) gram : (string list) =
  match w with
    | [] -> []
    | x :: _ when is_terminal x gram -> [x]
    | x :: rest -> let s = first_field (get_symbol_knowledge x kdg) in
          if generates_epsilon [x] kdg gram
          then union s (first rest kdg gram)
          else s;;

let follow (a:string) (kdg:knowledge) : string list =
  follow_field (get_symbol_knowledge a kdg);;

let rec map3 f l1 l2 l3 =
  match (l1, l2, l3) with
    | ([], [], []) -> []
    | (h1 :: t1, h2 :: t2, h3 :: t3) -> (f h1 h2 h3) :: map3 f t1 t2 t3
    | _ -> raise (Failure "mismatched_lists");;

(* Return knowledge structure for grammar.
   Start with (initial_knowledge grammar) and "iterate",
   until the structure doesn't change.
   Uses (get_right_context B gram), for all nonterminals B,
   to help compute follow sets. *)
let get_knowledge gram : knowledge =
  let nts : string list = nonterminals gram in
  let right_contexts : right_context list
    = map (fun s -> get_right_context s gram) nts in
  let rec helper kdg =
    let update : symbol_knowledge -> symbol_productions
      -> right_context -> symbol_knowledge
      = fun old_sym_kdg sym_prods sym_right_context ->
        let my_first s = first s kdg gram in
        let my_eps s = generates_epsilon s kdg gram in
        let filtered_follow p = if my_eps (snd p)
          then (follow (fst p) kdg)
          else [] in
          (
            symbol_field old_sym_kdg,       (* nonterminal itself *)
            (eps_field old_sym_kdg)         (* previous estimate *)
            || (fold_left (||) false (map my_eps (snd sym_prods))),
            union (first_field old_sym_kdg) (* previous estimate *)
              (fold_left union [] (map my_first (snd sym_prods))),
            union (union
                     (follow_field old_sym_kdg)  (* previous estimate *)
                     (fold_left union [] (map my_first
                                            (map (fun p ->
                                                    match snd p with
                                                      | [] -> []
                                                      | h :: t -> [h])
                                                sym_right_context))))
              (fold_left union [] (map filtered_follow sym_right_context))
          ) in    (* end of update *)
    let new_kdg = map3 update kdg gram right_contexts in
      (* body of helper: *)
      if new_kdg = kdg then kdg else helper new_kdg in
    (* body of get_knowledge: *)
    helper (initial_knowledge gram);;

let get_parse_table (gram:grammar) : parse_table =
  let kdg = get_knowledge gram in
    map (fun (lhs, rhss) ->
          (lhs, (map (fun rhs ->
                        (union (first rhs kdg gram)
                           (if (generates_epsilon rhs kdg gram)
                            then (follow lhs kdg) else []),
                         rhs))
                    rhss)))
      gram;;

(* convert string to list of char *)
let explode (s:string) : char list =
  let rec exp i l = if i < 0 then l else exp (i-1) (s.[i] :: l) in
    exp (String.length s - 1) [];;

(* convert list of char to string
   (This uses imperative features.  It used to be a built-in.) *)
let implode (l:char list) : string =
  let res = Bytes.create (length l) in
  let rec imp i l =
    match l with
      | [] -> Bytes.to_string res
      | c :: l -> Bytes.set res i c; imp (i + 1) l in
    imp 0 l;;

(*******************************************************************
   scanner:
 *******************************************************************)

type token = string * string;;
(*         category * name *)

let tokenize (program:string) : token list =
  let get_id prog =
    let rec gi tok p =
      match p with
        | c :: rest when (('a' <= c && c <= 'z')
                          || ('A' <= c && c <= 'Z')
                          || ('0' <= c && c <= '9') || (c = '_'))
          -> gi (c :: tok) rest
        | _ -> (implode (rev tok), p) in
      gi [] prog in
  let get_int prog =
    let rec gi tok p =
      match p with
        | c :: rest when ('0' <= c && c <= '9')
          -> gi (c :: tok) rest
        | _ -> (implode (rev tok), p) in
      gi [] prog in
  let get_num prog =
    let (tok, rest) = get_int prog in
      match rest with
        | '.' :: c :: r when ('0' <= c && c <= '9')
          -> let (tok2, rest2) = get_int (c :: r) in
              ((tok ^ "." ^ tok2), rest2)
        | _ -> (tok, rest) in
  let rec get_error tok prog =
    match prog with
      | [] -> (implode (rev tok), prog)
      | c :: rest ->
          match c with
            | ':' | '+' | '-' | '*' | '/' | '(' | ')' | '_'
            | '<' | '>' | '=' | 'a'..'z' | 'A'..'Z' | '0'..'9'
              -> (implode (rev tok), prog)
            | _ -> get_error (c :: tok) rest in
  let rec skip_space prog =
    match prog with
      | [] -> []
      | c :: rest -> if (c = ' ' || c = '\n' || c = '\r' || c = '\t')
          then skip_space rest else prog in
  let rec tkize toks prog =
    match prog with
      | []                 -> (("$$" :: toks), [])
      | '\n' :: rest
      | '\r' :: rest
      | '\t' :: rest
      | ' ' :: rest        -> tkize toks (skip_space prog)
      | ':' :: '=' :: rest -> tkize (":=" :: toks) rest
      | '-' :: rest        -> tkize ("-"  :: toks) rest
      | '+' :: rest        -> tkize ("+"  :: toks) rest
      | '/' :: rest        -> tkize ("/"  :: toks) rest
      | '*' :: rest        -> tkize ("*"  :: toks) rest
      | '(' :: rest        -> tkize ("("  :: toks) rest
      | ')' :: rest        -> tkize (")"  :: toks) rest
      | '<' :: '>' :: rest -> tkize ("<>" :: toks) rest
      | '<' :: '=' :: rest -> tkize ("<=" :: toks) rest
      | '<' :: rest        -> tkize ("<"  :: toks) rest
      | '>' :: '=' :: rest -> tkize (">=" :: toks) rest
      | '>' :: rest        -> tkize (">"  :: toks) rest
      | '=' :: '=' :: rest -> tkize ("==" :: toks) rest
      | h :: t -> match h with
        | '0'..'9' -> let (t, rest) = get_num prog in
              tkize (t :: toks) rest
        | 'a'..'z'
        | 'A'..'Z'
        | '_'      -> let (t, rest) = get_id prog in
              tkize (t :: toks) rest
        | c        -> let (t, rest) = get_error [c] t in
              tkize (t :: toks) rest in
  let (toks, _) = (tkize [] (explode program)) in
  let categorize tok =
    match tok with
      | "check" | "do" | "fi"
      | "if"    | "od" | "read" | "write"
      | ":=" | "+"  | "-"  | "*"  | "/"  | "("  | ")"
      | "<"  | "<=" | ">"  | ">=" | "==" | "<>" | "$$"
        -> (tok, tok)
      | _ -> match tok.[0] with
        | '0'..'9' -> ("num", tok)
        | 'a'..'z'
        | 'A'..'Z' | '_' -> ("id", tok)
        | _ -> ("error", tok) in
    map categorize (rev toks);;

(*******************************************************************
   The main parse routine below returns a parse tree (or PT_error if
   the input program is syntactically invalid).  To build that tree it
   employs a simplified version of the "attribute stack" described in
   Section 4.5.2 (pages 50-55) on the PLP companion site.

   When it predicts A -> B C D, the parser pops A from the parse stack
   and then, before pushing D, C, and B (in that order), it pushes a
   number (in this case 3) indicating the length of the right hand side.
   It also pushes A into the attribute stack.

   When it matches a token, the parser pushes this into the attribute
   stack as well.

   Finally, when it encounters a number (say k) in the stack (as opposed
   to a character string), the parser pops k+1 symbols from the
   attribute stack, joins them together into a list, and pushes the list
   back into the attribute stack.

   These rules suffice to accumulate a complete parse tree into the
   attribute stack at the end of the parse.

   Note that everything is done functionally.  We don't really modify
   the stacks; we pass new versions to a tail recursive routine.
 *******************************************************************)

(* Extract grammar from parse-tab, so we can invoke the various routines
   that expect a grammar as argument. *)
let grammar_of (parse_tab:parse_table) : grammar =
  map (fun p -> (fst p, (fold_left (@) [] (map (fun (a, b) -> [b])
                                             (snd p))))) parse_tab;;

type parse_tree =   (* among other things, parse_trees are *)
    | PT_error          (* the elements of the attribute stack *)
    | PT_id of string
    | PT_num of string
    | PT_term of string
    | PT_nt of (string * parse_tree list);;

(* Pop rhs-len + 1 symbols off the attribute stack,
   assemble into a production, and push back onto the stack. *)
let reduce_1_prod (astack:parse_tree list) (rhs_len:int) : parse_tree list =
  let rec helper atk k prod =
    match (k, atk) with
      | (0, PT_nt(nt, []) :: rest) -> PT_nt(nt, prod) :: rest
      | (n, h :: rest) when n <> 0 -> helper rest (k - 1) (h :: prod)
      | _ -> raise (Failure "expected nonterminal at top of astack") in
    helper astack rhs_len [];;

type parse_action = PA_error | PA_prediction of string list;;
(* Double-index to find prediction (list of RHS symbols) for
   nonterminal nt and terminal t.
   Return PA_error if not found. *)
let get_parse_action (nt:string) (t:string) (parse_tab:parse_table) : parse_action =
  let rec helper l =
    match l with
      | [] -> PA_error
      | (fs, rhs) :: rest -> if member t fs then PA_prediction(rhs)
          else helper rest in
    helper (assoc nt parse_tab);;

type ps_item =
    | PS_end of int
    | PS_sym of string;;

(* Parse program according to grammar.
   [Commented-out code would
       print predictions and matches (imperatively) along the way.]
   Return parse tree if the program is in the language; PT_error if it's not. *)
let parse (parse_tab:parse_table) (program:string) : parse_tree =
  let die msg = begin
    print_string ("syntax error: " ^ msg);
    print_newline ();
    PT_error 
  end in
  let gram = grammar_of parse_tab in
  let rec helper pstack tokens astack =
    match pstack with
      | [] ->
          if tokens = [] then
            (* assert: astack is nonempty *)
            hd astack
          else die "extra input beyond end of program"
      | PS_end(n) :: ps_tail ->
          helper ps_tail tokens (reduce_1_prod astack n)
      | PS_sym(tos) :: ps_tail ->
          match tokens with
            | [] -> die "unexpected end of program"
            | (term, tok) :: more_tokens ->
                (* if tok is an individual identifier or number,
                   term will be a generic "id" or "num" *)
                if is_terminal tos gram then
                  if tos = term then
                    begin
              (*
                print_string ("   match " ^ tos);
                print_string
                    (if tos <> term      (* value comparison *)
                         then (" (" ^ tok ^ ")") else "");
                print_newline ();
              *)
                      helper ps_tail more_tokens
                        (( match term with
                            | "id"  -> PT_id tok
                            | "num" -> PT_num tok
                            | _     -> PT_term tok ) :: astack)
                    end
                    (* note push of tok into astack *)
                  else die ("expected " ^ tos ^ " ; saw " ^ tok)
                else (* nonterminal *)
                  match get_parse_action tos term parse_tab with
                    | PA_error -> die ("no prediction for " ^ tos
                                       ^ " when seeing " ^ tok)
                    | PA_prediction(rhs) ->
                        begin
                (*
                  print_string ("   predict " ^ tos ^ " ->");
                  print_string (fold_left (fun a b -> a ^ " " ^ b) "" rhs);
                  print_newline ();
                *)
                          helper ((fold_left (@) [] 
                                     (map (fun s -> [PS_sym(s)]) rhs))
                                  @ [PS_end(length rhs)] @ ps_tail)
                            tokens (PT_nt(tos, []) :: astack)
                        end in
    helper [PS_sym(start_symbol gram)] (tokenize program) [];;

let cg_parse_table = get_parse_table calc_gram;;

let ecg_parse_table = get_parse_table ecg;;

(*******************************************************************
   translator:
 *******************************************************************)

(*******************************************************************
   This file ("translator.ml") provides stub functions for the extra
   code you need to write for this assignment.

   There are two major stub functions in "translator.ml":
   "ast_ize_P" (which transforms a parse tree for a program into
                an abstract syntax tree[AST]); and
   "translate" (which translates an AST into an equivalent C program).

   You are also being provided with a file called "parser.ml" which
   contains working code to produce a parse tree for a program.
   Everything in the file "parser.ml" is complete and usable as-is.

   The major entry point for the parser provided in "parser.ml" is
   a function called "parse" invoked with two parameters: A parse table,
   and a string containing a calculator language program.

   The file "parser.ml" constructs two example parse tables:
   "cg_parse_table" (for the original calculator language grammar), and
   "ecg_parse_table" (for the extended calculator language grammar).

   Here are two parser examples which will work as-is:

      parse cg_parse_table sum_ave_prog;;
      parse ecg_parse_table primes_prog;;
   		
   "sum_ave_prog" and "primes_prog" are provided at the end of this
   file (as strings). "sum_ave_prog" uses the original calculator
   language grammar, while "primes_prog" uses the extended calculator
   language grammar.

   When complete, your translator code should work when invoked as
   follows:

      print_string
        (snd
          (translate
            (ast_ize_P
              (parse ecg_parse_table primes_prog)
            )
          )
        );;
   	
   The result of this invocation should be a C program on standard
   output, which can be compiled and run to produce the first N primes
   (where N is a number typed by the user).
 *******************************************************************)

(*******************************************************************
   Declarations and functions to transform a parse tree into an
   abstract syntax tree (AST)
 *******************************************************************)
 
open Hashtbl;;
let symTab = Hashtbl.create 10;;


type ast_sl = ast_s list
and ast_s =
    | AST_error
    | AST_assign of (string * ast_e)
    | AST_read of string
    | AST_write of ast_e
    | AST_if of (ast_e * ast_sl)
    | AST_do of ast_sl
    | AST_check of ast_e
and ast_e =
    | AST_binop of (string * ast_e * ast_e)
    | AST_id of string
    | AST_num of string;;

let rec ast_ize_P (p:parse_tree) : ast_sl =
  match p with
    | PT_nt ("P", [sl; PT_term "$$"]) -> ast_ize_SL sl
    | _ -> raise (Failure "malformed parse tree in ast_ize_P")

and ast_ize_SL (sl:parse_tree) : ast_sl =
  match sl with
    | PT_nt ("SL", []) -> []	
    | PT_nt ("SL", [s; sl])-> (ast_ize_S s :: ast_ize_SL sl)
    | _ -> raise (Failure "malformed parse tree in ast_ize_SL")

and ast_ize_S (s:parse_tree) : ast_s =
  match s with
    | PT_nt ("S", [PT_id lhs; PT_term ":="; expr])
      -> AST_assign (lhs, (ast_ize_expr expr))
    | PT_nt ("S", [PT_term "write"; expr])
      -> AST_write (ast_ize_expr expr)
    | PT_nt ("S", [PT_term "if"; r ; sl; PT_term "fi"])
      -> AST_if ((ast_ize_expr r), (ast_ize_SL sl))
    | PT_nt ("S", [PT_term "read"; PT_id id])
      -> AST_read id
    | PT_nt ("S", [PT_term "check"; r ;])
      -> AST_check (ast_ize_expr r)
    | PT_nt ("S", [PT_term "do"; sl; PT_term "od"])
      -> AST_do (ast_ize_SL sl)
    | _ -> raise (Failure "malformed parse tree in ast_ize_S")

and ast_ize_expr (e:parse_tree) : ast_e =
(* e is an R, E, T, or F parse tree node *)
  match e with
    | PT_nt ("T", [f; ft])-> (ast_ize_expr_tail (ast_ize_expr f) ft)
    | PT_nt ("F", [PT_id id]) -> AST_id id
    | PT_nt ("E", [t; tt])-> (ast_ize_expr_tail (ast_ize_expr t) tt)
    | PT_nt ("F", [PT_term "("; expr; PT_term ")"])-> ast_ize_expr expr
    | PT_nt ("R", [e; et]) -> (ast_ize_reln_tail (ast_ize_expr e) et)
    | PT_nt ("F", [PT_num num])-> AST_num num
    | _ -> raise (Failure "malformed parse tree in ast_ize_expr")

and ast_ize_reln_tail (lhs:ast_e) (tail:parse_tree) : ast_e =
(* lhs in an inheritec attribute.
     tail is an ET parse tree node *)
  match tail with
    | PT_nt ("ET", []) -> lhs
    | PT_nt ("ET", [ro; expr])-> AST_binop (get_operator ro, lhs, (ast_ize_expr expr))
    | _ -> raise (Failure "malformed parse tree in ast_ize_reln_tail")

and ast_ize_expr_tail (lhs:ast_e) (tail:parse_tree) : ast_e =
  (* lhs in an inherited attribute.
     tail is a TT or FT parse tree node *)
  match tail with
    | PT_nt ("ET", []) -> lhs
    | PT_nt ("TT", [ao; t; tt]) -> AST_binop (get_operator ao, lhs, ast_ize_expr_tail (ast_ize_expr t) tt)
    | PT_nt ("ET", [PT_term ro; expr])-> AST_binop (ro, lhs, (ast_ize_expr expr))
    | PT_nt ("FT", []) -> lhs
    | PT_nt ("TT", []) -> lhs
    | PT_nt ("FT", [mo; f; ft])-> AST_binop (get_operator mo, lhs, ast_ize_expr_tail (ast_ize_expr f) ft)
    | PT_num n -> AST_num n
    | _ -> raise (Failure "malformed parse tree in ast_ize_expr_tail")

(* check get_operator  for a, m, r*)
and get_operator o : string = 
  match o with
  | PT_nt ("ao", [PT_term a]) -> a
  | PT_nt ("ro", [PT_term r]) -> r
  | PT_nt ("mo", [PT_term m]) -> m
  | _ -> raise (Failure "malformed parse tree in get_operator");;

(*keep track of already used*)
let alreadyUsed id =
  try let x = Hashtbl.find symTab id in
    if snd x != true then (Hashtbl.replace symTab id (fst x, true); ())
  with doesNotExist -> ();;


(*figure out id*)
let readId id : string =
  match id with 
  | AST_id n  -> (alreadyUsed n; n) 
  | AST_num n -> n 
  | _ -> "";;


(*******************************************************************
   Functions to transform an abstract syntax tree to C code
 *******************************************************************)
(* The code below is (obviously) a bare stub. The intent is that when
   you run translate on a full, correct AST, you'll get back code for an
   equivalent C program. If there are any variables that are written in
   the program but never read, you'll also get a warning message
   indicating their names and the lines on which the writes occur. Your
   C program should contain code to check for dynamic semantic errors. *)


(*print ids and operator*)
let printBinOp (oper, first, second) = 
  (readId first) ^ " " ^ oper ^ " " ^ (readId second);;

(*convert it to a list*)
let hashToList h = 
  Hashtbl.fold (fun x y a -> (x, y) :: a) h [];;


(* match string in hastable *)
let findSymTab (id:string): bool =
  try match Hashtbl.find symTab id with
  | _ -> true 
  with 
  | doesNotExist -> false;;


(*match e*)
let valId (e:ast_e): bool = 
  match e with 
  | AST_id id -> true 
  | _ -> false;;



(* recursive warnings *)
let rec generateWarnings w =
  match w with
  | [] -> ""
  | (id, (_, false)) :: e -> ("Warning: " ^ id ^ "unused\n\n") ^ generateWarnings e
  | _ :: e -> generateWarnings e
;;


let rec translate (ast:ast_sl) : string * string =
  let (err, c) = translate_sl ast in
    (
      (err ^ (generateWarnings (hashToList symTab)), 
	(*include header and getint/putint and outline for main*)
          "#include <stdio.h>\n#include <stdlib.h>\n\nint getint(){\nint n;\nscanf(\"%d\", &n);\nreturn n;\n}\n"
          ^ "\nvoid putint (int n) {\nprintf(\"%d\\n\", n);\n}\n"
          ^ "\nint main (int argc, char*, arg[v]) {\n" ^ c ^ "return 0;\n\n}\n")
    )

and translate_sl (ast:ast_sl) : string * string =
  match ast with
  | [] -> ("", "")
  | _  -> let (e1, code1) = translate_s (hd ast) 
          and (e2, code2) = translate_sl (tl ast)
            in (e1 ^ e2, code1 ^ code2)

and translate_s (ast:ast_s) : string * string =
  match ast with
  | AST_read  n   -> let (err, c) = translate_read n in (err, c)
  | AST_write n   -> let (err, c) = translate_write n in (err, c)
  | AST_if n      -> let (err, c) = translate_if n in (err, c)
  | AST_assign n  -> let (err, c) = translate_assign n in (err, c)
  | AST_do n      -> let (err, c) = translate_do n in (err, c)
  | AST_check n   -> let (err, c) = translate_expr n in (err, c)
  | _ -> ("", "")

 and translate_assign (a) : string * string =
  match a with
  | (id, e) -> let (err, c) = translate_expr e in
    if findSymTab id then (err, id ^ " = " ^ c ^ ";\n")
    else 
      (Hashtbl.add symTab id (true, true);
      (err, "int " ^ id ^ " = " ^ c ^ ";\n"))

and translate_read (n:string) : string * string = 
    (Hashtbl.add symTab n (true, false);
    ("", "int " ^ n ^ "; \n" ^ n ^ " = getint();\n"))

and translate_write (w:ast_e) : string * string =
  let (err, c) = translate_expr w in (err, "putint(" ^ c ^ ");\n")

and translate_if (n:ast_e * ast_sl) : string * string =
  let (e1, c1) = translate_expr (fst n)
  and (e2, c2) = translate_sl (snd n)
    in (e1 ^ e2, "if (" ^ c1 ^ ") {\n" ^ c2 ^ "}\n")


(*c code for do while*)
and translate_do (n:ast_sl) : string * string =
  let (e1, c1) = translate_sl (tl n) 
  and (e2, c2) = translate_check (hd n)
    in (e1 ^ e2, "\ndo {\n" ^ c1 ^ "\n} while (" ^ c2 ^ ");\n\n")

and translate_check (n:ast_s) : string * string =
  match n with
  | AST_check x -> let (err, c)= translate_expr x in (err, c)
  | _ -> ("ERROR\n", "")

and translate_expr (n:ast_e) : string * string =
  match n with
  | AST_id id ->  if not (findSymTab id) then
          (print_string ("Error: expected number / uninitialized:" ^ id ^ "\n"); exit 0;)
        else ("", id)
  | AST_num num -> ("", num)
  | AST_binop (op, first, second) (* check for diving by 0 and unitialized values*)
    ->  if (second = (AST_num "0") && op = "/") then
            (print_string ("Error: no div by 0: \n"); exit 0;)
        else if ( (valId first) && not ( findSymTab (readId first) ) ) then 
            (print_string ("Error:  expected number / uninitialized:" ^ (readId first) ^ " in: " ^ printBinOp (op, first, second) ^ "\n"); exit 0;)
        else if ( (valId second) && not ( findSymTab (readId second) ) ) then 
            (print_string ("Error:  expected number / uninitialized:" ^ (readId second) ^ " in: " ^ printBinOp (op, first, second) ^  "\n"); exit 0;)
        else ("", (readId first) ^ " " ^ op ^ " " ^ (readId second) ^ "")

;;

(*******************************************************************
   Sample programs in the calculator language
 *******************************************************************)

let sum_ave_prog = "
  read a
  read b
  read c
  sum := a + b
  write sum
  write sum / 2";;
	 
let primes_prog = "
  read n
  cp := 2
  do check n > 0
    found := 0
    cf1 := 2
    cf1s := cf1 * cf1
    do check cf1s <= cp
      cf2 := 2
      pr := cf1 * cf2
      do check pr <= cp
        if pr == cp
          found := 1
        fi
        cf2 := cf2 + 1
        pr := cf1 * cf2
      od
      cf1 := cf1 + 1
      cf1s := cf1 * cf1
    od
    if found == 0
      write cp
      n := n - 1
    fi
    cp := cp + 1
  od";;
