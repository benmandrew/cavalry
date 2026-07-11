open Core

(* A source location: the start position of a syntactic construct, enough to
   print [file:line:col] and to build a Why3 [Loc.position] for the verifier to
   carry through the WLP. Only the *start* is kept -- error messages point at
   where a construct begins, which is what a user scans for. *)
type t = { file : string; line : int; col : int } [@@deriving sexp_of, show]

(* Build a location from a Menhir [$loc] span (a start/end [Lexing.position]
   pair); only the start is retained. Requires the lexer to track newlines
   ([Lexing.new_line]) for [pos_lnum]/[pos_bol] to be accurate. *)
let of_span ((start, _) : Lexing.position * Lexing.position) : t =
  {
    file = start.Lexing.pos_fname;
    line = start.Lexing.pos_lnum;
    (* 1-based column, matching the [file:line:col] convention of gcc/rust and
       most editors ([pos_cnum - pos_bol] is 0-based). *)
    col = start.Lexing.pos_cnum - start.Lexing.pos_bol + 1;
  }

let to_string { file; line; col } =
  if String.length file = 0 then Printf.sprintf "%d:%d" line col
  else Printf.sprintf "%s:%d:%d" file line col

(* The verifier tags each proof obligation's Why3 term with this position, so a
   failing (split) subgoal can be traced back to source. Why3 wants a range; a
   degenerate start=end range is fine for a point diagnostic. *)
let to_why3 { file; line; col } = Why3.Loc.user_position file line col line col

let of_why3 (p : Why3.Loc.position) : t =
  let file, line, col, _, _ = Why3.Loc.get p in
  { file; line; col }
