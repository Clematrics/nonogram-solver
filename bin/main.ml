open Z3.Boolean
open Z3.Solver
open Z3.Arithmetic
open Z3.Arithmetic.Integer

exception TestFailedException of string

let rec window f = function
  a::b::l -> f a b; window f (b::l)
| _ -> ()

type nonogram = (int list array) * (int list array)

let cell x y = Format.sprintf "cell-%d-%d" x y

let chunk line_kind m n = Format.sprintf "%s-%d-%d" line_kind m n

let make_smt2 solver ctx (row_cells, col_cells) (chunks_rows, chunks_cols) =
  (* Writing constraints *)
  let make_constraints line chunks =
	  let line_length = Array.length line in
	  List.iter (fun (chunk_var, chunk_size) ->
      add solver [ mk_ge ctx chunk_var (mk_numeral_i ctx 0) ];
      add solver [ mk_le ctx (mk_add ctx [chunk_var; mk_numeral_i ctx chunk_size]) (mk_numeral_i ctx line_length) ];
	    (* fprintf fmt "(assert (>= %s 0))@;" chunk_var;
	    fprintf fmt "(assert (<= (+ %s %d) %d))@;" chunk_var chunk_size line_length *)
    ) chunks;
    window (fun (chunk_a, chunk_a_size) (chunk_b, _) ->
        add solver [ mk_lt ctx (mk_add ctx [chunk_a; mk_numeral_i ctx chunk_a_size]) chunk_b ];
      (* fprintf fmt "(assert (< (+ %s %d) %s))@;" chunk_a chunk_a_size chunk_b *)
	  ) chunks;
	  Array.iteri (fun k cell ->
      let cell_constraints = List.map (fun (chunk, chunk_size) ->
        let const_k = mk_numeral_i ctx k in
        let const_chunk_size = mk_numeral_i ctx chunk_size in
        (* sprintf "(and (>= %d %s) (< %d (+ %s %d)))" k chunk k chunk chunk_size *)
        mk_and ctx [mk_ge ctx const_k chunk; mk_lt ctx const_k (mk_add ctx [chunk; const_chunk_size])]
      ) chunks in
      (* fprintf fmt "(assert (= %s (or %a)))@;" cell (pp_print_list ~pp_sep:pp_print_space pp_print_string) cell_constraints *)
	    add solver [mk_eq ctx cell (mk_or ctx cell_constraints)]
	  ) line
	in
  Array.iter2 make_constraints row_cells chunks_rows;
  Array.iter2 make_constraints col_cells chunks_cols


let solve ctx (rows, cols) =
  let solver = mk_simple_solver ctx in
  let size_x = Array.length cols
	and size_y = Array.length rows in
	(* Make cells *)
	let col_cells = Array.init size_x (fun x ->
    Array.init size_y (fun y ->
      Z3.Boolean.mk_const_s ctx (cell x y)
    )
	) in
	let row_cells = Array.init size_y (fun y ->
    Array.init size_x (fun x -> col_cells.(x).(y))
	) in
  (* Make lines *)
  let make_chunks line_kind m chunks =
    List.mapi (fun n chunk_size ->
      let chunk_var = Integer.mk_const_s ctx (chunk line_kind m n) in
      chunk_var, chunk_size
    ) chunks
  in
  let chunks_rows = Array.mapi (make_chunks "row") rows
  and chunks_cols = Array.mapi (make_chunks "col") cols in
	make_smt2 solver ctx (row_cells, col_cells) (chunks_rows, chunks_cols);
  match check solver [] with
  UNSATISFIABLE -> raise (Failure "No solution exists")
  | UNKNOWN -> raise (Failure "Couldn't prove the existence or absence of solutions")
  | SATISFIABLE ->
    match get_model solver with
    None -> raise (Failure "Unreachable")
    | Some(model) ->
  let res = Array.make_matrix size_x size_y false in
  for i = 0 to size_x - 1 do
    for j = 0 to size_y - 1 do
      match Z3.Model.get_const_interp_e model row_cells.(j).(i) with
        None -> raise (Failure "Unreachable")
      | Some(expr) ->
          let str = Z3.Expr.to_string expr in
          if str = "true" then res.(i).(j) <- true;
    done
  done;
  Printf.printf "\n";
  for j = 0 to size_y - 1 do
    for i = 0 to size_x - 1 do
      Printf.printf "%s" (if res.(i).(j) then "◼ " else "◻ ")
    done;
    Printf.printf "\n";
  done

(* --- --- --- --- --- --- --- *)

open Nonogram_solver
open Puzzle
open Input

let usage_msg =
  Format.sprintf "%s <file path>" Sys.executable_name

let file = ref ""

let anon_fun str =
  file := str

let speclist =
  [
  ]

let () =
  Arg.parse speclist anon_fun usage_msg;
  let puzzle = read_file !file in
  let ctx = Z3.mk_context [] in
  solve ctx (puzzle.rows, puzzle.cols)