open Lambda.Ast
open Lambda.Evaluation

(* Helper to measure time in milliseconds *)
let time_f f x =
  let t0 = Unix.gettimeofday () in
  let _ = f x in
  let t1 = Unix.gettimeofday () in
  (t1 -. t0) *. 1000.0

(* Term generators *)

(* 1. Deep nesting of abstractions: \x.\x.\x... forces renaming if we are naive *)
let rec make_deep_term n : pterm =
  if n <= 0 then Var "x" else Abs ("x", make_deep_term (n - 1))

(* 2. Wide application tree: balanced tree of applications *)
let rec make_wide_term n : pterm =
  if n <= 0 then Var "x"
  else App (make_wide_term (n - 1), make_wide_term (n - 1))

(* 3. Mixed term with Let bindings (often triggers alpha conversion logic) *)
let rec make_let_term n : pterm =
  if n <= 0 then Var "x" else Let ("x", Var "x", make_let_term (n - 1))

(* Calculate size (number of nodes) *)
let rec term_size (t : pterm) =
  match t with
  | Var _ | N _ | Nil | Unit | Loc _ ->
      1
  | Abs (_, t) | Fix t | Head t | Tail t | Ref t | Deref t ->
      1 + term_size t
  | App (t1, t2) | Add (t1, t2) | Sub (t1, t2) | Cons (t1, t2) | Assign (t1, t2)
    ->
      1 + term_size t1 + term_size t2
  | Let (_, t1, t2) ->
      1 + term_size t1 + term_size t2
  | IfZero (t1, t2, t3) | IfEmpty (t1, t2, t3) ->
      1 + term_size t1 + term_size t2 + term_size t3

let run_benchmark () =
  let oc = open_out "benchmark_results.csv" in
  Printf.fprintf oc "TestType,Size,TimeClassic,TimeSet\n" ;
  (* Benchmark 1: Deep Abstractions *)
  Printf.printf "Running Deep Abstractions Benchmark...\n" ;
  for i = 1 to 50 do
    let n = i * 200 in
    (* Depth up to 10,000 *)
    let t = make_deep_term n in
    let size = term_size t in
    let time_classic = time_f alpha_convert t in
    let time_set =
      time_f (fun t -> alpha_convert_with_set t StringSet.empty) t
    in
    Printf.fprintf oc "DeepAbs,%d,%.6f,%.6f\n" size time_classic time_set
  done ;
  (* Benchmark 2: Wide Applications *)
  Printf.printf "Running Wide Applications Benchmark...\n" ;
  for i = 10 to 18 do
    let n = i in
    (* Depth of tree, size grows exponentially *)
    let t = make_wide_term n in
    let size = term_size t in
    let time_classic = time_f alpha_convert t in
    let time_set =
      time_f (fun t -> alpha_convert_with_set t StringSet.empty) t
    in
    Printf.fprintf oc "WideApp,%d,%.6f,%.6f\n" size time_classic time_set
  done ;
  (* Benchmark 3: Deep Let Bindings *)
  Printf.printf "Running Deep Let Bindings Benchmark...\n" ;
  for i = 1 to 50 do
    let n = i * 5000 in
    (* Depth up to 250,000 *)
    let t = make_let_term n in
    let size = term_size t in
    let time_classic = time_f alpha_convert t in
    let time_set =
      time_f (fun t -> alpha_convert_with_set t StringSet.empty) t
    in
    Printf.fprintf oc "DeepLet,%d,%.6f,%.6f\n" size time_classic time_set
  done ;
  close_out oc ;
  Printf.printf "Benchmark done. Results saved to benchmark_results.csv\n"

let () = run_benchmark ()
