open A6.Set
open Printf

(** [time f x] is the number of wall-clock seconds it takes to run [f x]. *)
let time f x =
  let start = Unix.gettimeofday () in
  let _ = f x in
  let stop = Unix.gettimeofday () in
  stop -. start

(** [median lst] returns the median timing in [lst]. *)
let median lst =
  let sorted = List.sort compare lst in
  let len = List.length sorted in
  if len mod 2 = 1 then List.nth sorted (len / 2)
  else
    let mid_right = List.nth sorted (len / 2) in
    let mid_left = List.nth sorted ((len / 2) - 1) in
    (mid_left +. mid_right) /. 2.0

(** [insert_n_elements n] inserts [n] distinct elements into the empty set. It
    first inserts even numbers 0, 2, 4... and then odd numbers 1, 3, 5... *)
let insert_n_elements n =
  let rec insert_evens tree i =
    if i >= n then tree else insert_evens (insert i tree) (i + 2)
  in
  let rec insert_odds tree i =
    if i >= n then tree else insert_odds (insert i tree) (i + 2)
  in
  let tree_with_evens = insert_evens empty 0 in
  insert_odds tree_with_evens 1

(** [measure_insert_time n num_samples] measures the amount of time to insert
    [n] elements into a set this will be repeated [num_samples] times and the
    median time will be returned*)
let measure_insert_time n num_samples =
  let rec collect_samples i acc =
    if i >= num_samples then acc
    else
      let t = time insert_n_elements n in
      collect_samples (i + 1) (t :: acc)
  in
  median (collect_samples 0 [])

let () =
  printf "N,Time\n";
  flush stdout;

  let values = [
    500000; 3026315; 5552631; 8078947; 10605263; 13131578; 15657894; 18184210;
    20710526; 23236842; 25763157; 28289473; 30815789; 33342105; 35868421;
    38394736; 40921052; 43447368; 45973684; 50000000
  ]
  

  in

  List.iter
    (fun n ->
      let time = measure_insert_time n 3 in
      printf "%d,%g\n" n time;
      flush stdout)
    values
