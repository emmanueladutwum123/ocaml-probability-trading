let roll sides = 1 + Random.int sides

let expected_sum_until target sides =
  (* Expected rolls until cumulative sum >= target with a d-sided die *)
  Monte_carlo.expected_value 30_000 (fun () ->
    let sum   = ref 0 in
    let rolls = ref 0 in
    while !sum < target do
      sum  := !sum + roll sides;
      incr rolls
    done;
    float_of_int !rolls)

let max_of_n_dice n sides =
  (* E[max of n independent d-sided dice rolls] *)
  Monte_carlo.expected_value 50_000 (fun () ->
    let best = ref 0 in
    for _ = 1 to n do
      let r = roll sides in
      if r > !best then best := r
    done;
    float_of_int !best)

(* Alias matching the spec *)
let first_to_sum = expected_sum_until

let non_transitive_dice () =
  (* Efron non-transitive dice:
     A beats B, B beats C, C beats A — demonstrating intransitivity *)
  let die_a = [| 4; 4; 4; 4; 0; 0 |] in
  let die_b = [| 3; 3; 3; 3; 3; 3 |] in
  let die_c = [| 6; 6; 2; 2; 2; 2 |] in
  let roll_die d = d.(Random.int (Array.length d)) in
  let trials = 100_000 in
  let p_ab = Monte_carlo.probability_of trials (fun () -> roll_die die_a > roll_die die_b) in
  let p_bc = Monte_carlo.probability_of trials (fun () -> roll_die die_b > roll_die die_c) in
  let p_ca = Monte_carlo.probability_of trials (fun () -> roll_die die_c > roll_die die_a) in
  (p_ab, p_bc, p_ca)

let chuck_a_luck trials =
  (* Player picks 1-6, rolls 3 dice.
     Wins $k for k matches, loses $1 for zero matches.
     House edge is ~7.87%. *)
  Monte_carlo.simulate trials (fun () ->
    let pick    = 1 + Random.int 6 in
    let matches = List.init 3 (fun _ -> roll 6)
                  |> List.filter (fun d -> d = pick)
                  |> List.length
    in
    if matches = 0 then -1.0
    else float_of_int matches)
