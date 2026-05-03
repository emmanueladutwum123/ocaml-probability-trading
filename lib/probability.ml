let birthday_problem n =
  (* P(at least two people share a birthday among n) *)
  let analytical =
    let rec no_match k acc =
      if k > n then 1.0 -. acc
      else no_match (k + 1) (acc *. float_of_int (366 - k) /. 365.0)
    in
    no_match 2 1.0
  in
  let mc =
    Monte_carlo.probability_of 50_000 (fun () ->
      let seen = Hashtbl.create n in
      let shared = ref false in
      for _ = 1 to n do
        if not !shared then begin
          let b = Random.int 365 in
          if Hashtbl.mem seen b then shared := true
          else Hashtbl.add seen b ()
        end
      done;
      !shared)
  in
  (analytical, mc)

let coupon_collector n =
  (* E[draws to collect all n distinct coupons] = n * H_n *)
  let analytical =
    let harmonic =
      List.init n (fun k -> 1.0 /. float_of_int (k + 1))
      |> List.fold_left ( +. ) 0.0
    in
    float_of_int n *. harmonic
  in
  let mc =
    Monte_carlo.expected_value 20_000 (fun () ->
      let collected = Array.make n false in
      let have      = ref 0 in
      let draws     = ref 0 in
      while !have < n do
        let c = Random.int n in
        incr draws;
        if not collected.(c) then (collected.(c) <- true; incr have)
      done;
      float_of_int !draws)
  in
  (analytical, mc)

let gambler_ruin win_prob start target =
  (* P(reach target before ruin | start position) *)
  let analytical =
    if Float.abs (win_prob -. 0.5) < 1e-10 then
      float_of_int start /. float_of_int target
    else
      let r = (1.0 -. win_prob) /. win_prob in
      (1.0 -. r ** float_of_int start)
      /. (1.0 -. r ** float_of_int target)
  in
  let mc =
    Monte_carlo.probability_of 30_000 (fun () ->
      let pos = ref start in
      let rec walk () =
        if !pos = 0      then false
        else if !pos = target then true
        else begin
          (if Random.float 1.0 < win_prob then incr pos else decr pos);
          walk ()
        end
      in
      walk ())
  in
  (analytical, mc)

let secretary_problem n =
  (* Optimal strategy: skip first floor(n/e), then pick the next best-so-far.
     Success probability converges to 1/e. *)
  let threshold  = int_of_float (float_of_int n /. Float.exp 1.0) in
  let analytical = 1.0 /. Float.exp 1.0 in
  let mc =
    Monte_carlo.probability_of 50_000 (fun () ->
      (* Assign each candidate a random quality rank 0..n-1; n-1 is best *)
      let quality = Array.init n (fun i -> i) in
      (* Fisher-Yates shuffle *)
      for i = n - 1 downto 1 do
        let j = Random.int (i + 1) in
        let tmp = quality.(i) in
        quality.(i) <- quality.(j);
        quality.(j) <- tmp
      done;
      (* Best quality seen during the skip phase *)
      let best_skip = ref (-1) in
      for i = 0 to threshold - 1 do
        if quality.(i) > !best_skip then best_skip := quality.(i)
      done;
      (* Selection phase: pick first who beats best_skip *)
      let selected = ref (-1) in
      let i = ref threshold in
      while !i < n && !selected = -1 do
        if quality.(!i) > !best_skip then selected := quality.(!i);
        incr i
      done;
      (* Win if we picked the globally best candidate *)
      !selected = n - 1)
  in
  (analytical, mc)

let monty_hall trials =
  (* Returns (switch_result, stay_result) *)
  let switch_result =
    Monte_carlo.simulate trials (fun () ->
      let car    = Random.int 3 in
      let choice = Random.int 3 in
      (* Switching wins iff initial choice was wrong *)
      if car <> choice then 1.0 else 0.0)
  in
  let stay_result =
    Monte_carlo.simulate trials (fun () ->
      let car    = Random.int 3 in
      let choice = Random.int 3 in
      if car = choice then 1.0 else 0.0)
  in
  (switch_result, stay_result)

let conditional_coin n k =
  (* MC estimate: P(at least k consecutive heads in n fair coin flips) *)
  Monte_carlo.probability_of 50_000 (fun () ->
    let streak = ref 0 in
    let found  = ref false in
    for _ = 1 to n do
      if not !found then begin
        if Random.bool () then begin
          incr streak;
          if !streak >= k then found := true
        end else
          streak := 0
      end
    done;
    !found)

let two_envelope trials =
  (* Simulate two-envelope: always switch; is EV > 0? *)
  Monte_carlo.simulate trials (fun () ->
    let smaller = 1.0 +. Random.float 99.0 in
    let larger  = smaller *. 2.0 in
    let first   = if Random.bool () then smaller else larger in
    let second  = if first = smaller then larger else smaller in
    second -. first)
