type simulation_result = {
  mean          : float;
  std_dev       : float;
  confidence_95 : float * float;
  n_samples     : int;
}

let simulate n trial =
  let samples = List.init n (fun _ -> trial ()) in
  let m   = Stats.mean    samples in
  let sd  = Stats.std_dev samples in
  let err = 1.96 *. sd /. Float.sqrt (float_of_int n) in
  { mean          = m
  ; std_dev       = sd
  ; confidence_95 = (m -. err, m +. err)
  ; n_samples     = n
  }

(* Variance reduction: pair uniform draws u and 1-u *)
let simulate_with_antithetic n f =
  let pairs =
    List.init n (fun _ ->
      let u = Random.float 1.0 in
      (f u +. f (1.0 -. u)) /. 2.0)
  in
  let m   = Stats.mean    pairs in
  let sd  = Stats.std_dev pairs in
  let err = 1.96 *. sd /. Float.sqrt (float_of_int n) in
  { mean          = m
  ; std_dev       = sd
  ; confidence_95 = (m -. err, m +. err)
  ; n_samples     = n
  }

let expected_value n trial =
  List.init n (fun _ -> trial ())
  |> List.fold_left ( +. ) 0.0
  |> fun s -> s /. float_of_int n

let probability_of n trial =
  List.init n (fun _ -> if trial () then 1.0 else 0.0)
  |> List.fold_left ( +. ) 0.0
  |> fun s -> s /. float_of_int n

let pp_result r =
  let (lo, hi) = r.confidence_95 in
  Printf.sprintf "mean=%.4f std=%.4f 95%%CI=[%.4f,%.4f] n=%d"
    r.mean r.std_dev lo hi r.n_samples
