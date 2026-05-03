type option_type = Call | Put

type option_params = {
  spot           : float;
  strike         : float;
  rate           : float;
  volatility     : float;
  time_to_expiry : float;
  option_type    : option_type;
}

let d1 p =
  (Float.log (p.spot /. p.strike)
   +. (p.rate +. 0.5 *. p.volatility ** 2.0) *. p.time_to_expiry)
  /. (p.volatility *. Float.sqrt p.time_to_expiry)

let d2 p = d1 p -. p.volatility *. Float.sqrt p.time_to_expiry

let black_scholes p =
  let d1v  = d1 p in
  let d2v  = d2 p in
  let disc = Float.exp (-. p.rate *. p.time_to_expiry) in
  match p.option_type with
  | Call ->
    p.spot  *. Stats.normal_cdf  d1v
    -. p.strike *. disc *. Stats.normal_cdf  d2v
  | Put  ->
    p.strike *. disc *. Stats.normal_cdf (-. d2v)
    -. p.spot  *. Stats.normal_cdf (-. d1v)

let monte_carlo_price p n =
  let sqrt_t = Float.sqrt p.time_to_expiry in
  let drift  = (p.rate -. 0.5 *. p.volatility ** 2.0) *. p.time_to_expiry in
  let disc   = Float.exp (-. p.rate *. p.time_to_expiry) in
  Monte_carlo.simulate n (fun () ->
    (* Draw a standard-normal via inverse CDF of a uniform *)
    let u  = Float.max 1e-10 (Float.min (1.0 -. 1e-10) (Random.float 1.0)) in
    let z  = Stats.normal_inv u in
    let st = p.spot *. Float.exp (drift +. p.volatility *. sqrt_t *. z) in
    let payoff = match p.option_type with
      | Call -> Float.max 0.0 (st -. p.strike)
      | Put  -> Float.max 0.0 (p.strike -. st)
    in
    payoff *. disc)

let binomial_tree p n =
  (* Cox-Ross-Rubinstein binomial tree *)
  let dt   = p.time_to_expiry /. float_of_int n in
  let u    = Float.exp (p.volatility *. Float.sqrt dt) in
  let d    = 1.0 /. u in
  let disc = Float.exp (-. p.rate *. dt) in
  let pu   = (Float.exp (p.rate *. dt) -. d) /. (u -. d) in
  let pd   = 1.0 -. pu in
  let payoff st =
    match p.option_type with
    | Call -> Float.max 0.0 (st -. p.strike)
    | Put  -> Float.max 0.0 (p.strike -. st)
  in
  (* Terminal node prices *)
  let vals = Array.init (n + 1) (fun j ->
    payoff (p.spot *. u ** float_of_int j *. d ** float_of_int (n - j)))
  in
  (* Backward induction *)
  for i = n - 1 downto 0 do
    for j = 0 to i do
      vals.(j) <- disc *. (pu *. vals.(j + 1) +. pd *. vals.(j))
    done
  done;
  vals.(0)

let bump = 0.001

let delta p =
  let up = { p with spot = p.spot *. (1.0 +. bump) } in
  let dn = { p with spot = p.spot *. (1.0 -. bump) } in
  (black_scholes up -. black_scholes dn) /. (2.0 *. p.spot *. bump)

let gamma p =
  let up  = { p with spot = p.spot *. (1.0 +. bump) } in
  let dn  = { p with spot = p.spot *. (1.0 -. bump) } in
  let mid = black_scholes p in
  (black_scholes up -. 2.0 *. mid +. black_scholes dn)
  /. (p.spot *. bump) ** 2.0

let vega p =
  let h  = 0.001 in
  let up = { p with volatility = p.volatility +. h } in
  let dn = { p with volatility = p.volatility -. h } in
  (black_scholes up -. black_scholes dn) /. (2.0 *. h)

let theta p =
  let h   = 1.0 /. 365.0 in
  let fwd = { p with time_to_expiry = p.time_to_expiry -. h } in
  (black_scholes fwd -. black_scholes p) /. h

let implied_vol p market_price =
  (* Newton-Raphson to invert Black-Scholes for volatility *)
  let tol      = 1e-7 in
  let max_iter = 200 in
  let rec newton vol iter =
    if iter >= max_iter then vol
    else
      let trial = { p with volatility = vol } in
      let price = black_scholes trial in
      let v     = vega trial in
      if Float.abs v < 1e-12 then vol
      else
        let next = vol -. (price -. market_price) /. v in
        let next = Float.max 1e-6 (Float.min 10.0 next) in
        if Float.abs (next -. vol) < tol then next
        else newton next (iter + 1)
  in
  newton 0.20 0
