type mm_params = {
  initial_fair_value : float;
  volatility         : float;
  spread             : float;
  max_position       : int;
  num_steps          : int;
  gamma              : float;
}

type mm_result = {
  pnl          : float;
  sharpe       : float;
  max_drawdown : float;
  num_trades   : int;
  avg_position : float;
  win_rate     : float;
}

let randn () =
  let u1 = Float.max (Random.float 1.0) 1e-15 in
  let u2 = Random.float 1.0 in
  Float.sqrt (-2.0 *. Float.log u1) *. Float.cos (2.0 *. Float.pi *. u2)

(* Core simulation loop parameterised by quoting strategy *)
let simulate_mm bid_offset ask_offset (p : mm_params) =
  Random.self_init ();
  let fv         = ref p.initial_fair_value in
  let position   = ref 0 in
  let cash       = ref 0.0 in
  let num_trades = ref 0 in
  let pnl_hist   = ref [] in
  let pos_sum    = ref 0.0 in
  let wins       = ref 0 in

  for _t = 1 to p.num_steps do
    fv := !fv +. p.volatility *. randn ();
    let bid = !fv -. bid_offset !position p in
    let ask = !fv +. ask_offset !position p in

    let hit_bid =
      Random.float 1.0 < 0.5 && abs !position < p.max_position
    in
    let hit_ask =
      Random.float 1.0 < 0.5 && abs !position < p.max_position
    in
    if hit_bid then begin
      position  := !position + 1;
      cash      := !cash -. bid;
      incr num_trades
    end;
    if hit_ask then begin
      position  := !position - 1;
      cash      := !cash +. ask;
      incr num_trades
    end;

    let mtm = !cash +. float_of_int !position *. !fv in
    (match !pnl_hist with
     | prev :: _ -> if mtm > prev then incr wins
     | []        -> ());
    pnl_hist := mtm :: !pnl_hist;
    pos_sum  := !pos_sum +. Float.abs (float_of_int !position)
  done;

  let pnl_list = List.rev !pnl_hist in
  let final_pnl =
    List.fold_left (fun _ x -> x) 0.0 pnl_list
  in
  let returns =
    let arr = Array.of_list pnl_list in
    let n   = Array.length arr in
    if n > 1 then
      List.init (n - 1) (fun i -> arr.(i + 1) -. arr.(i))
    else []
  in
  { pnl          = final_pnl
  ; sharpe       = Stats.sharpe_ratio returns 0.0
  ; max_drawdown = Stats.max_drawdown pnl_list
  ; num_trades   = !num_trades
  ; avg_position = !pos_sum /. float_of_int p.num_steps
  ; win_rate     =
      if p.num_steps > 1
      then float_of_int !wins /. float_of_int (p.num_steps - 1)
      else 0.0
  }

let avellaneda_stoikov p =
  (* Inventory-risk-adjusted quoting: tighten the side we want to lean,
     widen the side that grows our already-large position *)
  let bid_offset q params =
    let skew = params.gamma *. params.volatility ** 2.0
               *. (float_of_int q /. float_of_int params.max_position) in
    Float.max (params.spread /. 4.0) (params.spread /. 2.0 +. skew)
  in
  let ask_offset q params =
    let skew = params.gamma *. params.volatility ** 2.0
               *. (float_of_int q /. float_of_int params.max_position) in
    Float.max (params.spread /. 4.0) (params.spread /. 2.0 -. skew)
  in
  simulate_mm bid_offset ask_offset p

let simple_mm p =
  (* Naive symmetric fixed-spread market making *)
  let half = p.spread /. 2.0 in
  simulate_mm (fun _q _p -> half) (fun _q _p -> half) p

let compare_strategies p =
  (avellaneda_stoikov p, simple_mm p)

let pp_result name r =
  Printf.sprintf
    "=== %s ===\n\
     PnL: %.2f  Sharpe: %.3f  MaxDD: %.4f\n\
     Trades: %d  AvgPos: %.1f  WinRate: %.1f%%\n"
    name
    r.pnl r.sharpe r.max_drawdown
    r.num_trades r.avg_position (r.win_rate *. 100.0)
