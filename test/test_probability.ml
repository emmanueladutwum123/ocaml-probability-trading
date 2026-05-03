let approx ?(eps = 0.05) expected actual =
  Float.abs (actual -. expected) < eps

let test_birthday () =
  let (a, _) = Probability.birthday_problem 23 in
  Alcotest.(check bool) "n=23 ~ 0.507" true (approx 0.507 a)

let test_coupon_collector () =
  let (a, _) = Probability.coupon_collector 6 in
  Alcotest.(check bool) "n=6 ~ 14.7" true (approx ~eps:0.5 14.7 a)

let test_gambler_ruin_fair () =
  let (a, _) = Probability.gambler_ruin 0.5 5 10 in
  Alcotest.(check bool) "p=0.5,s=5,t=10 = 0.5" true (approx ~eps:0.01 0.5 a)

let test_gambler_ruin_biased () =
  (* With p=0.6, start=5, target=10 analytical = (1-(2/3)^5)/(1-(2/3)^10) *)
  let (a, mc) = Probability.gambler_ruin 0.6 5 10 in
  Alcotest.(check bool) "biased ruin within 3%" true (approx ~eps:0.03 a mc)

let test_secretary_threshold () =
  let (a, _) = Probability.secretary_problem 200 in
  Alcotest.(check bool) "~ 1/e" true (approx ~eps:0.02 (1.0 /. Float.exp 1.0) a)

let test_black_scholes_atm () =
  let p = Options.{ spot=100.; strike=100.; rate=0.05;
    volatility=0.20; time_to_expiry=1.0; option_type=Call } in
  Alcotest.(check bool) "ATM call ~ 10.45" true (approx ~eps:0.10 10.45 (Options.black_scholes p))

let test_mc_convergence () =
  let p = Options.{ spot=100.; strike=100.; rate=0.05;
    volatility=0.20; time_to_expiry=1.0; option_type=Call } in
  let bs = Options.black_scholes p in
  let mc = Options.monte_carlo_price p 100_000 in
  let err = Float.abs (mc.mean -. bs) /. bs in
  Alcotest.(check bool) "MC within 1% of BS" true (err < 0.01)

let test_put_call_parity () =
  let base = Options.{ spot=100.; strike=100.; rate=0.05;
    volatility=0.20; time_to_expiry=1.0; option_type=Call } in
  let call = Options.black_scholes base in
  let put  = Options.black_scholes { base with option_type = Put } in
  let disc = Float.exp (-. base.rate *. base.time_to_expiry) in
  let parity = call -. put -. (base.spot -. base.strike *. disc) in
  Alcotest.(check bool) "put-call parity" true (Float.abs parity < 1e-6)

let test_binomial_converges () =
  let p = Options.{ spot=100.; strike=100.; rate=0.05;
    volatility=0.20; time_to_expiry=1.0; option_type=Call } in
  let bs  = Options.black_scholes p in
  let bin = Options.binomial_tree p 500 in
  Alcotest.(check bool) "binomial within 0.5%" true
    (Float.abs (bin -. bs) /. bs < 0.005)

let test_normal_cdf () =
  Alcotest.(check bool) "Phi(0)=0.5"    true (approx ~eps:0.001 0.5   (Stats.normal_cdf 0.0));
  Alcotest.(check bool) "Phi(1.96)~0.975" true (approx ~eps:0.001 0.975 (Stats.normal_cdf 1.96));
  Alcotest.(check bool) "Phi(-1.96)~0.025" true (approx ~eps:0.001 0.025 (Stats.normal_cdf (-1.96)))

let test_normal_inv_roundtrip () =
  let p = 0.975 in
  let z = Stats.normal_inv p in
  Alcotest.(check bool) "inv(0.975) ~ 1.96" true (approx ~eps:0.001 1.96 z);
  let back = Stats.normal_cdf z in
  Alcotest.(check bool) "CDF(inv(p)) ~ p" true (approx ~eps:0.0001 p back)

let test_non_transitive_dice () =
  let (ab, bc, ca) = Dice_games.non_transitive_dice () in
  (* Each should be > 0.5 — true non-transitivity *)
  Alcotest.(check bool) "A beats B" true (ab > 0.5);
  Alcotest.(check bool) "B beats C" true (bc > 0.5);
  Alcotest.(check bool) "C beats A" true (ca > 0.5)

let () =
  let () = Random.self_init () in
  let open Alcotest in
  run "ProbabilityTrading" [
    "probability", [
      test_case "birthday problem"      `Quick test_birthday;
      test_case "coupon collector"      `Quick test_coupon_collector;
      test_case "gambler ruin (fair)"   `Quick test_gambler_ruin_fair;
      test_case "gambler ruin (biased)" `Quick test_gambler_ruin_biased;
      test_case "secretary threshold"   `Quick test_secretary_threshold;
    ];
    "options", [
      test_case "BS ATM call"         `Quick test_black_scholes_atm;
      test_case "put-call parity"     `Quick test_put_call_parity;
      test_case "binomial convergence"`Quick test_binomial_converges;
      test_case "MC convergence"      `Slow  test_mc_convergence;
    ];
    "stats", [
      test_case "normal CDF"          `Quick test_normal_cdf;
      test_case "normal inv roundtrip"`Quick test_normal_inv_roundtrip;
    ];
    "dice", [
      test_case "non-transitive dice" `Quick test_non_transitive_dice;
    ];
  ]
