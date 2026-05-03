let () = Random.self_init ()

let sep () = print_endline (String.make 52 '-')

let run_probability_puzzles () =
  sep ();
  print_endline "PROBABILITY PUZZLES";
  let (a, mc) = Probability.birthday_problem 23 in
  Printf.printf "Birthday (n=23):          analytical=%.4f  MC=%.4f\n" a mc;
  let (a, mc) = Probability.coupon_collector 6 in
  Printf.printf "Coupon Collector (n=6):   analytical=%.2f  MC=%.2f\n" a mc;
  let (a, mc) = Probability.gambler_ruin 0.5 5 10 in
  Printf.printf "Gambler Ruin (p=½,s=5,t=10): analytical=%.4f  MC=%.4f\n" a mc;
  let (a, mc) = Probability.secretary_problem 100 in
  Printf.printf "Secretary (n=100):        analytical=%.4f  MC=%.4f\n" a mc;
  let (sw, st) = Probability.monty_hall 50_000 in
  Printf.printf "Monty Hall — switch: %s\n" (Monte_carlo.pp_result sw);
  Printf.printf "Monty Hall — stay:   %s\n" (Monte_carlo.pp_result st);
  let two_env = Probability.two_envelope 50_000 in
  Printf.printf "Two Envelope EV: %s\n" (Monte_carlo.pp_result two_env)

let run_dice_games () =
  sep ();
  print_endline "DICE GAMES";
  Printf.printf "E[rolls until sum>=21, d6]: %.2f\n"
    (Dice_games.expected_sum_until 21 6);
  Printf.printf "E[max of 3d6]:              %.2f\n"
    (Dice_games.max_of_n_dice 3 6);
  let (ab, bc, ca) = Dice_games.non_transitive_dice () in
  Printf.printf "Efron dice: P(A>B)=%.3f  P(B>C)=%.3f  P(C>A)=%.3f\n" ab bc ca;
  let chuck = Dice_games.chuck_a_luck 100_000 in
  Printf.printf "Chuck-a-luck EV: %s\n" (Monte_carlo.pp_result chuck)

let run_card_games () =
  sep ();
  print_endline "CARD GAMES";
  Printf.printf "E[cards until first ace]:  %.2f\n"
    (Cards.expected_cards_until_ace 4);
  Printf.printf "P(flush, 5-card):          %.5f\n"
    (Cards.poker_hand_probability 200_000 "flush");
  Printf.printf "P(full house, 5-card):     %.5f\n"
    (Cards.poker_hand_probability 200_000 "full_house");
  Printf.printf "War expected length:       %.0f rounds\n"
    (Cards.war_expected_length 1_000);
  Printf.printf "Blackjack basic EV:        %.4f\n"
    (Cards.blackjack_basic_strategy 100_000)

let run_options () =
  sep ();
  print_endline "OPTIONS PRICING";
  let p = Options.{
    spot = 100.0; strike = 100.0; rate = 0.05;
    volatility = 0.20; time_to_expiry = 1.0; option_type = Call
  } in
  let bs = Options.black_scholes p in
  Printf.printf "Black-Scholes ATM call:  %.4f\n" bs;
  Printf.printf "Binomial tree (200):     %.4f\n" (Options.binomial_tree p 200);
  let mc = Options.monte_carlo_price p 100_000 in
  Printf.printf "Monte Carlo:             %s\n" (Monte_carlo.pp_result mc);
  Printf.printf "Greeks — Δ=%.4f  Γ=%.4f  ν=%.4f  Θ=%.4f\n"
    (Options.delta p) (Options.gamma p) (Options.vega p) (Options.theta p);
  let iv = Options.implied_vol p 10.45 in
  Printf.printf "Impl vol (price=10.45):  %.4f\n" iv

let run_market_making () =
  sep ();
  print_endline "MARKET MAKING";
  let params = Market_making.{
    initial_fair_value = 100.0;
    volatility         = 0.05;
    spread             = 0.10;
    max_position       = 50;
    num_steps          = 2000;
    gamma              = 0.1;
  } in
  let (as_r, simple_r) = Market_making.compare_strategies params in
  print_string (Market_making.pp_result "Avellaneda-Stoikov" as_r);
  print_string (Market_making.pp_result "Simple Fixed-Spread" simple_r)

let print_menu () =
  print_endline "\n=== OCaml Probability & Trading Library ===";
  print_endline "1. Probability puzzles";
  print_endline "2. Dice games";
  print_endline "3. Card simulations";
  print_endline "4. Options pricing";
  print_endline "5. Market making";
  print_endline "6. Run all";
  print_endline "7. Quit";
  print_string "> ";
  flush stdout

let () =
  let running = ref true in
  while !running do
    print_menu ();
    let line = try input_line stdin with End_of_file -> "7" in
    match String.trim line with
    | "1" -> run_probability_puzzles ()
    | "2" -> run_dice_games ()
    | "3" -> run_card_games ()
    | "4" -> run_options ()
    | "5" -> run_market_making ()
    | "6" ->
      run_probability_puzzles ();
      run_dice_games ();
      run_card_games ();
      run_options ();
      run_market_making ()
    | "7" | "q" | "quit" -> running := false
    | _ -> print_endline "Enter 1-7"
  done
