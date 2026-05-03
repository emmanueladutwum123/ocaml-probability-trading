type option_type = Call | Put

type option_params = {
  spot           : float;
  strike         : float;
  rate           : float;
  volatility     : float;
  time_to_expiry : float;
  option_type    : option_type;
}

val black_scholes     : option_params -> float
val monte_carlo_price : option_params -> int -> Monte_carlo.simulation_result
val binomial_tree     : option_params -> int -> float
val delta             : option_params -> float
val gamma             : option_params -> float
val vega              : option_params -> float
val theta             : option_params -> float
val implied_vol       : option_params -> float -> float
