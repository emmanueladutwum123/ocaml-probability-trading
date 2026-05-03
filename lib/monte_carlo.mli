type simulation_result = {
  mean          : float;
  std_dev       : float;
  confidence_95 : float * float;
  n_samples     : int;
}

val simulate                 : int -> (unit -> float) -> simulation_result
val simulate_with_antithetic : int -> (float -> float) -> simulation_result
val expected_value           : int -> (unit -> float) -> float
val probability_of           : int -> (unit -> bool)  -> float
val pp_result                : simulation_result -> string
