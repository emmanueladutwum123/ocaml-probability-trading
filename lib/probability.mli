(** Each function returns (analytical_answer, monte_carlo_estimate) where both apply. *)

val birthday_problem  : int -> float * float
val coupon_collector  : int -> float * float
val gambler_ruin      : float -> int -> int -> float * float
val secretary_problem : int -> float * float
val monty_hall        : int -> Monte_carlo.simulation_result * Monte_carlo.simulation_result
val conditional_coin  : int -> int -> float
val two_envelope      : int -> Monte_carlo.simulation_result
