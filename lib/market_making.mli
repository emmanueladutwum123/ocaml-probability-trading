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

val avellaneda_stoikov : mm_params -> mm_result
val simple_mm          : mm_params -> mm_result
val compare_strategies : mm_params -> mm_result * mm_result
val pp_result          : string -> mm_result -> string
