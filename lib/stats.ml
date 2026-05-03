let mean xs =
  let n = List.length xs in
  if n = 0 then 0.0
  else List.fold_left ( +. ) 0.0 xs /. float_of_int n

let variance xs =
  let n = List.length xs in
  if n < 2 then 0.0
  else
    let m = mean xs in
    List.fold_left (fun acc x -> acc +. (x -. m) ** 2.0) 0.0 xs
    /. float_of_int (n - 1)

let std_dev xs = sqrt (variance xs)

let median xs =
  let arr = Array.of_list xs in
  Array.sort compare arr;
  let n = Array.length arr in
  if n = 0 then 0.0
  else if n mod 2 = 1 then arr.(n / 2)
  else (arr.(n / 2 - 1) +. arr.(n / 2)) /. 2.0

let percentile p xs =
  let arr = Array.of_list xs in
  Array.sort compare arr;
  let n = Array.length arr in
  if n = 0 then 0.0
  else
    let idx  = p /. 100.0 *. float_of_int (n - 1) in
    let lo   = int_of_float (floor idx) in
    let hi   = min (lo + 1) (n - 1) in
    let frac = idx -. float_of_int lo in
    arr.(lo) +. frac *. (arr.(hi) -. arr.(lo))

let sharpe_ratio returns risk_free_rate =
  let excess = List.map (fun r -> r -. risk_free_rate) returns in
  let m = mean excess in
  let s = std_dev excess in
  if s = 0.0 then 0.0 else m /. s *. sqrt 252.0

let max_drawdown values =
  let (_, dd) =
    List.fold_left
      (fun (peak, max_dd) v ->
        let new_peak = max peak v in
        let dd =
          if new_peak > 0.0 then (new_peak -. v) /. new_peak
          else 0.0
        in
        (new_peak, max max_dd dd))
      (neg_infinity, 0.0)
      values
  in
  dd

(* Abramowitz & Stegun 26.2.17 approximation; max error 7.5e-8 *)
let normal_cdf x =
  let t   = 1.0 /. (1.0 +. 0.2316419 *. Float.abs x) in
  let poly =
    t *. (0.319381530
    +. t *. (-0.356563782
    +. t *. ( 1.781477937
    +. t *. (-1.821255978
    +. t *.   1.330274429))))
  in
  let pdf = Float.exp (-0.5 *. x *. x) /. Float.sqrt (2.0 *. Float.pi) in
  let cdf_pos = 1.0 -. pdf *. poly in
  if x >= 0.0 then cdf_pos else 1.0 -. cdf_pos

let normal_pdf x =
  Float.exp (-0.5 *. x *. x) /. Float.sqrt (2.0 *. Float.pi)

(* Peter Acklam's rational approximation; max relative error < 1.15e-9 *)
let normal_inv p =
  let a = [| -3.969683028665376e+01;  2.209460984245205e+02
           ; -2.759285104469687e+02;  1.383577518672690e+02
           ; -3.066479806614716e+01;  2.506628277459239e+00 |] in
  let b = [| -5.447609879822406e+01;  1.615858368580409e+02
           ; -1.556989798598866e+02;  6.680131188771972e+01
           ; -1.328068155288572e+01 |] in
  let c = [| -7.784894002430293e-03; -3.223964580411365e-01
           ; -2.400758277161838e+00; -2.549732539343734e+00
           ;  4.374664141464968e+00;  2.938163982698783e+00 |] in
  let d = [|  7.784695709041462e-03;  3.224671290700398e-01
           ;  2.445134137142996e+00;  3.754408661907416e+00 |] in
  let poly arr x =
    Array.fold_left (fun acc coef -> acc *. x +. coef) 0.0 arr
  in
  let plow  = 0.02425 in
  let phigh = 1.0 -. plow in
  if p < plow then
    let q = Float.sqrt (-2.0 *. Float.log p) in
    poly c q /. (1.0 +. q *. poly d q)
  else if p <= phigh then
    let q = p -. 0.5 in
    let r = q *. q in
    q *. poly a r /. poly b r
  else
    let q = Float.sqrt (-2.0 *. Float.log (1.0 -. p)) in
    -. poly c q /. (1.0 +. q *. poly d q)
