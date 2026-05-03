type card = { rank : int; suit : string }
type deck = card list

let suits = [| "S"; "H"; "D"; "C" |]

let new_deck () =
  Array.to_list suits
  |> List.concat_map (fun suit ->
       List.init 13 (fun i -> { rank = i + 1; suit }))

(* Fisher-Yates in functional style: copy to array, shuffle in-place, return list *)
let shuffle deck =
  let arr = Array.of_list deck in
  let n   = Array.length arr in
  for i = n - 1 downto 1 do
    let j   = Random.int (i + 1) in
    let tmp = arr.(i) in
    arr.(i) <- arr.(j);
    arr.(j) <- tmp
  done;
  Array.to_list arr

let expected_cards_until_ace _num_aces =
  (* Expected number of cards drawn until the first ace *)
  Monte_carlo.expected_value 50_000 (fun () ->
    let deck = shuffle (new_deck ()) in
    let rec find n = function
      | []      -> float_of_int n
      | c :: tl ->
        if c.rank = 1 then float_of_int (n + 1)
        else find (n + 1) tl
    in
    find 0 deck)

let poker_hand_probability trials hand_name =
  let is_flush cards =
    let s = (List.hd cards).suit in
    List.for_all (fun c -> c.suit = s) cards
  in
  let sorted_ranks cards =
    List.map (fun c -> c.rank) cards |> List.sort compare
  in
  let is_straight cards =
    let rs = sorted_ranks cards in
    let mn = List.hd rs in
    rs = List.init 5 (fun i -> mn + i)
    || List.sort compare rs = [1; 10; 11; 12; 13]
  in
  let rank_counts cards =
    let tbl = Hashtbl.create 13 in
    List.iter (fun c ->
      let n = try Hashtbl.find tbl c.rank with Not_found -> 0 in
      Hashtbl.replace tbl c.rank (n + 1)) cards;
    Hashtbl.fold (fun _ v acc -> v :: acc) tbl []
    |> List.sort (fun a b -> compare b a)
  in
  let check = match hand_name with
    | "straight_flush"  -> fun cs -> is_flush cs && is_straight cs
    | "four_of_a_kind"  -> fun cs -> (match rank_counts cs with 4 :: _ -> true | _ -> false)
    | "full_house"      -> fun cs -> rank_counts cs = [3; 2]
    | "flush"           -> fun cs -> is_flush cs && not (is_straight cs)
    | "straight"        -> fun cs -> is_straight cs && not (is_flush cs)
    | "three_of_a_kind" -> fun cs -> rank_counts cs = [3; 1; 1]
    | "two_pair"        -> fun cs -> rank_counts cs = [2; 2; 1]
    | "one_pair"        -> fun cs -> (match rank_counts cs with 2 :: 1 :: 1 :: 1 :: [] -> true | _ -> false)
    | _                 -> fun _  -> false
  in
  Monte_carlo.probability_of trials (fun () ->
    let hand = shuffle (new_deck ()) |> List.filteri (fun i _ -> i < 5) in
    check hand)

let war_expected_length trials =
  (* Simplified War: higher rank wins both cards; cap at 10 000 rounds *)
  Monte_carlo.expected_value trials (fun () ->
    let deck = shuffle (new_deck ()) in
    let half = List.length deck / 2 in
    let p1 = Queue.create () in
    let p2 = Queue.create () in
    List.iteri (fun i c ->
      if i < half then Queue.add c p1 else Queue.add c p2
    ) deck;
    let rounds = ref 0 in
    while not (Queue.is_empty p1) && not (Queue.is_empty p2)
          && !rounds < 10_000 do
      incr rounds;
      let c1 = Queue.pop p1 in
      let c2 = Queue.pop p2 in
      if c1.rank >= c2.rank
      then (Queue.add c1 p1; Queue.add c2 p1)
      else (Queue.add c2 p2; Queue.add c1 p2)
    done;
    float_of_int !rounds)

let blackjack_basic_strategy trials =
  (* Basic strategy: player and dealer both hit until >= 17.
     Returns mean outcome per hand: +1 win, 0 push, -1 loss *)
  let hand_value cards =
    let total = List.fold_left (fun acc c -> acc + min c.rank 10) 0 cards in
    let aces  = List.length (List.filter (fun c -> c.rank = 1) cards) in
    let rec add_ace t a =
      if a > 0 && t + 10 <= 21 then add_ace (t + 10) (a - 1) else t
    in
    add_ace total aces
  in
  Monte_carlo.expected_value trials (fun () ->
    let deck = ref (shuffle (new_deck ()) |> shuffle) in
    let draw () =
      match !deck with
      | []       -> deck := shuffle (new_deck ()); List.hd !deck
      | c :: tl  -> deck := tl; c
    in
    let p_hand = ref [draw (); draw ()] in
    let d_hand = ref [draw (); draw ()] in
    (* Player hits until >= 17 *)
    while hand_value !p_hand < 17 do p_hand := draw () :: !p_hand done;
    let pv = hand_value !p_hand in
    if pv > 21 then -1.0
    else begin
      (* Dealer hits until >= 17 *)
      while hand_value !d_hand < 17 do d_hand := draw () :: !d_hand done;
      let dv = hand_value !d_hand in
      if dv > 21 || pv > dv then 1.0
      else if pv = dv then 0.0
      else -1.0
    end)
