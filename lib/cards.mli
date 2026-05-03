type card = { rank : int; suit : string }
type deck = card list

val new_deck                  : unit -> deck
val shuffle                   : deck -> deck
val expected_cards_until_ace  : int -> float
val poker_hand_probability    : int -> string -> float
val war_expected_length       : int -> float
val blackjack_basic_strategy  : int -> float
