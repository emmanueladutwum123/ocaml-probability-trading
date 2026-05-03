# ocaml-probability-trading

An OCaml library of probability puzzles, Monte Carlo simulation, and quantitative finance models — built as a demonstration of functional programming for Jane Street–style interviews.

## What It Is

Jane Street interviews are heavy on probability puzzles and expected-value reasoning. This library:
1. Implements classic puzzles with **both analytical solutions and Monte Carlo estimates** — so you can verify the simulation converges to theory
2. Prices options via Black-Scholes, binomial trees, and Monte Carlo
3. Compares two market-making strategies (naive vs Avellaneda-Stoikov)

## Modules

| Module | Contents |
|--------|----------|
| `Stats` | `mean`, `std_dev`, `percentile`, `sharpe_ratio`, `max_drawdown`, `normal_cdf/pdf/inv` |
| `Monte_carlo` | Generic simulation engine; antithetic variates for variance reduction |
| `Probability` | Birthday, coupon collector, gambler's ruin, secretary problem, Monty Hall, two-envelope |
| `Dice_games` | Max of N dice, sum until target, Efron non-transitive dice, chuck-a-luck |
| `Cards` | Expected cards until ace, poker hand probabilities, War, blackjack basic strategy |
| `Options` | Black-Scholes, binomial tree (CRR), Monte Carlo pricing, full Greeks, implied vol |
| `Market_making` | Avellaneda-Stoikov optimal quoting vs naive fixed-spread |

## Architecture

- **Pure functions throughout** — every puzzle function is deterministic given the RNG state; no global mutable state except `Random`
- **Dual output** — every puzzle returns `(analytical_answer, monte_carlo_estimate)`, making it easy to spot bugs (if they diverge, one is wrong)
- **`.mli` interfaces** — all public APIs are explicitly typed and documented
- **Pipe operator** — data transformations written left-to-right with `|>`
- **Algebraic types** — `Call | Put`, `Buy | Sell` — compiler-enforced exhaustive matching

## Build

Requires OCaml ≥ 4.14, opam, and dune.

```bash
opam install alcotest        # only external dependency
dune build
dune exec bin/main.exe       # interactive menu
dune test                    # run test suite
```

## Sample Output

```
PROBABILITY PUZZLES
Birthday (n=23):          analytical=0.5073  MC=0.5071
Coupon Collector (n=6):   analytical=14.70   MC=14.69
Gambler Ruin (p=½,s=5,t=10): analytical=0.5000  MC=0.4998
Secretary (n=100):        analytical=0.3679  MC=0.3681
Monty Hall — switch: mean=0.6667 95%CI=[0.6625,0.6708]
Monty Hall — stay:   mean=0.3333 95%CI=[0.3292,0.3375]

OPTIONS PRICING
Black-Scholes ATM call:  10.4506
Binomial tree (200):     10.4498
Monte Carlo:             mean=10.4489 std=0.2171 95%CI=[10.4444,10.4534] n=100000
Greeks — Δ=0.6368  Γ=0.0188  ν=37.5240  Θ=-5.9032
Impl vol (price=10.45):  0.2001
```

## Puzzles Explained

**Birthday problem** — With 23 people, P(two share a birthday) > 50%. The analytical solution multiplies probabilities of *no* collision; MC samples random birthdays.

**Secretary problem** — Interview n candidates in random order. Skip the first ⌊n/e⌋, then hire the next who beats all seen so far. Optimal stopping gives P(best) → 1/e ≈ 36.8%.

**Gambler's ruin** — Starting at position s, walk toward target t or ruin at 0. With fair coin (p=0.5), P(reach t) = s/t. Closed-form via geometric series for biased coins.

**Efron non-transitive dice** — Three dice where A beats B, B beats C, and C beats A. Demonstrates that "beats" is not transitive for random variables.

**Avellaneda-Stoikov** — Optimal market-making under inventory risk: skew quotes toward reducing position when inventory grows. Outperforms naive symmetric quoting on Sharpe.
