# Simulating Independent Blackjack Games in R

# Requirements

The simulation of a single game only requires base R. To simulate many games using `run_ir_games` and to statistically analyse them, `dplyr` is required for some basic data wrangling. The RMarkdown file containing the paper and analysis expects the following packages. It most likely works with different versions as well, but the versions shown here were used:

- ggplot2 (3.4.1)
- tidyverse (1.3.2)
- janitor (2.2.0)
- kableExtra (1.3.4)
- patchwork (1.1.2)


# Simulating Games

## Simulating Single Games

In broad strokes, the code does the following. `shuffle_deck` creates n-decks, shuffles them, and stores the result in the global environment. If n-decks equals 1, and each game is set to require 52 cards, the deck would be shuffled before each game and the games (not the hands) would be independent. Alternatively, initializing only 1 deck and delaying the shuffle would make games in between each shuffle correlated, more closely approximating a casino from before automatic shufflers and multideck blackjack. This functionality was intended for an expansion into card counting that didn't make it into the final paper.

The `play_game` function calls on all other required functions so as to simulate one game of blackjack. `play_game` first deals 2 cards to the dealer, creates as many players as requested, places their initial bets, and deals them 2 cards using `deal_cards` which it takes and removes from the global deck. Differences between the simulation and other strategy results might derive from this, because calculations such as by Baldwin often allow for an approximation based on a deck that is always full (sampling with replacement), rather than taking out cards as a single game progresses.[^1]

Then, each player action is decided on through logical operators and look-up tables in the function `player_logic`. Each strategy consists of a list of three look-up tables (one for hard totals, one for soft totals, and one for splits), where the recommended action is looked up from rows of player hand values and columns of dealer face card values. `player_actions` executes the taken action: either to stand, hit, double, or split. The resulting hand then goes back to `play_game`. 

If the player didn't go bust already, `dealer_logic` runs through the fixed rules for a dealer to determine their final hand. Based on that value, and the final player hand, the outcome is determined by `game_outcome` and returned. To distinguish it from a normal 21, a natural blackjack counts as 21.5. The initial bet is always 1. 

In the case of a split, the hand is split into two new hands, and each hand is given an additional card within `player_actions`. Together with the existing dealer hand, these two hands are send back to the `play_game` function to determine their outcome. There is also a manual option to play some games of blackjack against the dealer based on your input actions. This wasn't the focus of this project and hasn't been tested thoroughly, but it works well enough.

After loading the script with `source("play_blackjack.R")`, a single game simulation can be called upon through for example `play_game(num_players = 1, initial_bet = 1, manual = FALSE, S_17 = TRUE, logic_board = lb_1)`, where:

- num_players (integer): Specifies the number of players, default 1.
- initial_bet (integer): Specifies the initial bet, default 1.
- logic_board (list of three matrices): Input for the required list of three matrices to follow a strategy. Possible lists included are lb_1 through lb_4, for respectively Baldwin *et al.*, Thorp, simplified, and mimicking the dealer.
- manual (boolean): Switches between manual games and automatic, default FALSE. If manual equals FALSE, the output will be a list of values, with the first value the outcome of the game and the second the value of the bet. Otherwise the response is given in text.
- S_17 (boolean): Switches between S17 (TRUE) and H17 (FALSE), default TRUE.
- player_list (list): Used internally to input data from split hands.
- dealer (list): Used internally, dealer info to play split hands.
- got_split (boolean): Used internally, to signal that the hand was split.
- required_cards (integer): Optional, minimum number of cards in deck before shuffling.

The output consists of 2 values for each game: the outcome and the bet. The outcome is either -1 for a lost game, 0 for a tie, 1 for a win, or 1.5 for a win with a natural 21. The bet values are either 1 for the initial bet, 2 for a double, and very occasionally 1.5 for when a hand was split and the resulting hands produced a normal bet of 1 and 2 for a double. The net result for each game is simply the game outcome multiplied by the bet value. The mean of these net results are the expected value for a strategy per initial bet of 1 currency unit. 

## Improvements

Currently, using multiple players works largely as it should, except for when a player splits. The way this is dealt with means that those two split hands veer off on their own, play a game with their dealer, report their result, and then the other players continue. This means that the dealer will draw different cards to determine the outcome of those split hands compared to the hands of the other players. This isn't a problem for the simulations below because those only consider a single player, but a multiplayer extension of this code needs to be aware of this. 

## Simulating Many Games

`run_ir_games` can be used to run independent replications of many blackjack games, for example `run_ir_games(r_runs = 10, m_obs = 100, S_17 = TRUE, logic_board = lb_1, seed = 12345, required_cards = 52)`. The inputs for the function are as follows:

- r_runs (integer): The number of replications.
- m_obs (integer): The number of observations per replication.
- S_17 (boolean): Switches between S17 (TRUE) and H17 (FALSE), default TRUE. 
- logic_board (list of three matrices): Input for the required list of three matrices to follow a strategy, lb_1 through lb_4 are available.
- seed (integer): Starting seed value to ensure reproducible output.
- required_cards (integer): Optional, defaults to 52. Cutoff for minimum remaining cards in deck before reshuffling occurs.

The output is a dataframe, containing the columns `game_outcomes`, `bets`, `run_num`, and `net`.

`batch_games` is also available as an alternative method, following the batch means method. This was intended for dependent games but not necessary in the end. The inputs and outputs are nearly identical to the `run_ir_games` function.

## Analysis

The functions `get_ir_sample_means`, `get_ir_grand_sample_mean`, `get_ir_sample_variance`, `get_ir_cis_full`, do what you would expect, the first taking as input the output from `run_ir_games`, the next two requiring the output from `get_ir_sample_means`, and the last function combines the information of the previous functions together with an alpha to produce confidence intervals. 


[^1]: For example, Baldwin *et al.*, "The Optimum Strategy", 434, their second assumption for dealer probabilities: "no matter how many cards the player draws, the probability of receiving any particular card on the next draw is still 1/52 ("sampling with replacement")".



