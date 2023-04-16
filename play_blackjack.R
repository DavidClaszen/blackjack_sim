# Cleanup 
# rm(list = ls())

# Create deck
deck <- rep(c(1:9, 10, 10, 10, 10), 4)

# Function to shuffle decks
shuffle_deck <- function(n_decks){
  sample(rep(deck, n_decks))
}

# Reshuffle deck if too small or on command
reshuffle_check <- function(required_cards = 1, shuffle_now = FALSE){
  if (length(current_deck) < required_cards) {
    assign(x = "current_deck", shuffle_deck(n_decks), envir = globalenv())
  } else if (shuffle_now) {
    assign(x = "current_deck", shuffle_deck(n_decks), envir = globalenv())  
  }
}

# Define number of decks, shuffle into live deck
n_decks <- 1
current_deck <- shuffle_deck(n_decks)

# Place bets by adding to player
place_bet <- function(player, bet){
  return(player$bet + bet)
}

# Draw cards from current_deck
deal_cards <- function(n_cards = 2){
  deck_copy <- current_deck
  indices <- sample(1:length(deck_copy), n_cards)
  # Remove drawn cards from deck; find better solution to globalenv?
  assign(x = "current_deck", deck_copy[-indices], envir = globalenv())
  return(deck_copy[indices])
}

hand_value <- function(cards){
  cur_value <- sum(cards)
  # Add 10 points for an ace if equal or below 11 points
  if (any(cards == 1) && cur_value <= 11) {
    cur_value <- cur_value + 10
  }
  # Return 0 if bust
  if (cur_value > 21) {
    return(0)
  }
  # Return 21.5 for blackjack
  if (cur_value == 21 && length(cards) == 2) { 
    return(21.5) } else
      return(cur_value)
}

# Reporting function just for manual play and testing
report_dealer <- function(dealer, only_hole = TRUE){
  if (only_hole){
    hole_card <- dealer$hand[[1]]
    print(paste("The hole card is:", hole_card))
  } else if (!only_hole) {
    print(paste("Dealer had", paste(dealer$hand, collapse = ", "), 
                "with a value of", hand_value(dealer$hand)))
  }
}

# Default for dealer
dealer_logic <- function(dealer, S_17 = TRUE, manual = FALSE, debug = FALSE){
  current_hand <- dealer$hand
  # Keep checking while at or below 17 but not bust
  while (hand_value(current_hand) <= 17 && hand_value(current_hand) != 0) {
    
    # Check whether hand is hard or not regardless of aces
    is_hard <- ((sum(current_hand) + 10) > 21)
    
    # Make sure there's still cards in deck before drawing if debugging
    # Maximally need 5 additional cards
    if (debug) { reshuffle_check(required_cards = 5) }
    # If below 17, hit
    if (hand_value(current_hand) < 17) {
      current_hand <- c(current_hand, deal_cards(n_cards = 1))
      # else, stand with hard 17
    } else if (hand_value(current_hand) == 17 && is_hard) {
      dealer$hand <- current_hand
      return(dealer)
      # else stand if at 17 and using S17 rule and hand is soft
    } else if (hand_value(current_hand) == 17 
               && S_17
               && !is_hard) {
      dealer$hand <- current_hand
      return(dealer)
      # else if at 17 and using H17 rule and hand is soft, hit, stay in loop
    } else if (hand_value(current_hand) == 17 
               && !S_17 
               && !is_hard) {
      current_hand <- c(current_hand, deal_cards(n_cards = 1))
    }
  }
  dealer$hand <- current_hand
  return(dealer)
}


# Used for testing of dealer logic
dealer_plays <- function(S_17 = TRUE, debug = TRUE) {
  reshuffle_check(required_cards = 10)
  dealer <- vector("list", 1)
  names(dealer) <- c("hand")
  dealer$hand <- deal_cards(n_cards = 2)
  dealer <- dealer_logic(dealer, S_17 = S_17, debug = debug)
  return(hand_value(dealer$hand))
}



player_logic <- function(player, dealer, initial_bet, manual, 
                         S_17, got_split, logic_board, debug = FALSE){
  # Initiate variables
  action <- ""
  counter <- 0
  
  if (manual) {
    
    # While not stand, split, or double, keep asking for action
    while (!(action %in% c("s", "d", "sp"))) {
      counter <- counter + 1
      can_split <- length(player$hand) == 2 & length(unique(player$hand)) == 1
      print(paste("Your current hand consists of:", 
                  paste(player$hand, collapse = ", "),
                  "with a value of", hand_value(player$hand)))
      # Actions aren't validated and player could, technically, cheat
      # If 2 card hand derived from a split, can't split, but can double
      if (got_split & counter == 1) {
        action <- readline(prompt = "What do you do? hit (h) / stand (s) / double (d): ")
        # Else if got split, only hit and stand
      } else if (got_split) {
        action <- readline(prompt = "What do you do? hit (h) / stand (s): ")
        # Else normal hand, first action, can split, double
      } else if (counter == 1 & can_split) {
        action <- readline(prompt = "What do you do? hit (h) / stand (s) / double (d) / split (sp): ")
        # Same but can't split
      } else if (counter == 1 & !can_split) {
        action <- readline(prompt = "What do you do? hit (h) / stand (s) / double (d): ")
        # Counter higher than 1 but can split
      } else if (can_split) {
        action <- readline(prompt = "What do you do? hit (h) / stand (s) / split (sp): ")
        # Everything else can only hit or stand
      } else {
        action <- readline(prompt = "What do you do? hit (h) / stand (s): ")
      }
      # Perform action
      player <- player_actions(player, dealer, action, 
                               manual = manual, S_17 = S_17,
                               logic_board = logic_board)
      if(is.null(player)){return()}
      
      # If bust, print result, return hand
      if (hand_value(player$hand) == 0) {
        print(paste("Your current hand consists of:", 
                    paste(player$hand, collapse = ", "), 
                    "with a value of", hand_value(player$hand)))
        print("Bust!")
        return(player)
        
        # Doubling down means no more actions possible, quits loop, return hand
        # Print hand value to let player know what they drew
      } else if (action == "double") {
        print(paste("Your current hand consists of:", 
                    paste(player$hand, collapse = ", "), 
                    "with a value of", hand_value(player$hand)))  
      }
    }
    return(player)
    
  } else if (!manual) {
    
    if (debug) {
      print("Player print in player_logic at location 001")
      print(player)      
    }
    
    # While not stand (s), double (d), split (sp), keep getting new actions
    while (!(action %in% c("s", "d", "sp"))) {
      counter <- counter + 1
      can_split <- counter == 1 & length(unique(player$hand)) == 1
      can_double <- counter == 1
      
      # Figure out rules for doubles 
      
      # If can split and hasn't been split before, logic board for splits
      if (can_split & !got_split) {
        lb_to_play <- logic_board[[3]]
        # Else if soft hand (contains ace), soft logic board
      } else if (1 %in% player$hand) {
        lb_to_play <- logic_board[[2]]
        # Else use hard logic board, no aces
      } else {lb_to_play <- logic_board[[1]]}
      
      # Print debugging below
      if (debug) {
        print("Player hand, dealer hand, logic board, player_logic at location 002")
        print(player$hand)
        print(dealer$hand)
        print(lb_to_play)        
      }
      
      if (can_split & !got_split) {
        player_lb_loc <- paste(player$hand, collapse = ", ")
        
        # Truncate below to deal with 21.5 values for blackjack
      } else {player_lb_loc <- paste(trunc(hand_value(player$hand)))}
      
      if (debug) {
        print("Print location in matrix to play, player_logic at location 003")
        print(player_lb_loc)        
      }
      
      action <- lb_to_play[player_lb_loc,
                           as.character(dealer$hand[1])]
      
      if (debug) {
        print("Print action, player_logic at location 004")
        print(action)        
      }
      
      # If want to double and can, then double
      if (grepl("d", action) & can_double) {
        action <- "d"
        # If want to double and can't, switch ds to s and d to h
      } else if (grepl("d", action) & !can_double) {
        action <- ifelse(action == "ds", "s", "h")
      }
      
      # Perform action
      player <- player_actions(player, dealer, action, 
                               manual = manual, S_17 = S_17, 
                               logic_board = logic_board)
      
      if (debug) {
        # If hand was a split, 2 games were called from inside player_actions
        # Return value is then a list of results of those two games
        # Thus no player attribute names
        print("Print player after action, player_logic at location 005")
        print(player)
        print("Class of player: ")
        print(class(player))
        print("Type")
        print(typeof(player))
        print("Attributes")
        print(attributes(player))        
      }
      
      if (!("names" %in% names(attributes(player)))) {
        if (debug) {
          print("No attributes detected at player_logic location 006, returning player")  
        }
        
        return(player)
      }
      
      # If bust, return hand
      if (hand_value(player$hand) == 0) {
        return(player)
      }
    }
    if (debug) {
      print("Print return value from player_logic at location 007")
      print(player)      
    }
    return(player)
  }
}


player_actions <- function(player, dealer, action,
                           manual, S_17, debug = FALSE, logic_board){
  if (debug) {
    print("Hand and actions received at player_actions location 008")
    print(player)
    print(action)
    print("Dealer:")
    print(dealer)    
  }
  
  if (action == "h") {
    player$hand <- c(player$hand, deal_cards(n_cards = 1))
    return(player)
  } else if (action == "s") {
    return(player)
  } else if (action == "d") {
    player$hand <- c(player$hand, deal_cards(n_cards = 1))
    player$bet <- player$bet * 2
    return(player)
  } else if (action == "sp") {
    # Create another player for the split hand
    # Feed temp list of players into play_game
    player_2 <- vector("list", 2)
    names(player_2) <- c("hand", "bet")
    player_2$bet <- player$bet
    player_2$hand <- player$hand[2]
    player_2$hand <- c(player_2$hand, deal_cards(n_cards = 1))
    player$hand <- player$hand[1]
    player$hand <- c(player$hand, deal_cards(n_cards = 1))
    
    if (debug) {
      print("Print players after split at player_actions location 009")
      print("INSIDE OF SEPARATE SPLIT GAMES AFTER THIS LINE -----------------")
      print(player)
      print(player_2)      
    }
    
    play_game(player_list = list(player, player_2),
              dealer = dealer, got_split = TRUE,
              manual = manual, S_17 = S_17,
              logic_board = logic_board)
  }
}


game_outcome <- function(player, dealer){
  (player > dealer & player > 21) * 1.5 +
    (player > dealer & player <= 21) * 1 +
    (player < dealer|player == 0) * -1
}



play_game <- function(num_players = 1, initial_bet = 1, logic_board,
                      manual = FALSE, S_17 = TRUE, player_list = NULL, 
                      dealer = NULL, got_split = FALSE, debug = FALSE){
  # Technically, order of cards dealt is different, but shouldn't matter too much
  # For convenience, dealer is dealt first, then players, each 2 cards
  # We'll also assume all players behave and bet the same for now
  # If game doesn't come from a split game, create new hands, else use existing
  if (!got_split & is.null(player_list)) {
    # Make sure deck is still large enough, otherwise shuffle
    # Need 11 cards for one player max; four aces, four 2's, three 3's equals 21
    # Add 4 for safety, splits can demand more
    reshuffle_check(required_cards = 15 * num_players)
    
    # Deal with dealer
    dealer <- vector("list", 1)
    names(dealer) <- c("hand")
    dealer$hand <- deal_cards(n_cards = 2)
    
    # Create players
    player_list <- vector("list", num_players)
    for (player in 1:num_players) {
      player_list[[player]] <- vector("list", 2)
      names(player_list[[player]]) <- c("hand", "bet")
      player_list[[player]]$bet <- initial_bet
      player_list[[player]]$hand <- deal_cards(n_cards = 2)
    }
  }
  
  if (manual) {

    for (player in player_list) {
      # Report hole card
      report_dealer(dealer, only_hole = TRUE)
      # Player decides what to do based on hand, visible hole card, or manual
      player_result <- player_logic(player, dealer, initial_bet,
                                    manual = manual, S_17 = S_17, 
                                    got_split = got_split, 
                                    logic_board = logic_board)
      # Report dealer cards if player went bust already
      if (hand_value(player_result$hand) == 0) {
        report_dealer(dealer, only_hole = FALSE)
      } else {
        # Dealer plays according to set logic
        # But we can use stand on soft 17 (TRUE) or stand on hard 17 rule (FALSE)
        dealer <- dealer_logic(dealer, S_17 = S_17, manual = manual)
        # Report outcome
        outcome <- game_outcome(hand_value(player_result$hand), hand_value(dealer$hand))
        report_dealer(dealer, only_hole = FALSE)
        if (outcome == 1.5) {
          result_txt <- paste("Blackjack! You win", outcome, "times", player_result$bet)
        } else if (outcome == 1) {
          result_txt <- paste("You win", outcome, "times", player_result$bet)
        } else if (outcome == -1) {
          result_txt <- paste("You lose your bet of", player_result$bet)
        } else if (outcome == 0) {
          result_txt <- paste("Tie! Bets are returned")
        }
        print(result_txt)
      }
    }
  } else if (!manual) {
    
    game_results <- list()
    player_results <- list()
    
    # Get final player hands based on dealer hand and player logic
    for (player in player_list) {
      
      if (debug) {
        print("Print player in play game function at play_game location 010")
        print(player)        
      }
      
      player_results <- append(player_results,
                               list(player_logic(player, dealer, initial_bet,
                                            manual = manual, S_17 = S_17, 
                                            got_split = got_split, 
                                            logic_board = logic_board)))
      if (debug) {
        print("Print player result value at play_game location 011")
        print(player_results)        
      }
    }

    # If a previous run was for two splits, player_logic returns a list of their results
    # Doesn't contain attributes, so in that case, return results
    if (!("names" %in% names(attributes(player_results[[1]])))) {
      if (debug) {print("No attributes detected, returning results at play_game location 012") }
      return(player_results)
    }        
        
    # Check whether all players went bust, in that case dealer doesn't need to draw
    all_bust <- all(sapply(player_results, function(x) hand_value(x$hand) == 0))

    if (all_bust) {
      for (player in player_results) {
        game_results <- append(game_results, c(-1, player$bet))
      }
      return(game_results)
    }
    
    # Else, go through each player separately but against the same dealer result
    dealer <- dealer_logic(dealer, S_17 = S_17, manual = manual)
    
    for (player in player_results) {
      if (hand_value(player$hand) == 0) {
        if (debug) {print("Adding lost game to list of results at play_game location 013")}
        game_results <- append(game_results, c(-1, player$bet))
      } else {
        if (debug) {print("Getting result of game at play_game location 014")}
        outcome <- game_outcome(hand_value(player$hand), hand_value(dealer$hand))
        game_results <- append(game_results, c(outcome, player$bet))
        if (debug) {
          print("Printing current results at play_game location 014")
          print(game_results)
        }
      }
    }
    if (debug) {print("Results printed, output below should be return from function -----") }
    return(game_results)
  }
}



# Creation of logic boards for hard, soft totals, and splits
# Columns are hole card values, rows are player totals
# s = stand, h = hit, sp = split
# d = double if possible else hit, ds = double if possible else stand
rnames_h <- c(21:3)
rnames_s <- c(21:12)
rnames_sp <- c("1, 1", "10, 10", "9, 9", "8, 8", "7, 7", 
               "6, 6", "5, 5", "4, 4", "3, 3", "2, 2")
cnames <- c(2:10, 1)

# Common strategy widely available online
# Such as https://www.blackjackapprenticeship.com/blackjack-strategy-charts/
lb_h1 <- matrix(
  c("s", "s", "s", "s", "s", "s", "s", "s", "s", "h", "d", "d", "h", "h", "h", "h", "h", "h", "h",
    "s", "s", "s", "s", "s", "s", "s", "s", "s", "h", "d", "d", "d", "h", "h", "h", "h", "h", "h",
    "s", "s", "s", "s", "s", "s", "s", "s", "s", "s", "d", "d", "d", "h", "h", "h", "h", "h", "h",
    "s", "s", "s", "s", "s", "s", "s", "s", "s", "s", "d", "d", "d", "h", "h", "h", "h", "h", "h",
    "s", "s", "s", "s", "s", "s", "s", "s", "s", "s", "d", "d", "d", "h", "h", "h", "h", "h", "h",
    "s", "s", "s", "s", "s", "h", "h", "h", "h", "h", "d", "d", "h", "h", "h", "h", "h", "h", "h",
    "s", "s", "s", "s", "s", "h", "h", "h", "h", "h", "d", "d", "h", "h", "h", "h", "h", "h", "h",
    "s", "s", "s", "s", "s", "h", "h", "h", "h", "h", "d", "d", "h", "h", "h", "h", "h", "h", "h",
    "s", "s", "s", "s", "s", "h", "h", "h", "h", "h", "d", "h", "h", "h", "h", "h", "h", "h", "h",
    "s", "s", "s", "s", "s", "h", "h", "h", "h", "h", "d", "h", "h", "h", "h", "h", "h", "h", "h"),
  nrow = 19, byrow = FALSE, dimnames = list(rnames_h, cnames))

lb_s1 <- matrix(c("s", "s", "s", "ds", "h", "h", "h", "h", "h", "h",
                  "s", "s", "s", "ds", "d", "h", "h", "h", "h", "h",
                  "s", "s", "s", "ds", "d", "d", "d", "h", "h", "h",
                  "s", "s", "s", "ds", "d", "d", "d", "d", "d", "h",
                  "s", "s", "ds", "ds", "d", "d", "d", "d", "d", "h",
                  "s", "s", "s", "s", "h", "h", "h", "h", "h", "h",
                  "s", "s", "s", "s", "h", "h", "h", "h", "h", "h",
                  "s", "s", "s", "h", "h", "h", "h", "h", "h", "h",
                  "s", "s", "s", "h", "h", "h", "h", "h", "h", "h",
                  "s", "s", "s", "h", "h", "h", "h", "h", "h", "h"),
                nrow = 10, byrow = FALSE, dimnames = list(rnames_s, cnames))

lb_sp1 <- matrix(c("sp", "s", "sp", "sp", "sp", "sp", "d", "h", "sp", "sp",
                   "sp", "s", "sp", "sp", "sp", "sp", "d", "h", "sp", "sp",
                   "sp", "s", "sp", "sp", "sp", "sp", "d", "h", "sp", "sp",
                   "sp", "s", "sp", "sp", "sp", "sp", "d", "sp", "sp", "sp",
                   "sp", "s", "sp", "sp", "sp", "sp", "d", "sp", "sp", "sp",
                   "sp", "s", "s", "sp", "sp", "h", "d", "h", "sp", "sp",
                   "sp", "s", "sp", "sp", "h", "h", "d", "h", "h", "h",
                   "sp", "s", "sp", "sp", "h", "h", "d", "h", "h", "h",
                   "sp", "s", "s", "sp", "h", "h", "h", "h", "h", "h",
                   "sp", "s", "s", "sp", "h", "h", "h", "h", "h", "h"),
                 nrow = 10, byrow = FALSE, dimnames = list(rnames_sp, cnames))

lb_1 <- list(lb_h1, lb_s1, lb_sp1)

# Simplified strategy following rules of thumb based on the perfect strategy
# https://blog.prepscholar.com/blackjack-strategy

lb_h2 <- matrix(
  c("s", "s", "s", "s", "s", "s", "s", "s", "s", "s", "d", "d", "d", "h", "h", "h", "h", "h", "h",
    "s", "s", "s", "s", "s", "s", "s", "s", "s", "s", "d", "d", "d", "h", "h", "h", "h", "h", "h",
    "s", "s", "s", "s", "s", "s", "s", "s", "s", "s", "d", "d", "d", "h", "h", "h", "h", "h", "h",
    "s", "s", "s", "s", "s", "s", "s", "s", "s", "s", "d", "d", "d", "h", "h", "h", "h", "h", "h",
    "s", "s", "s", "s", "s", "s", "s", "s", "s", "s", "d", "d", "d", "h", "h", "h", "h", "h", "h",
    "s", "s", "s", "s", "s", "h", "h", "h", "h", "h", "d", "d", "h", "h", "h", "h", "h", "h", "h",
    "s", "s", "s", "s", "s", "h", "h", "h", "h", "h", "d", "d", "h", "h", "h", "h", "h", "h", "h",
    "s", "s", "s", "s", "s", "h", "h", "h", "h", "h", "d", "d", "h", "h", "h", "h", "h", "h", "h",
    "s", "s", "s", "s", "s", "h", "h", "h", "h", "h", "h", "h", "h", "h", "h", "h", "h", "h", "h",
    "s", "s", "s", "s", "s", "h", "h", "h", "h", "h", "h", "h", "h", "h", "h", "h", "h", "h", "h"),
  nrow = 19, byrow = FALSE, dimnames = list(rnames_h, cnames))

lb_s2 <- matrix(c("s", "s", "s", "d", "d", "d", "h", "h", "h", "h",
                  "s", "s", "s", "d", "d", "d", "h", "h", "h", "h",
                  "s", "s", "s", "d", "d", "d", "h", "h", "h", "h",
                  "s", "s", "s", "d", "d", "d", "h", "h", "h", "h",
                  "s", "s", "s", "d", "d", "d", "h", "h", "h", "h",
                  "s", "s", "s", "h", "h", "h", "h", "h", "h", "h",
                  "s", "s", "s", "h", "h", "h", "h", "h", "h", "h",
                  "s", "s", "s", "h", "h", "h", "h", "h", "h", "h",
                  "s", "s", "s", "h", "h", "h", "h", "h", "h", "h",
                  "s", "s", "s", "h", "h", "h", "h", "h", "h", "h"),
                nrow = 10, byrow = FALSE, dimnames = list(rnames_s, cnames))

lb_sp2 <- matrix(c("sp", "s", "sp", "sp", "sp", "sp", "d", "h", "sp", "sp",
                   "sp", "s", "sp", "sp", "sp", "sp", "d", "h", "sp", "sp",
                   "sp", "s", "sp", "sp", "sp", "sp", "d", "h", "sp", "sp",
                   "sp", "s", "sp", "sp", "sp", "sp", "d", "h", "sp", "sp",
                   "sp", "s", "sp", "sp", "sp", "sp", "d", "h", "sp", "sp",
                   "sp", "s", "s", "sp", "h", "h", "d", "h", "h", "h",
                   "sp", "s", "s", "sp", "h", "h", "d", "h", "h", "h",
                   "sp", "s", "s", "sp", "h", "h", "d", "h", "h", "h",
                   "sp", "s", "s", "sp", "h", "h", "h", "h", "h", "h",
                   "sp", "s", "s", "sp", "h", "h", "h", "h", "h", "h"),
                 nrow = 10, byrow = FALSE, dimnames = list(rnames_sp, cnames))

lb_2 <- list(lb_h2, lb_s2, lb_sp2)

# play_game(num_players = 1, initial_bet = 1, manual = TRUE, S_17 = TRUE, logic_board = lb_1)
#play_game(num_players = 1, initial_bet = 1, manual = FALSE, S_17 = TRUE, logic_board = lb_1)


# Alternate deck full of split opportunities
#deck <- rep(c(9), 52)
#current_deck <- shuffle_deck(n_decks)


