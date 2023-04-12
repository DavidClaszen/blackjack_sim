# Cleanup 
rm(list = ls())

# Create deck
deck <- rep(c(1:9, 10, 10, 10, 10), 4)
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
  # Make sure deck is still large enough, otherwise shuffle
  reshuffle_check(n_cards)
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
dealer_logic <- function(dealer, S_17 = TRUE, manual = FALSE){
  current_hand <- dealer$hand
  # Keep checking while at or below 17 but not bust
  while (hand_value(current_hand) <= 17 && hand_value(current_hand) != 0) {

    # Make sure there's still cards in deck before drawing
    reshuffle_check(1)
    # If below 17, hit
    if (hand_value(current_hand) < 17) {
      current_hand <- c(current_hand, deal_cards(n_cards = 1))
    # else, stand with hard 17
    } else if (hand_value(current_hand) == 17 && all(current_hand != 1)) {
      dealer$hand <- current_hand
      return(dealer)
    # else stand if at 17 and using S17 rule
    } else if (hand_value(current_hand) == 17 
               && S_17
               && any(current_hand == 1)) {
      dealer$hand <- current_hand
      return(dealer)
    # else if at 17 and ace present and using H17 rule, hit, stay in loop
    } else if (hand_value(current_hand) == 17 
               && !S_17 
               && any(current_hand == 1)) {
      current_hand <- c(current_hand, deal_cards(n_cards = 1))
    }
  }
  dealer$hand <- current_hand
  return(dealer)
}


# Unused function at the moment, used for testing of dealer logic
dealer_plays <- function(S_17 = TRUE) {
  dealer <<- structure(list(hand = deal_cards(2)))
  dealer$hand <<- dealer_logic(dealer, S_17 = S_17)
  return(hand_value(dealer$hand))
}



player_logic <- function(player, dealer, initial_bet, manual, 
                         S_17, got_split, logic_board){
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
                               manual = manual, S_17 = S_17)
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
    
    # While not stand (s), double (d), split (sp), keep getting new actions
    while (!(action %in% c("s", "d", "sp"))) {
      counter <- counter + 1
      can_split <- counter == 1 & length(unique(player$hand)) == 1
      can_double <- counter == 1
      
      # Figure out rules for doubles 
      
      # If can split and hasn't been split before, logic board for splits
      if (can_split & !got_split) {
        lb_to_play <- lb[[3]]
      # Else if soft hand (contains ace), soft logic board
      } else if (1 %in% player$hand) {
        lb_to_play <- lb[[2]]
      # Else use hard logic board, no aces
      } else {lb_to_play <- lb[[1]]}
      
      # Print debugging below
      print(player$hand)
      print(dealer$hand)
      print(lb_to_play)
      
      if (can_split) {
        player_lb_loc <- paste(player$hand, collapse = ", ")
        # Truncate below to deal with 21.5 values for blackjack
      } else {player_lb_loc <- paste(trunc(hand_value(player$hand)))}
      
      action <- lb_to_play[player_lb_loc,
                           as.character(dealer$hand[1])]
      print(action)
      
      # If want to double and can, then double
      if ("d" %in% action & can_double) {
        action <- "d"
      # If want to double and can't, switch ds to s and d to h
      } else if ("d" %in% action & !can_double) {
        action <- ifelse(action == "ds", "s", "h")
      }
      # Perform action
      player <- player_actions(player, dealer, action, 
                               manual = manual, S_17 = S_17)
      # If hand splits, return null?
      # TODO Research, fix if needed
      if(is.null(player)){return()}
      
      # If bust, return hand
      if (hand_value(player$hand) == 0) {
        return(player)
      }
    }
    return(player)
  }
}


player_actions <- function(player, dealer, action,
                           manual, S_17){
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
    
    play_game(player_list = list(player, player_2),
              dealer = dealer, got_split = TRUE,
              manual = manual, S_17 = S_17)
  }
}


game_outcome <- function(player, dealer){
  (player > dealer & player > 21) * 1.5 +
  (player > dealer & player <= 21) * 1 +
  (player < dealer|player == 0) * -1
}



play_game <- function(num_players = 1, initial_bet = 1, logic_board = lb,
                      manual = FALSE, S_17 = TRUE, player_list = NULL, 
                      dealer = NULL, got_split = FALSE){
  # Technically, order of cards dealt is different, but shouldn't matter too much
  # For convenience, dealer is dealt first, then players, each 2 cards
  # We'll also assume all players behave and bet the same for now
  # If game doesn't come from a split game, create new hands, else use existing
  if (!got_split) {
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
    # Play the game to completion in turns for each player against same dealer
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
    for (player in player_list) {
      player_result <- player_logic(player, dealer, initial_bet,
                                    manual = manual, S_17 = S_17, 
                                    got_split = got_split, 
                                    logic_board = logic_board)
      if (hand_value(player_result$hand) == 0) {
        return(c(-1, player_result$bet))
      } else {
        dealer <- dealer_logic(dealer, S_17 = S_17, manual = manual)
        outcome <- game_outcome(hand_value(player_result$hand), hand_value(dealer$hand))
        return(c(outcome, player_result$bet))
      }
    }
  }
}



# Creation of logic boards for hard, soft totals, and splits
# Columns are hole card values, rows are player totals
# s = stand, h = hit, sp = split
# d = double if possible else hit, ds = double if possible else stand
rnames_h <- c(21:3)
rnames_s <- c(21:13)
rnames_sp <- c("1, 1", "10, 10", "9, 9", "8, 8", "7, 7", 
               "6, 6", "5, 5", "4, 4", "3, 3", "2, 2")
cnames <- c(2:10, 1)

# Common strategy widely available online
# For example https://www.blackjackapprenticeship.com/blackjack-strategy-charts/
lb_h1 <- matrix(
  c("s", "s", "s", "s", "s", "s", "s", "s", "s", "h", "d", "d", "h", "h", "h", "h", "h", "h", "h",
    "s", "s", "s", "s", "s", "s", "s", "s", "s", "h", "d", "d", "d", "h", "h", "h", "h", "h", "h",
    "s", "s", "s", "s", "s", "s", "s", "s", "s", "s", "d", "d", "d", "h", "h", "h", "h", "h", "h",
    "s", "s", "s", "s", "s", "s", "s", "s", "s", "s", "d", "d", "d", "h", "h", "h", "h", "h", "h",
    "s", "s", "s", "s", "s", "s", "s", "s", "s", "s", "d", "d", "d", "h", "h", "h", "h", "h", "h",
    "s", "s", "s", "s", "s", "h", "h", "h", "h", "h", "d", "d", "h", "h", "h", "h", "h", "h", "h",
    "s", "s", "s", "s", "s", "h", "h", "h", "h", "h", "d", "d", "h", "h", "h", "h", "h", "h", "h",
    "s", "s", "s", "s", "s", "h", "h", "h", "h", "h", "d", "d", "h", "h", "h", "h", "h", "h", "h",
    "s", "s", "s", "s", "s", "h", "h", "h", "h", "h", "d", "H", "h", "h", "h", "h", "h", "h", "h",
    "s", "s", "s", "s", "s", "h", "h", "h", "h", "h", "d", "h", "h", "h", "h", "h", "h", "h", "h"),
  nrow = 19, byrow = FALSE, dimnames = list(rnames_h, cnames))

lb_s1 <- matrix(c("s", "s", "s", "ds", "h", "h", "h", "h", "h",
                  "s", "s", "s", "ds", "d", "h", "h", "h", "h",
                  "s", "s", "s", "ds", "d", "d", "d", "h", "h",
                  "s", "s", "s", "ds", "d", "d", "d", "d", "d",
                  "s", "s", "ds", "ds", "d", "d", "d", "d", "d",
                  "s", "s", "s", "s", "h", "h", "h", "h", "h",
                  "s", "s", "s", "s", "h", "h", "h", "h", "h",
                  "s", "s", "s", "h", "h", "h", "h", "h", "h",
                  "s", "s", "s", "h", "h", "h", "h", "h", "h",
                  "s", "s", "s", "h", "h", "h", "h", "h", "h"),
                nrow = 9, byrow = FALSE, dimnames = list(rnames_s, cnames))

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



lb <- list(lb_h1, lb_s1, lb_sp1)

play_game(num_players = 1, initial_bet = 1, manual = TRUE, S_17 = TRUE, logic_board = lb)
play_game(num_players = 1, initial_bet = 1, manual = FALSE, S_17 = TRUE, logic_board = lb)

num_players <- 1
initial_bet <- 1
player <- player_list[[1]]
paste(player$hand, collapse = ", ")

# outcomes <- replicate(100000, dealer_plays(S_17 = TRUE))
# outcomes2 <- replicate(100000, dealer_plays(S_17 = FALSE))
# outcomes <- replicate(10, dealer_plays(S_17 = TRUE))
# hist(outcomes, breaks = 15:28)
# hist(outcomes2, breaks = 15:28)
# max(outcomes2)

# TODO Solve composite hands, 1 %in% player$hand & max_value(player$hand) < 22













