
# Independent replications before I decided to switch to batch means instead
# This isn't quite the correct method for blackjack games, but it does work
source("play_blackjack.R")

replicate_games <- function(r_runs,
                            m_replications,
                            S_17,
                            logic_board,
                            seed){
  
  
  simulation_results <- data.frame()
  for (i in 1:r_runs) {
    seed <- seed + i
    set.seed((seed))
    run_results <- replicate(m_replications,
                             play_game(num_players = 1,
                                       initial_bet = 1,
                                       manual = FALSE,
                                       S_17 = S_17,
                                       logic_board = logic_board))
    run_df <- data.frame(
      game_outcomes = unlist(unlist(run_results, recursive = TRUE)[c(TRUE, FALSE)]),
      bets = unlist(unlist(run_results, recursive = TRUE)[c(FALSE, TRUE)]),
      run_num = i)
    
    simulation_results <- simulation_results %>% 
      bind_rows(run_df)
  }
  return(simulation_results)
}


get_sample_means <- function(df) {
  df %>% 
    mutate(net_result = game_outcomes * bets) %>% 
    group_by(run_num) %>% 
    summarise(outcome_means = mean(game_outcomes),
              bet_means = mean(bets),
              net_result_means = mean(net_result))
}

get_grand_sample_mean <- function(df) {
  df %>% 
    ungroup() %>% 
    summarise(outcome_mean = mean(outcome_means),
              bet_mean = mean(bet_means),
              net_result_mean = mean(net_result_means))
}


get_sample_variance <- function(df) {
  df %>% 
    mutate(across(contains("means"), ~ (. - mean(.))^2)) %>% 
    summarise(across(contains("means"), ~ sum(.) / (n() - 1))) %>% 
    rename_with(~ gsub("means", "variance", .), contains("means"))
}


get_conf_interval <- function(z_bar, sample_var, r_runs, alpha) {
  t_value <- qt(1 - (alpha / 2), r_runs - 1)
  lower_bound <- z_bar - t_value * sqrt(sample_var / r_runs)
  upper_bound <- z_bar + t_value * sqrt(sample_var / r_runs)
  return(c(lower_bound, upper_bound))
}


get_all_cis <- function(z_bars, sample_vars, r_runs, alpha) {
  ci_df <- data.frame()
  
  for (i in seq_along(z_bars)) {
    ci <- get_conf_interval(z_bar = z_bars[[i]], 
                            sample_var = sample_vars[[i]], 
                            r_runs = r_runs, 
                            alpha = alpha)
    ci_df[1, i] <- ci[1]
    ci_df[2, i] <- ci[2]
  }
  colnames(ci_df) <- c("outcome", "bet", "net_result")
  add_means <- z_bars
  colnames(add_means) <- c("outcome", "bet", "net_result")
  ci_df <- ci_df %>%
    bind_rows(add_means)
  rownames(ci_df) <- c("lower", "upper", "mean")
  ci_df <- ci_df[order(row.names(ci_df)), ]
  return(ci_df)
}


df <- replicate_games(r_runs = 1000,
                      m_replications = 200,
                      S_17 = TRUE,
                      logic_board = lb_3,
                      seed = 3489)

s_means <- get_sample_means(df)
z_bars <- get_grand_sample_mean(get_sample_means(df))
sample_vars <- get_sample_variance(get_sample_means(df))
get_all_cis(z_bars = z_bars, sample_vars = sample_vars, r_runs = 1000, alpha = 0.01)
