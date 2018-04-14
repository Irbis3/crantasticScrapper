library(tibble)
library(dplyr)

# Generate one experiment
#
# arms A vector of probabilities for each "lever."
# belief A vector of prior beliefs about the levers which must sum to one.
# trials The number of trials.
# belief_strategy How beliefs get updated.
# decision_strategy How selections are made based on beliefs.

multi_armed_bandit <- function(arms, 
                               belief = 1/rep(length(arms), length(arms)), 
                               trials, belief_strategy, decision_strategy) {
  # Make a choice
  choice <- decision_strategy(belief)
  
  # Find out reward
  reward <- sample(c(0, 1), 1, prob = c(1 - arms[choice], arms[choice]))
  
  # Log stuff
  selection_history <- data_frame(Choice = choice, Reward = reward, Belief = list(belief))
  
  # Update beliefs
  belief <- belief_strategy(belief, selection_history)
  
  for (i in 2:trials) {
    # Make a choice
    choice <- decision_strategy(belief)
    
    # Find out reward
    reward <- sample(c(0, 1), 1, prob = c(1 - arms[choice], arms[choice]))
    
    # Log stuff
    selection_history <- selection_history %>% 
      add_row(Choice = choice, Reward = reward, Belief = list(belief))
    
    # Update beliefs
    belief <- belief_strategy(belief, selection_history)
  }
  
  selection_history
}

# Simple belief updating strategy
# Returns a new belief
simple_belief_updating_strategy <- function(belief, selection_history) {
   last_trial <- selection_history %>% 
    slice(n()) %>% 
    as.list()
  
  if (sum(belief[-last_trial$Choice]) < .1) {
    change <- .01
  } else {
    change <- .1
  }
   
  if (last_trial$Reward > 0) {
    belief[last_trial$Choice] <- belief[last_trial$Choice] + change
    belief[-last_trial$Choice] <- belief[-last_trial$Choice] - 
      change/length(belief[-last_trial$Choice])
  } else {
    belief[last_trial$Choice] <- belief[last_trial$Choice] - change
    belief[-last_trial$Choice] <- belief[-last_trial$Choice] + 
      change/length(belief[-last_trial$Choice])
  }
  
  belief
}

# Returns a selection
simple_decision_strategy <- function(belief) {
  which.max(belief)
}
