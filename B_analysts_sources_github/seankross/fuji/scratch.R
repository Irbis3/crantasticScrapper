library(tidyr)

belief <- c(.1, .2, .7)

simple_strategy(belief, selection_history)

selection_history <- data_frame(choice = c(1, 2, 1),
                                reward = c(0, 0, 1)
                                )

belief <- c(.1, .2, .7)

arms <- c(.1, .9, .1)

trials <- 5

set.seed(2018-02-13)

x <- multi_armed_bandit(arms = c(.6, .8, .6), trials = 40, 
                   belief_strategy = simple_belief_updating_strategy, 
                   decision_strategy = simple_decision_strategy)

barplot(table(x$Choice))


