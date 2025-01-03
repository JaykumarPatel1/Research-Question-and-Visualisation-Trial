# Load necessary libraries
library(ggplot2)
library(dplyr)

# Load the data
matches <- read.csv("matches.csv")
str(matches)

# converting variable types
matches$city = as.factor(matches$city)
matches$date = as.Date(matches$date)
matches$team1 = as.factor(matches$team1)
matches$team2 = as.factor(matches$team2)
matches$toss_winner = as.factor(matches$toss_winner)
matches$toss_decision = as.factor(matches$toss_decision)
matches$result = as.factor(matches$result)
matches$player_of_match = as.factor(matches$player_of_match)
matches$venue = as.factor(matches$venue)
matches$umpire1 = as.factor(matches$umpire1)
matches$umpire2 = as.factor(matches$umpire2)
matches$umpire3 = as.factor(matches$umpire3)

# summary of attributes
summary(matches)

# removing umpire3 (3rd umpire) as all values are missing
matchesprep = subset(matches, select = -c(umpire3))

# new variable to indicate if toss winner is the match winner
matchesprep$toss_and_match_win <- ifelse(matchesprep$toss_winner == matches$winner, "Yes", "No")

# output pre-processed dataset
write.csv(x = matchesprep, file = 'matches_clean.csv',row.names = FALSE)

# Hypothesis Testing
# Chi-square test for independence
table_toss <- table(matchesprep$toss_and_match_win)
chi_test <- chisq.test(table_toss)

# Print results
print(chi_test)


# Plot: Proportion of matches won by toss winners
ggplot(matchesprep, aes(x = toss_and_match_win, fill = toss_and_match_win)) +
  geom_bar(stat = "count") +
  ggtitle("Proportion of Matches Won by Toss Winners") +
  xlab("Toss Winner Also Won Match") +
  ylab("Count") +
  theme_minimal()

# Additional Plot: Win margin(runs+wickets) based on toss winner or not
matchesprep$win_margin <- matchesprep$win_by_runs + matchesprep$win_by_wickets

ggplot(matchesprep, aes(x = toss_and_match_win, y = win_margin, fill = toss_and_match_win)) +
  geom_boxplot() +
  ggtitle("Win Margin Based on Toss Win") +
  xlab("Toss Winner Also Won Match") +
  ylab("Win Margin (Runs + Wickets)") +
  theme_minimal()