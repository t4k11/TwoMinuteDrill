devtools::install_github(repo = "ryurko/nflscrapR")

library(nflscrapR)
library(dplyr)
library(ggplot2)
library(ggrepel)
library(ElemStatLearn)

master_df <- season_play_by_play(2018)

write.csv(master_df, file='C:/Users/trevor.krause/Documents/Projects/TwoMinuteDrill/data.csv')

# Reduce to only the 'real' plays
df <- master_df[master_df$PlayType %in% c('Pass', 'Spike', 'Sack', 'Run'),]

# Number each play within its respective drive
df <- df %>% group_by(GameID, Drive) %>% mutate(PlayOfDrive = row_number())

# filter less than 5 min, scorediff between 0 and 7, first play of drive, more than 40 yds to go
final <- subset(df, (PlayOfDrive == 1) & (TimeSecs <= 300) & (TimeSecs > 0) & (ScoreDiff <= 0) & (ScoreDiff >= -7) & (yrdline100 >= 40))

# select necessary columns
final <- final %>% select(Date, GameID, posteam, DefensiveTeam, yrdline100, TimeSecs, ScoreDiff, desc, Drive, PlayType, down, 
                          PlayOfDrive, HomeTeam, AwayTeam)

# create gamelabel column to identify the points in the chart
final$GameLabel <- paste(final$DefensiveTeam, format(final$Date, "%m/%d"), sep = " ")

#read in win dictionary and merge with dataframe
win_dict <- read.csv(file='C:/Users/trevor.krause/Documents/Projects/TwoMinuteDrill/win_dict.csv')
final <- merge(x=final, y=select(win_dict, GameID, posteam, WinOrLoss), by=c('GameID', 'posteam'), all.x=TRUE)

# manually make any necessary fixes
#write.csv(final, file='C:/Users/trevor.krause/Documents/Projects/TwoMinuteDrill/final.csv')
#final <- read.csv(file='C:/Users/trevor.krause/Documents/Projects/TwoMinuteDrill/final.csv')

# create classification model
training_set <- select(final[final$WinOrLoss != 'Tie',], ScoreDiff, TimeSecs, WinOrLoss)
training_set$WinOrLossB <- factor(training_set$WinOrLoss, levels = c('Loss', 'Win'), labels = c(0, 1))

fit <- glm(formula = WinOrLossB ~ ScoreDiff + TimeSecs, 
           family = "binomial",
           data = training_set)

# Create a grid of points to plot probability on
X1 <- seq(0, 300, by = 1)
X2 <- seq(-7, 0, by = .04)
grid_set <- expand.grid(X1, X2)
colnames(grid_set) <- c('TimeSecs', 'ScoreDiff')

# Generate predicitons on grid
prob_set <- predict(fit, type = 'response', newdata = grid_set)
y_grid <- ifelse(prob_set > 0.5, 1, 0)
grid_set$WinOrLoss <- y_grid
grid_set$WinProb <- factor(prob_set)

# Plot Data
ggplot() + #, label=GameLabel [final$posteam == 'GB',] color=WinOrLoss
  geom_point(data=grid_set, aes(x=TimeSecs, y=ScoreDiff, colour=WinOrLoss)) + 
  scale_colour_gradient(low = 'tomato', high='springgreen') +
  scale_x_reverse(lim=c(300, 0)) +
  scale_y_reverse(lim=c(0, -7)) +
  geom_point(data = training_set, aes(x=TimeSecs, y=ScoreDiff))

ggplot() + #, label=GameLabel [final$posteam == 'GB',] color=WinOrLoss
  geom_point(data=grid_set, aes(x=TimeSecs, y=ScoreDiff, colour=WinOrLoss)) + 
  scale_colour_gradient(low = 'tomato', high='springgreen') +
  scale_x_reverse(lim=c(300, 0)) +
  scale_y_reverse(lim=c(0, -7))
  
  geom_point(aes(colour=WinOrLoss)) + 
  #geom_text_repel() +
  scale_x_reverse(lim=c(300, 0)) +
  scale_y_reverse(lim=c(0, -7)) + 
  xlab("Time Left (seconds)") + 
  ylab("Point Differential") +
  labs(colour="")

