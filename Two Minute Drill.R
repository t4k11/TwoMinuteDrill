devtools::install_github(repo = "ryurko/nflscrapR")

library(nflscrapR)
library(dplyr)
library(ggplot2)
library(ggrepel)

master_df <- season_play_by_play(2018)

# Reduce to only the 'real' plays
df <- master_df[master_df$PlayType %in% c('Pass', 'Spike', 'Sack', 'Run', 'Timeout', 'No Play'),]

# Number each play within its respective drive
df <- df %>% group_by(GameID, Drive) %>% mutate(PlayOfDrive = row_number())

# filter less than 5 min, scorediff between 0 and 7, first play of drive, more than 40 yds to go
final <- subset(df, (PlayOfDrive == 1) & (TimeSecs <= 300) & (TimeSecs > 0) & (ScoreDiff <= 0) & (ScoreDiff >= -8) & (yrdline100 >= 40))

# select necessary columns
final <- final %>% select(Date, GameID, posteam, DefensiveTeam, yrdline100, TimeSecs, ScoreDiff, desc, Drive, PlayType, down, 
                          PlayOfDrive, HomeTeam, AwayTeam)

# create gamelabel column to identify the points in the chart
final$GameLabel <- paste(final$DefensiveTeam, format(final$Date, "%m/%d"), sep = " ")

#read in win dictionary and merge with dataframe
win_dict <- read.csv(file='win_dict.csv')
final <- merge(x=final, y=select(win_dict, GameID, posteam, WinOrLossRegulation), by=c('GameID', 'posteam'), all.x=TRUE)

# manually make any necessary fixes
write.csv(final, file='final.csv')
final <- read.csv(file='final.csv')

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
grid_set$WinOrLoss <- ifelse(prob_set > 0.5, 1, 0)
grid_set$WinProb <- factor(prob_set)

#################### Plot Data #######################
# Discrete Predictions
ggplot() + 
  geom_raster(data=grid_set, alpha=.1, aes(x=TimeSecs, y=ScoreDiff, fill=as.factor(WinOrLoss))) +
  scale_x_reverse(lim=c(300, 0)) +
  scale_y_reverse(lim=c(0, -7)) +
  scale_fill_manual(values=c("#CC79A7", "#009E73")) + 
  geom_point(data=final, aes(x=TimeSecs, y=ScoreDiff, colour=WinOrLoss)) +
  xlab("Time Left (seconds)") + 
  ylab("Point Differential") +
  guides(fill=FALSE)

# Continuous Predicitons
ggplot() + 
  geom_raster(data=grid_set, alpha=.5, aes(x=TimeSecs, y=ScoreDiff, fill=as.numeric(as.character(WinProb)))) +
  scale_x_reverse(lim=c(300, 0)) +
  scale_y_reverse(lim=c(0, -7)) +
  scale_fill_gradient2(high = 'lightskyblue1', low = 'lightpink', midpoint = .5) +
  geom_point(data=final, aes(x=TimeSecs, y=ScoreDiff, colour=WinOrLoss)) +
  xlab("Time Left (seconds)") + 
  ylab("Point Differential") +
  labs(fill='Win Prob.', colour='Outcome', title = 'Attempted Late Game Comeback Drives in 2018 Season')

########### Plot Teams Individually################
ggplot(data=final[final$posteam == 'ARI',], aes(x=TimeSecs, y=ScoreDiff, colour=WinOrLoss, label=GameLabel)) +
  geom_text_repel(show.legend = FALSE) +
  geom_point() +
  scale_x_reverse(lim=c(300, 0)) +
  scale_y_reverse(lim=c(0, -7)) +
  xlab("Time Left (seconds)") + 
  ylab("Point Differential") +
  labs(colour = "", title = "Arizona Cardinals")

ggplot(data=final[final$posteam == 'ATL',], aes(x=TimeSecs, y=ScoreDiff, colour=WinOrLoss, label=GameLabel)) +
  geom_text_repel(show.legend = FALSE) +
  geom_point() +
  scale_x_reverse(lim=c(300, 0)) +
  scale_y_reverse(lim=c(0, -7)) +
  xlab("Time Left (seconds)") + 
  ylab("Point Differential") +
  labs(colour = "", title = "Atlanta Falcons")

ggplot(data=final[final$posteam == 'BAL',], aes(x=TimeSecs, y=ScoreDiff, colour=WinOrLoss, label=GameLabel)) +
  geom_text_repel(show.legend = FALSE) +
  geom_point() +
  scale_x_reverse(lim=c(300, 0)) +
  scale_y_reverse(lim=c(0, -7)) +
  xlab("Time Left (seconds)") + 
  ylab("Point Differential") +
  labs(colour = "", title = "Baltimore Ravens")

ggplot(data=final[final$posteam == 'BUF',], aes(x=TimeSecs, y=ScoreDiff, colour=WinOrLoss, label=GameLabel)) +
  geom_text_repel(show.legend = FALSE) +
  geom_point() +
  scale_x_reverse(lim=c(300, 0)) +
  scale_y_reverse(lim=c(0, -7)) +
  xlab("Time Left (seconds)") + 
  ylab("Point Differential") +
  labs(colour = "", title = "Buffalo Bills")

ggplot(data=final[final$posteam == 'CAR',], aes(x=TimeSecs, y=ScoreDiff, colour=WinOrLoss, label=GameLabel)) +
  geom_text_repel(show.legend = FALSE) +
  geom_point() +
  scale_x_reverse(lim=c(300, 0)) +
  scale_y_reverse(lim=c(0, -7)) +
  labs(colour = "", title = "Carolina Panthers")

ggplot(data=final[final$posteam == 'CHI',], aes(x=TimeSecs, y=ScoreDiff, colour=WinOrLoss, label=GameLabel)) +
  geom_text_repel(show.legend = FALSE) +
  geom_point() +
  scale_x_reverse(lim=c(300, 0)) +
  scale_y_reverse(lim=c(0, -7)) +
  xlab("Time Left (seconds)") + 
  ylab("Point Differential") +
  labs(colour = "", title = "Chicago Bears")

ggplot(data=final[final$posteam == 'CIN',], aes(x=TimeSecs, y=ScoreDiff, colour=WinOrLoss, label=GameLabel)) +
  geom_text_repel(show.legend = FALSE) +
  geom_point() +
  scale_x_reverse(lim=c(300, 0)) +
  scale_y_reverse(lim=c(0, -7)) +
  xlab("Time Left (seconds)") + 
  ylab("Point Differential") +
  labs(colour = "", title = "Cincinnati Bengals")

ggplot(data=final[final$posteam == 'CLE',], aes(x=TimeSecs, y=ScoreDiff, colour=WinOrLoss, label=GameLabel)) +
  geom_text_repel(show.legend = FALSE) +
  geom_point() +
  scale_x_reverse(lim=c(300, 0)) +
  scale_y_reverse(lim=c(0, -7)) +
  labs(colour = "", title = "Cleveland Browns")

ggplot(data=final[final$posteam == 'DAL',], aes(x=TimeSecs, y=ScoreDiff, colour=WinOrLoss, label=GameLabel)) +
  geom_text_repel(show.legend = FALSE) +
  geom_point() +
  scale_x_reverse(lim=c(300, 0)) +
  scale_y_reverse(lim=c(0, -7)) +
  xlab("Time Left (seconds)") + 
  ylab("Point Differential") +
  labs(colour = "", title = "Dallas Cowboys")

ggplot(data=final[final$posteam == 'DEN',], aes(x=TimeSecs, y=ScoreDiff, colour=WinOrLoss, label=GameLabel)) +
  geom_text_repel(show.legend = FALSE) +
  geom_point() +
  scale_x_reverse(lim=c(300, 0)) +
  scale_y_reverse(lim=c(0, -7)) +
  xlab("Time Left (seconds)") + 
  ylab("Point Differential") +
  labs(colour = "", title = "Denver Broncos")

ggplot(data=final[final$posteam == 'DET',], aes(x=TimeSecs, y=ScoreDiff, colour=WinOrLoss, label=GameLabel)) +
  geom_text_repel(show.legend = FALSE) +
  geom_point() +
  scale_x_reverse(lim=c(300, 0)) +
  scale_y_reverse(lim=c(0, -7)) +
  labs(colour = "", title = "Detroit Lions")

ggplot(data=final[final$posteam == 'GB',], aes(x=TimeSecs, y=ScoreDiff, colour=WinOrLoss, label=GameLabel)) +
  geom_text_repel(show.legend = FALSE) +
  geom_point() +
  scale_x_reverse(lim=c(300, 0)) +
  scale_y_reverse(lim=c(0, -7)) +
  xlab("Time Left (seconds)") + 
  ylab("Point Differential") +
  labs(colour = "", title = "Green Bay Packers")

ggplot(data=final[final$posteam == 'HOU',], aes(x=TimeSecs, y=ScoreDiff, colour=WinOrLoss, label=GameLabel)) +
  geom_text_repel(show.legend = FALSE) +
  geom_point() +
  scale_x_reverse(lim=c(300, 0)) +
  scale_y_reverse(lim=c(0, -7)) +
  xlab("Time Left (seconds)") + 
  ylab("Point Differential") +
  labs(colour = "", title = "Houston Texans")

ggplot(data=final[final$posteam == 'IND',], aes(x=TimeSecs, y=ScoreDiff, colour=WinOrLoss, label=GameLabel)) +
  geom_text_repel(show.legend = FALSE) +
  geom_point() +
  scale_x_reverse(lim=c(300, 0)) +
  scale_y_reverse(lim=c(0, -7)) +
  xlab("Time Left (seconds)") + 
  ylab("Point Differential") +
  labs(colour = "", title = "Indianapolis Colts")

ggplot(data=final[final$posteam == 'JAX',], aes(x=TimeSecs, y=ScoreDiff, colour=WinOrLoss, label=GameLabel)) +
  geom_text_repel(show.legend = FALSE) +
  geom_point() +
  scale_x_reverse(lim=c(300, 0)) +
  scale_y_reverse(lim=c(0, -7)) +
  xlab("Time Left (seconds)") + 
  ylab("Point Differential") +
  labs(colour = "", title = "Jacksonville Jaguars")

ggplot(data=final[final$posteam == 'KC',], aes(x=TimeSecs, y=ScoreDiff, colour=WinOrLoss, label=GameLabel)) +
  geom_text_repel(show.legend = FALSE) +
  geom_point() +
  scale_x_reverse(lim=c(300, 0)) +
  scale_y_reverse(lim=c(0, -7)) +
  xlab("Time Left (seconds)") + 
  ylab("Point Differential") +
  labs(colour = "", title = "Kansas City Chiefs")

ggplot(data=final[final$posteam == 'LAC',], aes(x=TimeSecs, y=ScoreDiff, colour=WinOrLoss, label=GameLabel)) +
  geom_text_repel(show.legend = FALSE) +
  geom_point() +
  scale_x_reverse(lim=c(300, 0)) +
  scale_y_reverse(lim=c(0, -7)) +
  xlab("Time Left (seconds)") + 
  ylab("Point Differential") +
  labs(colour = "", title = "Los Angeles Chargers")

ggplot(data=final[final$posteam == 'LA',], aes(x=TimeSecs, y=ScoreDiff, colour=WinOrLoss, label=GameLabel)) +
  geom_text_repel(show.legend = FALSE) +
  geom_point() +
  scale_x_reverse(lim=c(300, 0)) +
  scale_y_reverse(lim=c(0, -7)) +
  xlab("Time Left (seconds)") + 
  ylab("Point Differential") +
  labs(colour = "", title = "Los Angeles Rams")

ggplot(data=final[final$posteam == 'MIA',], aes(x=TimeSecs, y=ScoreDiff, colour=WinOrLoss, label=GameLabel)) +
  geom_text_repel(show.legend = FALSE) +
  geom_point() +
  scale_x_reverse(lim=c(300, 0)) +
  scale_y_reverse(lim=c(0, -7)) +
  xlab("Time Left (seconds)") + 
  ylab("Point Differential") +
  labs(colour = "", title = "Miami Dolphins")

ggplot(data=final[final$posteam == 'MIN',], aes(x=TimeSecs, y=ScoreDiff, colour=WinOrLoss, label=GameLabel)) +
  geom_text_repel(show.legend = FALSE) +
  geom_point() +
  scale_x_reverse(lim=c(300, 0)) +
  scale_y_reverse(lim=c(0, -7)) +
  xlab("Time Left (seconds)") + 
  ylab("Point Differential") +
  labs(colour = "", title = "Minnesota Vikings")

ggplot(data=final[final$posteam == 'NE',], aes(x=TimeSecs, y=ScoreDiff, colour=WinOrLoss, label=GameLabel)) +
  geom_text_repel(show.legend = FALSE) +
  geom_point() +
  scale_x_reverse(lim=c(300, 0)) +
  scale_y_reverse(lim=c(0, -7)) +
  xlab("Time Left (seconds)") + 
  ylab("Point Differential") +
  labs(colour = "", title = "New England Patriots")

ggplot(data=final[final$posteam == 'NO',], aes(x=TimeSecs, y=ScoreDiff, colour=WinOrLoss, label=GameLabel)) +
  geom_text_repel(show.legend = FALSE) +
  geom_point() +
  scale_x_reverse(lim=c(300, 0)) +
  scale_y_reverse(lim=c(0, -7)) +
  xlab("Time Left (seconds)") + 
  ylab("Point Differential") +
  labs(colour = "", title = "New Orleans Saints")

ggplot(data=final[final$posteam == 'NYG',], aes(x=TimeSecs, y=ScoreDiff, colour=WinOrLoss, label=GameLabel)) +
  geom_text_repel(show.legend = FALSE) +
  geom_point() +
  scale_x_reverse(lim=c(300, 0)) +
  scale_y_reverse(lim=c(0, -7)) +
  xlab("Time Left (seconds)") + 
  ylab("Point Differential") +
  labs(colour = "", title = "New York Giants")

ggplot(data=final[final$posteam == 'NYJ',], aes(x=TimeSecs, y=ScoreDiff, colour=WinOrLoss, label=GameLabel)) +
  geom_text_repel(show.legend = FALSE) +
  geom_point() +
  scale_x_reverse(lim=c(300, 0)) +
  scale_y_reverse(lim=c(0, -7)) +  
  xlab("Time Left (seconds)") + 
  ylab("Point Differential") +
  labs(colour = "", title = "New York Jets")

ggplot(data=final[final$posteam == 'OAK',], aes(x=TimeSecs, y=ScoreDiff, colour=WinOrLoss, label=GameLabel)) +
  geom_text_repel(show.legend = FALSE) +
  geom_point() +
  scale_x_reverse(lim=c(300, 0)) +
  scale_y_reverse(lim=c(0, -7)) +
  xlab("Time Left (seconds)") + 
  ylab("Point Differential") +
  labs(colour = "", title = "Oakland Raiders")

ggplot(data=final[final$posteam == 'PHI',], aes(x=TimeSecs, y=ScoreDiff, colour=WinOrLoss, label=GameLabel)) +
  geom_text_repel(show.legend = FALSE) +
  geom_point() +
  scale_x_reverse(lim=c(300, 0)) +
  scale_y_reverse(lim=c(0, -7)) +
  xlab("Time Left (seconds)") + 
  ylab("Point Differential") +
  labs(colour = "", title = "Philadelphia Eagles")

ggplot(data=final[final$posteam == 'PIT',], aes(x=TimeSecs, y=ScoreDiff, colour=WinOrLoss, label=GameLabel)) +
  geom_text_repel(show.legend = FALSE) +
  geom_point() +
  scale_x_reverse(lim=c(300, 0)) +
  scale_y_reverse(lim=c(0, -7)) +
  xlab("Time Left (seconds)") + 
  ylab("Point Differential") +
  labs(colour = "", title = "Pittsburgh Steelers")

ggplot(data=final[final$posteam == 'SF',], aes(x=TimeSecs, y=ScoreDiff, colour=WinOrLoss, label=GameLabel)) +
  geom_text_repel(show.legend = FALSE) +
  geom_point() +
  scale_x_reverse(lim=c(300, 0)) +
  scale_y_reverse(lim=c(0, -7)) +
  xlab("Time Left (seconds)") + 
  ylab("Point Differential") +
  labs(colour = "", title = "San Francisco 49ers")

ggplot(data=final[final$posteam == 'SEA',], aes(x=TimeSecs, y=ScoreDiff, colour=WinOrLoss, label=GameLabel)) +
  geom_text_repel(show.legend = FALSE) +
  geom_point() +
  scale_x_reverse(lim=c(300, 0)) +
  scale_y_reverse(lim=c(0, -7)) +
  xlab("Time Left (seconds)") + 
  ylab("Point Differential") +
  labs(colour = "", title = "Seattle Seahawks")

ggplot(data=final[final$posteam == 'TB',], aes(x=TimeSecs, y=ScoreDiff, colour=WinOrLoss, label=GameLabel)) +
  geom_text_repel(show.legend = FALSE) +
  geom_point() +
  scale_x_reverse(lim=c(300, 0)) +
  scale_y_reverse(lim=c(0, -7)) +
  xlab("Time Left (seconds)") + 
  ylab("Point Differential") +
  labs(colour = "", title = "Tampa Bay Buccaneers")

ggplot(data=final[final$posteam == 'TEN',], aes(x=TimeSecs, y=ScoreDiff, colour=WinOrLoss, label=GameLabel)) +
  geom_text_repel(show.legend = FALSE) +
  geom_point() +
  scale_x_reverse(lim=c(300, 0)) +
  scale_y_reverse(lim=c(0, -7)) +
  xlab("Time Left (seconds)") + 
  ylab("Point Differential") +
  labs(colour = "", title = "Tennessee Titans")

ggplot(data=final[final$posteam == 'WAS',], aes(x=TimeSecs, y=ScoreDiff, colour=WinOrLoss, label=GameLabel)) +
  geom_text_repel(show.legend = FALSE) +
  geom_point() +
  scale_x_reverse(lim=c(300, 0)) +
  scale_y_reverse(lim=c(0, -7)) +
  xlab("Time Left (seconds)") + 
  ylab("Point Differential") +
  labs(colour = "", title = "Washington Redskins")
