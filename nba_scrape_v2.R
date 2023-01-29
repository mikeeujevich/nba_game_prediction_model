#MU: Library load
library(rvest) #MU: For scraping
library(magrittr) #MU: For variable assignment shortcut
library(dplyr) #MU: For data manipulation
library(lubridate) #MU: Date cleanup
library(stringr) #MU: For column cleanup

#MU: Create character lists to iterate through for URLs. May want to separate in multiple pulls to not hit the server too much too fast.
year <- c('2017','2018','2019','2022')
month <- c('october','november','december','january','february','march','april','may','june')

#MU: Nested for loop to scrape through unique schedules by year and by month to get scores to prior games.
empty_df <- data.frame()

for(j in 1:length(year)) {
  for(k in 1:length(month)) {
    url <- paste0("https://www.basketball-reference.com/leagues/NBA_", 
                  year[j], 
                  "_games-", 
                  month[k],
                  ".html")
    
    duh <- as.data.frame(read_html(url) %>% html_nodes(xpath = '//*[@id="schedule"]') %>% html_table())
    duh <- duh %>% cbind(year[j],month[k])
    
    empty_df %<>% rbind(duh) 
  }
}

#MU: Get all visitor scores for future union with home since we're predicting points
visitor <- empty_df %>% 
  select(Date,Visitor.Neutral,PTS,`year[j]`,`month[k]`,Home.Neutral) %>% 
  cbind('Visitor')

names(visitor) <- c('date','team','pts','season','month','opponent','location')

visitor$date <- substring(visitor$date, 6)
visitor$date <- mdy(visitor$date)
visitor$back_to_back_join <- visitor$date - 1
visitor$pts <- as.numeric(visitor$pts)
visitor %<>% na.omit()

#MU: Get all home scores for future union with visitor since we're predicting points
home <- empty_df %>% 
  select(Date,Home.Neutral,PTS,`year[j]`,`month[k]`,Visitor.Neutral) %>% 
  cbind('Home')

names(home) <- c('date','team','pts','season','month','opponent','location')

home$date <- substring(home$date, 6)
home$date <- mdy(home$date)
home$back_to_back_join <- home$date - 1
home$pts <- as.numeric(home$pts)
home %<>% na.omit()

#MU: Consolidate home & visitor into one data frame. Add back to back join to get that variable
consolidated_teams_v1 <- union_all(consolidated_teams,home,visitor)

back_to_back <- consolidated_teams_v1 %>% 
  mutate(back_to_back = 1) %>% 
  select(date,team,back_to_back)

for_general_model <- left_join(consolidated_teams_v1,back_to_back, by = c("back_to_back_join" = "date", "team" = "team"))
for_general_model$back_to_back[is.na(for_general_model$back_to_back)] <- 0
for_general_model %<>% select(date,season,team,pts,location,month,back_to_back,opponent)

#MU: Bring in team advanced stats for prediction
empty_df <- data.frame()

year <- c('2017','2018','2019','2022')

for(j in 1:length(year)) {
    url <- paste0("https://www.basketball-reference.com/leagues/NBA_", 
                  year[j], 
                  ".html")
    
    duh <- as.data.frame(read_html(url) %>% html_nodes(xpath = '//*[@id="advanced-team"]') %>% html_table())
    duh <- duh %>% cbind(year[j])
    
    empty_df %<>% rbind(duh) 
}

advanced_stats <- empty_df
names(advanced_stats) = c('one','two','three','four','five','six','seven','eight',
                          'nine','ten','eleven','twelve','thirteen','fourteen','fifteen','sixteen',
                          'seventeen','eighteen','nineteen','twenty','twenty_one','twenty_two','twenty_three','twenty_four',
                          'twenty_five','twenty_six','twenty_seven','twenty_eight','twenty_nine','thirty','thirty_one','thirty_two')

advanced_stats_v1 <- advanced_stats %>% 
  select(two,three,eleven,thirteen,fourteen,fifteen,sixteen,seventeen,nineteen,twenty,twenty_one,twenty_two,
         twelve,twenty_four,twenty_five,twenty_six,twenty_seven,thirty_two)
         
names(advanced_stats_v1) = c('team','age','ortg','nrtg','pace','ftr','attrte3','ts_pct','efg_pct',
                             'tov_pct','orb_pct','ft_per_fga',
                          'drtg','def_efg','def_tov_pct','drb_pct','def_ft_per_fga','year')

advanced_stats_v1 %<>% filter(team != 'League Average' & team != 'Team') 
advanced_stats_v1$team <- str_replace(advanced_stats_v1$team,"\\*","")
advanced_stats_v1$nrtg <- str_replace(advanced_stats_v1$nrtg,"\\+","")

#MU: Converting a bunch of the present string columns to numeric. Too lazy to write a for loop, probably the same amount of time anyhow.
advanced_stats_v1$age <- as.numeric(advanced_stats_v1$age)
advanced_stats_v1$ortg <- as.numeric(advanced_stats_v1$ortg)
advanced_stats_v1$nrtg <- as.numeric(advanced_stats_v1$nrtg)
advanced_stats_v1$pace <- as.numeric(advanced_stats_v1$pace)
advanced_stats_v1$ftr <- as.numeric(advanced_stats_v1$ftr)
advanced_stats_v1$attrte3 <- as.numeric(advanced_stats_v1$attrte3)
advanced_stats_v1$ts_pct <- as.numeric(advanced_stats_v1$ts_pct)
advanced_stats_v1$efg_pct <- as.numeric(advanced_stats_v1$efg_pct)
advanced_stats_v1$tov_pct <- as.numeric(advanced_stats_v1$tov_pct)
advanced_stats_v1$orb_pct <- as.numeric(advanced_stats_v1$orb_pct)
advanced_stats_v1$ft_per_fga <- as.numeric(advanced_stats_v1$ft_per_fga)
advanced_stats_v1$drtg <- as.numeric(advanced_stats_v1$drtg)
advanced_stats_v1$def_efg <- as.numeric(advanced_stats_v1$def_efg)
advanced_stats_v1$def_tov_pct <- as.numeric(advanced_stats_v1$def_tov_pct)
advanced_stats_v1$drb_pct <- as.numeric(advanced_stats_v1$drb_pct)
advanced_stats_v1$def_ft_per_fga <- as.numeric(advanced_stats_v1$def_ft_per_fga)

#MU: Bring in team general stats for prediction
empty_df <- data.frame()

for(j in 1:length(year)) {
  url <- paste0("https://www.basketball-reference.com/leagues/NBA_", 
                year[j], 
                ".html")
  
  duh <- as.data.frame(read_html(url) %>% html_nodes(xpath = '//*[@id="per_game-opponent"]') %>% html_table())
  duh <- duh %>% cbind(year[j])
  
  empty_df %<>% rbind(duh) 
}

general_stats <- empty_df
general_stats_v1 <- general_stats %>% 
  select(Team,FGA,X3PA,X3P.,X2PA,X2P.,FT.,ORB,TOV,DRB,STL,BLK,PF,`year[j]`)
names(general_stats_v1) = c('team','fga','three_pa','three_pct','two_pa','two_pct','ft_pct',
                            'orb','tov','drb','stl','blk','pf','year')

general_stats_v1$team <- str_replace(general_stats_v1$team,"\\*","")
general_stats_v1 %<>% filter(team != 'League Average' & team != 'Team') 

#MU: Let's try a basic linear model on the general stats
for_general_model_v1 <-
  left_join(for_general_model,general_stats_v1, by = c("team" = "team", "season" = "year"))

for_general_model_v2 <-
  left_join(for_general_model_v1,general_stats_v1, by = c("opponent" = "team", "season" = "year"))

model_data <- for_general_model_v2 %>% select(-date, -team,-opponent,-season)
lm_gen <- lm(pts ~ ., model_data)
summary(lm_gen)

#MU: Bleh. Let's try a basic linear model on the advanced stats
for_advanced_model_v1 <-
  left_join(for_general_model,advanced_stats_v1, by = c("team" = "team", "season" = "year"))

for_advanced_model_v2 <-
  left_join(for_advanced_model_v1,advanced_stats_v1, by = c("opponent" = "team", "season" = "year"))

model_data <- for_advanced_model_v2 %>% select(-date, -team,-opponent,-season)
lm_adv <- lm(pts ~ ., model_data)
summary(lm_adv)


combined_model <- left_join(for_general_model_v2, for_advanced_model_v2, 
                            by = c("date" = "date", "team" = "team",
                                   "back_to_back" = "back_to_back",
                                   "season" = "season", "pts" = "pts",
                                   "month" = "month", "opponent" = "opponent",
                                   "location" = "location"))
model_data <- combined_model %>% select(-date, -team, -opponent, -season)
lm_all <- lm(pts ~ ., model_data)
summary(lm_all)

#MU: Going to create a custom model for predictions

# If I was going to create a test/train set, but not doing that. Going to use Monte Carlo
# for_pred <- sample(c(TRUE,FALSE), nrow(model_data), replace=TRUE, prob=c(0.7,0.3))
# train  <- model_data[for_pred, ]
# test   <- model_data[!for_pred, ]

lm_select <- lm(pts ~ location * back_to_back +
                  month * back_to_back +
                  nrtg.x * nrtg.y +
                  age.x * age.y +
                  pace.x * pace.y +
                  ts_pct.x * ts_pct.y +
                  tov_pct.x * drtg.y +
                  three_pct.x * attrte3.x +
                  two_pct.x * blk.y +
                  orb_pct.x * drb.y +
                  tov_pct.y * drtg.x +
                  three_pct.y * attrte3.y +
                  two_pct.y * blk.x +
                  orb_pct.y * drb.x +
                  tov_pct.x * drtg.y, model_data)
summary(lm_select)

#MU: Building the prediction interval
teams <- combined_model %>% filter(location == "Visitor") %>% select(date, team, opponent)
for_join <- as.data.frame(round(predict(lm_select, combined_model, interval = "prediction", level = .95),2))
joined_up <- cbind(combined_model,for_join) %>% select(date,team,lwr,upr)

#MU: Wrote a for loop to simulate 1,000 runs for each game to generate probabilities
sim <- data.frame()

for(j in 1:1000) {
  joined_up$rand = round(runif(n = nrow(joined_up),min = joined_up$lwr, max = joined_up$upr),3)
  for_trans <- joined_up %>% select(team, date, rand)
  empty_df <- left_join(teams,for_trans, by = c("team" = "team", "date" = "date"))
  empty_df <- left_join(empty_df,for_trans, by = c("opponent" = "team", "date" = "date"))
  empty_df %<>% mutate(visitor_win = ifelse(rand.x > rand.y, 1, 0))
  
  sim %<>% rbind(empty_df)
  j + 1
}

#MU: Getting some analysis on predicted wins!
game_summaries <- sim %>% group_by(date,team,opponent) %>% summarise(visitor_prob = sum(visitor_win)/1000)
game_summaries$year = format(as.Date(game_summaries$date, format="%d/%m/%Y"),"%Y")
game_summaries$month = format(as.Date(game_summaries$date, format="%d/%m/%Y"),"%m")

best_teams_2017 <- 
  game_summaries %>% filter(month == 6 | month == 7 | month == 8 | (year != 2021 & month != 10) | (year != 2021 & month != 11) | (year != 2021 & month != 12)) %>% 
  group_by(team) %>% 
  summarise(total_wins = (sum(visitor_prob)/3)*2) %>% 
  arrange(-total_wins)
