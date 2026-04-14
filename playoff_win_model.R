library(tidyverse)
library(ggimage)
library(gt)
library(ggthemes)
library(gtExtras)
library(dplyr)
library(hoopR)
library(rvest)

#create database of playoff series results


schedule = data.frame(espn_nba_scoreboard(2003))|>
  filter(season_type==3)|>
  select(c("home_team_full_name","away_team_full_name","home_win","away_win","game_date","season","matchup"))


for (n in 2004:2025) {
  year_schedule <- data.frame(espn_nba_scoreboard(n))|>
    filter(season_type==3)|>
    select(c("home_team_full_name","away_team_full_name","home_win","away_win","game_date","season","matchup"))
  schedule <- rbind(schedule,year_schedule)
}

schedule_series <- schedule|>
  #remove duplicates
  distinct(game_date,matchup,.keep_all = TRUE)|>
  group_by(season,matchup)|>
  arrange(game_date)|>
  filter(!is.na(home_win))|>
  summarize(home_team=first(home_team_full_name),away_team=first(away_team_full_name),home_wins=sum(home_win),away_wins=sum(away_win),
            date = first(game_date))|>
  #fix clippers
  mutate(home_team = case_when(home_team=="LA Clippers"~"Los Angeles Clippers",
                        home_team!="LA Clippers"~home_team))|>
  mutate(away_team = case_when(away_team=="LA Clippers"~"Los Angeles Clippers",
                               away_team!="LA Clippers"~away_team))|>
  arrange(date)


#figure out how many games each team won and create home win variable


n = 1
i = 0
while(n<nrow(schedule_series)){
  i=n+1
  while(i<nrow(schedule_series)){
    #if matchup and season is the same
    if(schedule_series[n,3]==schedule_series[i,4]&schedule_series[i,3]==schedule_series[n,4]
       &schedule_series[n,1]==schedule_series[i,1]){
      #add home and away wins together for each team
      schedule_series[n,5]<-schedule_series[i,6]+schedule_series[n,5]
      schedule_series[n,6]<-schedule_series[n,6]+schedule_series[i,5]
      #remove second matchup row (there's one where each team is at home)
      schedule_series <- schedule_series[-c(i),]
    }
    #move onto next row to check matchup
    i <- i+1
  }
  #check next matchup
  n <- n+1
}
#manually input thunder pacers 2025 (change this every year to the most recent finals)
schedule_series[nrow(schedule_series)-1,5] <- 4
schedule_series[nrow(schedule_series)-1,6] <- 3
schedule_series <- schedule_series[-c(nrow(schedule_series)),]
#add variable for home team winning
schedule_series <- schedule_series|>
  mutate(home_series_win =case_when(home_wins==4~1,
                                    away_wins==4~0))



#scrape bball reference for advanced team stats

get_team_advanced_stats <- function(year){
  year_team_stuff <- read_html(paste("https://www.basketball-reference.com/leagues/NBA_",year,".html",sep=""))
  year_team_stuff <- year_team_stuff|>
    html_nodes("table")|>
    html_table()
  if(year<2016){
    year_advanced_stats <- data.frame(year_team_stuff[[9]])|>
      select(c("Var.2", "Var.13","Offense.Four.Factors","Offense.Four.Factors.1","Offense.Four.Factors.2",
               "Offense.Four.Factors.3","Defense.Four.Factors", "Defense.Four.Factors.1", "Defense.Four.Factors.2", "Defense.Four.Factors.3"))|>
      rename("Team"="Var.2","net_rating"="Var.13","off_eFG"="Offense.Four.Factors",
             "off_tov_rate"="Offense.Four.Factors.1","off_reb_rate"="Offense.Four.Factors.2","off_ft_fga_ratio"="Offense.Four.Factors.3",
             "def_eFG"="Defense.Four.Factors","def_tov_rate"="Defense.Four.Factors.1","def_reb_rate"="Defense.Four.Factors.2",
             "def_ft_fga_ratio"="Defense.Four.Factors.3")|>
      filter(Team!="Team",Team!="League Average")
  }
  else if(year<2024){
    year_advanced_stats <- data.frame(year_team_stuff[[11]])|>
      filter(!is.na(Var.1))|>
      select(c("Var.2", "Var.13","Offense.Four.Factors","Offense.Four.Factors.1","Offense.Four.Factors.2",
               "Offense.Four.Factors.3","Defense.Four.Factors", "Defense.Four.Factors.1", "Defense.Four.Factors.2", "Defense.Four.Factors.3"))|>
      rename("Team"="Var.2", "net_rating"="Var.13","off_eFG"="Offense.Four.Factors",
             "off_tov_rate"="Offense.Four.Factors.1","off_reb_rate"="Offense.Four.Factors.2","off_ft_fga_ratio"="Offense.Four.Factors.3",
             "def_eFG"="Defense.Four.Factors","def_tov_rate"="Defense.Four.Factors.1","def_reb_rate"="Defense.Four.Factors.2",
             "def_ft_fga_ratio"="Defense.Four.Factors.3")|>
      filter(Team!="Team",Team!="League Average")
  }
  else if(year<=2026){
    year_advanced_stats <- data.frame(year_team_stuff[[11]])|>
      filter(!is.na(Var.1))|>
      select(c("Var.2", "Var.13","Offense.Four.Factors","Offense.Four.Factors.1","Offense.Four.Factors.2",
               "Offense.Four.Factors.3","Defense.Four.Factors", "Defense.Four.Factors.1", "Defense.Four.Factors.2", "Defense.Four.Factors.3"))|>
      rename("Team"="Var.2", "net_rating"="Var.13","off_eFG"="Offense.Four.Factors",
             "off_tov_rate"="Offense.Four.Factors.1","off_reb_rate"="Offense.Four.Factors.2","off_ft_fga_ratio"="Offense.Four.Factors.3",
             "def_eFG"="Defense.Four.Factors","def_tov_rate"="Defense.Four.Factors.1","def_reb_rate"="Defense.Four.Factors.2",
             "def_ft_fga_ratio"="Defense.Four.Factors.3")|>
      filter(Team!="Team",Team!="League Average")
  }
}
team_stats <- data.frame()

for (i in 2003:2025){
  year_advanced_stats <- get_team_advanced_stats(i)|>
    mutate(season=i)|>
    mutate(net_rating = as.numeric(net_rating))|>
    #remove bball reference playoff team indicator thing
    mutate(across("Team",str_replace,"\\*",""))
  team_stats <- rbind(team_stats,year_advanced_stats)
  i <- i+1
  #sleep so site doesnt block me
  if(i==2014){
    Sys.sleep(60)
  }
}



#scrape bball reference to get best player and top 3 player VORPs
get_advanced_player_stats <- function(year){
  year_vorp <- read_html(paste("https://www.basketball-reference.com/leagues/NBA",year,"advanced.html",sep="_"))
  year_vorp <- year_vorp|>
    html_node("table")|>
    html_table()|>
    select(-20,-25)|>
    filter(Team!="TOT",Team!="2TM",!is.na("VORP"))|>
    group_by(Team)|>
    mutate(VORP = as.numeric(VORP))|>
    arrange(desc(VORP))|>
    slice_max(VORP,n=3)|>
    summarize(top_3_players = paste(first(Player),nth(Player,2),last(Player),sep=", "),
              top_3_vorp = sum(VORP),
              best_player=first(Player),
              best_vorp = first(VORP))
}
player_stats <- data.frame()

for (i in 2003:2025){
  year_player_stats <- get_advanced_player_stats(i)|>
    mutate(season=i)
  player_stats <- rbind(player_stats,year_player_stats)
  i <- i+1
  if(i==2014){
    Sys.sleep(60)
  }
}

#change team column to help with joining
player_stats_team_fix <- player_stats|>
  mutate(Team = case_when(Team=="ATL"~"Atlanta Hawks",Team=="BOS"~"Boston Celtics",Team=="BRK"~"Brooklyn Nets",Team=="CHA"~"Charlotte Bobcats",
                        Team=="CHI"~"Chicago Bulls",Team=="CHO"~"Charlotte Hornets",Team=="CLE"~"Cleveland Cavaliers",
                        Team=="DAL"~"Dallas Mavericks",Team=="DEN"~"Denver Nuggets",Team=="DET"~"Detroit Pistons",
                        Team=="GSW"~"Golden State Warriors",Team=="HOU"~"Houston Rockets",Team=="IND"~"Indiana Pacers",
                        Team=="LAC"~"Los Angeles Clippers",Team=="LAL"~"Los Angeles Lakers",Team=="MEM"~"Memphis Grizzlies",
                        Team=="MIA"~'Miami Heat',Team=="MIL"~"Milwaukee Bucks",Team=="MIN"~"Minnesota Timberwolves",
                        Team=="NJN"~"New Jersey Nets",Team=="NOH"~"New Orleans Hornets",Team=="NOK"~"New Orleans/Oklahoma City Hornets",
                        Team=="NOP"~"New Orleans Pelicans",Team=="NYK"~"New York Knicks",Team=="OKC"~"Oklahoma City Thunder",
                        Team=="ORL"~"Orlando Magic",Team=="PHI"~"Philadelphia 76ers",Team=="PHO"~"Phoenix Suns",
                        Team=="POR"~"Portland Trail Blazers",Team=="SAC"~"Sacramento Kings",Team=="SAS"~"San Antonio Spurs",
                        Team=="SEA"~"Seattle SuperSonics",Team=="TOR"~"Toronto Raptors",Team=="UTA"~"Utah Jazz",Team=="WAS"~"Washington Wizards"))|>
  filter(!is.na(Team))



#join data that will be used in the model


team_stats_total <- left_join(team_stats,player_stats_team_fix,by=c("Team","season"))|>
  mutate(off_eFG=as.numeric(off_eFG),
         off_tov_rate = as.numeric(off_tov_rate),
         off_reb_rate = as.numeric(off_reb_rate),
         off_ft_fga_ratio = as.numeric(off_ft_fga_ratio),
         def_eFG = as.numeric(def_eFG),
         def_tov_rate = as.numeric(def_tov_rate),
         def_reb_rate = as.numeric(def_reb_rate),
         def_ft_fga_ratio = as.numeric(def_ft_fga_ratio)
         )



#define functions to ensure home and away columns are separate

rename_home <- function(x){
  x <- x|>
    rename("h_net_rating"="net_rating",
           "h_off_eFG"="off_eFG","h_off_tov_rate"="off_tov_rate","h_off_reb_rate"="off_reb_rate",
           "h_off_ft_fga_ratio"="off_ft_fga_ratio","h_def_eFG"="def_eFG","h_def_tov_rate"="def_tov_rate",
           "h_def_reb_rate"="def_reb_rate","h_def_ft_fga_ratio"="def_ft_fga_ratio","h_top_3_players"="top_3_players",
           "h_top_3_vorp"="top_3_vorp","h_best_player"="best_player","h_best_vorp"="best_vorp")
  
}

rename_away <- function(x){
  x <- x|>
    rename("a_net_rating"="net_rating",
           "a_off_eFG"="off_eFG","a_off_tov_rate"="off_tov_rate","a_off_reb_rate"="off_reb_rate",
           "a_off_ft_fga_ratio"="off_ft_fga_ratio","a_def_eFG"="def_eFG","a_def_tov_rate"="def_tov_rate",
           "a_def_reb_rate"="def_reb_rate","a_def_ft_fga_ratio"="def_ft_fga_ratio","a_top_3_players"="top_3_players",
           "a_top_3_vorp"="top_3_vorp","a_best_player"="best_player","a_best_vorp"="best_vorp")
}

team_stats_home <- rename_home(team_stats_total)

model_data <- left_join(schedule_series,team_stats_home,by=c("home_team"="Team","season"))



team_stats_away <- rename_away(team_stats_total)


model_data <- left_join(model_data,team_stats_away,by=c("away_team"="Team","season"))|>
  rename("h_season" = "season")

model_data$a_season = model_data$h_season


##run model

subtract_data <- function(x){
  x <- x|>
    mutate(net_rating_diff = h_net_rating-a_net_rating, off_eFG_diff = h_off_eFG-a_off_eFG,
           off_tov_rate_diff = h_off_tov_rate - a_off_tov_rate, off_reb_rate_diff = h_off_reb_rate - a_off_reb_rate,
           off_reb_rate_diff = h_def_reb_rate-a_def_reb_rate, off_ft_fga_ratio_diff = h_off_ft_fga_ratio - a_off_ft_fga_ratio,
           def_eFG_diff = h_def_eFG - a_def_eFG, def_tov_rate_diff = h_def_tov_rate-a_def_tov_rate,
           def_reb_rate_diff = h_def_reb_rate - a_def_reb_rate, def_ft_fga_ratio_diff = h_def_ft_fga_ratio-a_def_ft_fga_ratio,
           top_3_vorp_diff = h_top_3_vorp - a_top_3_vorp, best_vorp_diff = h_best_vorp - a_best_vorp)
}

model_data <- subtract_data(model_data)

home_playoff_win_model <- glm(home_series_win~h_season+net_rating_diff+off_eFG_diff+off_tov_rate_diff+off_reb_rate_diff+
                                off_ft_fga_ratio_diff+def_eFG_diff+def_tov_rate_diff+def_reb_rate_diff+def_ft_fga_ratio_diff+
                                top_3_vorp_diff+best_vorp_diff+a_season,data=model_data,family="binomial")



#scrape 2026 standings and stats from bball reference
bball_2026 <- read_html("https://www.basketball-reference.com/leagues/NBA_2026.html")
standings_2026 <- bball_2026|>
  html_nodes("table")|>
  html_table()
standings_2026_ecf <- standings_2026[[1]]|>
  rename("Team"="Eastern Conference")|>
  mutate(rank=row_number())
standings_2026_wcf <- standings_2026[[2]]|>
  rename("Team"="Western Conference")|>
  mutate(rank=row_number())


#create first round matchup dataset


ecf_round1 <- data.frame(matrix(ncol=4,nrow=4))
colnames(ecf_round1)=(c("home_team","home_seed","away_team","away_seed"))
ecf_round1[[1]][[1]] <- standings_2026_ecf[[1]][[1]]
ecf_round1[[2]][[1]] <- standings_2026_ecf[[9]][[1]]
ecf_round1[[3]][[1]] <- standings_2026_ecf[[1]][[8]]
ecf_round1[[4]][[1]] <- standings_2026_ecf[[9]][[8]]
ecf_round1[[1]][[2]] <- standings_2026_ecf[[1]][[4]]
ecf_round1[[2]][[2]] <- standings_2026_ecf[[9]][[4]]
ecf_round1[[3]][[2]] <- standings_2026_ecf[[1]][[5]]
ecf_round1[[4]][[2]] <- standings_2026_ecf[[9]][[5]]
ecf_round1[[1]][[3]] <- standings_2026_ecf[[1]][[3]]
ecf_round1[[2]][[3]] <- standings_2026_ecf[[9]][[3]]
ecf_round1[[3]][[3]] <- standings_2026_ecf[[1]][[6]]
ecf_round1[[4]][[3]] <- standings_2026_ecf[[9]][[6]]
ecf_round1[[1]][[4]] <- standings_2026_ecf[[1]][[2]]
ecf_round1[[2]][[4]] <- standings_2026_ecf[[9]][[2]]
ecf_round1[[3]][[4]] <- standings_2026_ecf[[1]][[7]]
ecf_round1[[4]][[4]] <- standings_2026_ecf[[9]][[7]]

wcf_round1 <- data.frame(matrix(ncol=4,nrow=4))
colnames(wcf_round1)=(c("home_team","home_seed","away_team","away_seed"))
wcf_round1[[1]][[1]] <- standings_2026_wcf[[1]][[1]]
wcf_round1[[2]][[1]] <- standings_2026_wcf[[9]][[1]]
wcf_round1[[3]][[1]] <- standings_2026_wcf[[1]][[8]]
wcf_round1[[4]][[1]] <- standings_2026_wcf[[9]][[8]]
wcf_round1[[1]][[2]] <- standings_2026_wcf[[1]][[4]]
wcf_round1[[2]][[2]] <- standings_2026_wcf[[9]][[4]]
wcf_round1[[3]][[2]] <- standings_2026_wcf[[1]][[5]]
wcf_round1[[4]][[2]] <- standings_2026_wcf[[9]][[5]]
wcf_round1[[1]][[3]] <- standings_2026_wcf[[1]][[3]]
wcf_round1[[2]][[3]] <- standings_2026_wcf[[9]][[3]]
wcf_round1[[3]][[3]] <- standings_2026_wcf[[1]][[6]]
wcf_round1[[4]][[3]] <- standings_2026_wcf[[9]][[6]]
wcf_round1[[1]][[4]] <- standings_2026_wcf[[1]][[2]]
wcf_round1[[2]][[4]] <- standings_2026_wcf[[9]][[2]]
wcf_round1[[3]][[4]] <- standings_2026_wcf[[1]][[7]]
wcf_round1[[4]][[4]] <- standings_2026_wcf[[9]][[7]]




playoffs_2026 <- rbind(ecf_round1,wcf_round1)

#fix team columns so joining works

playoffs_2026$home_team <- str_remove_all(playoffs_2026$home_team,"[(12345678*)]")
playoffs_2026$away_team <- str_remove_all(playoffs_2026$away_team,"[(12345678*)]")
playoffs_2026$home_team <- str_trim(playoffs_2026$home_team,side=c("right"))
playoffs_2026$away_team <- str_trim(playoffs_2026$away_team,side=c("right"))


#fix 76ers
playoffs_2026$away_team <- case_when(playoffs_2026$away_team=="Philadelphia ers"~"Philadelphia 76ers",
                                     playoffs_2026$away_team!="Philadelphia ers"~playoffs_2026$away_team)
#get rid of some useless stuff
rm(player_stats)
rm(schedule)
rm(year_advanced_stats)
rm(year_clutch_stats)
rm(year_player_stats)
rm(year_schedule)

#get data for 2026 teams
team_stats_2026 <- get_team_advanced_stats(2026)|>
  mutate(across("Team",str_replace,"\\*",""))
player_stats_2026 <- get_advanced_player_stats(2026)|>
  mutate(Team = case_when(Team=="ATL"~"Atlanta Hawks",Team=="BOS"~"Boston Celtics",Team=="BRK"~"Brooklyn Nets",Team=="CHA"~"Charlotte Bobcats",
                        Team=="CHI"~"Chicago Bulls",Team=="CHO"~"Charlotte Hornets",Team=="CLE"~"Cleveland Cavaliers",
                        Team=="DAL"~"Dallas Mavericks",Team=="DEN"~"Denver Nuggets",Team=="DET"~"Detroit Pistons",
                        Team=="GSW"~"Golden State Warriors",Team=="HOU"~"Houston Rockets",Team=="IND"~"Indiana Pacers",
                        Team=="LAC"~"Los Angeles Clippers",Team=="LAL"~"Los Angeles Lakers",Team=="MEM"~"Memphis Grizzlies",
                        Team=="MIA"~'Miami Heat',Team=="MIL"~"Milwaukee Bucks",Team=="MIN"~"Minnesota Timberwolves",
                        Team=="NJN"~"New Jersey Nets",Team=="NOH"~"New Orleans Hornets",Team=="NOK"~"New Orleans/Oklahoma City Hornets",
                        Team=="NOP"~"New Orleans Pelicans",Team=="NYK"~"New York Knicks",Team=="OKC"~"Oklahoma City Thunder",
                        Team=="ORL"~"Orlando Magic",Team=="PHI"~"Philadelphia 76ers",Team=="PHO"~"Phoenix Suns",
                        Team=="POR"~"Portland Trail Blazers",Team=="SAC"~"Sacramento Kings",Team=="SAS"~"San Antonio Spurs",
                        Team=="SEA"~"Seattle SuperSonics",Team=="TOR"~"Toronto Raptors",Team=="UTA"~"Utah Jazz",Team=="WAS"~"Washington Wizards"))


#join 2026 data to playoff series

data_2026 <- left_join(team_stats_2026,player_stats_2026,by=c("Team"))|>
  mutate(net_rating = as.numeric(net_rating),
         off_eFG=as.numeric(off_eFG),
         off_tov_rate = as.numeric(off_tov_rate),
         off_reb_rate = as.numeric(off_reb_rate),
         off_ft_fga_ratio = as.numeric(off_ft_fga_ratio),
         def_eFG = as.numeric(def_eFG),
         def_tov_rate = as.numeric(def_tov_rate),
         def_reb_rate = as.numeric(def_reb_rate),
         def_ft_fga_ratio = as.numeric(def_ft_fga_ratio),
         season = 2026)
         
playoffs_data <- left_join(playoffs_2026,data_2026,by=c("home_team"="Team"))
playoffs_data <- rename_home(playoffs_data)
playoffs_data <- left_join(playoffs_data,data_2026,by=c("away_team"="Team"))
playoffs_data <- rename_away(playoffs_data)|>
  rename("h_season" = "season.x", "a_season" = "season.y")
playoffs_data <- subtract_data(playoffs_data)

#apply model to 2026 first round games
predictions <- predict(home_playoff_win_model,newdata=playoffs_data,type="response")
playoffs_2026$home_win_probability <- predictions   
playoffs_2026$round <- 1


#use first round results to create second round


second_round <- data.frame(matrix(ncol=4,nrow=4))
colnames(second_round)=(c("home_team","home_seed","away_team","away_seed"))
n = 1
i=1
while(i<=nrow(second_round)){
  if(playoffs_2026[[5]][[n]]>0.5){
    team1 <- playoffs_2026[[1]][[n]]
    seed1 <- playoffs_2026[[2]][[n]]
  }
  else{
    team1 <- playoffs_2026[[3]][[n]]
    seed1 <- playoffs_2026[[4]][[n]]
  }
  n = n+1
  if(playoffs_2026[[5]][[n]]>0.5){
    team2=playoffs_2026[[1]][[n]]
    seed2=playoffs_2026[[2]][[n]]
  }
  else{
    team2<-playoffs_2026[[3]][[n]]
    seed2<-playoffs_2026[[4]][[n]]
  }
  if(seed1>seed2){
    second_round[[1]][[i]] <- team2
    second_round[[2]][[i]] <- seed2
    second_round[[3]][[i]] <- team1
    second_round[[4]][[i]] <- seed1
  }
  else{
    second_round[[1]][[i]] <- team1
    second_round[[2]][[i]] <- seed1
    second_round[[3]][[i]] <- team2
    second_round[[4]][[i]] <- seed2
  }
  i = i+1
  n = n+1
}

#apply model to second round

second_round_model <- left_join(second_round,data_2026,by=c("home_team"="Team"))
second_round_model <- rename_home(second_round_model)

second_round_model <- left_join(second_round_model,data_2026,by=c("away_team"="Team"))
second_round_model <- rename_away(second_round_model)|>
  rename("h_season" = "season.x", "a_season" = "season.y")
second_round_model <- subtract_data(second_round_model)



predictions <- predict(home_playoff_win_model,newdata=second_round_model,type="response")
second_round$home_win_probability <- predictions   
second_round$round <- 2




#use round 2 projections to create round 3



third_round <- data.frame(matrix(ncol=4,nrow=2))
colnames(third_round)=(c("home_team","home_seed","away_team","away_seed"))
n = 1
i=1
while(i<=nrow(third_round)){
  if(second_round[[5]][[n]]>0.5){
    team1 <- second_round[[1]][[n]]
    seed1 <- second_round[[2]][[n]]
  }
  else{
    team1 <- second_round[[3]][[n]]
    seed1 <- second_round[[4]][[n]]
  }
  n = n+1
  if(second_round[[5]][[n]]>0.5){
    team2=second_round[[1]][[n]]
    seed2=second_round[[2]][[n]]
  }
  else{
    team2<-second_round[[3]][[n]]
    seed2<-second_round[[4]][[n]]
  }
  if(seed1>seed2){
    third_round[[1]][[i]] <- team2
    third_round[[2]][[i]] <- seed2
    third_round[[3]][[i]] <- team1
    third_round[[4]][[i]] <- seed1
  }
  else{
    third_round[[1]][[i]] <- team1
    third_round[[2]][[i]] <- seed1
    third_round[[3]][[i]] <- team2
    third_round[[4]][[i]] <- seed2
  }
  i = i+1
  n = n+1
}

#apply model to second round

third_round_model <- left_join(third_round,data_2026,by=c("home_team"="Team"))
third_round_model <- rename_home(third_round_model)

third_round_model <- left_join(third_round_model,data_2026,by=c("away_team"="Team"))
third_round_model <- rename_away(third_round_model)|>
  rename("h_season" = "season.x", "a_season" = "season.y")
third_round_model <- subtract_data(third_round_model)

predictions <- predict(home_playoff_win_model,newdata=third_round_model,type="response")
third_round$home_win_probability <- predictions   
third_round$round <- 3




#use round 3 predictions to make finals - wins are needed to determine home team
total_standings <- rbind(standings_2026_ecf,standings_2026_wcf)
total_standings$Team <- str_remove_all(total_standings$Team,"[(12345678*)]")
total_standings$Team <- str_trim(total_standings$Team,side=c("right"))
third_round_wins <- left_join(third_round,total_standings,by=c("home_team"="Team"))|>
  select(home_team,W,away_team,home_win_probability)|>
  rename("home_wins" = "W")
third_round_wins <- left_join(third_round_wins,total_standings,by=c("away_team"="Team"))|>
  select(home_team,home_wins,away_team,W,home_win_probability)|>
  rename("away_wins" = "W")


finals <- data.frame(matrix(ncol=4,nrow=1))
colnames(finals)=(c("home_team","home_wins","away_team","away_wins"))
n = 1
i=1
while(i<=nrow(finals)){
if(third_round_wins[[5]][[n]]>0.5){
  team1 <- third_round_wins[[1]][[n]]
  wins1 <- third_round_wins[[2]][[n]]
} else{
  team1 <- third_round_wins[[3]][[n]]
  wins1 <- third_round_wins[[4]][[n]]
}
  n = n+1
if(third_round_wins[[5]][[n]]>0.5){
  team2=third_round_wins[[1]][[n]]
  wins2=third_round_wins[[2]][[n]]
} else{
  team2<-third_round_wins[[3]][[n]]
  wins2<-third_round_wins[[4]][[n]]
}
if(wins1<wins2){
  finals[[1]][[i]] <- team2
  finals[[2]][[i]] <- wins2
  finals[[3]][[i]] <- team1
  finals[[4]][[i]] <- wins1
} else{
  finals[[1]][[i]] <- team1
  finals[[2]][[i]] <- wins1
  finals[[3]][[i]] <- team2
  finals[[4]][[i]] <- wins1
}
  i = i+1
  n = n+1
}


#apply model to finals

finals_model <- left_join(finals,data_2026,by=c("home_team"="Team"))
finals_model <- rename_home(finals_model)

finals_model <- left_join(finals_model,data_2026,by=c("away_team"="Team"))
finals_model <- rename_away(finals_model)|>
  rename("h_season" = "season.x", "a_season" = "season.y")

finals_model <- subtract_data(finals_model)
predictions <- predict(home_playoff_win_model,newdata=finals_model,type="response")
finals$home_win_probability <- predictions   
finals$round <- 4


#put all playoff results in one dataset
playoffs_2026 <- rbind(playoffs_2026,second_round)
playoffs_2026 <- rbind(playoffs_2026,third_round)|>
  select(-c("home_seed","away_seed"))
finals <- finals|>
  select(-c("home_wins","away_wins"))
playoffs_2026 <- rbind(playoffs_2026,finals)|>
  mutate(prediction = case_when(home_win_probability>0.875~paste(home_team,"in","4", sep=" "),
                                home_win_probability<0.875&home_win_probability>0.75~paste(home_team,"in","5", sep=" "),
                                home_win_probability<0.75&home_win_probability>0.625~paste(home_team,"in","6", sep=" "),
                                home_win_probability<0.625&home_win_probability>0.50~paste(home_team,"in","7", sep=" "),
                                home_win_probability<0.50&home_win_probability>0.375~paste(away_team,"in","7", sep=" "),
                                home_win_probability<0.375&home_win_probability>0.25~paste(away_team,"in","6", sep=" "),
                                home_win_probability<0.25&home_win_probability>0.125~paste(away_team,"in","5", sep=" "),
                                home_win_probability<0.125~paste(away_team,"in","4", sep=" ")))



#create data for table to be used in shiny app

all_data <- rbind(team_stats_total,data_2026)|>
  mutate(Team = case_when(Team=="Charlotte Bobcats"|Team=="Charlotte Hornets"~"Charlotte Hornets",
                          Team=="New Jersey Nets"|Team=="Brooklyn Nets"~"Brooklyn Nets",
                          Team=="New Orleans Hornets"|Team=="New Orleans Pelicans"|Team=="New Orleans/Oklahoma City Hornets"~"New Orleans Pelicans",
                          Team=="Oklahoma City Thunder"|Team=="Seattle SuperSonics"~"Oklahoma City Thunder",
                          .default = Team))|>
  arrange(Team)

write.csv(all_data,"team_data.csv")
write.csv(model_data, "model_data.csv")
