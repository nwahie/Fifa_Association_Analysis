#install.packages(c("dbplyr", "magrittr"))
library("dplyr")
library(magrittr)
library(ggplot2)
library(tidyr)
library(lubridate)
# install.packages('arules')
# install.packages("RColorBrewer")
# install.packages("arulesViz")
library(arules)
library(arulesViz)
library(RColorBrewer)
setwd('C://Users//nitin//Documents//EDA//HW1//HW 1')
con <- src_sqlite("euro_soccer.sqlite")

long_team_name <- 'Napoli'
#each of the followint tables are just dplyr connections to the database tables
#if or when I need to bring the table to local memory I need to run table <- collect(table)
country_tbl <- tbl(con, "country")
league_tbl <- tbl(con, "league")
match_tbl <- tbl(con, "match")
player_tbl <- tbl(con, "player")
player_atts_tbl <- tbl(con, "player_attributes")
team_tbl <- tbl(con, "team")
team_atts_tbl <- tbl(con, "team_attributes")

roma_record <- team_tbl %>% 
                collect() %>%
                filter(grepl(long_team_name, team_long_name))

country = country_tbl %>%
  collect()
league = league_tbl %>%
  collect()
match = match_tbl %>%
  collect()
player = player_tbl %>%
  collect()
player_atts = player_atts_tbl %>% 
  collect()
teams = team_tbl %>%
  collect()
team_atts = team_atts_tbl %>%
  collect()

# match = match[, colSums(is.na(match)) != nrow(match)]

home_matches <- match %>%
  filter(home_team_api_id == roma_record$team_api_id) %>%
  mutate(goal_diff = home_team_goal - away_team_goal) %>%
  mutate(type = 'home', opponent = away_team_api_id)
hist(home_matches$goal_diff)
summary(home_matches$goal_diff)

away_matches <- match %>%
  filter(away_team_api_id == roma_record$team_api_id) %>%
  mutate(goal_diff =away_team_goal - home_team_goal) %>%
  mutate(type = 'away', opponent = home_team_api_id)

hist(away_matches$goal_diff)
summary(away_matches$goal_diff)

all_match = rbind(home_matches, away_matches)
ggplot(all_match, aes(type, goal_diff, col = type)) +
  geom_boxplot()

all_match$result = as.numeric(all_match$goal_diff)
all_match[all_match$goal_diff > 0, ]$result = 'win'
all_match[all_match$goal_diff < 0, ]$result = 'lose'
all_match[all_match$goal_diff == 0, ]$result = 'tie'
ggplot(all_match, aes(result)) +
  geom_bar()

ggplot(all_match[all_match$result == 'lose',], aes(as.factor(opponent))) +
  geom_bar()
#####
for (i in 1:11){
  x = paste("home_player_X",i,sep='')
  y = paste("home_player_Y",i,sep='')
  inp = paste("hm_ply_pos",i,sep="_")
  all_match[inp] = ifelse(all_match[x]==1 & all_match[y]==1,"Goaly",
                      ifelse(all_match[y]>=2 & all_match[y]<=5,"Defender",
                             ifelse(all_match[y]>=6 & all_match[y]<=8,"Mid Fielders",
                                    ifelse(all_match[y]>=9 & all_match[y]<=11,"Forwards","NA"))))
}

for (i in 1:11){
  x = paste("away_player_X",i,sep='')
  y = paste("away_player_Y",i,sep='')
  inp = paste("away_ply_pos",i,sep="_")
  all_match[inp] = ifelse(all_match[x]==1 & all_match[y]==1,"Goaly",
                      ifelse(all_match[y]>=2 & all_match[y]<=5,"Defender",
                             ifelse(all_match[y]>=6 & all_match[y]<=8,"Mid Fielders",
                                    ifelse(all_match[y]>=9 & all_match[y]<=11,"Forwards","NA"))))
}

column_names = c("overall_rating","potential","crossing", "finishing","heading_accuracy",    
                 "short_passing", "volleys","dribbling",    
                 "curve", "free_kick_accuracy" ,"long_passing",    
                 "ball_control","acceleration","sprint_speed",    
                 "agility","reactions","balance",    
                 "shot_power","jumping","stamina",    
                 "strength","long_shots","aggression",    
                 "interceptions","positioning","vision",    
                 "penalties", "marking","standing_tackle",
                 "sliding_tackle","gk_diving","gk_handling",    
                 "gk_kicking","gk_positioning","gk_reflexes")
for (i in column_names){
  k = paste(i, "Bucket",sep="_")
  player_atts[k] = 
    cut(player_atts[[i]], breaks = 5, labels=c('low','low-med','med','med-high','high'))
}

player_atts <- select(player_atts, c(1:9, overall_rating_Bucket:gk_reflexes_Bucket))
player_atts = player_atts[, -c(5,6)]
all_match <- select(all_match, c(id:away_team_goal, home_player_1:away_player_11, opponent:away_ply_pos_11))

#######
# match_g <- gather(all_match, player, player_id, home_player_1:away_player_11, na.rm = TRUE)
# View(match_g)
# match_s <- arrange(match_g, match_api_id, player)
# View(match_s)
# #match_s <- select(match_s, c(1:10, player, player_id))

# colnames(match_s)[37] <- "player_api_id"

#HP1
match_player <- merge(x = all_match, y=player_atts,by.x='home_player_1',by.y='player_api_id',all.x=TRUE)
match_player$date.x<-ymd_hms(match_player$date.x)
match_player$date.y<-ymd_hms(match_player$date.y)
match_player$time_diff <- difftime(match_player$date.x, match_player$date.y,units='days')
match_player = match_player[match_player$time_diff >= 0,]
match_player_1 = match_player %>%
  group_by(match_api_id, home_player_1) %>%
  filter(time_diff == min(time_diff))
for (i in 61:98){
  colnames(match_player_1)[i] = paste(colnames(match_player_1)[i], "hp_1", sep = "_")
}
for (i in 61:98){
  match_player_1[[i]] = paste(match_player_1[[i]],match_player_1[[36]],sep="_")
}
match_player_1$time_diff=NULL
# HP2

match_player2 <- merge(x = match_player_1, y=player_atts,by.x='home_player_2',by.y='player_api_id',all.x=TRUE)
match_player2$date.x<-ymd(match_player2$date.x)
match_player2$date<-ymd_hms(match_player2$date)
match_player2$time_diff <- difftime(match_player2$date.x, match_player2$date,units='days')
match_player2 = match_player2[match_player2$time_diff >= 0,]
match_player2 = match_player2 %>%
  group_by(match_api_id, home_player_2) %>%
  filter(time_diff == min(time_diff))
for (i in 102:139){
  colnames(match_player2)[i] = paste(colnames(match_player2)[i], "hp_2", sep = "_")
}
for (i in 102:139){
  match_player2[[i]] = paste(match_player2[[i]],match_player2[[37]],sep="_")
}
match_player2$time_diff=NULL
match_player2$id.y = NULL
match_player2$date.y=NULL
match_player2$date=NULL
match_player2$id.x=NULL
##
#HP3

match_player3 <- merge(x = match_player2, y=player_atts,by.x='home_player_3',by.y='player_api_id',all.x=TRUE)
match_player3$date.x<-ymd(match_player3$date.x)
match_player3$date<-ymd_hms(match_player3$date)
match_player3$time_diff <- difftime(match_player3$date.x, match_player3$date,units='days')
match_player3 = match_player3[match_player3$time_diff >= 0,]
match_player3 = match_player3 %>%
  group_by(match_api_id, home_player_3) %>%
  filter(time_diff == min(time_diff))
for (i in 139:176){
  colnames(match_player3)[i] = paste(colnames(match_player3)[i], "hp_3", sep = "_")
}
for (i in 139:176){
  match_player3[[i]] = paste(match_player3[[i]],match_player3[[37]],sep="_")
}
match_player3$time_diff=NULL
match_player3$id.y = NULL
match_player3$date.y=NULL
match_player3$date=NULL
match_player3$id.x=NULL
match_player3$player_fifa_api_id=NULL

#HP4

match_player4 <- merge(x = match_player3, y=player_atts,by.x='home_player_4',by.y='player_api_id',all.x=TRUE)
match_player4$date.x<-ymd(match_player4$date.x)
match_player4$date<-ymd_hms(match_player4$date)
match_player4$time_diff <- difftime(match_player4$date.x, match_player4$date,units='days')
match_player4 = match_player4[match_player4$time_diff >= 0,]
match_player4 = match_player4 %>%
  group_by(match_api_id, home_player_4) %>%
  filter(time_diff == min(time_diff))
for (i in 176:213){
  colnames(match_player4)[i] = paste(colnames(match_player4)[i], "hp_4", sep = "_")
}
for (i in 176:213){
  match_player4[[i]] = paste(match_player4[[i]],match_player4[[38]],sep="_")
}
match_player4$time_diff=NULL
match_player4$id.y = NULL
match_player4$date.y=NULL
match_player4$date=NULL
match_player4$id.x=NULL
match_player4$id=NULL
match_player4$player_fifa_api_id.x=NULL
match_player4$player_fifa_api_id.y=NULL
#HP5

match_player5 <- merge(x = match_player4, y=player_atts,by.x='home_player_5',by.y='player_api_id',all.x=TRUE)
match_player5$date.x<-ymd(match_player5$date.x)
match_player5$date<-ymd_hms(match_player5$date)
match_player5$time_diff <- difftime(match_player5$date.x, match_player5$date,units='days')
match_player5 = match_player5[match_player5$time_diff >= 0,]
match_player5 = match_player5 %>%
  group_by(match_api_id, home_player_5) %>%
  filter(time_diff == min(time_diff))
for (i in 213:250){
  colnames(match_player5)[i] = paste(colnames(match_player5)[i], "hp_5", sep = "_")
}
for (i in 213:250){
  match_player5[[i]] = paste(match_player5[[i]],match_player5[[39]],sep="_")
}
match_player5$time_diff=NULL
match_player5$id.y = NULL
match_player5$date.y=NULL
match_player5$date=NULL
match_player5$id.x=NULL
match_player5$id=NULL
###
#HP6
match_player6 <- merge(x = match_player5, y=player_atts,by.x='home_player_6',by.y='player_api_id',all.x=TRUE)
match_player6$date.x<-ymd(match_player6$date.x)
match_player6$date<-ymd_hms(match_player6$date)
match_player6$time_diff <- difftime(match_player6$date.x, match_player6$date,units='days')
match_player6 = match_player6[match_player6$time_diff >= 0,]
match_player6 = match_player6 %>%
  group_by(match_api_id, home_player_6) %>%
  filter(time_diff == min(time_diff))
for (i in 252:289){
  colnames(match_player6)[i] = paste(colnames(match_player6)[i], "hp_6", sep = "_")
}
for (i in 252:289){
  match_player6[[i]] = paste(match_player6[[i]],match_player6[[40]],sep="_")
}
match_player6$time_diff=NULL
match_player6$id.y = NULL
match_player6$date.y=NULL
match_player6$date=NULL
match_player6$id.x=NULL
match_player6$id=NULL
match_player6$player_fifa_api_id.x=NULL
match_player6$player_fifa_api_id.y=NULL

#HP7
match_player7 <- merge(x = match_player6, y=player_atts,by.x='home_player_7',by.y='player_api_id',all.x=TRUE)
match_player7$date.x<-ymd(match_player7$date.x)
match_player7$date<-ymd_hms(match_player7$date)
match_player7$time_diff <- difftime(match_player7$date.x, match_player7$date,units='days')
match_player7 = match_player7[match_player7$time_diff >= 0,]
match_player7 = match_player7 %>%
  group_by(match_api_id, home_player_7) %>%
  filter(time_diff == min(time_diff))
for (i in 289:326){
  colnames(match_player7)[i] = paste(colnames(match_player7)[i], "hp_7", sep = "_")
}
for (i in 289:326){
  match_player7[[i]] = paste(match_player7[[i]],match_player7[[41]],sep="_")
}
match_player7$time_diff=NULL
match_player7$id.y = NULL
match_player7$date.y=NULL
match_player7$date=NULL
match_player7$id.x=NULL
match_player7$id=NULL
match_player7$player_fifa_api_id.x=NULL
match_player7$player_fifa_api_id.y=NULL

#HP8
match_player8 <- merge(x = match_player7, y=player_atts,by.x='home_player_8',by.y='player_api_id',all.x=TRUE)
match_player8$date.x<-ymd(match_player8$date.x)
match_player8$date<-ymd_hms(match_player8$date)
match_player8$time_diff <- difftime(match_player8$date.x, match_player8$date,units='days')
match_player8 = match_player8[match_player8$time_diff >= 0,]
match_player8 = match_player8 %>%
  group_by(match_api_id, home_player_8) %>%
  filter(time_diff == min(time_diff))
for (i in 326:363){
  colnames(match_player8)[i] = paste(colnames(match_player8)[i], "hp_8", sep = "_")
}
for (i in 326:363){
  match_player8[[i]] = paste(match_player8[[i]],match_player8[[42]],sep="_")
}
match_player8$time_diff=NULL
match_player8$id.y = NULL
match_player8$date.y=NULL
match_player8$date=NULL
match_player8$id.x=NULL
match_player8$id=NULL
match_player8$player_fifa_api_id.x=NULL
match_player8$player_fifa_api_id.y=NULL

#HP9
match_player9 <- merge(x = match_player8, y=player_atts,by.x='home_player_9',by.y='player_api_id',all.x=TRUE)
match_player9$date.x<-ymd(match_player9$date.x)
match_player9$date<-ymd_hms(match_player9$date)
match_player9$time_diff <- difftime(match_player9$date.x, match_player9$date,units='days')
match_player9 = match_player9[match_player9$time_diff >= 0,]
match_player9 = match_player9 %>%
  group_by(match_api_id, home_player_9) %>%
  filter(time_diff == min(time_diff))
for (i in 365:402){
  colnames(match_player9)[i] = paste(colnames(match_player9)[i], "hp_9", sep = "_")
}
for (i in 365:402){
  match_player9[[i]] = paste(match_player9[[i]],match_player9[[43]],sep="_")
}
match_player9$time_diff=NULL
match_player9$id.y = NULL
match_player9$date.y=NULL
match_player9$date=NULL
match_player9$id.x=NULL
match_player9$id=NULL
match_player9$player_fifa_api_id.x=NULL
match_player9$player_fifa_api_id.y=NULL

#HP10
match_player10 <- merge(x = match_player9, y=player_atts,by.x='home_player_10',by.y='player_api_id',all.x=TRUE)
match_player10$date.x<-ymd(match_player10$date.x)
match_player10$date<-ymd_hms(match_player10$date)
match_player10$time_diff <- difftime(match_player10$date.x, match_player10$date,units='days')
match_player10 = match_player10[match_player10$time_diff >= 0,]
match_player10 = match_player10 %>%
  group_by(match_api_id, home_player_10) %>%
  filter(time_diff == min(time_diff))
for (i in 402:439){
  colnames(match_player10)[i] = paste(colnames(match_player10)[i], "hp_10", sep = "_")
}
for (i in 402:439){
  match_player10[[i]] = paste(match_player10[[i]],match_player10[[44]],sep="_")
}
match_player10$time_diff=NULL
match_player10$id.y = NULL
match_player10$date.y=NULL
match_player10$date=NULL
match_player10$id.x=NULL
match_player10$id=NULL
match_player10$player_fifa_api_id.x=NULL
match_player10$player_fifa_api_id.y=NULL

#HP11
match_player11 <- merge(x = match_player10, y=player_atts,by.x='home_player_11',by.y='player_api_id',all.x=TRUE)
match_player11$date.x<-ymd(match_player11$date.x)
match_player11$date<-ymd_hms(match_player11$date)
match_player11$time_diff <- difftime(match_player11$date.x, match_player11$date,units='days')
match_player11 = match_player11[match_player11$time_diff >= 0,]
match_player11 = match_player11 %>%
  group_by(match_api_id, home_player_11) %>%
  filter(time_diff == min(time_diff))
for (i in 441:478){
  colnames(match_player11)[i] = paste(colnames(match_player11)[i], "hp_11", sep = "_")
}
for (i in 441:478){
  match_player11[[i]] = paste(match_player11[[i]],match_player11[[45]],sep="_")
}
match_player11$time_diff=NULL
match_player11$id.y = NULL
match_player11$date.y=NULL
match_player11$date=NULL
match_player11$id.x=NULL
match_player11$id=NULL
match_player11$player_fifa_api_id.x=NULL
match_player11$player_fifa_api_id.y=NULL

#AP1

match_player12 <- merge(x = match_player11, y=player_atts,by.x='away_player_1',by.y='player_api_id',all.x=TRUE)
match_player12$date.x<-ymd(match_player12$date.x)
match_player12$date<-ymd_hms(match_player12$date)
match_player12$time_diff <- difftime(match_player12$date.x, match_player12$date,units='days')
match_player12 = match_player12[match_player12$time_diff >= 0,]
match_player12 = match_player12 %>%
  group_by(match_api_id, away_player_1) %>%
  filter(time_diff == min(time_diff))
for (i in 478:515){
  colnames(match_player12)[i] = paste(colnames(match_player12)[i], "aw_p_1", sep = "_")
}
for (i in 478:515){
  match_player12[[i]] = paste(match_player12[[i]],match_player12[[46]],sep="_")
}
match_player12$time_diff=NULL
match_player12$id.y = NULL
match_player12$date.y=NULL
match_player12$date=NULL
match_player12$id.x=NULL
match_player12$id=NULL
match_player12$player_fifa_api_id.x=NULL
match_player12$player_fifa_api_id.y=NULL
#AP2
match_player13 <- merge(x = match_player12, y=player_atts,by.x='away_player_2',by.y='player_api_id',all.x=TRUE)
match_player13$date.x<-ymd(match_player13$date.x)
match_player13$date<-ymd_hms(match_player13$date)
match_player13$time_diff <- difftime(match_player13$date.x, match_player13$date,units='days')
match_player13 = match_player13[match_player13$time_diff >= 0,]
match_player13 = match_player13 %>%
  group_by(match_api_id, away_player_2) %>%
  filter(time_diff == min(time_diff))
for (i in 517:554){
  colnames(match_player13)[i] = paste(colnames(match_player13)[i], "aw_p_2", sep = "_")
}
for (i in 517:554){
  match_player13[[i]] = paste(match_player13[[i]],match_player13[[47]],sep="_")
}
match_player13$time_diff=NULL
match_player13$id.y = NULL
match_player13$date.y=NULL
match_player13$date=NULL
match_player13$id.x=NULL
match_player13$id=NULL
match_player13$player_fifa_api_id.x=NULL
match_player13$player_fifa_api_id.y=NULL

#AP3
match_player14 <- merge(x = match_player13, y=player_atts,by.x='away_player_3',by.y='player_api_id',all.x=TRUE)
match_player14$date.x<-ymd(match_player14$date.x)
match_player14$date<-ymd_hms(match_player14$date)
match_player14$time_diff <- difftime(match_player14$date.x, match_player14$date,units='days')
match_player14 = match_player14[match_player14$time_diff >= 0,]
match_player14 = match_player14 %>%
  group_by(match_api_id, away_player_3) %>%
  filter(time_diff == min(time_diff))
for (i in 554:591){
  colnames(match_player14)[i] = paste(colnames(match_player14)[i], "aw_p_3", sep = "_")
}
for (i in 554:591){
  match_player14[[i]] = paste(match_player14[[i]],match_player14[[48]],sep="_")
}
match_player14$time_diff=NULL
match_player14$id.y = NULL
match_player14$date.y=NULL
match_player14$date=NULL
match_player14$id.x=NULL
match_player14$id=NULL
match_player14$player_fifa_api_id.x=NULL
match_player14$player_fifa_api_id.y=NULL

#AP4
match_player15 <- merge(x = match_player14, y=player_atts,by.x='away_player_4',by.y='player_api_id',all.x=TRUE)
match_player15$date.x<-ymd(match_player15$date.x)
match_player15$date<-ymd_hms(match_player15$date)
match_player15$time_diff <- difftime(match_player15$date.x, match_player15$date,units='days')
match_player15 = match_player15[match_player15$time_diff >= 0,]
match_player15 = match_player15 %>%
  group_by(match_api_id, away_player_4) %>%
  filter(time_diff == min(time_diff))
for (i in 593:630){
  colnames(match_player15)[i] = paste(colnames(match_player15)[i], "aw_p_4", sep = "_")
}
for (i in 593:630){
  match_player15[[i]] = paste(match_player15[[i]],match_player15[[49]],sep="_")
}
match_player15$time_diff=NULL
match_player15$id.y = NULL
match_player15$date.y=NULL
match_player15$date=NULL
match_player15$id.x=NULL
match_player15$id=NULL
match_player15$player_fifa_api_id.x=NULL
match_player15$player_fifa_api_id.y=NULL

#AP5
match_player16 <- merge(x = match_player15, y=player_atts,by.x='away_player_5',by.y='player_api_id',all.x=TRUE)
match_player16$date.x<-ymd(match_player16$date.x)
match_player16$date<-ymd_hms(match_player16$date)
match_player16$time_diff <- difftime(match_player16$date.x, match_player16$date,units='days')
match_player16 = match_player16[match_player16$time_diff >= 0,]
match_player16 = match_player16 %>%
  group_by(match_api_id, away_player_5) %>%
  filter(time_diff == min(time_diff))
for (i in 630:667){
  colnames(match_player16)[i] = paste(colnames(match_player16)[i], "aw_p_5", sep = "_")
}
for (i in 630:667){
  match_player16[[i]] = paste(match_player16[[i]],match_player16[[50]],sep="_")
}
match_player16$time_diff=NULL
match_player16$id.y = NULL
match_player16$date.y=NULL
match_player16$date=NULL
match_player16$id.x=NULL
match_player16$id=NULL
match_player16$player_fifa_api_id.x=NULL
match_player16$player_fifa_api_id.y=NULL
#AP6
match_player17 <- merge(x = match_player16, y=player_atts,by.x='away_player_6',by.y='player_api_id',all.x=TRUE)
match_player17$date.x<-ymd(match_player17$date.x)
match_player17$date<-ymd_hms(match_player17$date)
match_player17$time_diff <- difftime(match_player17$date.x, match_player17$date,units='days')
match_player17 = match_player17[match_player17$time_diff >= 0,]
match_player17 = match_player17 %>%
  group_by(match_api_id, away_player_6) %>%
  filter(time_diff == min(time_diff))
for (i in 669:706){
  colnames(match_player17)[i] = paste(colnames(match_player17)[i], "aw_p_6", sep = "_")
}
for (i in 669:706){
  match_player17[[i]] = paste(match_player17[[i]],match_player17[[51]],sep="_")
}
match_player17$time_diff=NULL
match_player17$id.y = NULL
match_player17$date.y=NULL
match_player17$date=NULL
match_player17$id.x=NULL
match_player17$id=NULL
match_player17$player_fifa_api_id.x=NULL
match_player17$player_fifa_api_id.y=NULL
#AP7
match_player18 <- merge(x = match_player17, y=player_atts,by.x='away_player_7',by.y='player_api_id',all.x=TRUE)
match_player18$date.x<-ymd(match_player18$date.x)
match_player18$date<-ymd_hms(match_player18$date)
match_player18$time_diff <- difftime(match_player18$date.x, match_player18$date,units='days')
match_player18 = match_player18[match_player18$time_diff >= 0,]
match_player18 = match_player18 %>%
  group_by(match_api_id, away_player_7) %>%
  filter(time_diff == min(time_diff))
for (i in 706:743){
  colnames(match_player18)[i] = paste(colnames(match_player18)[i], "aw_p_7", sep = "_")
}
for (i in 706:743){
  match_player18[[i]] = paste(match_player18[[i]],match_player18[[52]],sep="_")
}
match_player18$time_diff=NULL
match_player18$id.y = NULL
match_player18$date.y=NULL
match_player18$date=NULL
match_player18$id.x=NULL
match_player18$id=NULL
match_player18$player_fifa_api_id.x=NULL
match_player18$player_fifa_api_id.y=NULL
#AP8
match_player19 <- merge(x = match_player18, y=player_atts,by.x='away_player_8',by.y='player_api_id',all.x=TRUE)
match_player19$date.x<-ymd(match_player19$date.x)
match_player19$date<-ymd_hms(match_player19$date)
match_player19$time_diff <- difftime(match_player19$date.x, match_player19$date,units='days')
match_player19 = match_player19[match_player19$time_diff >= 0,]
match_player19 = match_player19 %>%
  group_by(match_api_id, away_player_8) %>%
  filter(time_diff == min(time_diff))
for (i in 745:782){
  colnames(match_player19)[i] = paste(colnames(match_player19)[i], "aw_p_8", sep = "_")
}
for (i in 745:782){
  match_player19[[i]] = paste(match_player19[[i]],match_player19[[53]],sep="_")
}
match_player19$time_diff=NULL
match_player19$id.y = NULL
match_player19$date.y=NULL
match_player19$date=NULL
match_player19$id.x=NULL
match_player19$id=NULL
match_player19$player_fifa_api_id.x=NULL
match_player19$player_fifa_api_id.y=NULL
#AP9
match_player20 <- merge(x = match_player19, y=player_atts,by.x='away_player_9',by.y='player_api_id',all.x=TRUE)
match_player20$date.x<-ymd(match_player20$date.x)
match_player20$date<-ymd_hms(match_player20$date)
match_player20$time_diff <- difftime(match_player20$date.x, match_player20$date,units='days')
match_player20 = match_player20[match_player20$time_diff >= 0,]
match_player20 = match_player20 %>%
  group_by(match_api_id, away_player_9) %>%
  filter(time_diff == min(time_diff))
for (i in 782:819){
  colnames(match_player20)[i] = paste(colnames(match_player20)[i], "aw_p_9", sep = "_")
}
for (i in 782:819){
  match_player20[[i]] = paste(match_player20[[i]],match_player20[[54]],sep="_")
}
match_player20$time_diff=NULL
match_player20$id.y = NULL
match_player20$date.y=NULL
match_player20$date=NULL
match_player20$id.x=NULL
match_player20$id=NULL
match_player20$player_fifa_api_id.x=NULL
match_player20$player_fifa_api_id.y=NULL
#AP10
match_player21 <- merge(x = match_player20, y=player_atts,by.x='away_player_10',by.y='player_api_id',all.x=TRUE)
match_player21$date.x<-ymd(match_player21$date.x)
match_player21$date<-ymd_hms(match_player21$date)
match_player21$time_diff <- difftime(match_player21$date.x, match_player21$date,units='days')
match_player21 = match_player21[match_player21$time_diff >= 0,]
match_player21 = match_player21 %>%
  group_by(match_api_id, away_player_10) %>%
  filter(time_diff == min(time_diff))
for (i in 821:858){
  colnames(match_player21)[i] = paste(colnames(match_player21)[i], "aw_p_10", sep = "_")
}
for (i in 821:858){
  match_player21[[i]] = paste(match_player21[[i]],match_player21[[55]],sep="_")
}
match_player21$time_diff=NULL
match_player21$id.y = NULL
match_player21$date.y=NULL
match_player21$date=NULL
match_player21$id.x=NULL
match_player21$id=NULL
match_player21$player_fifa_api_id.x=NULL
match_player21$player_fifa_api_id.y=NULL
#AP11
match_player22 <- merge(x = match_player21, y=player_atts,by.x='away_player_11',by.y='player_api_id',all.x=TRUE)
match_player22$date.x<-ymd(match_player22$date.x)
match_player22$date<-ymd_hms(match_player22$date)
match_player22$time_diff <- difftime(match_player22$date.x, match_player22$date,units='days')
match_player22 = match_player22[match_player22$time_diff >= 0,]
match_player22 = match_player22 %>%
  group_by(match_api_id, away_player_11) %>%
  filter(time_diff == min(time_diff))
for (i in 858:895){
  colnames(match_player22)[i] = paste(colnames(match_player22)[i], "aw_p_11", sep = "_")
}
for (i in 858:895){
  match_player22[[i]] = paste(match_player22[[i]],match_player22[[56]],sep="_")
}
match_player22$time_diff=NULL
match_player22$id.y = NULL
match_player22$date.y=NULL
match_player22$date=NULL
match_player22$id.x=NULL
match_player22$id=NULL
match_player22$player_fifa_api_id.x=NULL
match_player22$player_fifa_api_id.y=NULL

write.csv(match_player22,'final.csv')

##Adding Formations

###for home team

list2=c('hm_ply_pos_1',	'hm_ply_pos_2',	'hm_ply_pos_3',	'hm_ply_pos_4',	
        'hm_ply_pos_5',	'hm_ply_pos_6',	'hm_ply_pos_7',	'hm_ply_pos_8',	
        'hm_ply_pos_9',	'hm_ply_pos_10',	'hm_ply_pos_11')

k=match_player22
for (i in 1:nrow(k)){
  
    count_def = 1
    count_mf = 1
    count_fr = 1
    count_gl = 1
    for (j in list2) {
      
       if (k[i,j]=='Defender'){
         k[i,'Defender_HP'] = count_def
         count_def = count_def + 1 
         }
        else if (k[i,j]=='Mid Fielders'){
          k[i,'Mid Fieldersr_HP'] = count_mf
          count_mf = count_mf + 1}
        else if (k[i,j]=='Forwards'){
            k[i,'Forwards_HP'] = count_fr
            count_fr = count_fr + 1}
        else if (k[i,j]=='Goaly'){
            k[i,'Goaly_HP'] = count_gl
            count_gl = count_gl + 1}
        }
}

## for away team
list3=c('away_ply_pos_1',	'away_ply_pos_2',	'away_ply_pos_3',	'away_ply_pos_4',	
        'away_ply_pos_5',	'away_ply_pos_6',	'away_ply_pos_7',	'away_ply_pos_8',	
        'away_ply_pos_9',	'away_ply_pos_10',	'away_ply_pos_11')

# k=match_player22
for (i in 1:nrow(k)){
  
  count_def = 1
  count_mf = 1
  count_fr = 1
  count_gl = 1
  for (j in list3) {
    
    if (k[i,j]=='Defender'){
      k[i,'Defender_AP'] = count_def
      count_def = count_def + 1 
    }
    else if (k[i,j]=='Mid Fielders'){
      k[i,'Mid Fieldersr_AP'] = count_mf
      count_mf = count_mf + 1}
    else if (k[i,j]=='Forwards'){
      k[i,'Forwards_AP'] = count_fr
      count_fr = count_fr + 1}
    else if (k[i,j]=='Goaly'){
      k[i,'Goaly_AP'] = count_gl
      count_gl = count_gl + 1}
  }
}

##Subsetting Data
m = select(k,c(date.x,home_team_api_id,away_team_api_id,result,894:901))
m = merge(x=m,y=teams[c('team_api_id','team_long_name')],by.x='home_team_api_id',by.y='team_api_id')
colnames(m)[15] = c('home_team')
m = merge(x=m,y=teams[c('team_api_id','team_long_name')],by.x='away_team_api_id',by.y='team_api_id')
colnames(m)[16] = c('away_team')

# for Team Layout - Roma Wins Only
m = unite(m,"Team_Formation_Home",Goaly_HP:Forwards_HP,remove=FALSE,sep=",")
m = unite(m,"Team_Formation_Away",Goaly_AP:Forwards_AP,remove=FALSE,sep=",")
h = select(m,c(date.x,match_api_id,result,Team_Formation_Home,Team_Formation_Away,home_team,away_team))
for (i in 1:nrow(h)){
  if (h[i,'home_team']=='Roma'){
    h[i,'Roma_Formation'] = h[i,'Team_Formation_Home']
    h[i,'Opponant Formation'] = h[i,'Team_Formation_Away']}
  else if (h[i,'away_team']=='Roma'){
    h[i,'Roma_Formation'] = h[i,'Team_Formation_Away']
    h[i,'Opponant Formation'] = h[i,'Team_Formation_Home']
  }
}

h = h[,-c(4:7)]
a = h %>% group_by(year(date.x),Roma_Formation,result) %>% 
        summarize(win_per = sum(result=='win')/n(),
                  win_count =sum(result=='win'),total=n()) 

ggplot(a, aes(x=Roma_Formation,y=total,fill=result))+
          geom_bar(stat='identity',position=position_dodge()) +
  facet_wrap(~`year(date.x)`,ncol=2)
b = h %>% group_by(Roma_Formation,result) %>% 
  summarize(win_per = sum(result=='win')/n()*100,
            win_count =sum(result=='win'),total=n()) 

ggplot(b, aes(x=Roma_Formation,y=total,fill=result))+
  geom_bar(stat='identity',position=position_dodge())
par(mf = c(5,2,1,1))

# for Team Layout - Javuntus Wins Only
m = unite(m,"Team_Formation_Home",Goaly_HP:Forwards_HP,remove=FALSE,sep=",")
m = unite(m,"Team_Formation_Away",Goaly_AP:Forwards_AP,remove=FALSE,sep=",")
h = select(m,c(date.x,match_api_id,result,Team_Formation_Home,Team_Formation_Away,home_team,away_team))
for (i in 1:nrow(h)){
  if (h[i,'home_team']=='Juventus'){
    h[i,'Juventus_Formation'] = h[i,'Team_Formation_Home']
    h[i,'Opponant Formation'] = h[i,'Team_Formation_Away']}
  else if (h[i,'away_team']=='Juventus'){
    h[i,'Juventus_Formation'] = h[i,'Team_Formation_Away']
    h[i,'Opponant Formation'] = h[i,'Team_Formation_Home']
  }
}

h = h[,-c(4:7)]
a = h %>% group_by(year(date.x),Juventus_Formation,result) %>% 
  summarize(win_per = sum(result=='win')/n(),
            win_count =sum(result=='win'),total=n()) 

ggplot(a, aes(x=Juventus_Formation,y=total,fill=result))+
  geom_bar(stat='identity',position=position_dodge()) +
  facet_wrap(~`year(date.x)`,ncol=2)
b = h %>% group_by(Juventus_Formation,result) %>% 
  summarize(win_per = sum(result=='win')/n()*100,
            win_count =sum(result=='win'),total=n()) 

ggplot(b, aes(x=Juventus_Formation,y=total,fill=result))+
  geom_bar(stat='identity',position=position_dodge())
par(mf = c(5,2,1,1))
# for Team Layout - Sampdoria Wins Only
m = unite(m,"Team_Formation_Home",Goaly_HP:Forwards_HP,remove=FALSE,sep=",")
m = unite(m,"Team_Formation_Away",Goaly_AP:Forwards_AP,remove=FALSE,sep=",")
h = select(m,c(date.x,match_api_id,result,Team_Formation_Home,Team_Formation_Away,home_team,away_team))
for (i in 1:nrow(h)){
  if (h[i,'home_team']=='Sampdoria'){
    h[i,'Sampdoria_Formation'] = h[i,'Team_Formation_Home']
    h[i,'Opponant Formation'] = h[i,'Team_Formation_Away']}
  else if (h[i,'away_team']=='Sampdoria'){
    h[i,'Sampdoria_Formation'] = h[i,'Team_Formation_Away']
    h[i,'Opponant Formation'] = h[i,'Team_Formation_Home']
  }
}

h = h[,-c(4:7)]
a = h %>% group_by(year(date.x),Sampdoria_Formation,result) %>% 
  summarize(win_per = sum(result=='win')/n(),
            win_count =sum(result=='win'),total=n()) 

ggplot(a, aes(x=Sampdoria_Formation,y=total,fill=result))+
  geom_bar(stat='identity',position=position_dodge()) +
  facet_wrap(~`year(date.x)`,ncol=2)
b = h %>% group_by(Sampdoria_Formation,result) %>% 
  summarize(win_per = sum(result=='win')/n()*100,
            win_count =sum(result=='win'),total=n()) 

ggplot(b, aes(x=Sampdoria_Formation,y=total,fill=result))+
  geom_bar(stat='identity',position=position_dodge())
par(mf = c(5,2,1,1))
# for Team Layout - Palermo Wins Only
m = unite(m,"Team_Formation_Home",Goaly_HP:Forwards_HP,remove=FALSE,sep=",")
m = unite(m,"Team_Formation_Away",Goaly_AP:Forwards_AP,remove=FALSE,sep=",")
h = select(m,c(date.x,match_api_id,result,Team_Formation_Home,Team_Formation_Away,home_team,away_team))
for (i in 1:nrow(h)){
  if (h[i,'home_team']=='Palermo'){
    h[i,'Palermo_Formation'] = h[i,'Team_Formation_Home']
    h[i,'Opponant Formation'] = h[i,'Team_Formation_Away']}
  else if (h[i,'away_team']=='Palermo'){
    h[i,'Palermo_Formation'] = h[i,'Team_Formation_Away']
    h[i,'Opponant Formation'] = h[i,'Team_Formation_Home']
  }
}

h = h[,-c(4:7)]
a = h %>% group_by(year(date.x),Palermo_Formation,result) %>% 
  summarize(win_per = sum(result=='win')/n(),
            win_count =sum(result=='win'),total=n()) 

ggplot(a, aes(x=Palermo_Formation,y=total,fill=result))+
  geom_bar(stat='identity',position=position_dodge()) +
  facet_wrap(~`year(date.x)`,ncol=2)
b = h %>% group_by(Palermo_Formation,result) %>% 
  summarize(win_per = sum(result=='win')/n()*100,
            win_count =sum(result=='win'),total=n()) 

ggplot(b, aes(x=Palermo_Formation,y=total,fill=result))+
  geom_bar(stat='identity',position=position_dodge())
par(mf = c(5,2,1,1))
# for Team Layout - Genoa Wins Only
m = unite(m,"Team_Formation_Home",Goaly_HP:Forwards_HP,remove=FALSE,sep=",")
m = unite(m,"Team_Formation_Away",Goaly_AP:Forwards_AP,remove=FALSE,sep=",")
h = select(m,c(date.x,match_api_id,result,Team_Formation_Home,Team_Formation_Away,home_team,away_team))
for (i in 1:nrow(h)){
  if (h[i,'home_team']=='Genoa'){
    h[i,'Genoa_Formation'] = h[i,'Team_Formation_Home']
    h[i,'Opponant Formation'] = h[i,'Team_Formation_Away']}
  else if (h[i,'away_team']=='Genoa'){
    h[i,'Genoa_Formation'] = h[i,'Team_Formation_Away']
    h[i,'Opponant Formation'] = h[i,'Team_Formation_Home']
  }
}

h = h[,-c(4:7)]
a = h %>% group_by(year(date.x),Genoa_Formation,result) %>% 
  summarize(win_per = sum(result=='win')/n(),
            win_count =sum(result=='win'),total=n()) 

ggplot(a, aes(x=Genoa_Formation,y=total,fill=result))+
  geom_bar(stat='identity',position=position_dodge()) +
  facet_wrap(~`year(date.x)`,ncol=2)
b = h %>% group_by(Genoa_Formation,result) %>% 
  summarize(win_per = sum(result=='win')/n()*100,
            win_count =sum(result=='win'),total=n()) 

ggplot(b, aes(x=Genoa_Formation,y=total,fill=result))+
  geom_bar(stat='identity',position=position_dodge())
par(mf = c(5,2,1,1))

# for Team Layout - Napoli Wins Only
m = unite(m,"Team_Formation_Home",Goaly_HP:Forwards_HP,remove=FALSE,sep=",")
m = unite(m,"Team_Formation_Away",Goaly_AP:Forwards_AP,remove=FALSE,sep=",")
h = select(m,c(date.x,match_api_id,result,Team_Formation_Home,Team_Formation_Away,home_team,away_team))
for (i in 1:nrow(h)){
  if (h[i,'home_team']=='Napoli'){
    h[i,'Napoli_Formation'] = h[i,'Team_Formation_Home']
    h[i,'Opponant Formation'] = h[i,'Team_Formation_Away']}
  else if (h[i,'away_team']=='Napoli'){
    h[i,'Napoli_Formation'] = h[i,'Team_Formation_Away']
    h[i,'Opponant Formation'] = h[i,'Team_Formation_Home']
  }
}

h = h[,-c(4:7)]
a = h %>% group_by(year(date.x),Napoli_Formation,result) %>% 
  summarize(win_per = sum(result=='win')/n(),
            win_count =sum(result=='win'),total=n()) 

ggplot(a, aes(x=Napoli_Formation,y=total,fill=result))+
  geom_bar(stat='identity',position=position_dodge()) +
  facet_wrap(~`year(date.x)`,ncol=2)
b = h %>% group_by(Napoli_Formation,result) %>% 
  summarize(win_per = sum(result=='win')/n()*100,
            win_count =sum(result=='win'),total=n()) 

ggplot(b, aes(x=Napoli_Formation,y=total,fill=result))+
  geom_bar(stat='identity',position=position_dodge())
##Data Transactional Form - For Market Basket
for (i in 1:nrow(m)){
  for (j in 7:10){
    m[i,j] = paste(m[i,j],m[i,15],sep='_')
  }
}
for (i in 1:nrow(m)){
  for (j in 11:14){
    m[i,j] = paste(m[i,j],m[i,16],sep='_')
  }
}
m = select(m, c(7:14,result))
m = m[,-c(1,5)]
m$Defender_HP = paste(m$Defender_HP,'Defender',sep="_")
m$`Mid Fieldersr_HP` = paste(m$`Mid Fieldersr_HP`,'Mid Fielder',sep="_")
m$Forwards_HP = paste(m$Forwards_HP,'Forwards',sep="_")
m$Defender_AP = paste(m$Defender_AP,'Defender',sep="_")
m$`Mid Fieldersr_AP` = paste(m$`Mid Fieldersr_AP`,'Mid Fielder',sep="_")
m$Forwards_AP = paste(m$Forwards_AP ,'Forwards',sep="_")

write.csv(m,'match_final.csv',row.names = FALSE)
match = read.transactions("match_final.csv", format = "basket",
                                  sep = ",", rm.duplicates = TRUE)
rules <- apriori(match, parameter = list(supp = 0.05, conf = 0.1),
                  appearance = list(rhs = 'lose'))
rules_lift = sort(rules,by='lift',decreasing = TRUE)
inspect(rules_lift)

######
# for (i in 1:11){
#   x = paste("home_player_X",i,sep='')
#   y = paste("home_player_Y",i,sep='')
#   inp = paste("hm_ply_pos",i,sep="_")
#   match[inp] = ifelse(match[x]==1 & match[y]==1,"Goaly",
#                       ifelse(match[y]>=2 & match[y]<=5,"Defender",
#                              ifelse(match[y]>=6 & match[y]<=8,"Mid Fielders",
#                                     ifelse(match[y]>=9 & match[y]<=11,"Forwards","NA"))))
# }
# 
# for (i in 1:11){
#   x = paste("away_player_X",i,sep='')
#   y = paste("away_player_Y",i,sep='')
#   inp = paste("away_ply_pos",i,sep="_")
#   match[inp] = ifelse(match[x]==1 & match[y]==1,"Goaly",
#                       ifelse(match[y]>=2 & match[y]<=5,"Defender",
#                              ifelse(match[y]>=6 & match[y]<=8,"Mid Fielders",
#                                     ifelse(match[y]>=9 & match[y]<=11,"Forwards","NA"))))
# }
# ###########################################
# # set arributes from numeric to levels
# divide = function(column){
#   column = cut(column, 5, label = c('low','low-med','med','high-med','high'))
# }
# att = player_atts[,-c(1,2,3,4,7,8,9)]
# attri = apply(att, 2, divide)
# 
# ###########################################
# match_outcomes_per_match <- match_tbl %>% 
#                     mutate(goal_diff =home_team_goal - away_team_goal) %>% 
#                     select(id, goal_diff) %>%
#                     rename(match_id = id) %>%
#                     collect()
# 
# roma_players_per_match <- select(home_matches, id,matches("home_player_[[:digit:]]")) %>%
#                 collect() %>%
#                 # the gather function is a method in the tidyr package. It lets me turn un-tidy data into tidy data
#                 # take a loook at the difference between the output of just the previous line an then all lines together
#                 # can you see how why the previous output was not tidy data?
#                 gather(player, player_api_id, -id) %>%
#                 rename(match_id = id)
# ############################################
# position = all_match[5,12:55]
# position_x = as.vector(position[1:22])
# position_y = position[23:44]
# position_xx = t(position_x)
# position_yy = t(position_y)
# plot(position_xx,position_yy)
# 
