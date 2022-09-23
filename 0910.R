library(rvest)
library(tidyverse)
library(RSelenium)
library(netstat)
library(stringr)
library(RODBC)


### download schedule
url <- read_html("https://lnb.com.br/nbb/tabela-de-jogos/?season%5B%5D=2")

date <- url %>% html_nodes(".date_value span:nth-child(1)") %>%
  html_text() %>% as.Date.character(format = c("%d/%m/%Y"))                          #convert character to date
time <- url %>% html_nodes(".date_value span+ span") %>% html_text()                 
home <- url %>% html_nodes(".home_team_value .team-shortname") %>% html_text()
home <- home %>%  str_replace_all("Franca", "Sesi Franca")               #time casa
away <- url %>% html_nodes(".visitor_team_value .team-shortname") %>% html_text()
away <- away %>% str_replace_all("Franca", "Sesi Franca")  
team <- c(rbind(home,away))                                                                 #team string
opponent <- c(rbind(away,home))                                                               #opponent string
home_score <- url %>% html_nodes(".home") %>% html_text() %>% as.numeric()           
away_score <- url %>% html_nodes(".away") %>% html_text() %>% as.numeric()           
home_score <- home_score[c(T,F)]
away_score <- away_score[c(T,F)]
match <- paste(home, home_score, "x", away_score, away) %>% rep(each = 2)            #match name

season_phase <- url %>% html_nodes(".stage_value") %>% html_text() %>% rep(each =2)   
season <- url %>% html_nodes(".champ_value") %>% html_text() %>% rep(each =2)         

id <- url %>% html_nodes(".position_value") %>% html_text() %>% rep(each = 2) %>%     #id do jogo
  as.numeric() %>% paste0(c("H", "A"))

schedule0910 <- tibble(id, home_away = rep(c("home","away"), length(date)/2), date, time, match, team, opponent, season_phase, season)

period0910 <- tibble(id, period = rep("game", length(id)),
                     score = c(rbind(home_score, away_score)) - c(rbind(away_score, home_score)))
period0910 <- period0910 %>% mutate(game_result = ifelse(score > 0, "won", "lost"))

links_0910 <- url %>% html_nodes(".match_score_relatorio") %>% html_attr("href")
links_0910 <- links_0910[c(T,F)]
links_0910 <- tibble(link = links_0910, id = c(1:length(links_0910)))

static_final <- tibble()

for (i in 1:nrow(links_0910)) {
  link <- links_0910$link[i]
  
  web <- read_html(link)
  stats <- web %>% html_nodes(".tablesorter") %>% html_table()
  
  nhome <- nrow(stats[[1]])
  naway <- nrow(stats[[length(stats)]])
 
  
  idh <- paste0(links_0910$id[i], "H")
  ida <- paste0(links_0910$id[i], "A")
  
  stats <- stats[c(1,length(stats))]
  l <- length(stats)/2
  stats <- stats %>% 
    bind_rows()     #turn list into dataframe
  stats <- tibble(id = c(rep(idh, nhome*l),rep(ida, naway*l)),
                  period = rep("game",nrow(stats)), stats)
  static_final <- rbind(static_final, stats)
}

static_final <- static_final %>% 
  separate(`Pts`, c ("p_points_made", "p_points_attempted"), convert = TRUE, extra = 'drop') %>% 
  separate(`RD+RO RT`, c("p_defensive_rebounds", "p_offensive_rebounds", "p_total_rebounds"), convert = TRUE) %>% 
  separate(`3P%`, c("p_three_points_made", "p_three_points_attempted"), convert = TRUE, extra = 'drop') %>% 
  separate(`2P%`, c("p_two_points_made", "p_two_points_attempted"), convert = TRUE, extra = 'drop') %>% 
  separate(`LL%`, c("p_free_throws_made", "p_free_throws_attempted"), convert = TRUE, extra = 'drop') %>% 
  mutate(p_points_percentage = p_points_made/p_points_attempted,
         p_three_points_percentage = p_three_points_made/p_three_points_attempted, 
         p_two_points_percentage = p_two_points_made/p_two_points_attempted, 
         p_free_throws_percentage = p_free_throws_made/p_free_throws_attempted,
         p_field_goal_percentage = (p_three_points_made + p_two_points_made)/(p_three_points_attempted + p_two_points_attempted),
         p_effective_fg_percentage = (p_three_points_made + p_two_points_made + 0.5*p_three_points_made)/(p_three_points_attempted + p_two_points_attempted),
         p_ft_per_fga = p_free_throws_made/(p_three_points_attempted + p_two_points_attempted)) %>% 
  rename(
    name = Jogador,
    jersey = Nr.,
    p_minutes = Min,
    p_assists = AS,
    p_steals = BR,
    p_blocks = TO,
    p_fouls_committed = FC,
    p_fouls_drawn = FR,
    p_turnovers = ER,
    p_dunks = EN,
    p_plus_minus = `+/-`,
    p_efficiency = EF)%>%
  select(-JO)

nbb0910 <-  static_final %>% group_by(id, period) %>% 
  mutate(t_minutes = round(sum(p_minutes)))

team0910 <- nbb0910 %>% 
  filter(name == "Equipe") %>% 
  rename(t_points_made = p_points_made,
         t_points_attempted = p_points_attempted,
         t_points_percentage = p_points_percentage,
         t_assists = p_assists,
         t_defensive_rebounds = p_defensive_rebounds,
         t_offensive_rebounds = p_offensive_rebounds,
         t_total_rebounds = p_total_rebounds,
         t_three_points_made = p_three_points_made,
         t_three_points_attempted = p_three_points_attempted,
         t_three_points_percentage = p_three_points_percentage,
         t_two_points_made = p_two_points_made,
         t_two_points_attempted = p_two_points_attempted,
         t_two_points_percentage = p_two_points_percentage,
         t_field_goal_percentage = p_field_goal_percentage,
         t_effective_fg_percentage = p_effective_fg_percentage,
         t_free_throws_made = p_free_throws_made,
         t_free_throws_attempted = p_free_throws_attempted,
         t_free_throws_percentage = p_free_throws_percentage,
         t_ft_per_fga = p_ft_per_fga,
         t_steals = p_steals,
         t_blocks = p_blocks,
         t_fouls_committed = p_fouls_committed,
         t_fouls_drawn = p_fouls_drawn,
         t_turnovers = p_turnovers,
         t_dunks = p_dunks,
         t_plus_minus = p_plus_minus,
         t_efficiency = p_efficiency) %>% 
  arrange(id) %>% 
  select(-p_minutes, -jersey)

home0910 <- team0910 %>% 
  subset(grepl("H", team0910$id)) %>% 
  rename(o_points_made = t_points_made,
         o_points_attempted = t_points_attempted,
         o_points_percentage = t_points_percentage,
         o_assists = t_assists,
         o_defensive_rebounds = t_defensive_rebounds,
         o_offensive_rebounds = t_offensive_rebounds,
         o_total_rebounds = t_total_rebounds,
         o_three_points_made = t_three_points_made,
         o_three_points_attempted = t_three_points_attempted,
         o_three_points_percentage = t_three_points_percentage,
         o_two_points_made = t_two_points_made,
         o_two_points_attempted = t_two_points_attempted,
         o_two_points_percentage = t_two_points_percentage,
         o_field_goal_percentage = t_field_goal_percentage,
         o_effective_fg_percentage = t_effective_fg_percentage,
         o_free_throws_made = t_free_throws_made,
         o_free_throws_attempted = t_free_throws_attempted,
         o_free_throws_percentage = t_free_throws_percentage,
         o_ft_per_fga = t_ft_per_fga,
         o_steals = t_steals,
         o_blocks = t_blocks,
         o_fouls_committed = t_fouls_committed,
         o_fouls_drawn = t_fouls_drawn,
         o_turnovers = t_turnovers,
         o_dunks = t_dunks,
         o_plus_minus = t_plus_minus,
         o_efficiency = t_efficiency) %>% 
  arrange(id) %>% 
  select(-t_minutes)

home0910$id <- home0910$id %>%  str_replace_all("H","A")

away0910 <- team0910 %>% 
  subset(grepl("A", team0910$id)) %>% 
  rename(o_points_made = t_points_made,
         o_points_attempted = t_points_attempted,
         o_points_percentage = t_points_percentage,
         o_assists = t_assists,
         o_defensive_rebounds = t_defensive_rebounds,
         o_offensive_rebounds = t_offensive_rebounds,
         o_total_rebounds = t_total_rebounds,
         o_three_points_made = t_three_points_made,
         o_three_points_attempted = t_three_points_attempted,
         o_three_points_percentage = t_three_points_percentage,
         o_two_points_made = t_two_points_made,
         o_two_points_attempted = t_two_points_attempted,
         o_two_points_percentage = t_two_points_percentage,
         o_field_goal_percentage = t_field_goal_percentage,
         o_effective_fg_percentage = t_effective_fg_percentage,
         o_free_throws_made = t_free_throws_made,
         o_free_throws_attempted = t_free_throws_attempted,
         o_free_throws_percentage = t_free_throws_percentage,
         o_ft_per_fga = t_ft_per_fga,
         o_steals = t_steals,
         o_blocks = t_blocks,
         o_fouls_committed = t_fouls_committed,
         o_fouls_drawn = t_fouls_drawn,
         o_turnovers = t_turnovers,
         o_dunks = t_dunks,
         o_plus_minus = t_plus_minus,
         o_efficiency = t_efficiency) %>% 
  arrange(id) %>% 
  select(-t_minutes)

away0910$id <- away0910$id %>%  str_replace_all("A","H")

opponent0910 <- rbind(home0910, away0910) %>% 
  arrange(id)

team0910 <- left_join(team0910, opponent0910) %>% 
  select(-name) %>% 
  arrange(id)

team0910 <- team0910 %>% ungroup() %>% 
  mutate(t_total_possessions = (((team0910$t_three_points_attempted + team0910$t_two_points_attempted +
                                    team0910$t_turnovers + (0.4*team0910$t_free_throws_attempted) -
                                    (team0910$t_offensive_rebounds/(team0910$t_offensive_rebounds + team0910$o_defensive_rebounds))*
                                    ((team0910$t_three_points_attempted + team0910$t_two_points_attempted) -
                                       (team0910$t_three_points_made + team0910$t_two_points_made))*1.07) + (
                                         team0910$o_three_points_attempted + team0910$o_two_points_attempted +
                                           team0910$o_turnovers + (0.4*team0910$o_free_throws_attempted) -
                                           (team0910$o_offensive_rebounds/(team0910$o_offensive_rebounds + team0910$t_defensive_rebounds))*
                                           ((team0910$o_three_points_attempted + team0910$o_two_points_attempted) -
                                              (team0910$o_three_points_made + team0910$o_two_points_made))*1.07 ))/2),
         
         t_scoring_possessions = (team0910$t_three_points_made + team0910$t_two_points_made +
                                    (1 - (1-team0910$t_free_throws_percentage)^2)*team0910$t_free_throws_attempted*0.4),
         
         t_floor_percentage = t_scoring_possessions/t_total_possessions,
         
         t_plays = (team0910$t_three_points_attempted + team0910$t_two_points_attempted +
                      team0910$t_free_throws_attempted*0.4 + team0910$t_turnovers),
         
         t_plays_percentage = t_scoring_possessions/t_plays,
         
         t_field_percentage = (team0910$t_three_points_made + team0910$t_two_points_made)/
           ((team0910$t_three_points_attempted + team0910$t_two_points_attempted - team0910$t_offensive_rebounds)/
              (team0910$t_offensive_rebounds + team0910$o_defensive_rebounds)*
              (team0910$t_three_points_attempted + team0910$t_two_points_attempted -
                 team0910$t_three_points_made - team0910$t_two_points_made)*1.07 +
              team0910$t_turnovers),
         
         t_offensive_rating = team0910$t_points_made/t_total_possessions*100,
         t_defensive_rating = team0910$o_points_made/t_total_possessions*100,
         
         t_expected_fg_percentage = (team0910$t_three_points_made + team0910$t_two_points_made +
                                       (0.5*team0910$t_three_points_made))/100,
         
         t_turnovers_percentage = (team0910$t_turnovers/
                                     (team0910$t_three_points_attempted + team0910$t_two_points_attempted +
                                        0.44*team0910$t_free_throws_attempted + team0910$t_turnovers)),
         o_turnovers_percentage = (team0910$o_turnovers/
                                     (team0910$o_three_points_attempted + team0910$o_two_points_attempted +
                                        0.44*team0910$o_free_throws_attempted + team0910$o_turnovers)),
         
         t_off_rebounds_percentage = team0910$t_offensive_rebounds/(team0910$t_offensive_rebounds + team0910$o_defensive_rebounds),
         
         t_def_rebounds_percentage = team0910$t_defensive_rebounds/(team0910$t_defensive_rebounds + team0910$o_offensive_rebounds),
         
         o_def_rebounds_percentage = team0910$o_defensive_rebounds/(team0910$o_defensive_rebounds + team0910$t_offensive_rebounds),
         
         o_off_rebounds_percentage = team0910$o_offensive_rebounds/(team0910$o_offensive_rebounds + team0910$t_defensive_rebounds)
  )


team0910[team0910 == "Inf"] <- NaN
team0910[team0910 == "-Inf"] <- NaN

players0910 <- left_join(
  nbb0910 %>% filter(name != "Equipe"),
  team0910 %>% select(id, period, starts_with("t_"), starts_with("o_")),
  by = c("id", "period", "t_minutes"))

#calculate individual scoring possessions
q5 <- 1.14*((players0910$t_assists - players0910$p_assists)/
              (players0910$t_two_points_made + players0910$t_three_points_made))

q12 <- ((players0910$t_assists/players0910$t_minutes)*players0910$p_minutes*5 - players0910$p_assists)/
  (((players0910$t_three_points_made + players0910$t_two_points_made)/players0910$t_minutes)*players0910$p_minutes*5 - players0910$p_three_points_made - players0910$p_two_points_made)

qast <- players0910$p_minutes/(players0910$t_minutes/5)*q5 + (1 - (players0910$p_minutes/(players0910$t_minutes/5)))*q12

fg_part <- (players0910$p_three_points_made + players0910$p_two_points_made)*(1-0.5*((
  players0910$p_points_made - players0910$p_free_throws_made)/
    (2 * (players0910$p_three_points_attempted + players0910$p_two_points_attempted)))*qast)

ast_part <- 0.5 * (((players0910$t_points_made - players0910$t_free_throws_made) -
                      (players0910$p_points_made - players0910$p_free_throws_made))/
                     (2 * (players0910$t_three_points_attempted + players0910$t_two_points_attempted - 
                             players0910$p_three_points_attempted - players0910$p_two_points_attempted)))*players0910$p_assists

ft_part <- (1 - (1 - players0910$p_free_throws_percentage)^2)*0.4*players0910$p_free_throws_attempted
ft_part <- gsub("NaN", 0, ft_part) %>% as.numeric()

tmor_weight <- ((1 - players0910$t_off_rebounds_percentage) * players0910$t_plays_percentage)/
  ((1 - players0910$t_off_rebounds_percentage) * players0910$t_plays_percentage + players0910$t_off_rebounds_percentage * 
     (1 - players0910$t_plays_percentage))

or_part <- players0910$p_offensive_rebounds * tmor_weight * players0910$t_plays_percentage

#calculate individual total possessions
missed_fg_part <- (players0910$p_three_points_attempted + players0910$p_two_points_attempted -
                     players0910$p_three_points_made - players0910$p_two_points_made)*(
                       1 - 1.07 * players0910$t_off_rebounds_percentage)

missed_ft_part <- (1 - players0910$p_free_throws_percentage)^2 * 0.4 *(players0910$p_free_throws_attempted)                     
missed_ft_part <- gsub("NaN", 0, missed_ft_part) %>% as.numeric()

#calculate individual points produced
fg_part_pp <- 2*(players0910$p_three_points_made + players0910$p_two_points_made + 0.5*players0910$p_three_points_made)*(
  1-0.5*((players0910$p_points_made - players0910$p_free_throws_made)/(2 * (players0910$p_three_points_attempted + players0910$p_two_points_attempted)))*qast)

ast_part_pp <- 2 * ((players0910$t_three_points_made + players0910$t_two_points_made - 
                       players0910$p_three_points_made - players0910$p_two_points_made +
                       0.5 * (players0910$t_three_points_made - players0910$p_three_points_made))/
                      (players0910$t_three_points_made + players0910$t_two_points_made - 
                         players0910$p_three_points_made - players0910$p_two_points_made))*0.5*(((
                           players0910$t_points_made - players0910$t_free_throws_made) - (
                             players0910$p_points_made - players0910$p_free_throws_made))/(
                               2 * (players0910$t_three_points_attempted + players0910$t_two_points_attempted - 
                                      players0910$p_three_points_attempted - players0910$p_two_points_attempted))) * players0910$p_assists

or_part_pp <- or_part * (players0910$t_points_made/(players0910$t_three_points_made + players0910$t_two_points_made +(
  1 - (1 - players0910$t_free_throws_percentage)^2)*0.4*players0910$t_free_throws_attempted))


players0910 <- players0910 %>% ungroup() %>% 
  mutate(p_scoring_possessions = ((fg_part + ast_part + ft_part) * 
                                    (1 - players0910$t_offensive_rebounds/players0910$t_scoring_possessions * tmor_weight * players0910$t_plays_percentage) + or_part),
         
         p_total_possessions = (p_scoring_possessions + missed_fg_part + missed_ft_part + players0910$p_turnovers),
         
         p_floor_percentage = (p_scoring_possessions/p_total_possessions),
         
         p_points_produced = ((fg_part_pp + ast_part_pp + players0910$p_free_throws_made) * (
           1 - players0910$t_offensive_rebounds/players0910$t_scoring_possessions * tmor_weight * players0910$t_plays_percentage) + or_part_pp),
         
         p_offensive_rating = (p_points_produced/p_total_possessions*100),
         
         p_possessions_percentage = (p_total_possessions/players0910$t_total_possessions))

#ADVANCED DEFENSIVE STATS

fmwt <- (players0910$o_field_goal_percentage * (1 - players0910$o_off_rebounds_percentage))/(
  players0910$o_field_goal_percentage * (1 - players0910$o_off_rebounds_percentage) + (
    1 - players0910$o_field_goal_percentage) * players0910$o_off_rebounds_percentage)

stops1 <- players0910$p_steals + players0910$p_blocks * fmwt * (1 - 1.07 *players0910$o_off_rebounds_percentage) +
  players0910$p_defensive_rebounds * (1 - fmwt)

stops2 <- (((players0910$o_three_points_attempted + players0910$o_two_points_attempted -
               players0910$o_three_points_made - players0910$o_two_points_made - players0910$t_blocks)/
              players0910$t_minutes) * fmwt * (1 - 1.07 * players0910$o_off_rebounds_percentage) + 
             ((players0910$o_turnovers - players0910$t_steals)/players0910$t_minutes)) * 
  players0910$p_minutes + (players0910$p_fouls_committed/players0910$t_fouls_committed) * 0.4 *
  players0910$o_free_throws_attempted * ((1 - players0910$o_free_throws_percentage)^2)

stops <- stops1 + stops2

p_def_pts_per_scposs <-  players0910$o_points_made/(players0910$o_three_points_made + players0910$o_two_points_made + (
  1 - (1 - ((players0910$o_three_points_made + players0910$o_two_points_made)/(
    players0910$o_three_points_attempted + players0910$o_two_points_attempted)))^2*
    players0910$o_free_throws_attempted*0.4))

players0910 <- players0910 %>% 
  mutate(p_stops_percentage = (stops * players0910$t_minutes)/(players0910$t_total_possessions * players0910$p_minutes),
         p_defensive_rating = players0910$t_defensive_rating + 0.2 * (100 * p_def_pts_per_scposs * (1 - p_stops_percentage) -
                                                                        players0910$t_defensive_rating))

players0910[players0910 == "Inf"] <- NaN
players0910[players0910 == "-Inf"] <- NaN
players0910 <- players0910 %>% 
  select(c(1:4), starts_with("p_"))

###
con <- odbcConnect("MySQL_FourFactors")
sqlQuery(con, "CREATE TABLE `schedule0910` (
         `id` varchar(4) NOT NULL PRIMARY KEY,
         `home_away` ENUM('home', 'away'),
         `date` date NOT NULL,
         `time` varchar(5) NOT NULL,
         `match` varchar(60) NOT NULL,
         `team` varchar(20) NOT NULL,
         `opponent` varchar(20) NOT NULL,
         `season_phase` varchar(8) NOT NULL,
         `season` varchar(9) NOT NULL
         ) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_0900_ai_ci;
")

sqlQuery(con, "CREATE TABLE `team0910` (
         `id` varchar(4) NOT NULL,
         `period` varchar(11) NOT NULL,
         `t_points_made` int DEFAULT NULL,
         `t_points_attempted` int DEFAULT NULL,
         `t_three_points_made` int DEFAULT NULL,
         `t_three_points_attempted` int DEFAULT NULL,
         `t_two_points_made` int DEFAULT NULL,
         `t_two_points_attempted` int DEFAULT NULL,
         `t_free_throws_made` int DEFAULT NULL,
         `t_free_throws_attempted` int DEFAULT NULL,
         `t_offensive_rebounds` int DEFAULT NULL,
         `t_defensive_rebounds` int DEFAULT NULL,
         `t_total_rebounds` int DEFAULT NULL,
         `t_assists` int DEFAULT NULL,
         `t_steals` int DEFAULT NULL,
         `t_blocks` int DEFAULT NULL,
         `t_fouls_committed` int DEFAULT NULL,
         `t_fouls_drawn` int DEFAULT NULL,
         `t_turnovers` int DEFAULT NULL,
         `t_dunks` int DEFAULT NULL,
         `t_plus_minus` int DEFAULT NULL,
         `t_efficiency` int DEFAULT NULL,
         `t_points_percentage` double DEFAULT NULL,
         `t_three_points_percentage` double DEFAULT NULL,
         `t_two_points_percentage` double DEFAULT NULL,
         `t_free_throws_percentage` double DEFAULT NULL,
         `t_field_goal_percentage` double DEFAULT NULL,
         `t_effective_fg_percentage` double DEFAULT NULL,
         `t_ft_per_fga` double DEFAULT NULL,
         `t_minutes` double DEFAULT NULL,
         `o_points_made` int DEFAULT NULL,
         `o_points_attempted` int DEFAULT NULL,
         `o_three_points_made` int DEFAULT NULL,
         `o_three_points_attempted` int DEFAULT NULL,
         `o_two_points_made` int DEFAULT NULL,
         `o_two_points_attempted` int DEFAULT NULL,
         `o_free_throws_made` int DEFAULT NULL,
         `o_free_throws_attempted` int DEFAULT NULL,
         `o_offensive_rebounds` int DEFAULT NULL,
         `o_defensive_rebounds` int DEFAULT NULL,
         `o_total_rebounds` int DEFAULT NULL,
         `o_assists` int DEFAULT NULL,
         `o_steals` int DEFAULT NULL,
         `o_blocks` int DEFAULT NULL,
         `o_fouls_committed` int DEFAULT NULL,
         `o_fouls_drawn` int DEFAULT NULL,
         `o_turnovers` int DEFAULT NULL,
         `o_dunks` int DEFAULT NULL,
         `o_plus_minus` int DEFAULT NULL,
         `o_efficiency` int DEFAULT NULL,
         `o_points_percentage` double DEFAULT NULL,
         `o_three_points_percentage` double DEFAULT NULL,
         `o_two_points_percentage` double DEFAULT NULL,
         `o_free_throws_percentage` double DEFAULT NULL,
         `o_field_goal_percentage` double DEFAULT NULL,
         `o_effective_fg_percentage` double DEFAULT NULL,
         `o_ft_per_fga` double DEFAULT NULL,
         `t_total_possessions` double DEFAULT NULL,
         `t_scoring_possessions` double DEFAULT NULL,
         `t_floor_percentage` double DEFAULT NULL,
         `t_plays` double DEFAULT NULL,
         `t_plays_percentage` double DEFAULT NULL,
         `t_field_percentage` double DEFAULT NULL,
         `t_offensive_rating` double DEFAULT NULL,
         `t_defensive_rating` double DEFAULT NULL,
         `t_expected_fg_percentage` double DEFAULT NULL,
         `t_turnovers_percentage` double DEFAULT NULL,
         `o_turnovers_percentage` double DEFAULT NULL,
         `t_off_rebounds_percentage` double DEFAULT NULL,
         `t_def_rebounds_percentage` double DEFAULT NULL,
         `o_def_rebounds_percentage` double DEFAULT NULL,
         `o_off_rebounds_percentage` double DEFAULT NULL,
         FOREIGN KEY (id) REFERENCES schedule0910(id)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_0900_ai_ci;")

sqlQuery(con, "CREATE TABLE `players0910` (
         `id` varchar(4) NOT NULL,
         `period` varchar(11) NOT NULL,
         `jersey` varchar(4) NOT NULL,
         `name` varchar(26) NOT NULL,
         `p_minutes` double DEFAULT NULL,
         `p_points_made` int DEFAULT NULL,
         `p_points_attempted` int DEFAULT NULL,
         `p_three_points_made` int DEFAULT NULL,
         `p_three_points_attempted` int DEFAULT NULL,
         `p_two_points_made` int DEFAULT NULL,
         `p_two_points_attempted` int DEFAULT NULL,
         `p_free_throws_made` int DEFAULT NULL,
         `p_free_throws_attempted` int DEFAULT NULL,
         `p_offensive_rebounds` int DEFAULT NULL,
         `p_defensive_rebounds` int DEFAULT NULL,
         `p_total_rebounds` int DEFAULT NULL,
         `p_assists` int DEFAULT NULL,
         `p_steals` int DEFAULT NULL,
         `p_blocks` int DEFAULT NULL,
         `p_fouls_committed` int DEFAULT NULL,
         `p_fouls_drawn` int DEFAULT NULL,
         `p_turnovers` int DEFAULT NULL,
         `p_dunks` int DEFAULT NULL,
         `p_plus_minus` int DEFAULT NULL,
         `p_efficiency` int DEFAULT NULL,
         `p_points_percentage` double DEFAULT NULL,
         `p_three_points_percentage` double DEFAULT NULL,
         `p_two_points_percentage` double DEFAULT NULL,
         `p_free_throws_percentage` double DEFAULT NULL,
         `p_field_goal_percentage` double DEFAULT NULL,
         `p_effective_fg_percentage` double DEFAULT NULL,
         `p_ft_per_fga` double DEFAULT NULL,
         `p_scoring_possessions` double DEFAULT NULL,
         `p_total_possessions` double DEFAULT NULL,
         `p_floor_percentage` double DEFAULT NULL,
         `p_points_produced` double DEFAULT NULL,
         `p_offensive_rating` double DEFAULT NULL,
         `p_possessions_percentage` double DEFAULT NULL,
         `p_stops_percentage` double DEFAULT NULL,
         `p_defensive_rating` double DEFAULT NULL,
         FOREIGN KEY (id) REFERENCES schedule0910(id)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_0900_ai_ci;")

sqlQuery(con, "CREATE TABLE period0910 (
         `id` varchar(4) NOT NULL,
         `period` varchar(11) NOT NULL,
         `score` INT DEFAULT NULL,
         `game_result` ENUM('won', 'lost'),
         FOREIGN KEY (id) REFERENCES schedule0910(id)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_0900_ai_ci;")


sqlSave(con, schedule0910, "schedule0910", rownames = F, fast = T, append = T)
sqlSave(con, team0910, "team0910", rownames = F, fast = T, append = T)
sqlSave(con, players0910, "players0910", rownames = F, fast = T, append = T)
sqlSave(con, period0910, "period0910", rownames = F, fast = T, append = T)


odbcCloseAll()
