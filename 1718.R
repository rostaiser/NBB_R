library(rvest)
library(tidyverse)
library(RSelenium)
library(netstat)
library(stringr)
library(RODBC)


### download schedule
url <- read_html("https://lnb.com.br/nbb/tabela-de-jogos/?season%5B%5D=41")

date <- url %>% html_nodes(".date_value span:nth-child(1)") %>%
  html_text() %>% as.Date.character(format = c("%d/%m/%Y"))                          #convert character to date
time <- url %>% html_nodes(".date_value span+ span") %>% html_text()                 
home <- url %>% html_nodes(".home_team_value .team-shortname") %>% html_text()
home <- home %>% str_replace_all("Campo Mourão", "VipTech CMB") %>% 
  str_replace_all("Basq. Cearense", "Fortaleza B. C.") %>% 
  str_replace_all("Caxias do Sul", "KTO/Caxias do Sul")               #time casa
away <- url %>% html_nodes(".visitor_team_value .team-shortname") %>% html_text()
away <- away %>% str_replace_all("Campo Mourão", "VipTech CMB") %>% 
  str_replace_all("Basq. Cearense", "Fortaleza B. C.") %>% 
  str_replace_all("Caxias do Sul", "KTO/Caxias do Sul") 
team <- c(rbind(home,away))                                                                 #team string
opponent <- c(rbind(away,home))                                                             #opponent string
home_score <- url %>% html_nodes(".home") %>% html_text() %>% as.numeric()           
away_score <- url %>% html_nodes(".away") %>% html_text() %>% as.numeric()           
home_score <- home_score[c(T,F)]
away_score <- away_score[c(T,F)]
match <- paste(home, home_score, "x", away_score, away) %>% rep(each = 2)            #match name

season_phase <- url %>% html_nodes(".stage_value") %>% html_text() %>% rep(each =2)   
season <- url %>% html_nodes(".champ_value") %>% html_text() %>% rep(each =2)         
venue <- url %>% html_nodes(".gym_value") %>% html_text() %>% rep(each =2) %>% trimws() #trim unnecessary characters

id <- url %>% html_nodes(".position_value") %>% html_text() %>% rep(each = 2) %>%     #id do jogo
  as.numeric() %>% paste0(c("H", "A"))

schedule1718 <- tibble(id, home_away = rep(c("home","away"), length(date)/2), date, time, match, team, opponent, season_phase, season, venue)


### stats
static_final <- tibble()
coach1718 <- tibble()
period1718 <- tibble()


links_1718 <- url %>% html_nodes(".match_score_relatorio") %>% html_attr("href")
links_1718 <- links_1718[c(T,F)]
links_1718 <- tibble(link = links_1718, id = c(1:length(links_1718)))
links_1718 <- links_1718 %>% filter(id != 136)

for (i in 1:nrow(links_1718)) {
  link <- links_1718$link[i]
  
  web <- read_html(link)
  stats <- web %>% html_nodes(".tablesorter") %>% html_table()
  
  nhome <- nrow(stats[[1]])
  naway <- nrow(stats[[length(stats)]])
  l <- length(stats)/2
  
  idh <- paste0(links_1718$id[i], "H")
  ida <- paste0(links_1718$id[i], "A")
  
  coach <- tibble(id = c(idh,ida), 
                  coach = c(web %>%
                              html_node(".medium-4~ .columns+ .columns tr:nth-child(2) td") %>%
                              html_text() %>%
                              trimws(),
                            web %>%
                              html_node(".medium-4~ .columns+ .columns tr~ tr+ tr td") %>%
                              html_text() %>%
                              trimws()))
  
  
  score <- c(web %>% html_element("#home_score") %>% html_text() %>% as.numeric() -
               web %>% html_element("#away_score") %>% html_text() %>% as.numeric(),
             web %>% html_element("#home_quarter_1 strong") %>% html_text() %>% as.numeric() -
               web %>% html_element("#away_quarter_1 strong") %>% html_text() %>% as.numeric(),
             web %>% html_element(".quarter:nth-child(2) #home_quarter_1 strong") %>% html_text() %>% as.numeric() -
               web %>% html_element(".quarter:nth-child(2) #away_quarter_1 strong") %>% html_text() %>% as.numeric(),
             web %>% html_element(".quarter:nth-child(3) #home_quarter_1 strong") %>% html_text() %>% as.numeric() -
               web %>% html_element(".quarter:nth-child(3) #away_quarter_1 strong") %>% html_text() %>% as.numeric(),
             web %>% html_element(".quarter:nth-child(4) #home_quarter_1 strong") %>% html_text() %>% as.numeric() -
               web %>% html_element(".quarter:nth-child(4) #away_quarter_1 strong") %>% html_text() %>% as.numeric())
  
  
  period <- c("game", "1st quarter", "2nd quarter", "3rd quarter", "4th quarter")
  period_home <- rep(period, each = nhome)
  period_away <- rep(period, each = naway)
  
  
  if (length(stats) == 12) {
    period <- c(period, "OT")
    period_home <- rep(period, each = nhome)
    period_away <- rep(period, each = naway)
    
    score <- c(score,
               web %>% html_element(".quarter:nth-child(5) #home_quarter_1 strong") %>% html_text() %>% as.numeric() -
                 web %>% html_element(".quarter:nth-child(5) #away_quarter_1 strong") %>% html_text() %>% as.numeric())
  } else if (length(stats) == 14) {
    period <- c(period, "OT", "2OT")
    period_home <- rep(period, each = nhome)
    period_away <- rep(period, each = naway)
    
    score <- c(score,
               web %>% html_element(".quarter:nth-child(5) #home_quarter_1 strong") %>% html_text() %>% as.numeric() -
                 web %>% html_element(".quarter:nth-child(5) #away_quarter_1 strong") %>% html_text() %>% as.numeric(),
               web %>% html_element(".quarter:nth-child(6) #home_quarter_1 strong") %>% html_text() %>% as.numeric() -
                 web %>% html_element(".quarter:nth-child(6) #away_quarter_1 strong") %>% html_text() %>% as.numeric())
  } else if (length(stats) == 16) {
    period <- c(period, "OT", "2OT", "3OT")
    period_home <- rep(period, each = nhome)
    period_away <- rep(period, each = naway)
    
    score <- c(score,
               web %>% html_element(".quarter:nth-child(5) #home_quarter_1 strong") %>% html_text() %>% as.numeric() -
                 web %>% html_element(".quarter:nth-child(5) #away_quarter_1 strong") %>% html_text() %>% as.numeric(),
               web %>% html_element(".quarter:nth-child(6) #home_quarter_1 strong") %>% html_text() %>% as.numeric() -
                 web %>% html_element(".quarter:nth-child(6) #away_quarter_1 strong") %>% html_text() %>% as.numeric(),
               web %>% html_element(".quarter:nth-child(7) #home_quarter_1 strong") %>% html_text() %>% as.numeric() -
                 web %>% html_element(".quarter:nth-child(7) #away_quarter_1 strong") %>% html_text() %>% as.numeric())
  }
  
  
  stats <- stats %>% 
    bind_rows()     #turn list into dataframe
  stats <- tibble(id = c(rep(idh, nhome*l),rep(ida, naway*l)),
                  period = c(period_home, period_away), stats)
  static_final <- rbind(static_final, stats) 
  coach1718 <- rbind(coach1718, coach)
  period_temp <- tibble(id = c(rep(idh, l), rep(ida,l)), period = rep(period, 2),
                        score = c(score, score*(-1)))
  period1718 <- rbind(period1718, period_temp)
  
}

period1718 <- period1718 %>% 
  mutate(period_result = ifelse(score > 0, "won",ifelse(score < 0, "lost", "draw"))) %>% 
  mutate(game_result = case_when(period == "game" & period_result == "won" ~ "won",
                                 period == "game" & period_result == "lost" ~ "lost")) %>% 
  fill(game_result)

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

nbb1718 <-  static_final %>% group_by(id, period) %>% 
  mutate(t_minutes = round(sum(p_minutes)))

team1718 <- nbb1718 %>% 
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

home1718 <- team1718 %>% 
  subset(grepl("H", team1718$id)) %>% 
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

home1718$id <- home1718$id %>%  str_replace_all("H","A")

away1718 <- team1718 %>% 
  subset(grepl("A", team1718$id)) %>% 
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

away1718$id <- away1718$id %>%  str_replace_all("A","H")

opponent1718 <- rbind(home1718, away1718) %>% 
  arrange(id)

team1718 <- left_join(team1718, opponent1718) %>% 
  select(-name) %>% 
  arrange(id)

team1718 <- team1718 %>% ungroup() %>% 
  mutate(t_total_possessions = (((team1718$t_three_points_attempted + team1718$t_two_points_attempted +
                                    team1718$t_turnovers + (0.4*team1718$t_free_throws_attempted) -
                                    (team1718$t_offensive_rebounds/(team1718$t_offensive_rebounds + team1718$o_defensive_rebounds))*
                                    ((team1718$t_three_points_attempted + team1718$t_two_points_attempted) -
                                       (team1718$t_three_points_made + team1718$t_two_points_made))*1.07) + (
                                         team1718$o_three_points_attempted + team1718$o_two_points_attempted +
                                           team1718$o_turnovers + (0.4*team1718$o_free_throws_attempted) -
                                           (team1718$o_offensive_rebounds/(team1718$o_offensive_rebounds + team1718$t_defensive_rebounds))*
                                           ((team1718$o_three_points_attempted + team1718$o_two_points_attempted) -
                                              (team1718$o_three_points_made + team1718$o_two_points_made))*1.07 ))/2),
         
         t_scoring_possessions = (team1718$t_three_points_made + team1718$t_two_points_made +
                                    (1 - (1-team1718$t_free_throws_percentage)^2)*team1718$t_free_throws_attempted*0.4),
         
         t_floor_percentage = t_scoring_possessions/t_total_possessions,
         
         t_plays = (team1718$t_three_points_attempted + team1718$t_two_points_attempted +
                      team1718$t_free_throws_attempted*0.4 + team1718$t_turnovers),
         
         t_plays_percentage = t_scoring_possessions/t_plays,
         
         t_field_percentage = (team1718$t_three_points_made + team1718$t_two_points_made)/
           ((team1718$t_three_points_attempted + team1718$t_two_points_attempted - team1718$t_offensive_rebounds)/
              (team1718$t_offensive_rebounds + team1718$o_defensive_rebounds)*
              (team1718$t_three_points_attempted + team1718$t_two_points_attempted -
                 team1718$t_three_points_made - team1718$t_two_points_made)*1.07 +
              team1718$t_turnovers),
         
         t_offensive_rating = team1718$t_points_made/t_total_possessions*100,
         t_defensive_rating = team1718$o_points_made/t_total_possessions*100,
         
         t_expected_fg_percentage = (team1718$t_three_points_made + team1718$t_two_points_made +
                                       (0.5*team1718$t_three_points_made))/100,
         
         t_turnovers_percentage = (team1718$t_turnovers/
                                     (team1718$t_three_points_attempted + team1718$t_two_points_attempted +
                                        0.44*team1718$t_free_throws_attempted + team1718$t_turnovers)),
         o_turnovers_percentage = (team1718$o_turnovers/
                                     (team1718$o_three_points_attempted + team1718$o_two_points_attempted +
                                        0.44*team1718$o_free_throws_attempted + team1718$o_turnovers)),
         
         t_off_rebounds_percentage = team1718$t_offensive_rebounds/(team1718$t_offensive_rebounds + team1718$o_defensive_rebounds),
         
         t_def_rebounds_percentage = team1718$t_defensive_rebounds/(team1718$t_defensive_rebounds + team1718$o_offensive_rebounds),
         
         o_def_rebounds_percentage = team1718$o_defensive_rebounds/(team1718$o_defensive_rebounds + team1718$t_offensive_rebounds),
         
         o_off_rebounds_percentage = team1718$o_offensive_rebounds/(team1718$o_offensive_rebounds + team1718$t_defensive_rebounds)
  )


team1718[team1718 == "Inf"] <- NaN
team1718[team1718 == "-Inf"] <- NaN

players1718 <- left_join(
  nbb1718 %>% filter(name != "Equipe"),
  team1718 %>% select(id, period, starts_with("t_"), starts_with("o_")),
  by = c("id", "period", "t_minutes"))

#calculate individual scoring possessions
q5 <- 1.14*((players1718$t_assists - players1718$p_assists)/
              (players1718$t_two_points_made + players1718$t_three_points_made))

q12 <- ((players1718$t_assists/players1718$t_minutes)*players1718$p_minutes*5 - players1718$p_assists)/
  (((players1718$t_three_points_made + players1718$t_two_points_made)/players1718$t_minutes)*players1718$p_minutes*5 - players1718$p_three_points_made - players1718$p_two_points_made)

qast <- players1718$p_minutes/(players1718$t_minutes/5)*q5 + (1 - (players1718$p_minutes/(players1718$t_minutes/5)))*q12

fg_part <- (players1718$p_three_points_made + players1718$p_two_points_made)*(1-0.5*((
  players1718$p_points_made - players1718$p_free_throws_made)/
    (2 * (players1718$p_three_points_attempted + players1718$p_two_points_attempted)))*qast)

ast_part <- 0.5 * (((players1718$t_points_made - players1718$t_free_throws_made) -
                      (players1718$p_points_made - players1718$p_free_throws_made))/
                     (2 * (players1718$t_three_points_attempted + players1718$t_two_points_attempted - 
                             players1718$p_three_points_attempted - players1718$p_two_points_attempted)))*players1718$p_assists

ft_part <- (1 - (1 - players1718$p_free_throws_percentage)^2)*0.4*players1718$p_free_throws_attempted
ft_part <- gsub("NaN", 0, ft_part) %>% as.numeric()

tmor_weight <- ((1 - players1718$t_off_rebounds_percentage) * players1718$t_plays_percentage)/
  ((1 - players1718$t_off_rebounds_percentage) * players1718$t_plays_percentage + players1718$t_off_rebounds_percentage * 
     (1 - players1718$t_plays_percentage))

or_part <- players1718$p_offensive_rebounds * tmor_weight * players1718$t_plays_percentage

#calculate individual total possessions
missed_fg_part <- (players1718$p_three_points_attempted + players1718$p_two_points_attempted -
                     players1718$p_three_points_made - players1718$p_two_points_made)*(
                       1 - 1.07 * players1718$t_off_rebounds_percentage)

missed_ft_part <- (1 - players1718$p_free_throws_percentage)^2 * 0.4 *(players1718$p_free_throws_attempted)                     
missed_ft_part <- gsub("NaN", 0, missed_ft_part) %>% as.numeric()

#calculate individual points produced
fg_part_pp <- 2*(players1718$p_three_points_made + players1718$p_two_points_made + 0.5*players1718$p_three_points_made)*(
  1-0.5*((players1718$p_points_made - players1718$p_free_throws_made)/(2 * (players1718$p_three_points_attempted + players1718$p_two_points_attempted)))*qast)

ast_part_pp <- 2 * ((players1718$t_three_points_made + players1718$t_two_points_made - 
                       players1718$p_three_points_made - players1718$p_two_points_made +
                       0.5 * (players1718$t_three_points_made - players1718$p_three_points_made))/
                      (players1718$t_three_points_made + players1718$t_two_points_made - 
                         players1718$p_three_points_made - players1718$p_two_points_made))*0.5*(((
                           players1718$t_points_made - players1718$t_free_throws_made) - (
                             players1718$p_points_made - players1718$p_free_throws_made))/(
                               2 * (players1718$t_three_points_attempted + players1718$t_two_points_attempted - 
                                      players1718$p_three_points_attempted - players1718$p_two_points_attempted))) * players1718$p_assists

or_part_pp <- or_part * (players1718$t_points_made/(players1718$t_three_points_made + players1718$t_two_points_made +(
  1 - (1 - players1718$t_free_throws_percentage)^2)*0.4*players1718$t_free_throws_attempted))


players1718 <- players1718 %>% ungroup() %>% 
  mutate(p_scoring_possessions = ((fg_part + ast_part + ft_part) * 
                                    (1 - players1718$t_offensive_rebounds/players1718$t_scoring_possessions * tmor_weight * players1718$t_plays_percentage) + or_part),
         
         p_total_possessions = (p_scoring_possessions + missed_fg_part + missed_ft_part + players1718$p_turnovers),
         
         p_floor_percentage = (p_scoring_possessions/p_total_possessions),
         
         p_points_produced = ((fg_part_pp + ast_part_pp + players1718$p_free_throws_made) * (
           1 - players1718$t_offensive_rebounds/players1718$t_scoring_possessions * tmor_weight * players1718$t_plays_percentage) + or_part_pp),
         
         p_offensive_rating = (p_points_produced/p_total_possessions*100),
         
         p_possessions_percentage = (p_total_possessions/players1718$t_total_possessions))

#ADVANCED DEFENSIVE STATS

fmwt <- (players1718$o_field_goal_percentage * (1 - players1718$o_off_rebounds_percentage))/(
  players1718$o_field_goal_percentage * (1 - players1718$o_off_rebounds_percentage) + (
    1 - players1718$o_field_goal_percentage) * players1718$o_off_rebounds_percentage)

stops1 <- players1718$p_steals + players1718$p_blocks * fmwt * (1 - 1.07 *players1718$o_off_rebounds_percentage) +
  players1718$p_defensive_rebounds * (1 - fmwt)

stops2 <- (((players1718$o_three_points_attempted + players1718$o_two_points_attempted -
               players1718$o_three_points_made - players1718$o_two_points_made - players1718$t_blocks)/
              players1718$t_minutes) * fmwt * (1 - 1.07 * players1718$o_off_rebounds_percentage) + 
             ((players1718$o_turnovers - players1718$t_steals)/players1718$t_minutes)) * 
  players1718$p_minutes + (players1718$p_fouls_committed/players1718$t_fouls_committed) * 0.4 *
  players1718$o_free_throws_attempted * ((1 - players1718$o_free_throws_percentage)^2)

stops <- stops1 + stops2

p_def_pts_per_scposs <-  players1718$o_points_made/(players1718$o_three_points_made + players1718$o_two_points_made + (
  1 - (1 - ((players1718$o_three_points_made + players1718$o_two_points_made)/(
    players1718$o_three_points_attempted + players1718$o_two_points_attempted)))^2*
    players1718$o_free_throws_attempted*0.4))

players1718 <- players1718 %>% 
  mutate(p_stops_percentage = (stops * players1718$t_minutes)/(players1718$t_total_possessions * players1718$p_minutes),
         p_defensive_rating = players1718$t_defensive_rating + 0.2 * (100 * p_def_pts_per_scposs * (1 - p_stops_percentage) -
                                                                        players1718$t_defensive_rating))

players1718[players1718 == "Inf"] <- NaN
players1718[players1718 == "-Inf"] <- NaN
players1718 <- players1718 %>% 
  select(c(1:4), starts_with("p_"))

###
con <- odbcConnect("MySQL_FourFactors")
sqlQuery(con, "CREATE TABLE `schedule1718` (
         `id` varchar(4) NOT NULL PRIMARY KEY,
         `home_away` ENUM('home', 'away'),
         `date` date NOT NULL,
         `time` varchar(5) NOT NULL,
         `match` varchar(60) NOT NULL,
         `team` varchar(20) NOT NULL,
         `opponent` varchar(20) NOT NULL,
         `season_phase` varchar(8) NOT NULL,
         `season` varchar(9) NOT NULL,
         `venue` varchar(60) DEFAULT NULL
         ) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_0900_ai_ci
")

sqlQuery(con, "CREATE TABLE `team1718` (
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
         FOREIGN KEY (id) REFERENCES schedule1718(id)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_0900_ai_ci;")

sqlQuery(con, "CREATE TABLE `players1718` (
         `id` varchar(4) NOT NULL,
         `period` varchar(11) NOT NULL,
         `jersey` varchar(3) NOT NULL,
         `name` varchar(20) NOT NULL,
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
         FOREIGN KEY (id) REFERENCES schedule1718(id)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_0900_ai_ci;")

sqlQuery(con, "CREATE TABLE period1718 (
         `id` varchar(4) NOT NULL,
         `period` varchar(11) NOT NULL,
         `score` INT DEFAULT NULL,
         `period_result` ENUM('won', 'lost', 'draw'),
         `game_result` ENUM('won', 'lost'),
         FOREIGN KEY (id) REFERENCES schedule2021(id)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_0900_ai_ci;")

sqlQuery(con, "CREATE TABLE `coach1718` (
  `id` varchar(255) DEFAULT NULL,
  `coach` varchar(255) DEFAULT NULL,
  FOREIGN KEY (id) REFERENCES schedule1718(id)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_0900_ai_ci;")


sqlSave(con, schedule1718, "schedule1718", rownames = F, fast = T, append = T)
sqlSave(con, team1718, "team1718", rownames = F, fast = T, append = T)
sqlSave(con, players1718, "players1718", rownames = F, fast = T, append = T)
sqlSave(con, period1718, "period1718", rownames = F, fast = T, append = T)
sqlSave(con, coach1718, "coach1718", rownames = F, fast = T, append = T)


odbcCloseAll()
