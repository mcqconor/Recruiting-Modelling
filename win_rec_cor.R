library(cfbfastR)
library(ggplot2)
library(tidyverse)
library(extrafont)
loadfonts(device = "win")

p5 <- c('Boston College','Clemson','Duke','Florida State','Georgia Tech','Louisville','Miami','North Carolina','NC State','Pittsburgh','Syracuse','Virginia',
        'Virginia Tech','Notre Dame','Wake Forest','Illinois','Indiana','Iowa','Maryland','Michigan','Michigan State','Minnesota','Nebraska','Northwestern',
        'Ohio State','Penn State','Purdue','Rutgers','Wisconsin','Baylor','Iowa State','Kansas','Kansas State','Oklahoma','Oklahoma State','TCU','Texas',
        'Texas Tech','West Virginia','Arizona','Arizona State','California','UCLA','USC','Oregon','Oregon State','Washington','Washington State','Utah',
        'Stanford','Colorado','Alabama','Arkansas','Auburn','Florida','Georgia','Kentucky','LSU','Ole Miss','Mississippi State','Missouri','Tennessee',
        'South Carolina','Texas A&M','Vanderbilt')

rec_ratings <- cfbd_recruiting_team()

team_records <- data.frame()
for(i in 2008:2020){
  team_records <- bind_rows(team_records, cfbd_game_records(i))
}

adj_team_records <- team_records %>% select(team, year, total_games, total_wins)

adj_rec_rats <- inner_join(rec_ratings, adj_team_records) %>% 
  group_by(team) %>% 
  mutate(
    last_tg = lag(total_games, order_by = year),
    last_wins = lag(total_wins, order_by = year),
    l2_tg = lag(last_tg, order_by = year),
    l2_wins = lag(last_wins, order_by = year),
    l3_tg = lag(l2_tg, order_by = year),
    l3_wins = lag(l2_wins, order_by = year)
  ) %>% 
  ungroup() %>% 
  select(-rank, -total_games, -total_wins, -last_tg, -last_wins) %>% 
  mutate(
    l3_wp = (l3_wins + l2_wins)/(l3_tg + l2_tg),
    rec_points = as.numeric(points)
  ) %>% 
  select(team, year, rec_points, l3_wp) %>% 
  na.omit()

p5_adj <- adj_rec_rats %>% filter((team %in% p5))
g5_adj <- adj_rec_rats %>% filter(!(team %in% p5))

rsq_p5 <- round(cor(p5_adj$l3_wp, p5_adj$rec_points)^2, 2)
rsq_g5 <- round(cor(g5_adj$l3_wp, g5_adj$rec_points)^2, 2)

ggplot(data = p5_adj)+
  geom_point(mapping = aes(x = l3_wp*100, y = rec_points))+
  geom_smooth(mapping = aes(x = l3_wp*100, y = rec_points), method = 'lm', se = FALSE)+
  geom_text(mapping = aes(x = 25, y = max(rec_points)*0.8, label = paste0('R^2 = ', rsq_p5)))+
  labs(
    x = 'Win Percentage in 2 Prior Seasons',
    y = 'Recruiting Class Points Current Season',
    title = 'How Much Impact Does On-Field Success Have On Recruiting?',
    subtitle = 'P5 Programs Only, 2011-2021',
    caption = 'By Conor McQuiston | Data from @CFB_Data via @cfbfastR'
  )+
  theme_bw()+
  theme(
    plot.title = element_text(size = 16, hjust = 0.5),
    plot.subtitle = element_text(size = 14, hjust = 0.5),
    axis.title = element_text(size = 12)
  )

ggsave('p5_win_rec_cor.png', height = 7, width = 13, path = '~/RecApp/Vignettes/')

ggplot(data = g5_adj)+
  geom_point(mapping = aes(x = l3_wp*100, y = rec_points))+
  geom_smooth(mapping = aes(x = l3_wp*100, y = rec_points), method = 'lm', se = FALSE)+
  geom_text(mapping = aes(x = 12.5, y = max(rec_points)*0.8, label = paste0('R^2 = ', rsq_g5)))+
  labs(
    x = 'Win Percentage in 2 Prior Seasons',
    y = 'Recruiting Class Points Current Season',
    title = 'How Much Impact Does On-Field Success Have On Recruiting?',
    subtitle = 'P5 Programs Only, 2011-2021',
    caption = 'By Conor McQuiston | Data from @CFB_Data via @cfbfastR'
  )+
  theme_bw()+
  theme(
    plot.title = element_text(size = 16, hjust = 0.5),
    plot.subtitle = element_text(size = 14, hjust = 0.5),
    axis.title = element_text(size = 12)
  )

ggsave('g5_win_rec_cor.png', height = 7, width = 13, path = '~/RecApp/Vignettes/')
