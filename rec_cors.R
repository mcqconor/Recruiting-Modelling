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

blue_bloods <- c('Alabama','Texas','Oklahoma','USC','Michigan','Notre Dame','Florida State','Miami','Penn State','Ohio State','Georgia','Florida','Tennessee','LSU',
                 'Auburn','Clemson')

g5 <- c('Cincinnati','East Carolina','Houston','Memphis','Navy','SMU','South Florida','Temple','Tulane','Tulsa','UCF',
        'Louisiana Tech','Rice','Southern Mississippi','North Texas','UAB','UTEP','UT San Antonio','Charlotte','Florida Atlantic','Florida International','Marshall',
        'Middle Tennessee State','Old Dominion','Western Kentucky',
        'Ball State','Central Michigan','Eastern Michigan','Northern Illinois','Toledo','Western Michigan','Akron','Bowling Green','Buffalo','Kent State','Miami (OH)','Ohio',
        'Fresno State','Hawai\'i','Nevada','UNLV','San Diego State','San José State','Air Force','Boise State','Colorado State','New Mexico','Utah State','Wyoming',
        'Arkansas State','Louisiana','Louisiana Monroe','South Alabama','Texas State','Appalachian State','Coastal Carolina','Georgia Southern','Georgia State','Troy',
        'Army','BYU','Liberty','New Mexico State','Connecticut','UMass')

rec_ratings <- cfbd_recruiting_team()

draft_years <- data.frame()
for(i in 2009:2020){
  draft_years<- bind_rows(draft_years, cfbd_draft_picks(i))
}

adj_draft_picks <- draft_years %>% select(year, team=college_team) %>% 
  group_by(year, team) %>% 
  summarise(
    picks = n()
  )

adj_rec_picks <- left_join(rec_ratings, adj_draft_picks) %>% 
  mutate(
    picks = ifelse(is.na(picks), 0, picks)
  ) %>% 
  group_by(team) %>% 
  mutate(
    last_picks = lag(picks, order_by = year),
    l2_picks = lag(last_picks, order_by = year)
  ) %>% 
  ungroup() %>% 
  select(-rank) %>% 
  mutate(
    tot_picks = last_picks + l2_picks,
    rec_points = as.numeric(points)
  ) %>% 
  select(team, year, rec_points, tot_picks) %>% 
  na.omit()

yoy_rec_rat <- rec_ratings %>% 
  mutate(
    rec_points = as.numeric(points)
  ) %>% 
  group_by(team) %>% 
  mutate(
    last_rec_points= lag(rec_points, order_by = year)
  ) %>% 
  filter(year >= 2012) %>% 
  na.omit() %>% 
  mutate(
    yoy_diff = rec_points - last_rec_points,
    diff_spread = sd(yoy_diff),
    mean_diff = mean(yoy_diff)
  ) %>% 
  filter(team %in% p5 | team %in% g5)

coaches <- cfbd_coaches()
clean_coaches <- coaches %>% 
  filter(year >= 2009) %>% 
  mutate(
    name = paste(first_name, last_name)
  ) %>% 
  select(name, school, year) %>% 
  group_by(school) %>% 
  mutate(
    last_coach = lag(name, order_by = year),
    rec_class = year + 1
  ) %>% 
  ungroup() %>% 
  mutate(
    new_coach = ifelse(name != last_coach, 1, 0)
  ) %>% 
  na.omit() %>% 
  group_by(school) %>% 
  mutate(
    change = ifelse(lead(new_coach, order_by = year) == 1, 1, 0)
  ) %>% 
  ungroup() 

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

coach_yoy <- left_join(yoy_rec_rat, clean_coaches %>% select(school, rec_class, name, new_coach, change) %>% rename('team'='school','year'='rec_class'))
nochange_yoy <- coach_yoy %>% filter(new_coach == 0 & change == 0)

p5_nc_yoy <- nochange_yoy %>% filter(team %in% p5)
p5_nbb_nc_yoy <- nochange_yoy %>% filter(team %in% p5 & !team %in% blue_bloods)
g5_nc_yoy <- nochange_yoy %>% filter(team %in% g5)

rsq_last_ncp5 <- round(cor(p5_nc_yoy$rec_points, p5_nc_yoy$last_rec_points)^2, 2)
rsq_last_ncp5nbb <- round(cor(p5_nbb_nc_yoy$rec_points, p5_nbb_nc_yoy$last_rec_points)^2, 2)
rsq_last_ncg5 <- round(cor(g5_nc_yoy$rec_points, g5_nc_yoy$last_rec_points)^2, 2)

ggplot(data = p5_nbb_nc_yoy)+
  geom_point(mapping = aes(x = last_rec_points, y = rec_points))+
  geom_smooth(mapping = aes(x = last_rec_points, y = rec_points), method = 'lm', se = FALSE)+
  geom_text(mapping = aes(x = 200, y = 275, label = paste0('R^2 = ', rsq_last_ncp5nbb)))+
  labs(
    x = 'Recruiting Class Points in Year N-1',
    y = 'Recruiting Class Points in Year N',
    title = 'How Stable are Recruiting Class Points Year Over Year Under the Same Staff?',
    subtitle = 'Non-Blue Blood P5 Programs Only, 2012-2020',
    caption = 'By Conor McQuiston | Data from @CFB_Data via @cfbfastR'
  )+
  theme_bw()+
  theme(
    plot.title = element_text(size = 16, hjust = 0.5),
    plot.subtitle = element_text(size = 14, hjust = 0.5),
    axis.title = element_text(size = 12)
  )

ggsave('p5_nbb_nc_yoy_rec_cor.png', height = 7, width = 13, path = '~/RecApp/Vignettes/')

ggplot(data = p5_nc_yoy)+
  geom_point(mapping = aes(x = last_rec_points, y = rec_points))+
  geom_smooth(mapping = aes(x = last_rec_points, y = rec_points), method = 'lm', se = FALSE)+
  geom_text(mapping = aes(x = 200, y = 275, label = paste0('R^2 = ', rsq_last_ncp5)))+
  labs(
    x = 'Recruiting Class Points in Year N-1',
    y = 'Recruiting Class Points in Year N',
    title = 'How Stable are Recruiting Class Points Year Over Year Under the Same Staff?',
    subtitle = 'P5 Programs Only, 2012-2020',
    caption = 'By Conor McQuiston | Data from @CFB_Data via @cfbfastR'
  )+
  theme_bw()+
  theme(
    plot.title = element_text(size = 16, hjust = 0.5),
    plot.subtitle = element_text(size = 14, hjust = 0.5),
    axis.title = element_text(size = 12)
  )

ggsave('p5_nc_yoy_rec_cor.png', height = 7, width = 13, path = '~/RecApp/Vignettes/')

ggplot(data = g5_nc_yoy)+
  geom_point(mapping = aes(x = last_rec_points, y = rec_points))+
  geom_smooth(mapping = aes(x = last_rec_points, y = rec_points), method = 'lm', se = FALSE)+
  geom_text(mapping = aes(x = 75, y = 150, label = paste0('R^2 = ', rsq_last_ncg5)))+
  labs(
    x = 'Recruiting Class Points in Year N-1',
    y = 'Recruiting Class Points in Year N',
    title = 'How Stable are Recruiting Class Points Year Over Year Under the Same Staff?',
    subtitle = 'G5 Programs Only, 2012-2020',
    caption = 'By Conor McQuiston | Data from @CFB_Data via @cfbfastR'
  )+
  theme_bw()+
  theme(
    plot.title = element_text(size = 16, hjust = 0.5),
    plot.subtitle = element_text(size = 14, hjust = 0.5),
    axis.title = element_text(size = 12)
  )

ggsave('g5_nc_yoy_rec_cor.png', height = 7, width = 13, path = '~/RecApp/Vignettes/')

p5_ncwp_yoy <- nochange_yoy %>% filter(team %in% p5) %>% inner_join(adj_rec_rats)
p5_nbb_ncwp_yoy <- nochange_yoy %>% filter(team %in% p5 & !team %in% blue_bloods) %>% inner_join(adj_rec_rats)
g5_ncwp_yoy <- nochange_yoy %>% filter(team %in% g5) %>% inner_join(adj_rec_rats)

rsq_last_ncwpp5 <- round(cor(p5_ncwp_yoy$rec_points, p5_ncwp_yoy$l3_wp)^2, 2)
rsq_last_ncwpp5nbb <- round(cor(p5_nbb_ncwp_yoy$rec_points, p5_nbb_ncwp_yoy$l3_wp)^2, 2)
rsq_last_ncwpg5 <- round(cor(g5_ncwp_yoy$rec_points, g5_ncwp_yoy$l3_wp)^2, 2)

ggplot(data = p5_nbb_ncwp_yoy)+
  geom_point(mapping = aes(x = l3_wp*100, y = rec_points))+
  geom_smooth(mapping = aes(x = l3_wp*100, y = rec_points), method = 'lm', se = FALSE)+
  geom_text(mapping = aes(x = 25, y = 250, label = paste0('R^2 = ', rsq_last_ncwpp5nbb)))+
  labs(
    x = 'Win Percentage Over 2 Prior Seasons',
    y = 'Recruiting Class Points in Current Season',
    title = 'How Much Impact Does On-Field Success Have On Recruiting Under the Same Coaching Staff?',
    subtitle = 'Non-Blue Blood P5 Programs Only, 2012-2020',
    caption = 'By Conor McQuiston | Data from @CFB_Data via @cfbfastR'
  )+
  theme_bw()+
  theme(
    plot.title = element_text(size = 16, hjust = 0.5),
    plot.subtitle = element_text(size = 14, hjust = 0.5),
    axis.title = element_text(size = 12)
  )

ggsave('p5_nbb_nc_wp_rec_cor.png', height = 7, width = 13, path = '~/RecApp/Vignettes/')

ggplot(data = p5_ncwp_yoy)+
  geom_point(mapping = aes(x = l3_wp*100, y = rec_points))+
  geom_smooth(mapping = aes(x = l3_wp*100, y = rec_points), method = 'lm', se = FALSE)+
  geom_text(mapping = aes(x = 25, y = 275, label = paste0('R^2 = ', rsq_last_ncwpp5)))+
  labs(
    x = 'Win Percentage Over 2 Prior Seasons',
    y = 'Recruiting Class Points in Current Season',
    title = 'How Much Impact Does On-Field Success Have On Recruiting Under the Same Coaching Staff?',
    subtitle = 'P5 Programs Only, 2012-2020',
    caption = 'By Conor McQuiston | Data from @CFB_Data via @cfbfastR'
  )+
  theme_bw()+
  theme(
    plot.title = element_text(size = 16, hjust = 0.5),
    plot.subtitle = element_text(size = 14, hjust = 0.5),
    axis.title = element_text(size = 12)
  )

ggsave('p5_nc_wp_rec_cor.png', height = 7, width = 13, path = '~/RecApp/Vignettes/')    

ggplot(data = g5_ncwp_yoy)+
  geom_point(mapping = aes(x = l3_wp*100, y = rec_points))+
  geom_smooth(mapping = aes(x = l3_wp*100, y = rec_points), method = 'lm', se = FALSE)+
  geom_text(mapping = aes(x = 75, y = 75, label = paste0('R^2 = ', rsq_last_ncwpg5)))+
  labs(
    x = 'Win Percentage Over 2 Prior Seasons',
    y = 'Recruiting Class Points in Year N',
    title = 'How Much Impact Does On-Field Success Have On Recruiting Under the Same Coaching Staff?',
    subtitle = 'G5 Programs Only, 2012-2020',
    caption = 'By Conor McQuiston | Data from @CFB_Data via @cfbfastR'
  )+
  theme_bw()+
  theme(
    plot.title = element_text(size = 16, hjust = 0.5),
    plot.subtitle = element_text(size = 14, hjust = 0.5),
    axis.title = element_text(size = 12)
  )

ggsave('g5_nc_wp_rec_cor.png', height = 7, width = 13, path = '~/RecApp/Vignettes/')

p5_ncdr_yoy <- nochange_yoy %>% filter(team %in% p5) %>% left_join(adj_rec_picks)
p5_ncdr_yoy[is.na(p5_ncdr_yoy)] <- 0
p5_nbb_ncdr_yoy <- nochange_yoy %>% filter(team %in% p5 & !team %in% blue_bloods) %>% left_join(adj_rec_picks)
p5_nbb_ncdr_yoy[is.na(p5_nbb_ncdr_yoy)] <- 0
g5_ncdr_yoy <- nochange_yoy %>% filter(team %in% g5) %>% left_join(adj_rec_picks)
g5_ncdr_yoy[is.na(g5_ncdr_yoy)] <- 0

rsq_last_ncdrp5 <- round(cor(p5_ncdr_yoy$rec_points, p5_ncdr_yoy$tot_picks)^2, 2)
rsq_last_ncdrp5nbb <- round(cor(p5_nbb_ncdr_yoy$rec_points, p5_nbb_ncdr_yoy$tot_picks)^2, 2)
rsq_last_ncdrg5 <- round(cor(g5_ncdr_yoy$rec_points, g5_ncdr_yoy$tot_picks)^2, 2)

ggplot(data = p5_nbb_ncdr_yoy)+
  geom_point(mapping = aes(x = tot_picks, y = rec_points))+
  geom_smooth(mapping = aes(x = tot_picks, y = rec_points), method = 'lm', se = FALSE)+
  geom_text(mapping = aes(x = 5, y = 275, label = paste0('R^2 = ', rsq_last_ncdrp5nbb)))+
  labs(
    x = 'Total Draft Picks in 2 Prior Seasons',
    y = 'Recruiting Class Points in Current Season',
    title = 'How Much Impact Does Getting Players Drafted Have On Recruiting Under the Same Coaching Staff?',
    subtitle = 'Non-Blue Blood P5 Programs Only, 2012-2020',
    caption = 'By Conor McQuiston | Data from @CFB_Data via @cfbfastR'
  )+
  theme_bw()+
  theme(
    plot.title = element_text(size = 16, hjust = 0.5),
    plot.subtitle = element_text(size = 14, hjust = 0.5),
    axis.title = element_text(size = 12)
  )

ggsave('p5_nbb_nc_dr_rec_cor.png', height = 7, width = 13, path = '~/RecApp/Vignettes/')

ggplot(data = p5_ncdr_yoy)+
  geom_point(mapping = aes(x = tot_picks, y = rec_points))+
  geom_smooth(mapping = aes(x = tot_picks, y = rec_points), method = 'lm', se = FALSE)+
  geom_text(mapping = aes(x = 17.5, y = 225, label = paste0('R^2 = ', rsq_last_ncdrp5)))+
  labs(
    x = 'Total Draft Picks in 2 Prior Seasons',
    y = 'Recruiting Class Points in Current Season',
    title = 'How Much Impact Does Getting Players Drafted Have On Recruiting Under the Same Coaching Staff?',
    subtitle = 'P5 Programs Only, 2012-2020',
    caption = 'By Conor McQuiston | Data from @CFB_Data via @cfbfastR'
  )+
  theme_bw()+
  theme(
    plot.title = element_text(size = 16, hjust = 0.5),
    plot.subtitle = element_text(size = 14, hjust = 0.5),
    axis.title = element_text(size = 12)
  )

ggsave('p5_nc_dr_rec_cor.png', height = 7, width = 13, path = '~/RecApp/Vignettes/')    

ggplot(data = g5_ncdr_yoy)+
  geom_point(mapping = aes(x = tot_picks, y = rec_points))+
  geom_smooth(mapping = aes(x = tot_picks, y = rec_points), method = 'lm', se = FALSE)+
  geom_text(mapping = aes(x = 7, y = 125, label = paste0('R^2 = ', rsq_last_ncdrg5)))+
  labs(
    x = 'Total Draft Picks in 2 Prior Years',
    y = 'Recruiting Class Points in Year N',
    title = 'How Much Impact Does On-Field Success Have On Recruiting Under the Same Coaching Staff?',
    subtitle = 'G5 Programs Only, 2012-2020',
    caption = 'By Conor McQuiston | Data from @CFB_Data via @cfbfastR'
  )+
  theme_bw()+
  theme(
    plot.title = element_text(size = 16, hjust = 0.5),
    plot.subtitle = element_text(size = 14, hjust = 0.5),
    axis.title = element_text(size = 12)
  )

ggsave('g5_nc_dr_rec_cor.png', height = 7, width = 13, path = '~/RecApp/Vignettes/')
