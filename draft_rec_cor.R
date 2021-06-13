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

draft_years <- data.frame()
for(i in 2009:2020){
  draft_years<- bind_rows(draft_years, cfbd_draft_picks(i))
}

adj_draft_picks <- draft_years %>% select(year, team=college_team) %>% 
  group_by(year, team) %>% 
  summarise(
    picks = n()
  )

adj_rec_picks <- inner_join(rec_ratings, adj_draft_picks) %>% 
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

p5_adj_draft <- adj_rec_picks %>% filter((team %in% p5))
g5_adj_draft <- adj_rec_picks %>% filter(!(team %in% p5))

rsq_p5 <- round(cor(p5_adj_draft$tot_picks, p5_adj_draft$rec_points)^2, 2)
rsq_g5 <- round(cor(g5_adj_draft$tot_picks, g5_adj_draft$rec_points)^2, 2)

ggplot(data = p5_adj_draft)+
  geom_point(mapping = aes(x = tot_picks, y = rec_points))+
  geom_smooth(mapping = aes(x = tot_picks, y = rec_points), method = 'lm', se = FALSE)+
  geom_text(mapping = aes(x = 17.5, y = 200, label = paste0('R^2 = ', rsq_p5)))+
  labs(
    x = 'Total Draft Picks in 2 Prior Seasons',
    y = 'Recruiting Class Points Current Season',
    title = 'How Much Impact Does Getting Players Drafted Have On Recruiting?',
    subtitle = 'P5 Programs Only, 2011-2021',
    caption = 'By Conor McQuiston | Data from @CFB_Data via @cfbfastR'
  )+
  theme_bw()+
  theme(
    plot.title = element_text(size = 16, hjust = 0.5),
    plot.subtitle = element_text(size = 14, hjust = 0.5),
    axis.title = element_text(size = 12)
  )

ggsave('p5_draft_rec_cor.png', height = 7, width = 13, path = '~/RecApp/Vignettes/')

ggplot(data = g5_adj_draft)+
  geom_point(mapping = aes(x = tot_picks, y = rec_points))+
  geom_smooth(mapping = aes(x = tot_picks, y = rec_points), method = 'lm', se = FALSE)+
  geom_text(mapping = aes(x = 6, y = 75, label = paste0('R^2 = ', rsq_g5)))+
  labs(
    x = 'Total Draft Picks in 2 Prior Seasons',
    y = 'Recruiting Class Points Current Season',
    title = 'How Much Impact Does Getting Players Drafted Have On Recruiting?',
    subtitle = 'G5 Programs Only, 2011-2021',
    caption = 'By Conor McQuiston | Data from @CFB_Data via @cfbfastR'
  )+
  theme_bw()+
  theme(
    plot.title = element_text(size = 16, hjust = 0.5),
    plot.subtitle = element_text(size = 14, hjust = 0.5),
    axis.title = element_text(size = 12)
  )

ggsave('g5_draft_rec_cor.png', height = 7, width = 13, path = '~/RecApp/Vignettes/')
