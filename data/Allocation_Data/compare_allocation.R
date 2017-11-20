setwd('~/Dropbox/2017-traits/Allocation_Data/')

rm(list = ls())

library(dplyr)
library(tidyr)
library(ggplot2)

my_cols <- c('#a6cee3','#1f78b4','#b2df8a','#33a02c')
  
ids <- read.csv('allocation_early_id_2017-02-24.csv')

r1 <- read.csv('allocation_early_2017-03-01.csv')
r2 <- read.csv('allocation_mid.csv')
r3 <- read.csv('allocation_late.csv')
r3_id <- read.csv('allocation_late_size.csv')

r1 <- r1 %>% left_join(ids, by = c('round', 'plant_id') )
r3 <- left_join(r3, r3_id, by = c('round', 'species', 'plant_id'))

df <- bind_rows(r1, r2, r3)

df$sample_date <- factor(df$round, labels = c('early', 'mid', 'late'))

df$species <- factor( df$species, labels = c('Agoseris', 'Euphorbia', 'Hemizonia', 'Lasthenia', 'Lotus', 'Navarretia', 'Plantago' , 'Salvia'))

df <- 
  df %>% 
  spread( tissue, weight ) %>% 
  mutate( f = ifelse(is.na(f), 0, f ),
          s = ifelse(is.na(s), 0, s), 
          r = ifelse(is.na(r), 0, r), 
          `tap root` = ifelse(is.na(`tap root`), 0, `tap root`), 
          `small root` = ifelse(is.na(`small root`), 0, `small root`),           
          r = ifelse( r == 0, `tap root` + `small root`,  r), 
          total_mass = f + l + s + r, 
          AGB = f + l + s, 
          f_frac = f/total_mass, 
          l_frac = l/total_mass, 
          r_frac = r/total_mass, 
          s_frac = s/total_mass, 
          AGBfrac = AGB/total_mass) %>% 
  filter( l_frac > 0 , r_frac > 0 ) 

df1 <- 
  df %>% 
  gather( tissue, fraction, l_frac, r_frac, s_frac, f_frac) %>% 
  mutate( fraction = ifelse(fraction == 0, NA, fraction))

df1$tissue <- factor( df1$tissue , labels = c('flower/fruit', 'leaf', 'root', 'stem'))


p1 <- ggplot( df1, aes( x = total_mass, y = fraction, shape = sample_date, color = tissue, group = tissue) ) + 
  geom_point() + 
  geom_smooth(method = 'lm', se = F,formula =  y ~ log(x)) + 
  facet_wrap(~species) + 
  scale_color_manual(values = my_cols) + 
  ylab('fraction of total biomass')  + 
  theme_bw() + theme(panel.grid = element_blank())

p2 <- ggplot( df1, aes( x = log(total_mass), y = fraction, shape = sample_date, color = tissue, group = tissue) ) + 
  geom_point() + 
  geom_smooth(method = 'lm', se = F) + 
  ylab('fraction of total biomass') + 
  facet_wrap(~species) + 
  scale_color_manual(values = my_cols) + 
  theme_bw() + theme(panel.grid = element_blank())

df2 <- 
  df %>% 
  mutate( `leaf to root ratio` = l/r)

p3 <- ggplot( df2, aes( x = log(total_mass), y = `leaf to root ratio`, color = sample_date, group = sample_date) ) + 
  geom_point() + 
  geom_smooth(method = 'lm', se = F) + 
  facet_wrap(~species) + 
  scale_color_manual(values = my_cols) + 
  theme_bw() + theme(panel.grid = element_blank())

models <- df1 %>% group_by(species, round) %>% do( slope = coefficients(lm(data = . , formula = AGB ~ - 1 + total_mass)))
names( models$slope) <- paste(models$species, models$round)

cbind( models$slope )

m1 <- lm(data = df1, AGB ~ - 1 + total_mass)

m1$coefficients

ggplot(df1, aes( x = total_mass, y = AGB, group = round, color = factor(round))) + 
  geom_point() + 
  facet_wrap(~species) + 
  theme_bw() + 
  theme(panel.grid = element_blank()) + 
  geom_smooth(se = F, formula = 'y ~ - 1 +   x ', method = 'lm') + 
  geom_abline(intercept = 0, slope = 1, linetype = 2) + 
  scale_x_continuous(trans = 'log' ) + 
  scale_y_continuous(trans = 'log')



pdf( file = 'temp/allocation_fig.pdf' )
print(p2)
print(p3)
dev.off()

