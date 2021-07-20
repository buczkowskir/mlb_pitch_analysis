

# Loading Libraries
library(tidyverse)
library(scales)
library(ggrepel)

# Importing pitch data
pitches <- read_csv("C:/Users/ryanb/Desktop/baseball_data/pitches.csv")


# Looking at data
str(pitches)

hist(pitches$spin_rate)

pitches %>% 
  filter(code %in% c('D', 'E'))

pitches %>% 
  ggplot(aes(x = code, y = start_speed)) +
  geom_boxplot()

# Visualizing spin rate and break length
pitches %>% 
  filter(pitch_type %in% c('FF', 'CH', 'FC', 'FT', 'CU', 'SL')) %>% 
  mutate(pitch_type = case_when(pitch_type == 'CH' ~ 'Changeup',
                                pitch_type == 'FC' ~ 'Cutter',
                                pitch_type == 'FF' ~ 'Four-Seam Fastball',
                                pitch_type == 'FT' ~ 'Two-seam Fastball',
                                pitch_type == 'CU' ~ 'Curveball',
                                pitch_type == 'SL' ~ 'Slider')) %>% 
  sample_n(10000) %>% 
  ggplot(aes(x = spin_rate, y = break_length)) +
  geom_point(aes(color = pitch_type)) +
  facet_wrap(~pitch_type) +
  theme_minimal() +
  geom_smooth(method = 'lm') +
  scale_color_brewer(palette = 'Spectral', guide = FALSE) +
  labs(x        = 'Spin Rate',
       y        = 'Break Length (in)',
       title    = 'Pitch Spin Rate effect on Break Length',
       subtitle = 'By: Pitch-type | n = 10000') +
  theme(axis.text        = element_text(face  = 'bold.italic',
                                        size  = 11),
        axis.title       = element_text(face  = 'bold',
                                        size  = 14),
        strip.background = element_rect(color = 'black'),
        strip.text       = element_text(face  = 'bold.italic',
                                        size  = 11),
        plot.title       = element_text(face  = 'bold',
                                        size  = 16),
        plot.subtitle    = element_text(face  = 'italic',
                                        size  = 9)) -> plot1

# Visualizing average pitch speed and break length
pitches %>% 
  filter(pitch_type %in% c('FF', 'CH', 'FC', 'FT', 'CU', 'SL')) %>% 
  mutate(pitch_type = case_when(pitch_type == 'CH' ~ 'Changeup',
                                pitch_type == 'FC' ~ 'Cutter',
                                pitch_type == 'FF' ~ 'Four-Seam Fastball',
                                pitch_type == 'FT' ~ 'Two-seam Fastball',
                                pitch_type == 'CU' ~ 'Curveball',
                                pitch_type == 'SL' ~ 'Slider'),
         avg_speed  = (start_speed + end_speed) / 2) %>% 
  sample_n(10000) %>% 
  ggplot(aes(x = avg_speed, y = break_length)) +
  geom_point(aes(color = pitch_type)) +
  facet_wrap(~pitch_type) +
  theme_minimal() +
  geom_smooth(method = 'lm') +
  scale_color_brewer(palette = 'Spectral', guide = FALSE) +
  labs(x        = 'Pitch Speed (mph)',
       y        = 'Break Length (in)',
       title    = 'Pitch Speed effect on Break Length',
       subtitle = 'By: Pitch-type | n = 10000') +
  theme(axis.text        = element_text(face  = 'bold.italic',
                                        size  = 11),
        axis.title       = element_text(face  = 'bold',
                                        size  = 14),
        strip.background = element_rect(color = 'black'),
        strip.text       = element_text(face  = 'bold.italic',
                                        size  = 11),
        plot.title       = element_text(face  = 'bold',
                                        size  = 16),
        plot.subtitle    = element_text(face  = 'italic',
                                        size  = 9)) -> plot2

# Visualizing average pitch speed and spin rate
pitches %>% 
  filter(pitch_type %in% c('FF', 'CH', 'FC', 'FT', 'CU', 'SL')) %>% 
  mutate(pitch_type = case_when(pitch_type == 'CH' ~ 'Changeup',
                                pitch_type == 'FC' ~ 'Cutter',
                                pitch_type == 'FF' ~ 'Four-Seam Fastball',
                                pitch_type == 'FT' ~ 'Two-seam Fastball',
                                pitch_type == 'CU' ~ 'Curveball',
                                pitch_type == 'SL' ~ 'Slider'),
         avg_speed  = (start_speed + end_speed) / 2) %>% 
  sample_n(10000) %>% 
  ggplot(aes(x = avg_speed, y = spin_rate)) +
  geom_point(aes(color = pitch_type)) +
  facet_wrap(~pitch_type) +
  theme_minimal() +
  geom_smooth(method = 'lm') +
  scale_color_brewer(palette = 'Spectral', guide = FALSE) +
  labs(x        = 'Pitch Speed (mph)',
       y        = 'Spin Rate',
       title    = 'Pitch Speed effect on Spin Rate',
       subtitle = 'By: Pitch-type | n = 10000') +
  theme(axis.text        = element_text(face  = 'bold.italic',
                                        size  = 11),
        axis.title       = element_text(face  = 'bold',
                                        size  = 14),
        strip.background = element_rect(color = 'black'),
        strip.text       = element_text(face  = 'bold.italic',
                                        size  = 11),
        plot.title       = element_text(face  = 'bold',
                                        size  = 16),
        plot.subtitle    = element_text(face  = 'italic',
                                        size  = 9)) -> plot3

# Looking at break down of pitch type by runners on bases

# Creating variable for runners on
pitches %>% 
  mutate(on_1b_2b     = on_1b + on_2b,
         on_1b_3b     = on_1b + on_3b,
         on_2b_3b     = on_2b + on_3b,
         bases_loaded = on_1b + on_2b + on_3b,
         runners      = case_when(bases_loaded == 3 ~ 'Bases Loaded',
                                  on_1b_2b     == 2 ~ 'First and Second',
                                  on_1b_3b     == 2 ~ 'First and Third',
                                  on_2b_3b     == 2 ~ 'Second and Third',
                                  on_1b        == 1 ~ 'First',
                                  on_2b        == 1 ~ 'Second',
                                  on_3b        == 1 ~ 'Third',
                                  TRUE ~ 'No Runners')
         
         ) -> pitches_with_runners

# Number of pitches thrown given runners on base
pitches_with_runners %>% 
  group_by(runners) %>% 
  count() %>% 
  filter(runners != 'No Runners') %>% 
  ggplot(aes(x = reorder(runners, n), y = n)) +
  geom_col(fill = 'royalblue3', color = 'black') +
  theme_minimal() +
  labs(x        = 'Runner Position',
       y        = 'Number of Pitches Thrown',
       title    = 'Number of Pitches thrown given runner position',
       subtitle = 'All pitches thrown from 2015-2018',
       caption  = 'Visualization creating using ggplot2 in RStudio \nCreator: Ryan Buczkowski \nSource: https://www.kaggle.com/pschale/mlb-pitch-data-20152018') +
  scale_y_continuous(labels = comma) +
  theme(axis.text.x      = element_text(face  = 'bold.italic',
                                        size  = 11,
                                        angle = 45,
                                        hjust = 1),
        axis.text.y      = element_text(face  = 'bold.italic',
                                        size  = 11),
        axis.title       = element_text(face  = 'bold',
                                        size  = 14),
        strip.background = element_rect(color = 'black'),
        strip.text       = element_text(face  = 'bold.italic',
                                        size  = 11),
        plot.title       = element_text(face  = 'bold',
                                        size  = 16),
        plot.subtitle    = element_text(face  = 'italic',
                                        size  = 9),
        plot.caption     = element_text(face  = 'italic',
                                        size  = 9,
                                        hjust = 0)) -> plot4

# Pitch totals with runners on base
pitches_with_runners %>% 
  group_by(runners) %>% 
  count(name = 'total_pitches') -> pitch_total_ob

# Creating a summarized data frame for percentage of pitches thrown by pitch type
pitches_with_runners %>% 
  group_by(pitch_type, runners) %>% 
  count() %>% 
  filter(n > 1000) %>% 
  left_join(pitch_total_ob) %>% 
  mutate(pct_pitch_thrown = round(((n / total_pitches)), 2)) %>% 
  ungroup() -> ob_pitches_pct

ob_pitches_pct %>% 
  select(pitch_type) %>% 
  distinct()

# Visualizing Pitch break down with runners on base
ob_pitches_pct %>% 
  na.omit() %>% 
  filter(pct_pitch_thrown > 0.06) %>% 
  ggplot(aes(x = reorder(pitch_type, pct_pitch_thrown), y = pct_pitch_thrown)) +
  geom_hline(yintercept = c(0.1,0.2,0.3), color = 'gray', linetype = 'dotted', size = 1) +
  geom_col(aes(fill = pitch_type), color = 'black') +
  facet_wrap(~runners, nrow = 2) +
  theme_minimal() +
  scale_y_continuous(labels = percent) +
  geom_label(aes(label = pct_pitch_thrown), vjust = 1.5, fill = 'black', color = 'white', fontface = 'bold.italic') +
  labs(x = '',
       y = 'Pitch Thrown (%)',
       title = 'Breakdown of Pitches Thrown with Runners on',
       subtitle = 'Pitches thrown from 2015-2018',
       caption  = 'Visualization creating using ggplot2 in RStudio \nCreator: Ryan Buczkowski \nSource: https://www.kaggle.com/pschale/mlb-pitch-data-20152018') +
  scale_fill_brewer(labels  = c('Changeup', 'Curveball', 'Four-seam Fastball',
                                'Two-seam Fastball', 'Sinker', 'Slider'),
                    palette = 'PuOr',
                    name    = 'Pitch Type') +
  theme(axis.text.x       = element_blank(),
        axis.text.y       = element_text(face  = 'bold.italic',
                                         size  = 11),
        axis.title        = element_text(face  = 'bold',
                                         size  = 14),
        strip.background  = element_rect(color = 'black',
                                         fill  = 'black'),
        strip.text        = element_text(face  = 'bold.italic',
                                         size  = 11,
                                         color = 'white'),
        plot.title        = element_text(face  = 'bold',
                                         size  = 16),
        plot.subtitle     = element_text(face  = 'italic',
                                         size  = 9),
        plot.caption      = element_text(face  = 'italic',
                                         size  = 9,
                                         hjust = 0),
        legend.position   = 'bottom',
        legend.background = element_rect(color = 'black',
                                         size  = 1),
        legend.text       = element_text(face  = 'bold.italic',
                                         size  = 11),
        legend.title      = element_text(face  = 'bold',
                                         size  = 12),
        panel.background = element_rect(color = 'black',
                                        size  = 1),
        panel.grid = element_blank()) -> plot5

ggsave(plot1, filename = 'plot1.jpg')
ggsave(plot2, filename = 'plot2.jpg')
ggsave(plot3, filename = 'plot3.jpg')
ggsave(plot4, filename = 'plot4.jpg')
ggsave(plot5, filename = 'plot5.jpg')  
