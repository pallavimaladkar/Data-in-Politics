# title: Final Project
# author: Pallavi Maladkar, Brennan Maynard, Mary Grace Martin
# date: December 9th, 2021

rm(list=ls())

library(ggplot2)
library(dplyr)

setwd("/Users/PallaviMaladkar/Documents/UNC/Classes/2021-2022/POLI 281")

vdem <- read.csv("vdem_selectedvars.csv")

# cleaning data
vdem <- vdem %>%
  select(v2x_corr, v2x_regime, e_peaveduc, e_migdppc, year) %>% # selecting variables
  na.omit %>% # take out NAs
  rename( # renaming all variables to be more accessible
    corr = v2x_corr,
    regime = v2x_regime,
    educ = e_peaveduc,
    GDP = e_migdppc
  ) %>%
  filter(year >= 2008) # filtering for most recent 10 years

# looking at all variables
summary(vdem$corr)
summary(vdem$regime)
summary(vdem$educ)
summary(vdem$GDP)
summary(vdem$year)

# setting limit that divides entries of higher/lower education
educ_median <- 7.918 # 4.366 before filtering for last 10 years

# creating higher and lower education datasets to use
vdem_lowereduc <- vdem %>%
  filter(educ <= educ_median)

vdem_highereduc <- vdem %>%
  filter(educ > educ_median)

# creating vector containing names of x-axis tick labels
regime_labels <- c("Closed Autocracy", 
                   "Electoral Autocracy", 
                   "Electoral Democracy", 
                   "Liberal Democracy")

# plot of corruption vs. regime for lower education
lowereduc_plot <- ggplot(vdem_lowereduc, aes(x = regime, y = corr, alpha = 0.01)) +
  geom_point() + 
  geom_jitter() + # to make density of points more visible
  geom_smooth(method = "lm") + # regression line
  labs(title = "Corruption vs. Regime Type, Lower Education", 
       x = "Regime", 
       y = "Corruption Level") + 
  scale_alpha(guide = 'none') + # remove useless legend
  ylim(0, 1) + # standardize the limits of the y-axis
  scale_x_continuous(guide = guide_axis(angle = 30), 
                     breaks = 0:3,
                     labels = paste0(regime_labels)) # rename x-axis tick labels

# plot of corruption vs. regime for higher education
highereduc_plot <- ggplot(vdem_highereduc, aes(x = regime, y = corr, alpha = 0.01)) +
  geom_point() + 
  geom_jitter() + 
  geom_smooth(method = "lm") +
  labs(title = "Corruption vs. Regime Type, Higher Education", 
       x = "Regime", 
       y = "Corruption Level") + 
  scale_alpha(guide = 'none') +
  ylim(0, 1) + 
  scale_x_continuous(guide = guide_axis(angle = 30), 
                     breaks = 0:3,
                     labels = paste0(regime_labels))

lowereduc_plot
highereduc_plot




# regressions for tables
reg <- lm(corr ~ regime + GDP, data = vdem) # all data
summary(reg)

reg_low <- lm(corr ~ regime + GDP, data = vdem_lowereduc) # regression for lower education
summary(reg_low)

reg_high <- lm(corr ~ regime + GDP, data = vdem_highereduc) # regression for higher education
summary(reg_high)






# editing the data so regime is factor variable
vdem$regime <- ifelse(vdem$regime == 0, "Closed Autocracy",
                      ifelse(vdem$regime == 1, "Electoral Autocracy",
                             ifelse(vdem$regime == 2, "Electoral Democracy",
                                    ifelse(vdem$regime == 3, "Liberal Democracy",
                                           NA))))

# recoding educ to be either Low/High
vdem$educ <- ifelse(vdem$educ <= educ_median, "Low",
                    ifelse(vdem$educ > educ_median, "High", 
                           NA))

# SINGLE BAR PLOT, no splitting based on education
vdem_singlebarplot <- vdem %>% # creating dataset for the single bar plot
  group_by(regime) %>% # grouping by regime
  summarize(mean = mean(corr, na.rm = TRUE)) # calculate mean of corruption by regime groups

singlebarplot <- ggplot(data = vdem_singlebarplot, aes(x = regime, y = mean)) +
  geom_bar(stat="identity") +
  labs(title = "Corruption Levels for Regimes", 
       x ="Regime", 
       y = "Mean Corruption Level") +
  scale_x_discrete(guide = guide_axis(angle = 30)) # angle the x-axis labels

singlebarplot



# DOUBLE BAR PLOT, splitting based on education
vdem_doublebarplot <- vdem %>% # creating dataset for the single bar plot
  group_by(regime, educ) %>% # group by both regime and education (8 types of categories)
  summarize(mean = mean(corr, na.rm = TRUE)) %>% # calculate mean of corruption by categories
  rename(Education = educ)

doublebarplot <- ggplot(data = vdem_doublebarplot, aes(x = regime, y = mean, fill = Education)) +
  geom_bar(stat="identity", position = position_dodge()) + # to make double columns
  labs(title = "Corruption Levels for Regimes", 
       x = "Regime", 
       y = "Mean Corruption Level") +
  scale_x_discrete(guide = guide_axis(angle = 30)) # angle the x-axis labels

doublebarplot






