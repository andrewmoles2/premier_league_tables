library(tidyverse)
library(ggrepel)
library(ggtext)
library(glue)

prem_tables <- read_csv("data/premier_league_tables.csv")
champ_tables <- read_csv("data/championship_tables.csv")
l1_tables <- read_csv("data/l1_tables.csv")
l2_tables <- read_csv("data/l2_tables.csv")

f <- "Segoe UI"

years <- seq(1992, 2024, 1)
years_sub <- as.integer(substr(years, 3, 4))

# try and bind tables together
# need to make sure column names match
# need to add a column to state what league it is
prem_tables <- prem_tables |>
  rename("info" = "qualification_or_relegation") |>
  mutate(league = "prem",
         pos_league = paste0(pos, "-",league))
champ_tables <- champ_tables |>
  rename("info" = "promotion_qualification_or_relegation") |>
  mutate(league = "champ",
         pos_league = paste0(pos, "-",league))
l1_tables <- l1_tables |>
  rename("info" = "promotion_qualification_or_relegation") |>
  mutate(league = "l_one",
         pos_league = paste0(pos, "-",league))
l2_tables <- l2_tables |>
  rename("info" = "promotion_qualification_or_relegation") |>
  mutate(league = "l_two",
         pos_league = paste0(pos, "-",league))

all_tables <- bind_rows(
  list(
    prem_tables,
    champ_tables,
    l1_tables,
    l2_tables
  )
)

# setting up factors
all_levels <- c(
  "1-prem",  "2-prem",  "3-prem",  "4-prem",  
  "5-prem",  "6-prem",  "7-prem",  "8-prem",  
  "9-prem",  "10-prem", "11-prem", "12-prem", 
  "13-prem", "14-prem", "15-prem", "16-prem", 
  "17-prem", "18-prem", "19-prem", "20-prem", 
  "21-prem", "22-prem",
  "1-champ",  "2-champ",  "3-champ",  "4-champ",
  "5-champ",  "6-champ",  "7-champ",  "8-champ",
  "9-champ",  "10-champ", "11-champ", "12-champ",
  "13-champ", "14-champ", "15-champ", "16-champ",
  "17-champ", "18-champ", "19-champ", "20-champ",
  "21-champ", "22-champ", "23-champ", "24-champ",
  "1-l_one",  "2-l_one",  "3-l_one",  "4-l_one",
  "5-l_one",  "6-l_one",  "7-l_one",  "8-l_one",
  "9-l_one",  "10-l_one", "11-l_one", "12-l_one",
  "13-l_one", "14-l_one", "15-l_one", "16-l_one",
  "17-l_one", "18-l_one", "19-l_one", "20-l_one",
  "21-l_one", "22-l_one", "23-l_one", "24-l_one",
  "1-l_two",  "2-l_two",  "3-l_two",  "4-l_two",
  "5-l_two",  "6-l_two",  "7-l_two",  "8-l_two", 
  "9-l_two",  "10-l_two", "11-l_two", "12-l_two",
  "13-l_two", "14-l_two", "15-l_two", "16-l_two",
  "17-l_two", "18-l_two", "19-l_two", "20-l_two", 
  "21-l_two", "22-l_two", "23-l_two", "24-l_two"
)

all_tables <- all_tables |>
  mutate(pos_league = factor(pos_league, levels = all_levels))

# short season label
s1 <- substr(unique(all_tables$season),3,4)
s2 <- substr(unique(all_tables$season),8,9)
season_short <- paste0(s1,"-\n",s2)

all_tables |>
  filter(team == "Leicester City") |>
  ggplot(aes(x = season, y = pos)) +
  geom_point(aes(colour = league), size = 8) +
  geom_line(aes(group = 1)) +
  scale_x_discrete(labels = season_short) +
  scale_y_continuous(breaks = seq(1, 24, 1))

all_tables |>
  mutate(
    colouring = ifelse(team == "Leicester City", "black", "white"),
    sizing = ifelse(team == "Leicester City", 4, 0)
  ) |>
  ggplot(aes(x = season, y = pos_league, colour = colouring, size = sizing)) +
  geom_point() +
  geom_hline(aes(yintercept = 22.5),
             linetype = 2, colour = "grey70") +
  geom_hline(aes(yintercept = 46.5),
             linetype = 2, colour = "grey70") +
  geom_hline(aes(yintercept = 70.5),
             linetype = 2, colour = "grey70") +
  scale_colour_identity() +
  scale_size_identity() +
  scale_x_discrete(labels = season_short) +
  theme_minimal()

