library(tidyverse)
library(ggrepel)

prem_tables <- read_csv("data/premier_league_tables.csv")

# replicate - https://www.bbc.co.uk/sport/football/articles/ce3xqv0k6xgo

f <- "Segoe UI"

years <- seq(1992, 2024, 1)
years_sub <- as.integer(substr(years, 3, 4))

season_short <- paste0(years_sub, "-", years_sub + 1) 
season_short[18] <- paste0(0, years_sub[18],"-", years_sub[19])
season_short[8] <- paste0(years_sub[8], "-", 0 ,years_sub[9])
season_short[9:17] <- paste0(0,years_sub[9:17], "-", 0, years_sub[9:17] + 1)

winners <- prem_tables %>%
  filter(pos == 1) %>%
  mutate(
    plot_label = paste0(
      team,": ", season_short
    )
  )

club_colours <- c(
  "#978251", #CA2B1F
  "#5295EA",
  "#041384",
  "#F1BD41",
  "#E24C54",
  "#A1C4E6",
  "#C93530"
)
names(club_colours) <- sort(unique(winners$team))

winners %>%
  ggplot(aes(x = ga, y = gf)) +
  geom_text_repel(aes(label = str_wrap(plot_label, width = 5)),
                  max.overlaps = 20, size = 3.5,
                  family = f) +
  geom_point(aes(colour = team), size = 2.5) +
  annotate("text", x = 50, y = 110,  family = f,
           label = "Most goals scored\nMost goals conceded",
           vjust = "inward", hjust = "inward") +
  annotate("text", x = 10, y = 60, family = f,
           label = "Fewest goals scored\nFewest goals conceded",
           vjust = "inward", hjust = "inward") +
  scale_x_continuous(breaks = seq(10, 50, 5), limits = c(10, 50)) +
  scale_y_continuous(breaks = seq(60, 110, 5), limits = c(60, 110)) +
  scale_colour_manual(values = club_colours) +
  guides(colour = "none") +
  labs(x = "Goals conceded",
       y = "Goals scored",
       title = "Goal differences of Premier League title winners",
       subtitle = "Liverpool in 19/20 and Chelsea in 16/17 achieved\nthe same scored and conceded goals") +
  theme_minimal(base_family = f, base_size = 14) +
  theme(plot.title.position = "plot")

ggsave("figures/winners_gd.png", units = "px", dpi = 300, device = ragg::agg_png,
       width = 3250, height = 3000, bg = "white")



 