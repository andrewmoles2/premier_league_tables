library(tidyverse)
library(ggrepel)
library(ggtext)
library(glue)

prem_tables <- read_csv("data/premier_league_tables.csv")

# title winners goal diff ----
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

# points per position ----

fancy_prem_pos_plot <- function(df, positions = c(1, 20), 
                                pal = c("#35a35c","#4c3596","#f7e21f",
                                        "#469E95","#f74734","#95cb62",
                                        "#8d56a5")) {
  
  # short version of season for plotting
  s1 <- substr(unique(df$season),3,4)
  s2 <- substr(unique(df$season),8,9)
  season_short <- paste0(s1,"-\n",s2)
  
  # filtering set-up (using tidyverse approach as demo as its more technical due to {{}})
  league_filt <- positions
  season_pts <- df |>
    filter(pos %in% {{ league_filt }}) |>
    mutate(pos = factor(pos))
  
  # setting up the title
  pos <- unique(season_pts$pos)
  
  if (length(pos) <= 2) {
    t <- glue("Premier League points per season for teams in positions 
     <span style = 'color:{pal[1]};'>{pos[1]}</span> and <span style = 'color:{pal[2]};'>{pos[2]}</span>")
  } else if (length(pos) > 2 & length(pos) <= 3) {
    t <- glue("Premier League points per season for teams in positions 
     <span style = 'color:{pal[1]};'>{pos[1]}</span>, <span style = 'color:{pal[2]};'>{pos[2]}</span>
    , and <span style = 'color:{pal[3]};'>{pos[3]}</span>")
  } else if (length(pos) > 3 & length(pos) <= 4) {
    t <- glue("Premier League points per season for teams in positions 
     <span style = 'color:{pal[1]};'>{pos[1]}</span>, <span style = 'color:{pal[2]};'>{pos[2]}</span>
    , <span style = 'color:{pal[3]};'>{pos[3]}</span>, and <span style = 'color:{pal[4]};'>{pos[4]}</span>")
  } else if (length(pos) > 4 & length(pos) <= 5) {
    t <- glue("Premier League points per season for teams in positions 
     <span style = 'color:{pal[1]};'>{pos[1]}</span>, <span style = 'color:{pal[2]};'>{pos[2]}</span>
    , <span style = 'color:{pal[3]};'>{pos[3]}</span>, <span style = 'color:{pal[4]};'>{pos[4]}</span>
    , and <span style = 'color:{pal[5]};'>{pos[5]}</span>")
  } else if (length(pos) > 5 & length(pos) <= 6) {
    t <- glue("Premier League points per season for teams in positions 
     <span style = 'color:{pal[1]};'>{pos[1]}</span>, <span style = 'color:{pal[2]};'>{pos[2]}</span>
    , <span style = 'color:{pal[3]};'>{pos[3]}</span>, <span style = 'color:{pal[4]};'>{pos[4]}</span>
    , <span style = 'color:{pal[5]};'>{pos[5]}</span>, and <span style = 'color:{pal[6]};'>{pos[6]}</span>")
  } else if (length(pos) > 6 & length(pos) <= 7) {
    t <- glue("Premier League points per season for teams in positions 
     <span style = 'color:{pal[1]};'>{pos[1]}</span>, <span style = 'color:{pal[2]};'>{pos[2]}</span>
    , <span style = 'color:{pal[3]};'>{pos[3]}</span>, <span style = 'color:{pal[4]};'>{pos[4]}</span>
    , <span style = 'color:{pal[5]};'>{pos[5]}</span>, <span style = 'color:{pal[6]};'>{pos[6]}</span>
    , and <span style = 'color:{pal[7]};'>{pos[7]}</span>")
  }
  
  p <- season_pts |>
    ggplot(aes(x = season, y = pts, 
               group = pos, colour = pos)) +
    geom_line(linewidth = 2) +
    geom_point(size = 4) +
    scale_y_continuous(limits = c(0, 100),
                       breaks = seq(0, 100, 10)) +
    scale_x_discrete(labels = season_short) +
    scale_colour_manual(values = pal) +
    guides(colour = "none") +
    labs(title = t, 
         colour = "League position",
         x = "", y = "") +
    theme_minimal(base_family = "Segoe UI") +
    theme(plot.title.position = "plot",
          plot.title = ggtext::element_markdown(size = 16, lineheight = 1.2),
          legend.position = "bottom")
  
  return(p)
}

fancy_prem_pos_plot(prem_tables, positions = c(1, 2, 5, 17, 18, 20))

# points per position v2 ----
pts_by_pos <- prem_tables |>
  group_by(pos) |>
  summarise(
    avg_points = mean(pts),
    sd_points = sd(pts)
  )

pts_by_pos |>
  ggplot(aes(x = pos, y = avg_points)) +
  geom_point() +
  geom_errorbar(aes(ymin=avg_points-sd_points, ymax=avg_points+sd_points), width=0.2) +
  scale_y_continuous(breaks = seq(0, 100, 10), limits = c(0, 100)) +
  scale_x_continuous(breaks = seq(1, 22, 1))

# Excluding years with 22 teams. Seemed best to exclude to be more accurate for current format. 
pts_by_pos <- prem_tables |>
  filter(pld < 42) |>
  group_by(pos) |>
  summarise(
    avg_points = mean(pts),
    sd_points = sd(pts)
  ) |>
  mutate(
    colour = case_when(
      pos == 1 ~ "#917200",
      pos %in% c(18, 19, 20) ~ "#8F1100",
      .default = "#001F8F"
    )
  )

cols <- unique(pts_by_pos$colour)

t <- glue("How many points are needed to <span style = 'color:{cols[1]};'>win</span>, 
           <span style = 'color:{cols[2]};'>stay up</span>, or <span style = 'color:{cols[3]};'>go down</span>
          in the Premier League?")

pts_by_pos |>
  ggplot(aes(x = pos, y = avg_points, colour = colour)) +
  geom_jitter(data = subset(prem_tables, pld == 38),
              aes(x = pos, y = pts),
              width = 0.2, height = 0.15,
              colour = "grey70") +
  geom_pointrange(aes(ymin=avg_points-sd_points, ymax=avg_points+sd_points),
                  linewidth = 2, size = 2,
                  lineend = "round") +
  geom_text(aes(label = round(avg_points, 1)),
            colour = "grey99", size = 3,
            family = "Segoe UI") +
  geom_hline(yintercept = 40, linetype = 2) +
  annotate(geom = "text", x = 4, y = 42, 
           label = "Guaranteed Premier League safety?",
           family = "Segoe UI") +
  scale_colour_identity() +
  scale_y_continuous(breaks = seq(0, 100, 5)) +
  scale_x_continuous(breaks = seq(1, 20, 1)) +
  labs(title = t,
    #title = "How many points do you need to win or stay up in the Premier League?",
       subtitle = "Includes only years with 20 teams (1995-2025)",
       caption = "Data: Wikipedia  |  Visual: Andrew Moles",
       y = "Average points per position",
       x = "League position") +
  theme_minimal(base_family = "Segoe UI",
                base_size = 14) +
  theme(plot.title.position = "plot",
        plot.title = ggtext::element_markdown(size = 18, lineheight = 1.2))


