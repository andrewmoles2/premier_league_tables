# premier league table historical
# scraped from wikipedia

# libraries ----
library(tidyverse)
library(rvest)
library(polite)
library(glue)
library(janitor)

# setup years and seasons ----
years <- seq(1992, 2024, 1)
years_sub <- substr(years + 1, 3, 4)

season <- glue("{years}-{years + 1}")

# vectors for setting up links ----
fa_1 <- 1:7
nfa_1 <- 8
fa_2 <- 9:15
nfa_2 <- 16:length(years)

# making the links ----
url_list <- c(
  glue("https://en.wikipedia.org/wiki/{years[fa_1]}%E2%80%93{years_sub[fa_1]}_FA_Premier_League"),
  glue("https://en.wikipedia.org/wiki/{years[nfa_1]}%E2%80%93{years_sub[nfa_1]}_Premier_League"),
  glue("https://en.wikipedia.org/wiki/{years[fa_2]}%E2%80%93{years_sub[fa_2]}_FA_Premier_League"),
  glue("https://en.wikipedia.org/wiki/{years[nfa_2]}%E2%80%93{years_sub[nfa_2]}_Premier_League")
)

# extracting just the league tables for each season ----
prem_table_list <- list()

for (i in seq_along(url_list)) {
  url_bow <- bow(url_list[i])
  
  all_tables <- scrape(url_bow) %>%
    html_elements("table.wikitable") %>%
    html_table(fill = TRUE)
  
  n_cols <- sapply(all_tables, ncol)
  prem_table <- all_tables[[which(n_cols == 11)[1]]]
  
  prem_table_list[[i]] <- prem_table
  
  Sys.sleep(1)
}

# adding the season to the data frames ----
prem_table_list_tidy <- c()

for (i in seq_along(prem_table_list)) {
  prem_table_list_tidy[[i]] <- tibble::add_column(
    prem_table_list[[i]], season = season[i]
  )
}

names(prem_table_list_tidy) <- season

# fix 19-20 col names 
table_cols <- colnames(prem_table_list_tidy[["2018-2019"]])
colnames(prem_table_list_tidy[["2019-2020"]]) <- table_cols

# tidy each data frame in list ----

for (s in seq_along(prem_table_list_tidy)) {
  prem_table_list_tidy[[s]] <- prem_table_list_tidy[[s]] |>
    clean_names() |>
    mutate(
      team = str_replace(team, "\\(.*", ""),
      qualification_or_relegation = str_replace(qualification_or_relegation,
                                                "\\[.*", ""),
      pts = str_replace(pts, "\\[.*", ""),
      pts = as.integer(pts)
    ) |>
    mutate(
      gd = gf - ga
    )
}

# combine to one data frame for ease of use and saving ----
all_prem_years <- data.table::rbindlist(prem_table_list_tidy)
# other options
#all_prem_years <- bind_rows(prem_table_list_tidy)
#all_prem_years <- do.call("rbind", prem_table_list_tidy)

# save combined dataset ----
write_csv(all_prem_years_clean, "data/premier_league_tables.csv")

# save data for each season ----
if (dir.exists("data/seasons") == FALSE) {
  print("No seasons directory, making it now....")
  dir.create("data/seasons")
} else {
  print("seaons directory already exists!")
}

for (save_file in names(prem_table_list_tidy)) {
  write_csv(prem_table_list_tidy[[save_file]],
            file = paste0("data/seasons/", save_file, ".csv"))
}


