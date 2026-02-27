# championship and lower leagues table historical
# scraped from Wikipedia
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

# table links ----
# pre-champ links will work for lower leagues also
pre_champ <- 1:12
pre_champ1 <- 1:7
millen <- 8
pre_champ2 <- 9:12

foot_league_urls <- c(
  glue("https://en.wikipedia.org/wiki/{years[pre_champ1]}%E2%80%93{years_sub[pre_champ1]}_Football_League"),
  glue("https://en.wikipedia.org/wiki/{years[millen]}%E2%80%93{years[millen+1]}_Football_League"),
  glue("https://en.wikipedia.org/wiki/{years[pre_champ2]}%E2%80%93{years_sub[pre_champ2]}_Football_League")
)

# * championship ----
champ <- 13:24
efl_champ <- 25:length(years)

champ_url_list <- c(
  #glue("https://en.wikipedia.org/wiki/{years[pre_champ]}%E2%80%93{years_sub[pre_champ]}_Football_League"),
  glue("https://en.wikipedia.org/wiki/{years[champ]}%E2%80%93{years_sub[champ]}_Football_League_Championship"),
  glue("https://en.wikipedia.org/wiki/{years[efl_champ]}%E2%80%93{years_sub[efl_champ]}_EFL_Championship")
)

# * league 1 -----
l1 <- 13:24
efl_l1 <- 25:length(years)

l1_url_list <- c(
  #glue("https://en.wikipedia.org/wiki/{years[pre_champ]}%E2%80%93{years_sub[pre_champ]}_Football_League"),
  glue("https://en.wikipedia.org/wiki/{years[l1]}%E2%80%93{years_sub[l1]}_Football_League_One"),
  glue("https://en.wikipedia.org/wiki/{years[efl_l1]}%E2%80%93{years_sub[efl_l1]}_EFL_League_One")
)

# * league 2 -----
l2 <- 13:24
efl_l2 <- 25:length(years)

l2_url_list <- c(
  #glue("https://en.wikipedia.org/wiki/{years[pre_champ]}%E2%80%93{years_sub[pre_champ]}_Football_League"),
  glue("https://en.wikipedia.org/wiki/{years[l2]}%E2%80%93{years_sub[l2]}_Football_League_Two"),
  glue("https://en.wikipedia.org/wiki/{years[efl_l2]}%E2%80%93{years_sub[efl_l2]}_EFL_League_Two")
)

# extracting tables from links -----
# * pre-champ ----
champ_table_list1 <- list()
l1_table_list1 <- list()
l2_table_list1 <- list()

for (i in seq_along(foot_league_urls)) {
  url_bow <- bow(foot_league_urls[i])
  
  all_tables <- scrape(url_bow) %>%
    html_elements("table.wikitable") %>%
    html_table(fill = TRUE)
  
  n_cols <- sapply(all_tables, ncol)
  champ_table <- all_tables[[which(n_cols == 11)[1]]]
  l1_table <- all_tables[[which(n_cols == 11)[2]]]
  l2_table <- all_tables[[which(n_cols == 11)[3]]]
  
  champ_table_list1[[i]] <- champ_table
  l1_table_list1[[i]] <- l1_table
  l2_table_list1[[i]] <- l2_table
  
  Sys.sleep(1)
}

# * championship ----
champ_table_list2 <- list()
for (i in seq_along(champ_url_list)) {
  url_bow <- bow(champ_url_list[i])
  
  all_tables <- scrape(url_bow) %>%
    html_elements("table.wikitable") %>%
    html_table(fill = TRUE)
  
  n_cols <- sapply(all_tables, ncol)
  champ_table <- all_tables[[which(n_cols == 11)[1]]]
  
  champ_table_list2[[i]] <- champ_table
  
  Sys.sleep(1)
}
# append lists
champ_table_list <- c(champ_table_list1, champ_table_list2)

# * l1 ----
# 2019-2020 has 12 columns....
# add in if statement for that season to adjust...
l1_table_list2 <- list()
for (i in seq_along(l1_url_list)) {
  if (i == 16) {
    url_bow <- bow(l1_url_list[i])
    
    all_tables <- scrape(url_bow) %>%
      html_elements("table.wikitable") %>%
      html_table(fill = TRUE)
    
    n_cols <- sapply(all_tables, ncol)
    l1_table <- all_tables[[which(n_cols == 12)[1]]]
    
    l1_table_list2[[i]] <- l1_table
    
    Sys.sleep(1)
  } else {
    url_bow <- bow(l1_url_list[i])
    
    all_tables <- scrape(url_bow) %>%
      html_elements("table.wikitable") %>%
      html_table(fill = TRUE)
    
    n_cols <- sapply(all_tables, ncol)
    l1_table <- all_tables[[which(n_cols == 11)[1]]]
    
    l1_table_list2[[i]] <- l1_table
    
    Sys.sleep(1)
  }
}
# append lists
l1_table_list <- c(l1_table_list1, l1_table_list2)

# * l2 ----
l2_table_list2 <- list()
for (i in seq_along(l2_url_list)) {
  if (i == 16) {
    url_bow <- bow(l2_url_list[i])
    
    all_tables <- scrape(url_bow) %>%
      html_elements("table.wikitable") %>%
      html_table(fill = TRUE)
    
    n_cols <- sapply(all_tables, ncol)
    l2_table <- all_tables[[which(n_cols == 12)[1]]]
    
    l2_table_list2[[i]] <- l2_table
    
    Sys.sleep(1)
  } else {
    url_bow <- bow(l2_url_list[i])
    
    all_tables <- scrape(url_bow) %>%
      html_elements("table.wikitable") %>%
      html_table(fill = TRUE)
    
    n_cols <- sapply(all_tables, ncol)
    l2_table <- all_tables[[which(n_cols == 11)[1]]]
    
    l2_table_list2[[i]] <- l2_table
    
    Sys.sleep(1)
  }
}
# append lists
l2_table_list <- c(l2_table_list1, l2_table_list2)


# adding the season to the data frames ----
# * championship ----
for (i in seq_along(champ_table_list)) {
  champ_table_list[[i]] <- tibble::add_column(
    champ_table_list[[i]], season = season[i]
  )
}
names(champ_table_list) <- season
# fix 97-96 col names 
table_cols <- colnames(champ_table_list[["1996-1997"]])
colnames(champ_table_list[["1997-1998"]]) <- table_cols

# * l1 ----
# one season has added column which we will remove
n_cols <- sapply(l1_table_list, ncol)
l1_table_list[[which(n_cols == 12)]] <- l1_table_list[[which(n_cols == 12)]] |>
  select(-PPG)

for (i in seq_along(l1_table_list)) {
  l1_table_list[[i]] <- tibble::add_column(
    l1_table_list[[i]], season = season[i]
  )
}
names(l1_table_list) <- season

# * l2 ----
n_cols <- sapply(l2_table_list, ncol)
l2_table_list[[which(n_cols == 12)]] <- l2_table_list[[which(n_cols == 12)]] |>
  select(-PPG)

for (i in seq_along(l2_table_list)) {
  l2_table_list[[i]] <- tibble::add_column(
    l2_table_list[[i]], season = season[i]
  )
}
names(l2_table_list) <- season

# tidy each data frame ----
column_names <- janitor::make_clean_names(names(champ_table_list[["2024-2025"]]))

# * championship ----
for (s in seq_along(champ_table_list)) {
  champ_table_list[[s]] <- champ_table_list[[s]] |>
    rename_with(
      ~column_names,
      .cols = all_of(names(champ_table_list[[s]]))
    ) |>
    mutate(
      team = str_replace(team, "\\(.*", ""),
      team = str_replace(team, "\\[.*", ""),
      team = trimws(team),
      promotion_qualification_or_relegation = str_replace(promotion_qualification_or_relegation,
                                                "\\[.*", ""),
      pts = str_replace(pts, "\\[.*", ""),
      pts = as.integer(pts)
    ) |>
    mutate(
      gd = gf - ga
    )
}

# * league 1 ---- 
for (s in seq_along(l1_table_list)) {
  l1_table_list[[s]] <- l1_table_list[[s]] |>
    rename_with(
      ~column_names,
      .cols = all_of(names(l1_table_list[[s]]))
    ) |>
    mutate(
      team = str_replace(team, "\\(.*", ""),
      team = str_replace(team, "\\[.*", ""),
      team = trimws(team),
      promotion_qualification_or_relegation = str_replace(promotion_qualification_or_relegation,
                                                          "\\[.*", ""),
      pts = str_replace(pts, "\\[.*", ""),
      pts = as.integer(pts)
    ) |>
    mutate(
      gd = gf - ga
    )
}

# * league 2 ----
for (s in seq_along(l2_table_list)) {
  l2_table_list[[s]] <- l2_table_list[[s]] |>
    rename_with(
      ~column_names,
      .cols = all_of(names(l2_table_list[[s]]))
    ) |>
    mutate(
      team = str_replace(team, "\\(.*", ""),
      team = str_replace(team, "\\[.*", ""),
      team = trimws(team),
      promotion_qualification_or_relegation = str_replace(promotion_qualification_or_relegation,
                                                          "\\[.*", ""),
      pts = str_replace(pts, "\\[.*", ""),
      pts = as.integer(pts)
    ) |>
    mutate(
      gd = gf - ga
    )
}

# combine into one data frame ----
all_champ_years <- bind_rows(champ_table_list)
all_l1_years <- bind_rows(l1_table_list)
all_l2_years <- bind_rows(l2_table_list)

# save combined datasets ----
write_csv(all_champ_years, "data/championship_tables.csv")
write_csv(all_l1_years, "data/l1_tables.csv")
write_csv(all_l2_years, "data/l2_tables.csv")

# save data for each season ----
# * championship ----
if (dir.exists("data/champ_seasons") == FALSE) {
  print("No 'champ_seasons' directory, making it now....")
  dir.create("data/champ_seasons")
} else {
  print("'champ_seasons' directory already exists!")
}

for (save_file in names(champ_table_list)) {
  write_csv(champ_table_list[[save_file]],
            file = paste0("data/champ_seasons/", save_file, ".csv"))
}

# * l1 ----
if (dir.exists("data/l1_seasons") == FALSE) {
  print("No 'l1_seasons' directory, making it now....")
  dir.create("data/l1_seasons")
} else {
  print("'l1_seasons' directory already exists!")
}

for (save_file in names(l1_table_list)) {
  write_csv(l1_table_list[[save_file]],
            file = paste0("data/l1_seasons/", save_file, ".csv"))
}

# * l2 ----
if (dir.exists("data/l2_seasons") == FALSE) {
  print("No 'l2_seasons' directory, making it now....")
  dir.create("data/l2_seasons")
} else {
  print("'l2_seasons' directory already exists!")
}

for (save_file in names(l2_table_list)) {
  write_csv(l2_table_list[[save_file]],
            file = paste0("data/l2_seasons/", save_file, ".csv"))
}

