library(httr)
library(jsonlite)
library(dplyr)
library(readr)
library(lubridate)
library(rvest)
library(dplyr)
library(tidyverse)
library(fuzzyjoin)
library(zoo)
library(blscrapeR)

setwd("/CinemaData/data/")

movies <- data.frame()
filenames <- list.files(pattern="^tmdb\\.omdb\\.combined\\..*\\.csv")

# ### Step 1 - Combine files
# Load all tmdb.omdb yearly files and bind into 1 dataframe.  there were problems
# with empty files and omdb.title missing sometimes so below figures all that out.
for (i in 1:length(filenames)) {
  movies_temp <- read.csv(filenames[i], colClasses =c("character"))
  
  # Some years had 22 variables, most had 23, so this adds the missing 1 where needed
  if (i==1) {
    names_list <- names(movies_temp)
  }
  # 1980s are empty sometimes so exclude where no data
  if (nrow(movies_temp) > 0) {
  Missing <- setdiff(names_list, names(movies_temp))  # Find names of missing columns
  movies_temp[Missing] <- ""
  
  # Add to movies dataframe
  movies <- rbind(movies, movies_temp)
  }
}

movies$tmdb.id <- as.integer(movies$tmdb.id)

# #### Step 2 - Collections ######
# # Build collections dataframe, to work out sequels of movies
base_uri <- "https://api.themoviedb.org/3/collection/"
api_key <- "api_key=<Insert API Key Here>"
keep_columns <- c("collection_id", "collection_name", "id", "release_date", "title")

collections <- data.frame()
# Get unique list of collection ids
collection_list <- unique(na.omit(movies$tmdb.collection.id))

# For each collection, find the movie details
for (i in 1:length(collection_list)) {
  
  piece_by_piece_url <- paste(base_uri, collection_list[i], "?", api_key, sep="")
  
  r1 <- GET(url = piece_by_piece_url)
  r2 <- content(r1, as = "text", encoding = "UTF-8")
  # Call the API using the url. TMDb only uses JSON, so use fromJSON to convert the data into R objects and use data.frame to convert the objects into a data frame 
  # temp <- data.frame(fromJSON(r2)) 
  coll_df <- fromJSON(r2)
  temp <- coll_df$parts
  temp$release_date <- ymd(temp$release_date)
  temp$collection_id <- coll_df$id
  temp$collection_name <- coll_df$name
  temp <- temp[ , (names(temp) %in% keep_columns)]
  
  collections <- rbind(collections, temp)
  
}

# Based on release date in the collection assign a sequel number to each movie
# so 1st movie is 1, then 2, etc..
collections <- collections %>% group_by(collection_id) %>%
  # arrange in right order and then do row number over group - collection id
  arrange(release_date, id) %>%
  mutate(collection.sequel.number = row_number()) %>%
  ungroup()

write.csv(collections, "collections.csv", row.names = FALSE)

# ### Step 3 - Genres to binary columns
# ### deconstruct Genres into separate binary columns
movies$tmdb.id <- as.integer(movies$tmdb.id)
movies$tmdb.genre <- gsub ("c(", "", movies$tmdb.genre, fixed = TRUE)
movies$tmdb.genre <- gsub(")", "", movies$tmdb.genre, fixed = TRUE)
movies$tmdb.genre <- gsub(")", "", movies$tmdb.genre, fixed = TRUE)
# 4 movies had a : instead of , between genres
movies$tmdb.genre <- gsub(":", ",", movies$tmdb.genre, fixed = TRUE)

genre <- read.csv("genres.csv", stringsAsFactors = FALSE)

# Separate genre list to rows, then join to Genre data frame
# remove genre.ids, set dummy column to 1 and then spread names
# back in, using the dummy value and setting nulls to 0..
movies_comb <- movies %>% 
  mutate(tmdb.genre.sep = tmdb.genre) %>% 
  separate_rows(tmdb.genre.sep, sep=',') %>% 
  mutate(tmdb.genre.sep = as.numeric(tmdb.genre.sep)) %>%
  left_join(genre, by=c("tmdb.genre.sep" =  "genres.id")) %>% 
  modify_at(c("tmdb.genre.sep"),~NULL) %>% 
  mutate(genres.value = 1) %>% 
  distinct() %>%
  spread(genres.name, value=genres.value, fill=0)

# get rid of 1st column which is row.names
movies_comb$X <- NULL

# write out data frame, then read back in to convert to ints, numbers, etc..
write.csv(movies_comb, "tmdb.omdb.combined.csv", row.names = FALSE)

####  Step 4 - Fuzzy Join #####
movies_comb <- read.csv("tmdb.omdb.combined.csv", stringsAsFactors = FALSE)
movies_comb$release.date <- ymd(movies_comb$release.date)

#### Read Weekly/weekend #######
weekend <- read.csv("Weekend.csv", stringsAsFactors = FALSE)
Weekly <- read.csv("Weekly.csv", stringsAsFactors = FALSE)
weekend$date <- ymd(weekend$date)
Weekly$date <- ymd(Weekly$date)
Weekly$movie_year <- as.numeric(Weekly$movie_year)

time_dimension <- read.csv("time_dimension.csv", stringsAsFactors = FALSE)
time_dimension$calendar_date <- ymd(time_dimension$calendar_date)
collections  <- read.csv("collections.csv", stringsAsFactors = FALSE)

# some titles were too hard to match so a manual match was done on the
# bigger movies and put to a file..
extra_movie_map <- read.csv("extra_movie_map.csv", stringsAsFactors = FALSE)

# Put the start of week date, event name at any point in week, whether week
# had a holiday and any event type in the week against each record
week.time_dimension <- time_dimension %>% 
  group_by(week_num, week_num_year) %>%
  mutate(weekly.date = min(calendar_date),
         weekly.event_name = max(event_name, na.rm=TRUE),
         weekly.holiday_ind = max(holiday_ind),
         weekly.event_type = max(event_type, na.rm=TRUE))

# Taking weekly, make a join name that strips out punctuation, spaces and
# makes everything lower case.  It also converts & to and, 2 to two
# and specific cleanup for stat wars..
Weekly_fuzzy <- Weekly %>% filter(!is.na(movie_year)) %>%
  group_by(movie_name, movie_year) %>%
  mutate(movie_name.join =  str_replace_all(tolower(gsub("\\bEp\\.", "episode", gsub("Star Wars Ep\\. VIII\\b|Star Wars Ep\\. VII\\b", "Star Wars", 
                                                                                     gsub("\\b2\\b", "two", 
                                                                                          gsub("&", "and", movie_name))))), "[^[:alnum:]]", ""),
         release_year = min(year(date))) %>%
  ungroup()

# Do a similar join name for the combined movie details
movies_fuzzy <- movies_comb %>% 
  mutate(tmdb.title.join = str_replace_all(tolower(gsub("\\bEp\\.", "episode", gsub("Star Wars Ep\\. VIII\\b|Star Wars Ep\\. VII\\b", "Star Wars", 
                                                                                    gsub("\\b2\\b", "two", 
                                                                                         gsub("&", "and", 
                                                                                              tmdb.title))))), "[^[:alnum:]]", ""),
         omdb.title.join = str_replace_all(tolower(gsub("\\bEp\\.", "episode", gsub("Star Wars Ep\\. VIII\\b|Star Wars Ep\\. VII\\b", "Star Wars", 
                                                                                    gsub("\\b2\\b", "two", 
                                                                                         gsub("&", "and", 
                                                                                              omdb.title))))), "[^[:alnum:]]", ""),
         release.year = year(release.date)) %>%
  select (tmdb.title.join, omdb.title.join, release.year, tmdb.id)

# join the weekly numbers to th movies details on:
# - name from tmdb and release year
# - name from omdb and release year
# - name from tmdb and year of first week from weekly data
# - name from omdb and year of first week from weekly data
Weekly_fuzzy.1 <- Weekly_fuzzy %>% 
  left_join(movies_fuzzy %>% select(tmdb.title.join, release.year, tmdb.id) %>%
              distinct(), 
            c("movie_name.join" = "tmdb.title.join", "release_year" = "release.year")) %>%
  left_join(movies_fuzzy %>% select(omdb.title.join, release.year, tmdb.id) %>%
              distinct(), 
            c("movie_name.join" = "omdb.title.join", "release_year" = "release.year")) %>%
  left_join(movies_fuzzy %>% select(tmdb.title.join, release.year, tmdb.id) %>%
              distinct(), 
            c("movie_name.join" = "tmdb.title.join", "movie_year" = "release.year")) %>%
  left_join(movies_fuzzy %>% select(omdb.title.join, release.year, tmdb.id) %>%
              distinct(), 
            c("movie_name.join" = "omdb.title.join", "movie_year" = "release.year")) %>%
  left_join(extra_movie_map, 
            c("movie_name" = "extra.movie_name", "movie_year" = "extra.movie_year")) %>%
  mutate(tmdb.id = case_when(
    !is.na(tmdb.id.x) ~ tmdb.id.x,
    !is.na(tmdb.id.y) ~ tmdb.id.y,
    !is.na(tmdb.id.x.x) ~ tmdb.id.x.x,
    !is.na(tmdb.id.y.y) ~ tmdb.id.y.y,
    TRUE ~ extra.tmdb.id)) 

# Rollup weekly sales to 1 row per movie, taking min/max to group by
rollup_weekly <- Weekly_fuzzy.1 %>% filter(!is.na(movie_year)) %>%
  group_by(movie_name, movie_year, tmdb.id) %>%
  summarise(weekly.total_gross = max(total_gross, na.rm = TRUE),
            weekly.number_wks = n(),
            weekly.budget = min(budget),
            weekly.source = min(source),
            weekly.franchise = min(franchise),
            weekly.max_theaters = max(theaters, na.rm = TRUE),
            weekly.release_wk = min(date),
            weekly.release_year = year(weekly.release_wk)) %>%
  inner_join(week.time_dimension %>% 
               select(calendar_date, week_num, week_num_year, weekly.date, weekly.event_name, 
                      weekly.holiday_ind, weekly.event_type, season), 
             by = c("weekly.release_wk" = "calendar_date"))

attr(rollup_weekly, 'vars') <- NULL
attr(rollup_weekly, 'drop') <- NULL

colnames(rollup_weekly) <- c("weekly.movie_name", "weekly.movie_year", "weekly.tmdb.id",            
                             "weekly.total_gross", "weekly.number_wks", "weekly.budget",      
                             "weekly.source", "weekly.franchise", "weekly.max_theaters",
                             "weekly.release_wk", "weekly.release_year", "weekly.week_num",
                             "weekly.week_num_year", "weekly.date", "weekly.event_name", 
                             "weekly.holiday_ind", "weekly.event_type", "weekly.season")
write.csv(rollup_weekly, "rollup_weekly.csv", row.names = FALSE)

##################################################################################
# Weekend Section
##################################################################################

# Taking weekend, make a join name that strips out punctuation, spaces and
# makes everything lower case.  It also converts & to and, 2 to two
# and specific cleanup for stat wars..

weekend_fuzzy <- weekend %>% 
  filter(!is.na(movie_year) ) %>% #& weeks_release == 1) %>%
  group_by(movie_name, movie_year) %>%
  arrange(date) %>%
  mutate(movie_name.join =  str_replace_all(tolower(gsub("\\bEp\\.", "episode", gsub("Star Wars Ep\\. VIII\\b|Star Wars Ep\\. VII\\b", "Star Wars", gsub("\\b2\\b", "two", gsub("&", "and", movie_name))))), "[^[:alnum:]]", ""), 
         release_year = min(year(date)),
         rn = row_number()) %>% # Take the first weekend as the opening weekend, may not be weeks_release = 1
  filter(rn == 1) %>%
  ungroup()

# join the weekend numbers to th movies details on:
# - name from tmdb and release year
# - name from omdb and release year
# - name from tmdb and year of first week from weekly data
# - name from omdb and year of first week from weekly data

Weekend_fuzzy.1 <- weekend_fuzzy %>% 
  left_join(movies_fuzzy %>% select(tmdb.title.join, release.year, tmdb.id) %>%
              distinct(), 
            c("movie_name.join" = "tmdb.title.join", "release_year" = "release.year")) %>%
  left_join(movies_fuzzy %>% select(omdb.title.join, release.year, tmdb.id) %>%
              distinct(), 
            c("movie_name.join" = "omdb.title.join", "release_year" = "release.year")) %>%
  left_join(movies_fuzzy %>% select(tmdb.title.join, release.year, tmdb.id) %>%
              distinct(), 
            c("movie_name.join" = "tmdb.title.join", "movie_year" = "release.year")) %>%
  left_join(movies_fuzzy %>% select(omdb.title.join, release.year, tmdb.id) %>%
              distinct(), 
            c("movie_name.join" = "omdb.title.join", "movie_year" = "release.year")) %>%
  left_join(extra_movie_map, 
            c("movie_name" = "extra.movie_name", "movie_year" = "extra.movie_year")) %>%
  mutate(tmdb.id = case_when(
    !is.na(tmdb.id.x) ~ tmdb.id.x,
    !is.na(tmdb.id.y) ~ tmdb.id.y,
    !is.na(tmdb.id.x.x) ~ tmdb.id.x.x,
    !is.na(tmdb.id.y.y) ~ tmdb.id.y.y,
    TRUE ~ extra.tmdb.id)) %>% group_by(tmdb.id) %>%
      mutate(rn = row_number()) %>% filter(!is.na(tmdb.id) & rn == 1) %>%
  ungroup() 

colnames(Weekend_fuzzy.1)[3] <- "weekend.opening_gross"
colnames(Weekend_fuzzy.1)[22] <- "weekend.tmdb.id"

# Now, join the combined movies to the rolled up weekly data on the tmdb.id
# and set a budget column that is the budget from either weekly or tmdb..
# Also join to Weekend just to get the weekend opening gross figure
final.combined <- movies_comb %>%
  left_join(rollup_weekly, by=c("tmdb.id" = "weekly.tmdb.id")) %>%
  mutate(final.budget = ifelse(!is.na(weekly.budget), weekly.budget, ifelse(budget <= 1000, NA, budget))) %>%
  left_join(Weekend_fuzzy.1 %>% select(weekend.tmdb.id, weekend.opening_gross) %>%  
              filter(!is.na(weekend.tmdb.id)), by=c("tmdb.id" = "weekend.tmdb.id")) %>%
  group_by(tmdb.id, release.date) %>% filter(row_number() == 1) %>%
  ungroup()

### Step 5 - Profitability Calculation ###
# Calculate the cumulative mean for each director and actor based on release date
# cumulative mean is last 5 movies
# Then get the lag to get what it was before this movie for predicting what 
# this movie will be
# zoo package does rollapply

# Profitability score for each movie will be used as basis for Actor/Director profitability
final.combined$profitability<- ifelse(
  is.na(final.combined$final.budget) | is.na(final.combined$weekly.release_wk), 0,
  (final.combined$weekly.total_gross-final.combined$final.budget)/final.combined$final.budget)

# Rescale negatives form between 0 to -1 to between 0 to -5 to punish bad movies
# Also cap successful movies at 5 as they are rare above this. 
final.combined$profitability <- case_when(final.combined$profitability > 5 ~ 5,
                                          final.combined$profitability < 0 ~ final.combined$profitability * 5,
                                          TRUE ~ final.combined$profitability)

# Rescale to 0 to 100 for hopefully better interpretability - a score below 50 
# loses money, above starts making money
final.combined$profitability <- rescale(final.combined$profitability, to = c(0, 100))


# separate directors into rows where there are more than 1 director on a movie.
# then calculate the cumulative mean up to this movie and the mean for this movie 
# and the previous 4 movies
# Note, my profitability calc is a little different to ROI as I'm trying to punish
# losses equally as profits
directors <- final.combined %>% filter(!is.na(final.budget) & !is.na(weekly.release_wk)) %>%
  separate_rows(director, sep=', ') %>%
  # Remove (co-director) from director names
  mutate(director = gsub("\\(.*\\)","",director)) %>%
  group_by(director) %>%
  arrange(release.date) %>%
  mutate(profitability.director.alltime = cummean(profitability) ,
         profitability.director.last5 = rollapply(profitability, 5, mean, na.rm = TRUE, fill = NA, align = 'right', partial = TRUE)) %>%
  ungroup()

write.csv(directors, "directors.csv", row.names = FALSE)

# Split actors out into individual rows and then work out their profitability
# over all the movies they have done  and a rolling 5 movie score
actors <-final.combined %>% filter(!is.na(final.budget) & !is.na(weekly.release_wk)) %>%
  separate_rows(actors, sep=', ') %>%
  group_by(actors) %>%
  arrange(release.date) %>%
  mutate(profitability.actors.alltime = cummean(profitability) ,
         profitability.actors.last5 = rollapply(profitability, 5, mean, na.rm = TRUE, fill = NA, align = 'right', partial = TRUE))  %>%
  ungroup()

write.csv(actors, "actors.csv", row.names = FALSE)

# separate directors into rows where there are more than 1 director on a movie.
# then calculate the cumulative mean up to this movie and the mean for this movie 
# and the previous 4 movies
# Note, my profitability calc is a little different to ROI as I'm trying to punish
# losses equally as profits
producers <- final.combined %>% filter(!is.na(final.budget) & !is.na(weekly.release_wk)) %>%
  group_by(production) %>%
  arrange(release.date) %>%
  mutate(profitability.producer.alltime = cummean(profitability) ,
         profitability.producer.last5 = rollapply(profitability, 5, mean, na.rm = TRUE, fill = NA, align = 'right', partial = TRUE)) %>%
  ungroup()

write.csv(producers, "producers.csv", row.names = FALSE)


directors <- read.csv("directors.csv", stringsAsFactors = FALSE)
# find the previous (lag) score for the director as this is what it was
# BEFORE this current movie was made and hence can be used as a predictor
# then roll back up to the movie id level so the movie has 1 director score
directors <- directors %>% 
  group_by(director) %>%
  arrange(release.date) %>%
  mutate(profitability.director.alltime = lag(profitability.director.alltime),
         profitability.director.last5 = lag(profitability.director.last5)) %>%
  ungroup() %>%
  group_by(tmdb.id) %>%
  summarise(profitability.director.alltime = mean(profitability.director.alltime, na.rm = TRUE),
            profitability.director.last5 = mean(profitability.director.last5, na.rm = TRUE))
# Set NaN to NA
directors$profitability.director.alltime[is.nan(directors$profitability.director.alltime)] <- NA
directors$profitability.director.last5[is.nan(directors$profitability.director.last5)] <- NA

producers <- read.csv("producers.csv", stringsAsFactors = FALSE)
# Now find the previous profitability for the actor and then combine back onto the movie
# so if there are 4 actors on a movie, we get their previous profitability (could be on 
# different movies) and then take the mean of the 4 on this movie to give this movie 
# 1 actor profitability score that can be a Predictor
producers <- producers %>% 
  group_by(production) %>%
  arrange(release.date) %>%
  mutate(profitability.producer.alltime = lag(profitability.producer.alltime),
         profitability.producer.last5 = lag(profitability.producer.last5)) %>%
  ungroup() %>%
  group_by(tmdb.id) %>%
  summarise(profitability.producer.alltime = mean(profitability.producer.alltime, na.rm = TRUE),
            profitability.producer.last5 = mean(profitability.producer.last5, na.rm = TRUE))
# Set NaN to NA
producers$profitability.producer.alltime[is.nan(producers$profitability.producer.alltime)] <- NA
producers$profitability.producer.last5[is.nan(producers$profitability.producer.last5)] <- NA

actors <- read.csv("actors.csv", stringsAsFactors = FALSE)
# Now find the previous profitability for the actor and then combine back onto the movie
# so if there are 4 actors on a movie, we get their previous profitability (could be on 
# different movies) and then take the mean of the 4 on this movie to give this movie 
# 1 actor profitability score that can be a Predictor
actors <- actors %>% 
  group_by(actors) %>%
  arrange(release.date) %>%
  mutate(profitability.actors.alltime = lag(profitability.actors.alltime),
         profitability.actors.last5 = lag(profitability.actors.last5)) %>%
  ungroup() %>%
  group_by(tmdb.id) %>%
  summarise(profitability.actors.alltime = mean(profitability.actors.alltime, na.rm = TRUE),
            profitability.actors.last5 = mean(profitability.actors.last5, na.rm = TRUE))
# Set NaN to NA
actors$profitability.actors.alltime[is.nan(actors$profitability.actors.alltime)] <- NA
actors$profitability.actors.last5[is.nan(actors$profitability.actors.last5)] <- NA

# Get CPI Data to adjust to 2018 dollars
cpi <- inflation_adjust(2018)

cpi$year <- as.integer(cpi$year)
todays_rate <- cpi[cpi$year == 2018, ]$avg_cpi
cpi$cpi_factor <- ((todays_rate - cpi$avg_cpi)/cpi$avg_cpi + 1)

# Join Actors, Directors and Producers back to main dataframe to tack scores on the end
final.combined <- final.combined %>%
  left_join(directors, by=c("tmdb.id" = "tmdb.id")) %>%
  left_join(actors, by=c("tmdb.id" = "tmdb.id")) %>%
  left_join(producers, by=c("tmdb.id" = "tmdb.id")) %>%
  left_join(collections %>% select(id, collection_id, collection.sequel.number), by = c("tmdb.id" = "id", "tmdb.collection.id" = "collection_id")) %>%
  left_join(cpi %>% select(year, cpi_factor), by = c("weekly.release_year" = "year")) %>%
  mutate(final.budget.cpi = round(final.budget * cpi_factor, 0) ,
         weekly.total_gross.cpi = round(weekly.total_gross * cpi_factor, 0) ,
         weekend.opening_gross.cpi = round(weekend.opening_gross * cpi_factor, 0) ,
         revenue.cpi = round(revenue * cpi_factor, 0))

# Where there is a budget and the profitability for Directors and Actors are NA, this will be the 1st movie they did
# so no history.  Set these to 0 as they are neutral in profitability at this point, so they can still be included as separate to NAs..
final.combined[!is.na(final.combined$final.budget) & is.na(final.combined$profitability.director.alltime), ]$profitability.director.alltime <- 50
final.combined[!is.na(final.combined$final.budget) & is.na(final.combined$profitability.director.last5), ]$profitability.director.last5 <- 50
final.combined[!is.na(final.combined$final.budget) & is.na(final.combined$profitability.actors.alltime), ]$profitability.actors.alltime <- 50
final.combined[!is.na(final.combined$final.budget) & is.na(final.combined$profitability.actors.last5), ]$profitability.actors.last5 <- 50
final.combined[!is.na(final.combined$final.budget) & is.na(final.combined$profitability.producer.alltime), ]$profitability.producer.alltime <- 50
final.combined[!is.na(final.combined$final.budget) & is.na(final.combined$profitability.producer.last5), ]$profitability.producer.last5 <- 50
final.combined[is.na(final.combined$collection.sequel.number),]$collection.sequel.number <- 0

final.combined$is.sequel <- ifelse(final.combined$collection.sequel.number > 1, TRUE, FALSE)
final.combined$is.franchise <- ifelse(final.combined$weekly.franchise == "" | is.na(final.combined$weekly.franchise), FALSE, TRUE)
final.combined$type.sequel <- as.factor(case_when(final.combined$collection.sequel.number == 0 ~ "None",
                                                  final.combined$collection.sequel.number == 1 ~ "First",
                                          TRUE ~ "Sequel"))

# If revenue not set, use total gross as a proxy..
final.combined$revenue.final <- ifelse(final.combined$revenue.cpi > 0, final.combined$revenue.cpi, 1.98*final.combined$weekly.total_gross.cpi )

final.combined$roi<- ifelse(
  is.na(final.combined$final.budget) | is.na(final.combined$weekly.release_wk), NA,
  ((0.55*final.combined$revenue.final)-final.combined$final.budget.cpi)/final.combined$final.budget.cpi)
final.combined$profitable <- ifelse(final.combined$roi >= 0, 1, 0)

# Write out the final set
write.csv(final.combined, "tmdb.omdb.weekly.combined.csv", row.names = FALSE)

# Cleanup
rm(list = ls())
