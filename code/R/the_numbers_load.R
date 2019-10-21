# ========================
# Example of HTML Scraping 
# ========================
library(httr)
library(jsonlite)
library(dplyr)
library(readr)
library(lubridate)
library(rvest)
library(stringr)
library(dplyr)

setwd("//CinemaData/data/")
# The-numbers.com has a yearly page of all movies released that year.  Each line
# then has a link to the movie summary with sales data.
# So, loop through each year we are interested in
start_yr <- 1987
end_yr   <- 1990
for (year_num in start_yr:end_yr) {
  url <- paste("https://www.the-numbers.com/movies/year/", year_num, sep="")
  webpage <- read_html(url)
  
  # Find each movie link that was released in the Theatres
  tbls_ls <- webpage %>%
    html_nodes(xpath ="//table/tr[contains(., 'Theatrical')]/td/b/a") %>%
    html_attr("href")
  
  weekend <- data.frame(stringsAsFactors = FALSE)
  weekly  <- data.frame(stringsAsFactors = FALSE)
  error_df <- data.frame(stringsAsFactors = FALSE)
  
  base_url <- "https://www.the-numbers.com"
  
  # For each thearical movie, get summary
  for (i in 1:length(tbls_ls)) {
    webpage_sum <- read_html(paste(base_url, tbls_ls[i], sep=""))
    
    # Movie name has name and a date in brackets at the end.
    # It will be in div element called main and then h1 tag
    movie_name_yr <- webpage_sum %>% 
      html_nodes(xpath ="//div[contains(@id, 'main')]/div/h1 | //div[contains(@id, 'main')]/h1") %>%
      html_text
    
    # Movie name has name and date in brackets at end so separate
    movie_name<-gsub(" \\(.*\\)","",movie_name_yr)
    movie_year<-gsub("\\)", "", gsub(".*\\(", "", movie_name_yr) )
    
    # Just in case there are errors reading a page, do in trycatch block
    tryCatch({
      # We are only interested in movies with a Domestic Boc Office
      domestic_bo <- webpage_sum %>% 
        html_nodes(xpath ="///table[contains(@id, 'movie_finances')]/tr[contains(., 'Domestic Box Office')]/td[contains(@class, 'data')]") %>%
        html_text
      
      # is $0, ignore the movie (strictly overseas movie)
      if (domestic_bo != '$0') {
        # pick up the source - book, remake, play, game, etc..
        Source <- webpage_sum %>%
          html_nodes(xpath ="//table/tr[contains(., 'Source:')]/td/a") %>%
          html_text
        # Get Franchise name - MArvel, James Bond, etc..
        Franchise <- webpage_sum %>%
          html_nodes(xpath ="//table/tr[contains(., 'Franchise:')]/td/a") %>%
          html_text
        # Find budget if populated
        budget <- webpage_sum %>%
          html_nodes(xpath ="//table/tr[contains(., 'Budget:')]/td[contains(., '$')]") %>%
          html_text
        
        # Then pick up box office charts which will be either Weekend, daily or Weekly tables
        # Per Theater search only looks for cinema numbers (not video)
        table.nodes <- webpage_sum %>% 
          html_nodes(xpath ="//div[contains(@id, 'box_office_chart')]/table[contains(., 'Per Theater')]")
        
        # If there are theatre sales data:
        if (length(table.nodes) > 0) {
          # Weekend table will be 1st table and Weekly will be last
          weekend.table <- as.data.frame(table.nodes[1] %>% html_table())
          weekly.table <- as.data.frame(table.nodes[length(table.nodes)] %>% html_table())
          
          # Add movie level data to each observation
          weekend.table$movie_name <- movie_name
          weekend.table$movie_year <- movie_year
          weekend.table$budget <- ifelse(length(budget) > 0, budget, "")
          weekend.table$source <- ifelse(length(Source) > 0, Source, "")
          weekend.table$franchise <- ifelse(length(Franchise) > 0, Franchise, "")
          
          weekly.table$movie_name <- movie_name
          weekly.table$movie_year <- movie_year
          weekly.table$budget <- ifelse(length(budget) > 0, budget, "")
          weekly.table$source <- ifelse(length(Source) > 0, Source, "")
          weekly.table$franchise <- ifelse(length(Franchise) > 0, Franchise, "")
          
          # Make sure column names are consistent
          colnames(weekly.table) <- c("date", "rank", "gross", "pcnt_change",
                                      "theaters", "per_theater", "total_gross",
                                      "weeks_release", "movie_name", "movie_year",
                                      "budget", "source", "franchise")
          
          colnames(weekend.table) <- c("date", "rank", "gross", "pcnt_change",
                                       "theaters", "per_theater", "total_gross",
                                       "weeks_release", "movie_name", "movie_year",
                                       "budget", "source", "franchise")
          # Add new rows to complete set for this year
          weekly <- rbind(weekly, weekly.table)
          weekend <- rbind(weekend, weekend.table)
          
          print(paste(i, movie_name, domestic_bo, Source, Franchise))
          
        }
      }
    }, error=function(e){
      # Display errors and write to data frame
      cat(movie_name_yr, "ERROR :",conditionMessage(e), "\n")
      error_df <- rbind(error_df, movie_name_yr)})
  }
  
  # write out Data Frames
  write.csv(weekend, paste("Weekend_", year_num, ".csv", sep=""), row.names = FALSE)
  write.csv(weekly, paste("Weekly_", year_num, ".csv", sep=""), row.names = FALSE)
  
}


# Once all yearly files have been produced, they will then be combined into
# one data frame, with cleaning up dates, numeric fields
combine_files <- function (file_pattern, output_filename) {
  filenames <- list.files(pattern=file_pattern)
  dataset <- do.call("rbind",lapply(filenames,FUN=function(files){ read.csv(files, colClasses =c("character"))}))
  
  dataset$date <- as.Date(dataset$date ,format='%b %d, %Y')
  
  dataset$rank <- as.numeric(dataset$rank)
  dataset$gross <- as.numeric(gsub('[$,]', '', dataset$gross))
  dataset$total_gross <- as.numeric(gsub('[$,]', '', dataset$total_gross))
  dataset$per_theater <- as.numeric(gsub('[$,]', '', dataset$per_theater))
  dataset$theaters <- as.numeric(gsub('[,]', '', dataset$theaters))
  
  dataset$budget <- as.numeric(gsub('[$,]', '', dataset$budget))
  dataset$weeks_release <- as.numeric(dataset$weeks_release)
  dataset$movie_year <- as.numeric(dataset$movie_year)
  
  write.csv(dataset, output_filename, row.names = FALSE)
}

# Build Weekly and Weekend combined files
combine_files("^Weekly_", "Weekly.csv")
combine_files("^Weekend_", "Weekend.csv")

# Cleanup
rm(list = ls())