library(dplyr)
library(readr)
library(lubridate)
library(rvest)
library(dplyr)
library(tidyverse)
library(zoo)
library(scales)
library(magick)
library(ggrepel)
library(ggbeeswarm)
library(lme4)

setwd("/tmp/testg/CinemaData/data/26042019/") 

# Read movie dataset 
movies <- read.csv("tmdb.omdb.weekly.combined.csv", 
                   stringsAsFactors = FALSE)
movies$release.date <- ymd(movies$release.date)

movies$weekly.release_wk <- ymd(movies$weekly.release_wk)
movies <- movies %>% group_by(weekly.franchise) %>%
  arrange(weekly.release_wk) %>%
  mutate(weekly.franchise.number = row_number(),
         weekly.franchise.start_wk = min(weekly.release_wk),
         weekly.previous.start_wk = lag(weekly.release_wk)) %>%
  ungroup()

# Set Sagas indicator, based on domain knowledge
sagas <- c(10, 119, 230, 263, 1241, 2883, 31562,  33514,
           121938, 131635, 283579, 295130, 344830, 575987)
movies$Sequel.Source <- ifelse(movies$tmdb.collection.id %in% sagas, "Saga", "Repeat")

# Filter to collections for analysis of sequels
movies.coll <- movies %>% filter(!is.na(tmdb.collection.id) & !is.na(roi) &
                                   tmdb.collection.id != 286162) %>%
  group_by(tmdb.collection.id) %>%
  mutate(cnt = n(), Series_length = max(collection.sequel.number),
         previous.movie = lag(release.date), 
         last_movie.date = max(release.date),
         min_seq = min(collection.sequel.number)) %>%
  filter(cnt > 1)

# Get the set of standalone movies as based on our original analysis of mainstream movies 
movies.set <- movies %>% filter(!is.na(final.budget) & weekly.max_theaters > 400  &
                                  collection.sequel.number == 0)


# Create summary data about each collection, 1 row per collection
colls <- movies %>% filter(revenue.final > 0 & budget.adjusted > 0 &
                    !is.na(movies$tmdb.collection.id) & tmdb.collection.id != 286162) %>%
                    group_by(tmdb.collection.id, tmdb.collection.name) %>%
  summarise(min_seq = min(collection.sequel.number),
            max_seq = max(collection.sequel.number),
            cnt = n(), mean_roi = mean(roi),
            franchise = max(weekly.franchise, na.rm = TRUE),
            min_year = min(ifelse(is.na(weekly.movie_year), year(release.date), weekly.movie_year)),
            max_year = max(ifelse(is.na(weekly.movie_year), year(release.date), weekly.movie_year)),
            mean_roi = mean(roi, na.rm = TRUE),
            median_roi = median(roi, na.rm = TRUE),
            first_roi = max(ifelse(collection.sequel.number == 1, roi, NA), na.rm=TRUE),
            first_budget = max(ifelse(collection.sequel.number == 1, budget.adjusted, NA), na.rm = TRUE),
            first_revenue = max(ifelse(collection.sequel.number == 1, revenue.final, NA), na.rm = TRUE),
            total_revenue = sum(revenue.final, na.rm = TRUE),
            total_budget.adjusted = sum(budget.adjusted, na.rm = TRUE),
            total_sequel_revenue = sum(ifelse(collection.sequel.number == 1, 0, revenue.final), na.rm = TRUE),
            total_sequel_budget = sum(ifelse(collection.sequel.number == 1, 0, budget.adjusted), na.rm = TRUE),
            roi_sequel = (sum(total_sequel_revenue) - sum(total_sequel_budget))/sum(total_sequel_budget),
            roi_series = (sum(total_revenue)-sum(total_budget.adjusted))/sum(total_budget.adjusted),
            saga_ind = max(Sequel.Source))%>%
    filter(cnt > 1 & min_seq == 1)

# Summarise ROI based on series length
coll_roi <- colls %>% group_by(max_seq) %>%
  summarise(series_roi = (sum(total_revenue)-sum(total_budget.adjusted))/sum(total_budget.adjusted),
            series_no1_roi = (sum(total_sequel_revenue)-sum(total_sequel_budget))/sum(total_sequel_budget),
            roi.series = mean(roi_series, na.rm = TRUE),
            roi.series.med = median(roi_series, na.rm = TRUE),
            first_roi = mean(first_roi, na.rm = TRUE),
            series_med_roi = median((total_revenue-total_budget.adjusted)/total_budget.adjusted),
            first_med_roi = median(first_roi, na.rm = TRUE),
            num_series = n())

# ===== Multi-level Modelling ====
options("scipen"=100, "digits"=4)
# Slope and Intercept varying
model.full <- lmer(I(revenue.final/1000000) ~ I(budget.adjusted/1000000) + collection.sequel.number+
                     Sequel.Source + 
                     (collection.sequel.number|tmdb.collection.name), movies.coll)
summary(model.full)

# Linear Regression on Budget, Sequel and Source
model.lm <- lm(I(revenue.final/1000000) ~ I(budget.adjusted/1000000) + collection.sequel.number+
                 Sequel.Source, movies.coll)
summary(model.lm)

# Linear Regression on Budget
model.lm1 <- lm(I(revenue.final/1000000) ~ I(budget.adjusted/1000000), movies.coll)
summary(model.lm1)

# Slope varies but intercept is fixed
model1 <- lmer(I(revenue.final/1000000) ~ I(budget.adjusted/1000000) + collection.sequel.number+
                 Sequel.Source + 
                 (0 + collection.sequel.number|tmdb.collection.name), movies.coll)
summary(model1)

# intercept is random, but Slope is fixed 
model2 <- lmer(I(revenue.final/1000000) ~ I(budget.adjusted/1000000) + collection.sequel.number+
                 Sequel.Source + 
                 (1|tmdb.collection.name), movies.coll)
summary(model2)

# Slope and Intercept varying, and does not take into account Sagas.
model3 <- lmer(I(revenue.final/1000000) ~ I(budget.adjusted/1000000) + collection.sequel.number+
                 (collection.sequel.number|tmdb.collection.name), movies.coll)
summary(model3)

# Choose the best model from above
anova(model.full, model.lm, model.lm1, model1, model2, model3)

# combine fixed and random coefficients
coeffs.combined <- coef(model.full)$tmdb.collection.name

# Only look at Random coefficients
coeffs.random <- ranef(model.full)$tmdb.collection.name
colnames(coeffs.random) <- c("Intercept", "Sequel")

# ===== Graphs for Report ====

# Figure 1 - Movies ROI
sequel.all <- movies.coll %>% 
  mutate(roi.capped = ifelse(roi >10, 10, roi),
         sequel.factor = ifelse(collection.sequel.number < 5, as.character(collection.sequel.number), '5+')) # cap roi at 1000% for plotting purposes

sequel.mean <- sequel.all %>%
  group_by(sequel.factor) %>% 
  summarise(mean.roi = mean(roi), median.roi = median(roi))

ggplot(sequel.all, aes(x=sequel.factor, y=roi.capped)) +
  geom_quasirandom(aes(color = ifelse(roi.capped < 0, "green", "red")), 
                   alpha = 0.7) +
  geom_point(data = sequel.mean, aes(x = sequel.factor, y = mean.roi), 
             color="black", shape=16, size=3, fill="black") +
  geom_point(data = sequel.mean, aes(x = sequel.factor, y = median.roi), 
             color="black", shape=15, size=3, fill="black") +
  labs(x = "Sequel number", y = "Return on investment", 
       title = "Movie Return on Investment by Sequel Number") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set1") +
  scale_color_brewer(palette = "Set1") +
  theme(legend.position = "none") +
  theme(plot.caption = element_text(hjust = 0, face = "italic")) +
  theme(plot.title = element_text(color = "black", size = 16, face = "bold", hjust = 0)) +
  scale_x_discrete() +
  scale_y_continuous(labels = percent_format(accuracy = 1), limits = c(-2,10), 
                     breaks = c(-2, 0, 2, 4, 6, 8, 10)) +
  geom_label_repel(data = sequel.mean, aes(y = mean.roi, label=ifelse(sequel.factor == 4, 
                                                                      "mean roi", '')), 
                   box.padding = 3, point.padding = 0.5, 
                   segment.color = 'grey50', seed = 112) +
  geom_label_repel(data = sequel.mean, aes(y = median.roi, label=ifelse(sequel.factor == "5+", 
                                                                        "median roi", '')), 
                   box.padding = 3, point.padding = 0.5, 
                   segment.color = 'grey50', seed = 112) 

# Figure 2 - Collections ROI (excluding 1st movie)
coll.all <- colls %>% 
  mutate(roi.capped = ifelse(roi_sequel >10, 10, roi_sequel),
         sequel.factor = ifelse(max_seq < 5, as.character(max_seq), '5+')) # cap roi at 1000% for plotting purposes

coll.mean <- coll.all %>%
  group_by(sequel.factor) %>% 
  summarise(mean.roi = mean(roi_sequel), median.roi = median(roi_sequel),
            mean.roi.incl = mean(roi_series), median.roi.incl = median(roi_series))

ggplot(coll.all, aes(x=sequel.factor, y=roi.capped)) +
  geom_quasirandom(aes(color = ifelse(roi.capped < 0, "green", "red")), 
                   alpha = 0.7) +
  geom_point(data = coll.mean, aes(x = sequel.factor, y = mean.roi), 
             color="black", shape=16, size=3, fill="black") +
  geom_point(data = coll.mean, aes(x = sequel.factor, y = median.roi), 
             color="black", shape=15, size=3, fill="black") +
  labs(x = "Series Length", y = "Return on investment", 
       title = "Series ROI Excluding First Movie") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set1") +
  scale_color_brewer(palette = "Set1") +
  theme(legend.position = "none") +
  theme(plot.caption = element_text(hjust = 0, face = "italic")) +
  theme(plot.title = element_text(color = "black", size = 16, face = "bold", hjust = 0)) +
  scale_x_discrete() +
  scale_y_continuous(labels = percent_format(accuracy = 1), limits = c(-2,10), 
                     breaks = c(-2, 0, 2, 4, 6, 8, 10)) +
  ggsave("sequel_roi_points.jpg")

# Figure 3 - Incredible 1st movie
  ggplot(movies.coll %>%
                       filter(tmdb.collection.id %in%
                                c(1575, 41437, 656)), 
                     aes(x = as.integer(collection.sequel.number), y = revenue.final/1000000)) + 
  geom_point(color = "blue", alpha = 0.7) + 
  labs(x = "Sequel Number", y = "$ Million", 
       title = "Low Budget Initial Movies") +
  theme_minimal() +
  geom_smooth(method = "lm", color = "blue", se=F) +
  geom_point(aes(x = as.integer(collection.sequel.number), y = budget.adjusted/1000000), color = "red", alpha = 0.7) + 
  geom_smooth(method = "lm", color = "red", se=F, aes(x = as.integer(collection.sequel.number), y = budget.adjusted/1000000)) +
  facet_wrap(~tmdb.collection.name, scales = "free")

# Figure 4 - Blockbuster
  ggplot(movies.coll %>%
                       filter(tmdb.collection.id %in%
                                c(87359, 1570, 2150)), 
                     aes(x = as.integer(collection.sequel.number), y = revenue.final/1000000)) + 
  geom_point(color = "blue", alpha = 0.7) + 
  labs(x = "Sequel Number", y = "$ Million", 
       title = "Blockbusters") +
  theme_minimal() +
  geom_smooth(method = "lm", color = "blue", se=F) +
  geom_point(aes(x = as.integer(collection.sequel.number), y = budget.adjusted/1000000), color = "red", alpha = 0.7) + 
  geom_smooth(method = "lm", color = "red", se=F, aes(x = as.integer(collection.sequel.number), y = budget.adjusted/1000000)) +
  facet_wrap(~tmdb.collection.name, scales = "free")

# Figure 5 - Saga
  ggplot(movies.coll %>%
                       filter(tmdb.collection.id %in%
                                c(1241, 33514, 131635)), 
                     aes(x = as.integer(collection.sequel.number), y = revenue.final/1000000)) + 
  geom_point(color = "blue", alpha = 0.7) + 
  labs(x = "Sequel Number", y = "$ Million", 
       title = "Sagas") +
  theme_minimal() +
  geom_smooth(method = "lm", color = "blue", se=F) +
  geom_point(aes(x = as.integer(collection.sequel.number), y = budget.adjusted/1000000), color = "red", alpha = 0.7) + 
  geom_smooth(method = "lm", color = "red", se=F, aes(x = as.integer(collection.sequel.number), y = budget.adjusted/1000000)) +
  facet_wrap(~tmdb.collection.name, scales = "free")
 
# Figure 7 - Histogram of Intercept
  ggplot(coeffs.random,aes(x=Intercept, fill = cut(Sequel, 100))) +
    geom_histogram(show.legend = FALSE, binwidth = 25) +
    theme_minimal() +
    labs(x = "(Intercept)", y = "Number of Novies") +
    ggtitle("Random Effects Intercept Histogram") +
    scale_fill_discrete(h = c(200, 250), c = 50, l = 40)
  
# Figure 8 - Histogram of Sequel
  ggplot(coeffs.random,aes(x=Sequel, fill = cut(Sequel, 100))) +
    geom_histogram(show.legend = FALSE, binwidth = 10) +
    theme_minimal() +
    labs(x = "Sequel", y = "Number of Novies") +
    ggtitle("Random Effects Sequel Histogram") +
    scale_fill_discrete(h = c(200, 250), c = 50, l = 40)
  
# Figure 10 - Marvel movies intersection
movies[movies$tmdb.id == 299537, ]$tmdb.collection.name <- 'Captain Marvel'
movies[movies$tmdb.id == 284052, ]$tmdb.collection.name <- 'Doctor Strange'
  
ggplot(movies %>% filter(tmdb.collection.id %in% c(529892, 131292, 131296,
                                                 131295, 86311, 131292, 422834, 284433) |
                             tmdb.id %in% c(299537, 284052, 315635)), 
         aes(x=release.date, y=tmdb.collection.name, size = roi))+#(revenue.final-budget.adjusted)/1000000)) +
    geom_point(color = "steel blue") +
  labs(x = "Release Date", y = "Collection/Movie", 
       title = "Marvel Cinematic Universe Timeline") +
    theme_minimal()
  
# Figure 11 - Marvel revenue v budget
ggplot(movies.coll %>%
                       filter(tmdb.collection.id %in%
                                c(529892, 131292, 131296, 131295, 86311, 131292, 422834, 284433)), 
                     aes(x = (collection.sequel.number), y = revenue.final/1000000)) + 
  geom_point(color = "blue", alpha = 0.7) + 
  labs(x = "Sequel Number", y = "$ Million", 
       title = "Marvel Cinematic Universe Budgets and Revenues") +
  theme_minimal() +
  geom_smooth(method = "lm", color = "blue", se=F) +
  geom_point(aes(x = (collection.sequel.number), y = budget.adjusted/1000000), color = "red", alpha = 0.7) + 
  geom_smooth(method = "lm", color = "red", se=F, aes(x = as.integer(collection.sequel.number), y = budget.adjusted/1000000)) +
  facet_wrap(~tmdb.collection.name, scales = "free")  +
    scale_x_continuous(breaks = c(1:4))  +
    scale_y_continuous(limits=c(0,NA))

# Figure 12 - Bond revenue v budget
movies.bond <- movies.coll %>%
    filter(tmdb.collection.id %in% c(645))
  
# Show the 1st movie done by the different lead actor.
movies.bond$lead <- ""
movies.bond[movies.bond$tmdb.id == 646, ]$lead <- "Sean Connery"
movies.bond[movies.bond$tmdb.id == 668, ]$lead <- "George Lazenby"
movies.bond[movies.bond$tmdb.id == 253, ]$lead <- "Roger Moore"
movies.bond[movies.bond$tmdb.id == 708, ]$lead <- "Timothy Dalton"
movies.bond[movies.bond$tmdb.id == 710, ]$lead <- "Pierce Brosnan"
movies.bond[movies.bond$tmdb.id == 36557, ]$lead <- "Daniel Craig"

ggplot(movies.bond, 
         aes(x = as.integer(collection.sequel.number), y = revenue.final/1000000, label=lead)) + 
    geom_point(color = "blue", alpha = 0.7) + 
    labs(x = "Sequel Number", y = "$ Million", 
         title = "James Bond") +
    theme_minimal() +
    geom_smooth(method = "lm", color = "blue", se=F) +
    geom_point(aes(x = as.integer(collection.sequel.number), y = budget.adjusted/1000000), color = "red", alpha = 0.7) + 
    geom_smooth(method = "lm", color = "red", se=F, aes(x = as.integer(collection.sequel.number), y = budget.adjusted/1000000)) +
    geom_label_repel(box.padding = 3, point.padding = 0.5, 
                   segment.color = 'grey50',
                   nudge_y = ifelse(movies.bond$tmdb.id == 668, -5, 0)) 

# Cleanup
rm(list = ls())
