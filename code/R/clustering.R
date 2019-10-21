library(tidyverse)
library(readr)
library(dplyr)
library(ggplot2)
library(factoextra)
library(cluster)
library(ggrepel)

setwd("/GitDev/Footyapp/data/") 

footy_team_cluster <- read.csv("FootyTeamCluster.csv", stringsAsFactors = FALSE)

# Select the columns to include for clustering
# These are columns that to some extent the team can
# control themselves
cols = c('ppm', 'aveVelocity', 
         'isZone17Incursion_pcnt', 'isZone14Incursion_pcnt', 
         'isZone11PassIncursion_pcnt', 'totalBackPasses_pcnt', 
         'totalFwdPasses_pcnt', 'totalSidePasses_pcnt',
         'total10mPasses_pcnt', 'total10_20mPasses_pcnt', 
         'total20mPasses_pcnt', 'crossfieldPlay_pcnt', 
         'backPassTurnover_pcnt', 'fwdPassTurnover_pcnt', 
         'sidePassTurnover_pcnt', 'AMPlay_pcnt', 
         'MDPlay_pcnt', 'backKeeperPasses_pcnt', 
         'clearances_pcnt', 'passZero_pcnt', 
         'passPlay_pcnt', 'pass1_pcnt',
         'pass2_3_pcnt', 'pass4_6_pcnt', 
         'pass7Plus_pcnt')

# Take subset of columns for cluster analysis
ftc_cluster <- footy_team_cluster[, c("competition", "teamName", "playByUs", "age_group", cols)]

# Rename columns for displaying
colnames(ftc_cluster) <- c('competition', 'teamName', 
              'playByUs', 'age_group',
              'Passes Per Minute', 'Velocity', 
              'Zone 17 Incursions', 'Zone 14 Incursions', 
              'Passes into Zone 11', 'Back Pass', 
              'Forward Pass', 'Side Pass',
              '10m Passes', '10-20m Passes', 
              '20m+ Passes', 'Crossfield Play across Back', 
              'Back Pass from Turnover', 'Forward Pass from Turnover', 
              'Side Pass from Turnover', 'Attack to Midfield', 
              'Midfield to Defence', 'Passes to Keeper', 
              'Clearances', 'Plays with No Passes', 
              'Plays with Passes', 'Plays with 1 Pass',
              'Plays with 2-3 Passes', 'Plays with 4-6 Passes', 
              'Plays with 7+ Passes')

# Filter on competitions to cluster
ftc_cluster <- ftc_cluster %>%   
               filter(competition %in% c("2019 NSFA 1 Girls Under 16", 
        #  "2019 NSFA 1 Boys Under 13", 
           "2019 World Cup 2019 Mens Open",             
           "2019 International Friendly Mens Open"   ,  
           "2019 Icc Football Mens Open",
           "2019 Mens Open")
           )

# ==============================
# Principal component analysis
# ==============================
ftpca.pr <- prcomp(ftc_cluster[, -c(1:4)], center = TRUE, scale = TRUE)

# Get the variable contributions that make up PCA
res.var <- get_pca_var(ftpca.pr)

# Take 1st 2 dimensions
pca_vars <- data.frame(pca_1 = res.var$coord[, 1], 
                  pca_2 = res.var$coord[, 2],
                  variable = rownames(res.var$coord)
                  )


# Work out distance and those furthest away will be shown 
pca_vars$distance <- pca_vars$pca_1 ^ 2 + pca_vars$pca_2 ^ 2
pca_vars <- pca_vars %>% ungroup() %>%
  arrange(-distance) %>%
  mutate(ranking = row_number())
top20 = pca_vars[pca_vars$ranking <= 20, ]$variable 

# Plot the 2-d PCA Plot plus important features
fviz_pca_biplot(ftpca.pr, geom = c("point", "text"),
                geom.var = c("text"),
                geom.ind = c("point"),
                select.var = list(name=top20),
                col.var="steel blue",
                invisible = "none",
                pointshape = 21, 
                pointsize = 4.5, 
                title = "2019 NSFA 1 Girls Under 16",
                fill.ind = ifelse(ftc_cluster$competition=="2019 NSFA 1 Girls Under 16", 
                                  paste(ifelse(ftc_cluster$playByUs == "true", "Us", "Opposition")),
                                  ifelse(ftc_cluster$competition=="2019 Boys Under 14", "14s", 
                                  "Top-Flight Teams")), #paste(fmpca$matchName, fmpca$result), 
                alpha.var = 0.1, 
                repel = TRUE,
                mean.point = FALSE)+
  theme_minimal()+
  labs(fill = "Team") +
  geom_text_repel(aes(label=ftc_cluster$teamName), 
                  box.padding = 0.1, point.padding =0.1, 
                  segment.color = 'grey50', seed = 13,
                  size=3.5) 

# ========================
# K Means Clustering
# ========================
# Convert to matrix for distance calculations
m<-as.matrix(ftc_cluster[ -c(1:4)])
rownames(m) <- ftc_cluster$teamName
# Scale all columns so each feature has equal importance in distance
m <- scale(m)

# Create Cosine Distance Function
cosineSim <- function(x){
  as.dist(x%*%t(x)/(sqrt(rowSums(x^2) %*% t(rowSums(x^2)))))
}
# Compute cosine distance of each team
cs <- cosineSim(m)
cd <- 1-cs

# Do clustering, splitting to 3 which should be:
# - Top-Flight Teams
# - Best of competition
# - Rest of competition
kfit <- kmeans(cd, 3, nstart=100)

# Graph k-means cluster
fviz_cluster(kfit, cd, ellipse = TRUE, ellipse.alpha= 0.1,
             palette = "jco",repel = TRUE, ggtheme = theme_minimal(), 
             main= FALSE, xlab= FALSE, ylab = FALSE)

# For Agglomerative Hierarchical Clustering,
# create groups to display
groups <- hclust(cd,method="ward.D")
# Visualize hierarchical clustering
# Again 3 groups
fviz_dend(groups, k = 3, 
          cex = 0.8, 
          main = "2019 NSFA 1 Girls Under 16",
          labels_track_height=3,
          horiz= TRUE, rect = TRUE # Add rectangle around groups
         )

# ===================================================
# T-distributed Stochastic Neighbor Embedding (t-SNE)
# ===================================================
library(Rtsne)
# Make sure data is scaled before applying t-sne
fmpca <- ftc_cluster %>%
  mutate_at(-c(1:4), funs(c(scale(.))))
tsne <- Rtsne(fmpca[-c(1:4)], dims = 2, perplexity=3, verbose=TRUE, max_iter = 500)

# Get 2 dimensions for display
fmpca$sne_X <- tsne$Y[, 1]
fmpca$sne_Y <- tsne$Y[, 2]

# Plot the 2 dimensions
ggplot(fmpca, mapping = aes(x=sne_X, y=sne_Y, colour=competition)) +
  geom_point() +
  geom_text_repel(aes(label=teamName), 
                  box.padding = 0.1, point.padding =0.1, 
                  segment.color = 'grey50', seed = 13,
                  size=3.5)

## ==== Cleanup ====
rm(list = ls())
