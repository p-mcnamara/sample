#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(tidyverse)
library(readr)
library(dplyr)
library(ggplot2)
library(shiny)
library(scales)
library(DT)
library(spatstat)
library(ggmap)
library(lubridate)      
library(ggsoccer)      
library(ggrepel)  
library(cluster)  
library(factoextra)

# Load U16s data
FootyGames <- read.csv("FootyGames1Pro.csv", stringsAsFactors = FALSE)
footy_match_play <- read.csv("FootyMatchPlayPro.csv", stringsAsFactors = FALSE)
footy_match_age <- read.csv("FootyMatchAgeFullPro.csv", stringsAsFactors = FALSE)
footy_team_cluster <- read.csv("FootyTeamClusterPro.csv", stringsAsFactors = FALSE)

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
ftc_cluster <- footy_team_cluster[, c("competition", "teamName", "playByUs", "age_group", cols)]#  %>% filter(competition == "2019 NSFA 1 Boys Under 13")

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

# Reference teams for clustering and age comparisons
top_flight_comps <- c("2019 World Cup 2019 Mens Open",             
                      "2019 International Friendly Mens Open",        
                      "2019 Icc Football Mens Open",
                      "2019 Mens Open")
# Which competition is the one the website is concerned with
our_comp <- "2019 Regional Boys Under 14"
Dashboard_name <- "U14 Boys"

FootyGames$orderDate <- ymd(substr(FootyGames$time, 1, 10))

# For heat maps, our team runs left to right,
# Opposition runs right to left
FootyGames$lengthMetres = ifelse(FootyGames$playByUs == "false", 110 - FootyGames$lengthMetres, FootyGames$lengthMetres)
FootyGames$widthMetres = ifelse(FootyGames$playByUs == "false", 75 - FootyGames$widthMetres, FootyGames$widthMetres)
FootyGames$lengthFraction = ifelse(FootyGames$playByUs == "false", 1 - FootyGames$lengthFraction, FootyGames$lengthFraction)
FootyGames$widthFraction = ifelse(FootyGames$playByUs == "false", 1 - FootyGames$widthFraction, FootyGames$widthFraction)
# Define the soccer field dimensions for ggsoccer display
stats_field = list()
stats_field$length = 110
stats_field$width = 75
stats_field$penalty_box_length = 16.5
stats_field$penalty_box_width = 40.32
stats_field$six_yard_box_length = 5.5
stats_field$six_yard_box_width = 18.32
stats_field$penalty_spot_distance = 11
stats_field$goal_width = 7.32
stats_field$origin_x = 0
stats_field$origin_y = 0

# Setup metaData for Season Reporting
# This has what labels to display, columns to include
# in stacking and whether proportion and raw counts can be shown
listGraphs <- list()
listGraphs[["AttackSides"]] <- list(columns=c("leftAttack", "rightAttack", "middleAttack"),
                                    levels=c("leftAttack", "middleAttack", "rightAttack"),
                                    labels=c("Left", "Middle", "Right"),
                                    title="Channel into Attacking Third",
                                    ylab="Attack Channel",
                                    type="both")
listGraphs[["PassSequence"]] <- list(columns=c("passZero", "pass1", "pass2_3", "pass4_6", "pass7Plus"),
                                     levels=c("pass7Plus", "pass4_6", "pass2_3", "pass1", "passZero"),
                                     labels=c("7+", "4 - 6", "2 - 3", "1", "0"),
                                     title="Number of Passes per Play",
                                     ylab="Possession",
                                     type="Both")
listGraphs[["TurnoverDirection"]] <- list(columns=c("backPassTurnover", "sidePassTurnover", "fwdPassTurnover"),
                                          levels=c("backPassTurnover", "sidePassTurnover", "fwdPassTurnover"),
                                          labels=c("Back", "Side", "Forward"),
                                          title="First Pass Direction After Turnover",
                                          ylab="Pass Direction",
                                          type="both")
listGraphs[["RegionDuration"]] <- list(columns=c("AttackDuration", "MidfieldDuration", "DefenceDuration"),
                                       levels=c("AttackDuration", "MidfieldDuration", "DefenceDuration"),
                                       labels=c("Attack", "Midfield", "Defence"),
                                       title="Time Spent in Each Region",
                                       ylab="Region",
                                       type="both")
listGraphs[["PassDirection"]] <- list(columns=c("totalBackPasses", "totalSidePasses", "totalFwdPasses"),
                                      levels=c("totalBackPasses", "totalSidePasses", "totalFwdPasses"),
                                      labels=c("Back", "Side", "Forward"),
                                      title="Direction of Passes",
                                      ylab="Direction",
                                      type="both")
listGraphs[["PassLength"]] <- list(columns=c("total10mPasses", "total10_20mPasses", "total20mPasses"),
                                   levels=c("total10mPasses", "total10_20mPasses", "total20mPasses"),
                                   labels=c("< 10m", "10 - 20m", "20m +"),
                                   title="Length of Forward Passes",
                                   ylab="Length",
                                   type="both")
listGraphs[["RegionDistance"]] <- list(columns=c("attackDistance", "midfieldDistance", "defenceDistance"),
                                       levels=c("attackDistance", "midfieldDistance", "defenceDistance"),
                                       labels=c("Attack", "Midfield", "Defence"),
                                       title="Distance of Ball in Regions",
                                       ylab="Region",
                                       type="both")
listGraphs[["RegionPass"]] <- list(columns=c("attackPasses", "midfieldPasses", "defencePasses"),
                                   levels=c("attackPasses", "midfieldPasses", "defencePasses"),
                                   labels=c("Attack", "Midfield", "Defence"),
                                   title="Number of Passes in Regions",
                                   ylab="Region",
                                   type="both")
listGraphs[["CrossfieldPlay"]] <- list(columns=c("crossfieldPlay"),
                                       levels=c("crossfieldPlay"),
                                       labels=c("CrossField"),
                                       title="Changes of Attack in Defence",
                                       ylab="Number",
                                       type="count")
listGraphs[["ppm"]] <- list(columns=c("ppm"),
                            levels=c("ppm"),
                            labels=c("Passes Per Minute"),
                            title="Passes Per Minute",
                            ylab="Passes",
                            type="count")
listGraphs[["velocity"]] <- list(columns=c("aveVelocity"),
                                 levels=c("aveVelocity"),
                                 labels=c("Velocity"),
                                 title="Velocity of Ball in Possession",
                                 ylab="Velocity",
                                 type="count")
listGraphs[["backKeeper"]] <- list(columns=c("backKeeperPasses"),
                                   levels=c("backKeeperPasses"),
                                   labels=c("Back to Keeper"),
                                   title="Number of Passes back to Keeper Box",
                                   ylab="Passes",
                                   type="count")
listGraphs[["RegionMoves"]] <- list(columns=c("MAPlay", "AMPlay", "MDPlay", "DMPlay"),
                                    levels=c("MAPlay", "AMPlay", "MDPlay", "DMPlay"),
                                    labels=c("Mid - Att", "Att - Mid", "Mid - Def", "Def - Mid"),
                                    title="Switch Between Regions",
                                    ylab="Plays",
                                    type="Both")
listGraphs[["goalRegion"]] <- list(columns=c("goalAttack", "goalMidfield", "goalDefence"),
                                   levels=c("goalAttack", "goalMidfield", "goalDefence"),
                                   labels=c("Attack", "Midfield", "Defence"),
                                   title="Zone Where Goals Start From",
                                   ylab="Goals",
                                   type="both")
listGraphs[["goalTransition"]] <- list(columns=c("goalTurnover", "goalRestart"),
                                       levels=c("goalTurnover", "goalRestart"),
                                       labels=c("Turnover", "Restart"),
                                       title="Play Where Goals Start From",
                                       ylab="Goals",
                                       type="both")

stat_value = "stack"

comp <- "2019 Regional Boys Under 14"

team <- "Bears"

footy_match_play$orderCol <- case_when(footy_match_play$competition == comp & footy_match_play$teamName == team ~ 0,
                                       footy_match_play$competition %in% c("2019 NSFA 1 Boys Under 13",
                                                                           "2019 NSFA 1 Girls Under 16", "2019 Boys Under 14") ~ 1,
                                       footy_match_play$competition=="2019 International Friendly Mens Open" ~ 2,
                                       footy_match_play$competition=="2019 Icc Football Mens Open" ~ 3,
                                       TRUE ~ 4)

# Define UI for application 
ui <- dashboardPage(
    dashboardHeader(title = Dashboard_name),
    # Side menu items
    dashboardSidebar(
        sidebarMenu(
            menuItem("Passing Breakdown", tabName = "passing"),
            menuItem("Heat map", tabName = "heatmap"),
            menuItem("Play map", tabName = "playmap"),
            menuItem("Team Statistics", tabName = "teamstats"),
            menuItem("Age Group Statistics", tabName = "agestats"),
            menuItem("Game Possession", tabName = "gamestats"),
            menuItem("Team Clusters", tabName = "cluster")
        )
    ),
    
    # Main body of page
    dashboardBody(
        tabItems(
            # First tab content - Passing Stats
            tabItem(tabName = "passing",
                    
                    # Boxes need to be put in a row (or column)
                    fluidPage(
                        # Filters at the top of the page 
                        fluidRow(
                            column(2, checkboxInput("Passes", "Include no passes",
                                                    value = FALSE)
                            ),
                            column(4, selectInput("FirstEvent",
                                                  "Phase Play:",
                                                  choices = c("All" = "all",
                                                              "Turnover" = "first touch",
                                                              "Throw In" = "throw in", 
                                                              "Goal Kick" = "goal kick", 
                                                              "Corner Kick" = "corner kick", 
                                                              "Kick Off" = "kick off",    
                                                              "Keeper Punt" = "keeper punt",
                                                              "Foul Restart" = "foul"),
                                                  selected = "all" )
                            ),
                            column(4, selectInput("Opponent",
                                                  "Opponent:",
                                                  choices = c("All", unique(footy_match_play$theirName)),
                                                  selected = "All" ) 
                            )
                        ), # end fluidRow
                        
                        # Show a plot of the generated distribution
                        fluidRow(
                            plotOutput("passPlot")
                        ),
                        br(),
                        p("This website is part of the capstone project for iLab1 in the Masters of Data Science and Innovation
                           MDSI) program at the University of Technology Sydney (UTS).
                           My project was to analyse data from a ", a(href="https://footystats-996cf.firebaseapp.com/", "football tracking app",
                                                                              title="https://footystats-996cf.firebaseapp.com/"), "and
                           create a tool to aid coaches in understanding the team play, opposition patterns
                           and to feed improvements back into training."),
                        p("All data shown here is an amalgamation of various teams to
                           demonstrate visualisations, but keeping individual team strategies
                           hidden.  The graph above allows the coach to understand how teams hold possession.
                           It shows the percentage of plays for each number of passes in a play sequence.  The more passes in the play
                           sequence, generally means better possession and control.")
                    ) ),
            
            # Second tab content - heat maps
            tabItem(tabName = "heatmap",
                    # Filters at top of page
                    fluidPage(
                        fluidRow(
                            column(4, 
                                   checkboxInput("hmPasses", "Include no passes",
                                                 value = FALSE),
                                   br(),
                                   checkboxInput("hmJustFirst", "Show just 1st contact",
                                                 value = FALSE,)
                            ),
                            column(4, 
                                   selectInput("hmFirstEvent",
                                               "Phase Play:",
                                               choices = c("All" = "all",
                                                           "Turnover" = "first touch",
                                                           "Throw In" = "throw in", 
                                                           "Goal Kick" = "goal kick", 
                                                           "Corner Kick" = "corner kick", 
                                                           "Kick Off" = "kick off",    
                                                           "Keeper Punt" = "keeper punt",
                                                           "Foul Restart" = "foul"),
                                               selected = "all" ),
                                   br(),
                                   selectInput("hmResult",
                                               "Play Result:",
                                               choices = c("All" = "all",
                                                           "Attacking 3rd" = "AttackIncursion",
                                                           "Goal" = "goal"),
                                               selected = "all" )
                            ),
                            column(4, 
                                   selectInput("hmOpponent",
                                               "Opponent:",
                                               choices = c("All", unique(FootyGames$theirName)),
                                               selected = "All" ),
                                   br(),
                                   selectInput("hmMatch",
                                               "Match:",
                                               choices = c("All", rev(unique(FootyGames$matchName))),
                                               selected = "All"
                                   )
                            )
                        ),
                        # Then heat maps side by side
                        fluidRow(column(6, plotOutput("heatPlot")),
                                 column(6, plotOutput("heatPlota")) 
                        ),
                        br(),
                        p("The above shows the ball position for each team, as a density map. 
                          Filters can be applied to show starting positions of plays, play results
                          and down to individual teams or games.  The Us team is shown
                          running left to right (scoring on the right) whilst the opposition
                          is running right to left.")
                        
                    ) # fluidpage tabitem 2
            ), # tab item 2 (Heat maps)
            
            # Third tab content - Playmap 
            tabItem(tabName = "playmap",
                    fluidPage(
                        fluidRow(
                            column(6, sliderInput("pmGameTime", "Time (%):",
                                                  min = 0, max = 100, value = c(0, 100)
                            )
                            ),
                            column(6, selectInput("pmFirstEvent",
                                                  "Phase Play:",
                                                  choices = c("All" = "all",
                                                              "Turnover" = "first touch",
                                                              "Throw In" = "throw in", 
                                                              "Goal Kick" = "goal kick", 
                                                              "Corner Kick" = "corner kick", 
                                                              "Kick Off" = "kick off",    
                                                              "Keeper Punt" = "keeper punt",
                                                              "Foul Restart" = "foul"),
                                                  selected = "all" )
                            )
                        ),
                        fluidRow(column(6, selectInput("pmMatch",
                                                       "Match:",
                                                       choices = c(rev(unique(FootyGames$matchName)))
                        )
                        ),
                        column(6, selectInput("pmResult",
                                              "Play Result:",
                                              choices = c("All" = "all",
                                                          "Attacking 3rd" = "AttackIncursion",
                                                          "Goal" = "goal"),
                                              selected = "all" )
                        )),
                        fluidRow(plotOutput("playPlot")),
                        br(),
                        p("Individual plays can be analysed here. The red lines are Us (scoring on the right) and the
                          blue is the Opposition (scoring in the left hand goal).  Plays can be filtered by event,
                          time within the game, and result.")
                    ) # fluidpage tabitem 3
            ), # tab item 3
            
            # Fourth tab content - Team Stats
            tabItem(tabName = "teamstats",
                    fluidPage(
                        fluidRow(
                            column(6,
                                   selectInput("tsAttribute",
                                               "Statistic:",
                                               choices = c("Passing Sequences" = "PassSequence",
                                                           "Turnover Direction" = "TurnoverDirection", 
                                                           "Attack Channel" = "AttackSides", 
                                                           "Pass Direction" = "PassDirection", 
                                                           "Pass Length" = "PassLength",
                                                           "Region Duration" = "RegionDuration",
                                                           "Region Distance" = "RegionDistance", 
                                                           "Region Passes" = "RegionPass",  
                                                           "Passes Per Minute" = "ppm", 
                                                           "Play Speed" = "velocity",
                                                           "Crossfield Plays" = "CrossfieldPlay",
                                                           "Back to Keeper" = "backKeeper",
                                                           "Region Moves" = "RegionMoves",
                                                           "Goals (Starting Region)" = "goalRegion", 
                                                           "Goals (Starting Event)" = "goalTransition"),
                                               selected = "PassSequence" )
                            ),
                            column(6, 
                                   conditionalPanel( condition = "output.tsShowRB != 'count'",
                                                     radioButtons("tsType", "Statistic type:",
                                                                  c("Proportions" = "fill",
                                                                    "Counts" = "stack"),
                                                                  selected = "fill")
                                   )
                            ),
                            width = 3
                        ),
                        
                        # Display the plot
                        fluidRow(
                            plotOutput("teamPlot")
                        ),
                        br(),
                        p("This tab allows various aspects of play to be 
                          compared between Our team and the opposition per game. 
                          The text colours of the games indicate blue - win, black - draw and red - loss.")
                    ) # fluidpage tabitem 4 - Team Plots
            ), # tab item 4
            
            # Fifth tab content - Age Statistics
            tabItem(tabName = "agestats",
                    fluidPage(
                        fluidRow(
                            column(6,
                                   selectInput("asAttribute",
                                               "Statistic:",
                                               choices = c("Passing Sequences" = "PassSequence",
                                                           "Turnover Direction" = "TurnoverDirection", 
                                                           "Attack Channel" = "AttackSides", 
                                                           "Pass Direction" = "PassDirection", 
                                                           "Pass Length" = "PassLength",
                                                           "Region Duration" = "RegionDuration",
                                                           "Region Distance" = "RegionDistance", 
                                                           "Region Passes" = "RegionPass",  
                                                           "Passes Per Minute" = "ppm", 
                                                           "Play Speed" = "velocity",
                                                           "Back to Keeper" = "backKeeper",
                                                           "Crossfield Plays" = "CrossfieldPlay",
                                                           "Region Moves" = "RegionMoves",
                                                           "Goals (Starting Region)" = "goalRegion", 
                                                           "Goals (Starting Event)" = "goalTransition"),
                                               selected = "PassSequences" )
                            ),
                            column(6, 
                                   conditionalPanel( condition = "output.asShowRB != 'count'",
                                                     radioButtons("asType", "Statistic type:",
                                                                  c("Proportions" = "fill",
                                                                    "Counts" = "stack"),
                                                                  selected = "fill")
                                   )
                            ),
                            width = 3
                        ),
                        
                        # Plot Results
                        fluidRow(
                            plotOutput("agePlot")
                        ),
                        br(),
                        p("This tab allows various aspects of play to be 
                          compared between different ages.  This allows comparison of 
                          where teams can aim for and what higher levels teams improve on.")
                    ) # fluidpage tabitem 5 - Age
            ), # tab item 5
            
            # Sixth tab content - Game Possession
            tabItem(tabName = "gamestats",
                    fluidPage(
                        fluidRow(
                            column(6, 
                                   selectInput("gsMatch",
                                               "Match:",
                                               choices = c(rev(unique(FootyGames$matchName)))
                                   )
                            ),
                            column(6,
                                   selectInput("gsType",
                                               "Possession:",
                                               choices = c("All" = "all",
                                                           "Defensive Possession" = "defPossession",
                                                           "Attacking Possession" = "attPossession"),
                                               selected = "all"
                                   )
                            )
                        ),
                        
                        plotOutput("gamePlot"),
                        br(),
                        p("Ball Possession graph for a game.  Total, Attacking, and 
                          Defensive possession can be graphed.")
                    ) # fluidpage  Game Stats
            ), # tab item 6
            
            # Seventh tab content - Clustering
            tabItem(tabName = "cluster",
                    fluidPage(
                        fluidRow(
                            column(6, 
                                   checkboxInput("cpTopFlight", "Include Top-Flight Teams",
                                                 value = TRUE)
                            ),
                            column(6, 
                                   radioButtons("cpDisplay", "Display Options:",
                                                c("Teams and Variables" = "none",
                                                  "Teams Only" = "var",
                                                  "Variables Only" = "ind"),
                                                selected = "none")
                            )
                        ),
                        tabsetPanel(
                            tabPanel("PCA", fluidPage(plotOutput("PCAPlot"),
                                     br(),
                                     p("Principal Component Analysis (PCA) is a mathematical technique that converts a set of attributes that may be correlated into a set 
                                       of uncorrelated variables.  It produces the same number of components as there were original attributes.  However, the transformation to the new principal components is done so that 
                                       the first principal component explains the largest variance in the data.  Each subsequent 
                                       component then explains in order the largest remaining variance.  In this way, dimensions can be reduced by 
                                       ignoring components that contribute little or no variance."),
                                     p("The advantage here is that the first 2 components can be mapped that explain the greatest variances between them
                                       (about 50% of the total variance in the model).
                                       It also allows mapping of the contributions of the original variables on the graph,
                                       showing that longer pass sequences are typical of the higher level teams."))
                                     ),
                            tabPanel("Hierarchy", fluidPage(plotOutput("HierPlot"),
                                     br(),
                                     p("Agglomerative Hierarchical Clustering is another technique for clustering.
                                       Here, each team starts as its own cluster and then gets merged to its closest neighbour.  In this way a hierarchy is built up, hence the name.  
                                       The teams are clustered into 3 groups, expected to be the top teams, next best group
                                       and then the rest.  Groupings are somewhat similar to PCA.")))
                        ) # tabPanel End
                    ) # fluidpage end
                    
            ) # tab item 7
            
        ) # tabitems
        
    ) # dashboardBody
) # dashboard page


# Define server logic required to draw a histogram
server <- function(input, output, session) {
    observe({
        query <- parseQueryString(session$clientData$url_search)
    })
    
    # Dashboard 1 - Passing Breakdown
    # -------------------------------
    output$passPlot <- renderPlot({
        
        data <- footy_match_play
        # filter data based on inputs set
        if (input$FirstEvent != "all") {
            if (input$FirstEvent == "foul") {
                eventsList <- c("offside restart", "direct restart", "indirect restart")
            } else {
                eventsList <- input$FirstEvent
            }
            data <- data %>% filter(firstEvent %in% eventsList)
        }
        # Show zero passes
        if (!input$Passes) {
            data <- data %>% filter(totalPasses > 0)
        }
        if (input$Opponent != "All") {
            data <- data %>% filter(theirName == input$Opponent)
        }
        ggplot(data %>%
                   ungroup() %>% 
                   group_by(playByUs) %>%
                   mutate(tp = n() ) %>%
                   group_by(totalPasses, playByUs) %>%
                   summarise(perc = n() / max(tp), cnt = n(), max_tp = max(tp) ),
               aes(x=factor(totalPasses), y= perc) ) +
            geom_bar(stat = "identity", fill = "light blue") +
            theme(plot.title = element_text(hjust=0.5)) + 
            scale_y_continuous(labels = percent_format(accuracy = 1)) +
            labs(x = "Number of Passes", y = "Percentage") +
            facet_grid(rows = vars(factor(playByUs,
                                          ordered = TRUE,
                                          levels = c("true", "false"),
                                          labels = c("Us", "Opponents"))) )
        
    })
    
    # Dashboard 2 - Heat Map
    # ----------------------
    # First create reactive variable that is then used
    # in each heat map plot
    my_heat <- reactive({
        
        data <- FootyGames
        if (input$hmFirstEvent != "all") {
            if (input$hmFirstEvent == "foul") {
                eventsList <- c("offside restart", "direct restart", "indirect restart")
            } else {
                eventsList <- input$hmFirstEvent
            }
            data <- data %>% filter(firstEvent %in% eventsList)
        }
        if (!input$hmPasses) {
            data <- data %>% filter(numberPasses > 0)
        }
        
        if (input$hmResult != "All") {
            if (input$hmResult == "goal") {
                data <- data %>% filter(isGoal == 1) 
            } else if (input$hmResult == "AttackIncursion") {
                data <- data %>% filter(isAttackIncursion == 1)
            }
        }
        
        if (input$hmJustFirst) {
            data <- data %>% filter(sequenceNumber == 1)
        }
        if (input$hmOpponent != "All") {
            data <- data %>% filter(theirName == input$hmOpponent)
        }
        if (input$hmMatch != "All") {
            data <- data %>% filter(matchName == input$hmMatch)
        }
        
        data
    })
    
    # Heat map for Our Team
    # my_heat reactive variable takes care of the filtering
    output$heatPlot <- renderPlot({
        
        # Final filter on Our Team
        data <- my_heat() %>% filter(playByUs == "true")
        pppxy <- ppp(data$lengthMetres, data$widthMetres,
                     c(0,110), c(0,75), checkdup=FALSE)
        plot(density(pppxy, diggle=TRUE), multiplot=FALSE,
             main = "Us (running L to R)")
        
    })
    
    # Heat map for Opposition Team
    output$heatPlota <- renderPlot({
        
        # Filter on Opposition team
        data <- my_heat() %>% filter(playByUs == "false")
        pppxy <- ppp(data$lengthMetres, data$widthMetres,
                     c(0,110), c(0,75), checkdup=FALSE)
        plot(density(pppxy, diggle=TRUE), multiplot=FALSE,
             main = "Opposition (running R to L)")
        
    })
    
    # Dashboard 3 - Play Map
    # ----------------------
    # Create reactive variable first to do filtering
    my_play <- reactive({
        
        data <- FootyGames
        if (input$pmFirstEvent != "all") {
            if (input$pmFirstEvent == "foul") {
                eventsList <- c("offside restart", "direct restart", "indirect restart")
            } else {
                eventsList <- input$pmFirstEvent
            }
            data <- data %>% filter(firstEvent %in% eventsList)
        }
        if (input$pmResult != "All") {
            if (input$pmResult == "goal") {
                data <- data %>% filter(isGoal == 1) 
            } else if (input$pmResult == "AttackIncursion") {
                data <- data %>% filter(isAttackIncursion == 1)
            }
        }
        data <- data %>% filter(matchName == input$pmMatch)
        start_time = min(data$epochTime)
        end_time = max(data$epochTime)
        
        data <- data %>% filter(epochTime >= start_time + (end_time - start_time) * input$pmGameTime[1]/100 &
                                    epochTime <= start_time + (end_time - start_time) * input$pmGameTime[2]/100)
        
        data
    })
    
    # Then display the plot  
    output$playPlot <- renderPlot({
        
        our_goals = sum(FootyGames[FootyGames$matchName == input$pmMatch & FootyGames$playByUs == "true" &
                                       FootyGames$adjEventName == "goal", ]$isGoal)
        their_goals = sum(FootyGames[FootyGames$matchName == input$pmMatch & FootyGames$playByUs == "false" &
                                         FootyGames$adjEventName == "goal", ]$isGoal)
        data <- my_play() 
        ggplot(data %>% group_by(gameID, playNumber) %>%
                   arrange(gameID, playNumber, sequenceNumber) %>%
                   mutate(x1 = lead(lengthMetres),
                          y1 = lead(widthMetres)) %>%
                   ungroup(),
               aes(x = lengthMetres, y = widthMetres, 
                   xend = x1, yend = y1)) +
            annotate_pitch(dimensions = stats_field, fill = "light green") +
            geom_segment(data = . %>% filter(sequenceNumber != maxSequenceNumber),
                         mapping = aes(color = playByUs),
                         arrow = arrow(length = unit(0.2, "cm"),
                                       type = "open")) +
            geom_point(color="grey50") + 
            theme_pitch() +
            scale_color_manual(values = c("true" = "red", "false" = "blue")) +
            theme(legend.position = "none",
                  plot.title = element_text(hjust = 0.5, face = "bold")) +
            labs(title=paste("Result:", our_goals, '-', their_goals ))
        
    })
    
    # Dashboard 4 - Team Statistics
    # -----------------------------
    # Team Stats filtering as reactive variable
    my_team <- reactive({
        data <- footy_match_play %>%
            mutate(goalAttack = ifelse(isGoal == 1 & Area == "Attack", 1, 0),
                   goalMidfield = ifelse(isGoal == 1 & Area == "Midfield", 1, 0),
                   goalDefence = ifelse(isGoal == 1 & Area == "Defence", 1, 0),
                   goalTurnover = ifelse(isGoal == 1 & firstEvent == "first touch", 1, 0),
                   goalRestart = ifelse(isGoal == 1 & firstEvent != "first touch", 1, 0)
            )
        
        data
    })
    
    # Conditional display of Radio buttons on Team Stats
    output$tsShowRB <- reactive({
        listGraphs[[input$tsAttribute]]$type
    })
    # Make sure element stay active
    outputOptions(output, "tsShowRB", suspendWhenHidden = FALSE)
    
    # Display Team statistics
    output$teamPlot <- renderPlot({
        
        # Roll to game level
        game_level <- my_team() %>% filter(playNumber > 0) %>%
            mutate(gameDate = substr(gameDate, 1, 10)) %>%
            group_by(gameID, matchName, playByUs, gameDate) %>%
            summarise_if(is.numeric, list(sum), na.rm = TRUE) %>%
            mutate(ppm = totalPasses/(possessionDuration/60),
                   ppmAttack = attackPasses/(AttackDuration/60),
                   ppmMidfield = midfieldPasses/(MidfieldDuration/60),
                   ppmDefence = defencePasses/(DefenceDuration/60),
                   aveVelocity = distance/(possessionDuration),
                   attackVelocity = attackDistance/(AttackDuration),
                   midfieldVelocity = midfieldDistance/(MidfieldDuration),
                   defenceVelocity = defenceDistance/(DefenceDuration)
            )
        
        # Record which games were won, lost, drawn to change
        # colour of match text (black = Draw, Red = Loss, Blue = Win)
        HomeWins <- game_level  %>% 
            group_by(matchName, gameDate)  %>%
            summarise(wld = case_when(sum(ifelse(playByUs=="true", isGoal, 0))==sum(ifelse(playByUs=="false", isGoal, 0)) ~ 2,
                                      sum(ifelse(playByUs=="true", isGoal, 0)) < sum(ifelse(playByUs=="false", isGoal, 0)) ~ 1,
                                      TRUE ~ 3)) 
        myPalette <- c("red", "black", "blue")
        
        # Are both counts and proportions allowed?
        if (listGraphs[[input$tsAttribute]]$type != "count") {
            p = ggplot(game_level %>% 
                           gather(key = "Key", value = "Measurement",
                                  listGraphs[[input$tsAttribute]]$columns),
                       aes(x=reorder(matchName, gameDate, FUN=max), y= Measurement, 
                           fill = factor(Key, ordered = TRUE,
                                         levels = listGraphs[[input$tsAttribute]]$levels,
                                         labels = listGraphs[[input$tsAttribute]]$labels) 
                       ) 
            ) + 
                geom_bar(position = input$tsType, stat = "identity") +
                scale_fill_discrete(name="") +
                theme(axis.text.x = element_text(angle = 45, hjust = 1,
                                                 color=myPalette[unlist(HomeWins[order(HomeWins$gameDate), 3])]),
                      plot.title = element_text(hjust=0.5),
                      legend.position = "bottom") + 
                labs(x = "Team", y = listGraphs[[input$tsAttribute]]$ylab, 
                     title = listGraphs[[input$tsAttribute]]$title,
                     legend = "") +
                # Print Us and Opposition on same graph as facets
                facet_grid(rows = vars(factor(playByUs,
                                              ordered = TRUE,
                                              levels = c("true", "false"),
                                              labels = c("Us", "Opponents"))) )
            
            if (input$tsType == "fill") {
                p + scale_y_continuous(labels = percent_format(accuracy = 1))
            } else {
                p
            }  
            
            # Otherwise only counts are shown
        } else {
            ggplot(game_level %>% 
                       gather(key = "Key", value = "Measurement",
                              listGraphs[[input$tsAttribute]]$columns),
                   aes(x=reorder(matchName, gameDate, FUN=max), y= Measurement, 
                       fill = factor(Key, ordered = TRUE,
                                     levels = listGraphs[[input$tsAttribute]]$levels,
                                     labels = listGraphs[[input$tsAttribute]]$labels) 
                   ) ) + 
                geom_bar(position = "stack", stat = "identity") +
                scale_fill_discrete(name="") +
                theme(axis.text.x = element_text(angle = 45, hjust = 1,
                                                 color=myPalette[unlist(HomeWins[order(HomeWins$gameDate), 3])]),
                      plot.title = element_text(hjust=0.5),
                      legend.position = "none") + 
                labs(x = "Team", y = listGraphs[[input$tsAttribute]]$ylab, 
                     title = listGraphs[[input$tsAttribute]]$title,
                     legend = "") +
                facet_grid(rows = vars(factor(playByUs,
                                              ordered = TRUE,
                                              levels = c("true", "false"),
                                              labels = c("Us", "Opponents"))) )
        }
        
    })
    
    # Dashboard 5 - Age Statistics
    # ----------------------------
    # Conditional display of Radio buttons on Age Stats
    output$asShowRB <- reactive({
        listGraphs[[input$asAttribute]]$type
    })
    # Make sure element stay active
    outputOptions(output, "asShowRB", suspendWhenHidden = FALSE)
    
    output$agePlot <- renderPlot({
        game_level <- footy_match_age 
        
        # If Proportions and Counts allowed, print here
        # with a legend
        if (listGraphs[[input$asAttribute]]$type != "count") {
            p = ggplot(game_level %>% 
                           gather(key = "Key", value = "Measurement",
                                  listGraphs[[input$asAttribute]]$columns),
                       aes(x=age_group, y= Measurement, 
                           fill = factor(Key, ordered = TRUE,
                                         levels = listGraphs[[input$asAttribute]]$levels,
                                         labels = listGraphs[[input$asAttribute]]$labels) 
                       ) ) + 
                geom_bar(position = input$asType, stat = "identity") +
                scale_fill_discrete(name="") +
                theme(axis.text.x = element_text(angle = 45, hjust = 1),
                      plot.title = element_text(hjust=0.5),
                      legend.position = "bottom") + 
                labs(x = "Age Group", y = listGraphs[[input$asAttribute]]$ylab, 
                     title = listGraphs[[input$asAttribute]]$title,
                     legend = "") +
                facet_grid(rows = vars(factor(playByUs,
                                              ordered = TRUE,
                                              levels = c("true", "false"),
                                              labels = c("Us", "Opponents"))) )
            if (input$asType == "fill") {
                p + scale_y_continuous(labels = percent_format(accuracy = 1))
            } else {
                p
            }
            # Else, graph just counts and no legend required 
        } else {
            ggplot(game_level %>% 
                       gather(key = "Key", value = "Measurement",
                              listGraphs[[input$asAttribute]]$columns),
                   aes(x=age_group, y= Measurement, 
                       fill = factor(Key, ordered = TRUE,
                                     levels = listGraphs[[input$asAttribute]]$levels,
                                     labels = listGraphs[[input$asAttribute]]$labels) 
                   ) ) + 
                geom_bar(position = "stack", stat = "identity") +
                scale_fill_discrete(name="") +
                theme(axis.text.x = element_text(angle = 45, hjust = 1),
                      plot.title = element_text(hjust=0.5),
                      legend.position = "none") + 
                labs(x = "Age Group", y = listGraphs[[input$asAttribute]]$ylab, 
                     title = listGraphs[[input$asAttribute]]$title,
                     legend = "") +
                facet_grid(rows = vars(factor(playByUs,
                                              ordered = TRUE,
                                              levels = c("true", "false"),
                                              labels = c("Us", "Opponents"))) )
        }
    })
    
    # Dashboard 6 - Game Possession
    # -----------------------------
    output$gamePlot <- renderPlot({
        data <- FootyGames %>% filter(matchName == input$gsMatch)
        if (input$gsType != "all") {
            if (input$gsType == "defPossession") {
                data <- data %>%
                    gather(key = "TeamPossession", value = "TimePossesion",
                           ourDefensivePossession, theirDefensivePossession)
            } else {
                data <- data  %>%
                    gather(key = "TeamPossession", value = "TimePossesion",
                           ourAttackingPossession, theirAttackingPossession)
            }
        } else {
            data <- data  %>%
                gather(key = "TeamPossession", value = "TimePossesion",
                       ourPossession, theirPossession)
            
        }
        
        ggplot(data ,
               aes(x=epochTime, y= TimePossesion, colour=TeamPossession)) +
            geom_line() 
    })
    
    # Dashboard 7 - Clustering
    # ------------------------
    # Create Reactive variable first to do filtering
    my_cluster <- reactive({
        
        # Filter on competitions to cluster
        if (input$cpTopFlight) {
            ftc_data <- ftc_cluster %>%   
                filter(competition %in% c(top_flight_comps, our_comp))
        } else {
            ftc_data <- ftc_cluster %>%   
                filter(competition %in% c(our_comp))
        }
        ftc_data
    })
    
    # Then feed to PCA and display
    output$PCAPlot <- renderPlot({
        
        # Principal Component processing on numeric fields 
        ftpca.pr <- prcomp(my_cluster()[, -c(1:4)], center = TRUE, scale = TRUE)
        
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
        p = fviz_pca_biplot(ftpca.pr, geom = c("point", "text"),
                            geom.var = c("text"),
                            geom.ind = c("point"),
                            select.var = list(name=top20),
                            col.var="steel blue",
                            pointshape = 21, 
                            pointsize = 4.5, 
                            invisible=input$cpDisplay,
                            title = our_comp,
                            fill.ind = ifelse(my_cluster()$competition==our_comp, 
                                              paste(ifelse(my_cluster()$playByUs == "true", "Us", "Opposition")),
                                              "Top-Flight Teams"), 
                            alpha.var = 0.1, 
                            repel = TRUE,
                            mean.point = FALSE)+
            theme_minimal()+
            labs(fill = "Team")
        
        if (input$cpDisplay == "ind") {
            p
        } else {
            p + 
                geom_text_repel(aes(label=my_cluster()$teamName), 
                                box.padding = 0.1, point.padding =0.1, 
                                segment.color = 'grey50', seed = 13,
                                size=3.5) 
        }
        
    })
    
    # Clustering - Hierarchy Tab
    output$HierPlot <- renderPlot({
        
        m<-as.matrix(my_cluster()[ -c(1:4)])
        rownames(m) <- my_cluster()$teamName
        # Scale all columns so each feature has equal importance in distance
        m <- scale(m)
        
        # Create Cosine Distance Function
        cosineSim <- function(x){
            as.dist(x%*%t(x)/(sqrt(rowSums(x^2) %*% t(rowSums(x^2)))))
        }
        # Compute cosine distance of each team
        cs <- cosineSim(m)
        cd <- 1-cs
        
        # For Agglomerative Hierarchical Clustering,
        # create groups to display
        groups <- hclust(cd,method="ward.D")
        # Visualize hierarchical clustering
        # Again 3 groups
        fviz_dend(groups, k = 3, 
                  cex = 0.8, 
                  main = our_comp,
                  labels_track_height=3,
                  horiz= TRUE, rect = TRUE # Add rectangle around groups
        )
        
    })
    
}


# Run the application 
shinyApp(ui = ui, server = server)
