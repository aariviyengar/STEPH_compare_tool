library(shiny)
library(tidyverse)
library(ggimage)
library(gt)
library(ggthemes)
library(gtExtras)
library(dplyr)
library(hoopR)
library(rvest)
all_data <- read_csv("team_data.csv")
model_data <- read_csv("model_data.csv")
ui <- fluidPage(

    # Application title
    titlePanel("Compare Two Historical NBA Teams"),
    sidebarLayout(
      sidebarPanel(
        #application inputs for teams to compare
        sliderInput(inputId = "home_season",label="Home Team Season",min=2003,max=2024,step=1,value=2003,sep=""),
        selectInput(inputId = "home_team",label="Home Team Name",choices = all_data$Team),
        sliderInput(inputId = "away_season",label="Away Team Season",min=2003,max=2024,step=1,value=2003,sep=""),
        selectInput(inputId = "away_team",label="Away Team Name",choices = all_data$Team),
        submitButton(text="Create Table")),
      mainPanel(
        #display table and model's prediction
        gt_output("comparison_table"),
        span(textOutput("prediction"),style="font-size:20px;text-align:center")
      )
    )

    
)

server <- function(input, output, session){
  home_playoff_win_model <- glm(home_series_win~h_net_rating++h_off_eFG+h_off_tov_rate+h_off_reb_rate+
                                  h_off_ft_fga_ratio+h_def_eFG+h_def_tov_rate+h_def_reb_rate+h_def_ft_fga_ratio+
                                  h_top_3_vorp+h_best_vorp+h_clutch_plus_minus+a_net_rating+
                                  a_off_eFG+a_off_tov_rate+a_off_reb_rate+a_off_ft_fga_ratio+a_def_eFG+a_def_tov_rate+a_def_reb_rate+          
                                  a_def_ft_fga_ratio+a_top_3_vorp+a_best_vorp+a_clutch_plus_minus,data=model_data,family="binomial")
  
  rename_home <- function(x){
    x <- x|>
      rename("h_net_rating"="net_rating",
             "h_off_eFG"="off_eFG","h_off_tov_rate"="off_tov_rate","h_off_reb_rate"="off_reb_rate",
             "h_off_ft_fga_ratio"="off_ft_fga_ratio","h_def_eFG"="def_eFG","h_def_tov_rate"="def_tov_rate",
             "h_def_reb_rate"="def_reb_rate","h_def_ft_fga_ratio"="def_ft_fga_ratio","h_top_3_players"="top_3_players",
             "h_top_3_vorp"="top_3_vorp","h_best_player"="best_player","h_best_vorp"="best_vorp","h_clutch_plus_minus"="clutch_plus_minus")
    
  }
  
  rename_away <- function(x){
    x <- x|>
      rename("a_net_rating"="net_rating",
             "a_off_eFG"="off_eFG","a_off_tov_rate"="off_tov_rate","a_off_reb_rate"="off_reb_rate",
             "a_off_ft_fga_ratio"="off_ft_fga_ratio","a_def_eFG"="def_eFG","a_def_tov_rate"="def_tov_rate",
             "a_def_reb_rate"="def_reb_rate","a_def_ft_fga_ratio"="def_ft_fga_ratio","a_top_3_players"="top_3_players",
             "a_top_3_vorp"="top_3_vorp","a_best_player"="best_player","a_best_vorp"="best_vorp","a_clutch_plus_minus"="clutch_plus_minus")
  }
  #create dataset to be used in table
  home_away <- reactive({
    req(input$home_team)
    home_team_data <- all_data|>
    filter(Team==input$home_team&season==input$home_season)
  
  away_team_data <- all_data|>
    filter(Team==input$away_team&season==input$away_season)
  
  home_away <- rbind(home_team_data,away_team_data)|>
    select(-c("...1"))
  i = 1
  home_away[nrow(home_away)+1,] <- NA
  #create column to identify which team is better in each stat
  while(i<ncol(home_away)+1){
    if(i==1){
      home_away[[i]][[3]] <- "home_better"
    } else if(i==2|i==3|i==5|i==6|i==8|i==9|i==13|i==15|i==16){
      if(home_away[[i]][[1]]>home_away[[i]][[2]]){
        home_away[[i]][[3]] <- 1
      } else{
        home_away[[i]][[3]] <- 0
      }
    } else if(i==4|i==7|i==10){
      if(home_away[[i]][[1]]<home_away[[i]][[2]]){
        home_away[[i]][[3]] <- 1
      } else{
        home_away[[i]][[3]] <- 0
      }
    } else if(i==11){
      home_away[[i]][[3]] <- 2
    }
    i = i+1
  }
  
  #get logos to eventually add to team column
  logos <- read_csv("logos.csv")
  
  
  home_away <- home_away|>
    #format data as percents - can't be done later because of the pivot
    mutate(net_rating = as.character(net_rating),
           off_eFG = paste(off_eFG*100,"%",sep=""),
           off_tov_rate = paste(off_tov_rate,"%",sep=""),
           off_reb_rate = paste(off_reb_rate,"%",sep=""),
           off_ft_fga_ratio = paste(off_ft_fga_ratio*100,"%",sep=""),
           def_eFG = paste(def_eFG*100,"%",sep=""),
           def_tov_rate = paste(def_tov_rate,"%",sep=""),
           def_reb_rate = paste(def_reb_rate,"%",sep=""),
           def_ft_fga_ratio = paste(def_ft_fga_ratio*100,"%",sep=""),
           top_3_vorp = as.character(top_3_vorp),
           best_vorp = as.character(best_vorp),
           clutch_plus_minus = as.character(clutch_plus_minus),
           season = as.character(season))|>
    #format best player and big three data
    mutate(best = paste(best_player," (",best_vorp,")",sep=("")))|>
    mutate(big_three = paste(top_3_players," (",top_3_vorp, ")",sep=("")))|>
    select(-c(best_player,best_vorp,top_3_players,top_3_vorp))|>
    #rename now because they will be row names later so can't change them in GT
    rename("Net Rating" = "net_rating",
           "Season" = "season",
           "Offensive Effective Field Goal Percentage" = "off_eFG",
           "Offensive Turnover Rate" = "off_tov_rate",
           "Offensive Rebound Rate" = "off_reb_rate",
           "Offensive Free Throw Attempt Rate" = "off_ft_fga_ratio",
           "Defensive Effective Field Goal Percentage" = "def_eFG",
           "Defensive Turnover Rate" = "def_tov_rate",
           "Defensive Rebound Rate" = "def_reb_rate",
           "Defensive Free Throw Attempt Rate" = "def_ft_fga_ratio",
           "Best Player (VORP)" = "best",
           "Big Three (Total VORP)" = "big_three",
           "Clutch Plus-Minus" = "clutch_plus_minus")|>
    #add logos to team column
    left_join(logos, join_by("Team" == "team_name_full")) |>
    mutate(logo = glue::glue("<img src='{logo}' style='height: 20px; width: auto; vertical-align: -15%;'> {Team}"))
  
  

  
  home_away <- home_away|>
    select(-c("Team"))|>
    rename("Team"="logo")
  
  
  
  #make the table vertical
  pivoted_table <- pivot_longer(home_away,cols=c("Team","Season","Net Rating","Offensive Effective Field Goal Percentage",
                                                 "Offensive Turnover Rate","Offensive Rebound Rate","Offensive Free Throw Attempt Rate",
                                                 "Defensive Effective Field Goal Percentage","Defensive Turnover Rate",
                                                 "Defensive Rebound Rate","Defensive Free Throw Attempt Rate","Big Three (Total VORP)",
                                                 "Best Player (VORP)","Clutch Plus-Minus"))
  home <- slice_head(pivoted_table, n=nrow(pivoted_table)/3)
  away <- slice(pivoted_table,15:28)
  home_away <- left_join(home,away,by=c("name"))
  home_away <- left_join(home_away,slice_tail(pivoted_table,n=nrow(pivoted_table)/3),by=c("name"))
  })
  model_prediction <- reactive({
    req(input$home_team)
    #get data to apply to model
    home_team_data <- all_data|>
      filter(Team==input$home_team&season==input$home_season)
    
    away_team_data <- all_data|>
      filter(Team==input$away_team&season==input$away_season)
    
    home_away <- rbind(home_team_data,away_team_data)
    home_team_data <- home_team_data|>
      rename("home_team"="Team","home_season"="season")
    home_team_data <- rename_home(home_team_data)
    away_team_data <- away_team_data|>
      rename("away_team"="Team","away_season"="season")
    away_team_data <- rename_away(away_team_data)
    playoff_sim <- cbind(home_team_data,away_team_data)|>
      select(-c("...1"))
    
    #use model to get home win probability
    playoff_sim$home_win_probability <- predict(home_playoff_win_model,newdata = playoff_sim,type="response")
    
    #convert win probability into easy to digest series prediction - increments of 0.125 since 8 possible outcomes
    playoff_sim <- playoff_sim|>
      mutate(prediction = case_when(home_win_probability>0.875~paste(home_season,home_team,"in","4", sep=" "),
                                    home_win_probability<0.875&home_win_probability>0.75~paste(home_season,home_team,"in","5", sep=" "),
                                    home_win_probability<0.75&home_win_probability>0.625~paste(home_season,home_team,"in","6", sep=" "),
                                    home_win_probability<0.625&home_win_probability>0.50~paste(home_season,home_team,"in","7", sep=" "),
                                    home_win_probability<0.50&home_win_probability>0.375~paste(away_season,away_team,"in","7", sep=" "),
                                    home_win_probability<0.375&home_win_probability>0.25~paste(away_season,away_team,"in","6", sep=" "),
                                    home_win_probability<0.25&home_win_probability>0.125~paste(away_season,away_team,"in","5", sep=" "),
                                    home_win_probability<0.125~paste(away_season,away_team,"in","4", sep=" ")))
  
    return(paste("STEPH's Prediction:",playoff_sim$prediction,sep=" "))
  })
  #create title variable for table
  title <- reactive({paste(input$home_season,input$home_team,"vs.",input$away_season,input$away_team,sep=" ")})
  
  
  
  output$comparison_table <- render_gt({
    compare_gt <- home_away()|>
    mutate(across("value",str_replace,"%",""))|>
    #get rid of asterisks in the player name columns
    mutate(across("value.x",str_replace_all,"\\*",""))|>
    mutate(across("value.y",str_replace_all,"\\*",""))|>
    gt()|>
    #markdown to make the logos display
    fmt_markdown("value.x")|>
    fmt_markdown("value.y")|>
    cols_align(align="center")|>
    tab_style(cell_text(weight="bold"),locations = cells_body(columns="name"))|>
    #mark the columns where each team is better
    tab_style(style=cell_fill(color="palegreen"),locations=cells_body(columns=c("value.x"),rows=(value==1|value=="NA (1)"|value==100)))|>
    tab_style(style=cell_fill(color="palegreen"),locations=cells_body(columns=c("value.y"),rows=(value==0|value=="NA (0)")))|>
    tab_style(style=cell_text(weight="bold"),locations=cells_body(columns=c("value.x"),rows=(value==1|value==100|value=="NA (1)")))|>
    tab_style(style=cell_text(weight="bold"),locations=cells_body(columns=c("value.y"),rows=(value==0|value=="NA (0)")))|>
    fmt_percent(columns=c("value.x","value.y"),rows=c(4,7,8,11),decimals=1)|>
    cols_hide(c("value"))|>
    cols_align(align="left",columns="name")|>
    cols_label(value.x = "Home Team",
               value.y = "Away Team",
               name="")|>
    tab_source_note(source_note = "Data from Basketball Reference and hoopR")|>
    tab_header(title=title(),
               subtitle = "By Aariv Iyengar | @AarivAnalytics")|>
    gt_theme_538()
  return(compare_gt)
  })
  output$prediction <- renderText(model_prediction())
  
  }



# Run the application 
shinyApp(ui = ui, server = server)
