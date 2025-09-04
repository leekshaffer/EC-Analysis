library(shiny)
library(tidyverse)
library(DT)
library(bslib)
library(scales)

## Next Steps:
### Create version of res/Census_Trend files with multiple denominators
### Implement Focus Analysis parts of the file

## Info needed to run:
source("../R/00-key_values.R", local=TRUE)

## Import Census Res Data Files
for (Name in Names) {
  load(file=paste0("../res/Census_Trend/",Name,"_Trend.Rda"))
  for (Year in CensusYrs) {
    load(file=paste0("../res/Census/",Name,"_",Year,".Rda"))
  }
}

## UI:
ui <- fluidPage(
  tabsetPanel(
    id = "tabset",
    tabPanel("Census", 
             page_sidebar(
               title = "Distortions in U.S. House, Senate, and Electoral College",
               sidebar=sidebar(
                 selectInput("Name", 
                             label = h4("Demographic Variable"),
                             choices = as.list(Names_Titled),
                             selected = "Race/Ethnicity"),
                 selectInput("denom",
                             label=h4("Baseline Population"),
                             choices=as.list(Denom_Titled),
                             selected= "With DC, Not PR"),
                 selectInput("Year",
                             label = h4("Focus Year"),
                             choices = as.list(CensusYrs),
                             selected = 2020),
                 selectInput("Numerator",
                             label = h4("Focus Analysis"),
                             choices= as.list(Num_Titled),
                             selected = "Electoral College"),
                 h5("Data Sources:"),
                 a("tidycensus",
                   href="https://walker-data.com/tidycensus/")
               ),
               accordion(
                 open = c("All Analyses: Proportions",
                          "All Analyses: Trends"),
                 accordion_panel("All Analyses: Proportions",
                                 plotOutput("AllProp", width="95%")),
                 # accordion_panel("All Analyses: Trends",
                 #                 plotOutput("AllTrend", width="95%")),
                 # accordion_panel("Detailed Trends",
                 #                 plotOutput("DetTrend", width="95%")),
                 accordion_panel("Focus Year: Table",
                                 DT::dataTableOutput("AllTbl"))
                 # accordion_panel("Focus Analysis: Table",
                 #                 DT::dataTableOutput("SelTbl"))
               )
             )
    )
  )
)

## Server:
server <- function(input, output) {
  Yr_Data <- reactive({
    return(get(paste(input$Name, input$Year, "Res", sep="_"))[[input$denom]])
  })
  
  # plot1_title <- reactive({
  #   if (input$InStat==All_token_SC) {
  #     return("Plot 1: SCM Estimates for OBP")
  #   } else {
  #     return("Plot 1: Outcome Value Trajectories")
  #   }
  # })
  # output$plot1_title_out <- renderText(plot1_title())
  
  # output$AllURLs <- renderUI({
  #   tagList(
  #     a("Batter Positioning Leaderboard, 2022", 
  #       href=paste0("https://baseballsavant.mlb.com/visuals/batter-positioning?",
  #                   "playerId=545361&teamId=&opponent=&firstBase=0",
  #                   "&shift=1&season=2022&attempts=250&batSide=R")))
  # })
  
  output$AllTbl <- DT::renderDataTable({
    DT::datatable(Yr_Data() %>% 
                    dplyr::mutate(across(.cols=ends_with(c("Proportion","Weight")),
                                         .fns=~format(round(.x, digits=3), digits=3, nsmall=3)),
                                  `Excess Pop.`=format(`Excess Pop.`, nsmall=0, big.mark=",")) %>%
                    dplyr::select(-c("Excess Pop.")),
                  options = list(paging = FALSE,
                                 searching = FALSE))
  })
  
  output$AllProp <- renderPlot({
    Dat <- get(paste(input$Name, input$Year, "Res", sep="_"))[[input$denom]]
    
    
    if (input$Name=="HH_RO") {
      if (input$Year==2000) {
        Cols_Spec <- c(1,4)
      } else {
        Cols_Spec <- 1:3
      }
    } else if (input$Name=="Pop_UR") {
      if (input$Year==2020) {
        Cols_Spec <- 1:2
      } else {
        Cols_Spec <- c(1,3:4)
      }
    } else {
      Cols_Spec <- NULL
    }
    
    ColNames <- ColOrders[[input$Name]]
    if (is.null(Cols_Spec)) {
      Cols <- ColNames[ColNames %in% unique(Yr_Data()$Category)]
    } else {
      Cols <- ColNames[Cols_Spec]
    }
    
    Pop <- Yr_Data() %>% dplyr::filter(Analysis==Numerators[1]) %>%
      dplyr::select(Analysis,Category,`Population Proportion`) %>%
      dplyr::mutate(Analysis=input$denom) %>%
      rename(Proportion=`Population Proportion`)
    
    Props <- Yr_Data() %>% dplyr::select(-c("Population Proportion")) %>%
      bind_rows(Pop) %>%
      dplyr::filter(Category %in% Cols) %>%
      dplyr::mutate(Analysis=factor(Analysis, levels=c(Numerators,input$denom))) %>%
      dplyr::arrange(Analysis, Category)
    
    Props_Viz <- Props %>%
      bind_cols(Props %>% group_by(Analysis) %>%
                  dplyr::reframe(CS=cumsum(Proportion)-Proportion) %>%
                  dplyr::select(CS)) %>%
      dplyr::mutate(Position=CS+Proportion/2,
                    Percentage=paste0(format(round(Proportion*100, digits=2),
                                             digits=2, nsmall=2), "%"),
                    Label=if_else(is.na(`Abs. Weight`), Percentage,
                                  paste(Percentage, paste("Wt:", format(round(`Abs. Weight`, digits=2), 
                                                                        digits=2, nsmall=2),
                                                          sep=" "),
                                        sep="\n")),
                    Label_Use=if_else(Proportion < 0.05,
                                      "", Label))
    
    plot <- ggplot(Props_Viz) +
      geom_col(mapping=aes(y=Analysis, fill=Category,
                           x=Proportion),
               position = position_stack(reverse=TRUE)) +
      geom_text(mapping=aes(x=Position,
                            y=Analysis,
                            label=Label_Use),
                hjust="center",
                size=2) +
      theme_bw() +
      labs(y="", fill="",
           title=paste0("Population Proportions by ",Titles[input$Name])) +
      theme(legend.position="bottom")
    if (length(Cols) > 4) {
      plot <- plot + 
        guides(fill=guide_legend(nrow=2, byrow=TRUE))
    }
    if (!is.null(Cols_Spec)) {
      plot <- plot +
        scale_fill_manual(values=hue_pal()(length(ColNames))[Cols_Spec])
    }
    
    plot
  })
}

## Run the App:
shinyApp(ui = ui, server = server)
