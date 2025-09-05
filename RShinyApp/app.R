library(shiny)
library(tidyverse)
library(patchwork)
library(DT)
library(bslib)
library(scales)

## Info needed to run:
source("./00-key_values.R", local=TRUE)

## Import Census Res Data Files
for (Type in c("Census")) {
  for (Name in Names) {
    load(file=paste0("res/",Type,"/",Name,"_Res.Rda"))
    load(file=paste0("res/",Type,"_Facets/",Name,".Rda"))
    for (Yr in CensusYrs) {
      load(file=paste0("res/",Type,"_Props/",Name,"_",Yr,".Rda"))
      for (Num in Numerators) {
        load(file=paste0("res/",Type,"_Weights/",
                         Name, "_", Yr, "_", Num, ".Rda"))
      }
    }
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
                 selectInput("Denom",
                             label=h4("Baseline Population"),
                             choices=as.list(Denominators),
                             selected= "50 States and DC"),
                 selectInput("Year",
                             label = h4("Focus Year"),
                             choices = as.list(CensusYrs),
                             selected = 2020),
                 selectInput("Numerator",
                             label = h4("Focus Analysis"),
                             choices= as.list(Num_Titled),
                             selected = "Electoral College"),
                 h5("Details at GitHub:"),
                 a("https://bit.ly/Elec-Analysis", href="https://bit.ly/Elec-Analysis"),
                 h5("Data Source:"),
                 h6("Census API accessed with:"),
                 a("tidycensus",
                   href="https://walker-data.com/tidycensus/")
               ),
               accordion(
                 open = c("All Analyses: Proportions"),
                 accordion_panel("All Analyses: Proportions",
                                 plotOutput("AllProp", width="95%")),
                 accordion_panel("All Analyses: Trends",
                                 plotOutput("AllTrend", width="95%")),
                 accordion_panel("Focus Year and Analysis: Weights",
                                 plotOutput("FocWeight", width="95%")),
                 accordion_panel("Focus Year: Table",
                                 DT::dataTableOutput("AllTbl")),
                 accordion_panel("Focus Analysis: Table",
                                 DT::dataTableOutput("FocTbl"))
               )
             )
    )
  )
)

## Server:
server <- function(input, output) {
  Yr_Data <- reactive({
    return(get(paste(input$Name, "Res", sep="_")) %>%
             dplyr::filter(Year==input$Year,
                           Denominator==input$Denom))
  })
  
  An_Data <- reactive({
    return(get(paste(input$Name, "Res", sep="_")) %>%
             dplyr::filter(Analysis==input$Numerator,
                           Denominator==input$Denom))
  })
  
  output$AllTbl <- DT::renderDataTable({
    Dat <- Yr_Data()
    NumCats <- length(unique(Dat$Category))
    if (NumCats > 4) {
      Nums <- NumCats*(1:(ceiling(nrow(Dat)/NumCats)-1))
      DT::datatable(Dat %>%
                      dplyr::mutate(across(.cols=ends_with(c("Proportion","Weight")),
                                           .fns=~format(round(.x, digits=3), digits=3, nsmall=3)),
                                    `Excess Pop.`=format(round(`Excess Pop.`, digits=0), nsmall=0, big.mark=",")) %>%
                      dplyr::select(-c("Denominator")),
                    options = list(lengthMenu=list(c(Nums, -1),
                                                   c(as.character(Nums), "All")),
                                   pageLength=NumCats,
                                   searching = TRUE))
    } else {
      DT::datatable(Dat %>%
                      dplyr::mutate(across(.cols=ends_with(c("Proportion","Weight")),
                                           .fns=~format(round(.x, digits=3), digits=3, nsmall=3)),
                                    `Excess Pop.`=format(round(`Excess Pop.`, digits=0), nsmall=0, big.mark=",")) %>%
                      dplyr::select(-c("Denominator")),
                    options = list(paging=FALSE,
                                   searching = TRUE))
    }
  })
  
  output$FocTbl <- DT::renderDataTable({
    Dat <- An_Data()
    NumCats <- length(unique(Dat$Category))
    if (NumCats > 4) {
      Nums <- NumCats*(1:(ceiling(nrow(Dat)/NumCats)-1))
      DT::datatable(Dat %>%
                      dplyr::mutate(across(.cols=ends_with(c("Proportion","Weight")),
                                           .fns=~format(round(.x, digits=3), digits=3, nsmall=3)),
                                    `Excess Pop.`=format(round(`Excess Pop.`, digits=0), nsmall=0, big.mark=",")) %>%
                      dplyr::select(-c("Denominator")),
                    options = list(lengthMenu=list(c(Nums, -1),
                                                   c(as.character(Nums), "All")),
                                   pageLength=NumCats,
                                   searching = TRUE))
    } else {
      DT::datatable(Dat %>%
                      dplyr::mutate(across(.cols=ends_with(c("Proportion","Weight")),
                                           .fns=~format(round(.x, digits=3), digits=3, nsmall=3)),
                                    `Excess Pop.`=format(round(`Excess Pop.`, digits=0), nsmall=0, big.mark=",")) %>%
                      dplyr::select(-c("Denominator")),
                    options = list(paging = FALSE,
                                   searching = TRUE))
    }
  })
  
  output$AllProp <- renderPlot({
    DenNum <- (1:length(Denominators))[Denominators==input$Denom]
    get(x=paste("Props",DenNum,input$Name,input$Year, sep="_"))
  })
  
  output$AllTrend <- renderPlot({
    DenNum <- (1:length(Denominators))[Denominators==input$Denom]
    get(x=paste("Facet",DenNum,input$Name, sep="_"))
  })
  
  output$FocWeight <- renderPlot({
    DenNum <- (1:length(Denominators))[Denominators==input$Denom]
    get(x=paste("Weights",DenNum,input$Name,input$Year,input$Numerator, sep="_"))
  })
}

## Run the App:
shinyApp(ui = ui, server = server)
