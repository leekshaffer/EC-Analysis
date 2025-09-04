library(shiny)
library(tidyverse)
library(DT)
library(bslib)
library(scales)

## Info needed to run:
source("../R/00-key_values.R", local=TRUE)

## Import Census Res Data Files
for (Name in Names) {
  load(file=paste0("../res/Census_Trend/",Name,"_Trend.Rda"))
  for (Year in CensusYrs) {
    load(file=paste0("../res/Census/",Name,"_",Year,".Rda"))
  }
}



# ## Data needed to run:
# Interv <- 2023
# types <- c("2023","2024","2023_24") ## The analysis year types
# load(file="int/Player_pool_data.Rda")
# load(file="int/DID_data.Rda")
# for (type in c(types, paste(types,"full",sep="_"))) {
#   Shifts <- get(paste0("Player_pool_",type)) %>% dplyr::select(Player_ID, Shift_Perc_2022, Shift_Cat, 
#                                                                Shift_Perc_Max)
#   load(file=paste0("res/SC-",gsub("_","-",type),"-Complete.Rda"))
#   assign(x=paste0("MSPEs_PRes_",type),
#          value=MSPEs_PRes %>% left_join(Shifts, by=join_by(Placebo_ID==Player_ID)))
#   assign(x=paste0("MSPEs_Results_",type),
#          value=MSPEs_Results %>% left_join(Shifts, by=join_by(Player_ID)))
#   assign(x=paste0("SCs_Results_",type),
#          value=SCs_Results %>% left_join(Shifts, by=join_by(Player_ID)))
#   rm(list=c("MSPEs_Results","MSPEs_PRes","SCs_Results"))
# }
# source("./04-FigureCommands.R", local=TRUE)
# 
# ## Lists
# Player_Choices <- unique(bind_rows(Player_pool_2023,Player_pool_2023_24,Player_pool_2024) %>% 
#   dplyr::filter(Shift_Cat=="High") %>% pull(Name_Disp))
# BStats_Use <- BStats %>% dplyr::filter(Use)
# All_token_SC <- "-All-"
# All_token_DID <- "-All (Table Only)-"
# 
# ## Data functions for player & outcome combination:
# ### Get player info from name:
# player_info <- function(display_name,targS) {
#   B.250_row <- B.250_pool %>% dplyr::filter(Season==2022 & Name_Disp==display_name)
#   Pool_row <- get(paste0("Player_pool_",gsub("-","_",targS))) %>% 
#     dplyr::filter(Name_Disp==display_name)
#   list(First=B.250_row$name_first, Last=B.250_row$name_last,
#        Shift_Perc_2022=Pool_row$Shift_Perc_2022,
#        Shade_Perc_2023=Pool_row$Shade_Perc_2023,
#        Shade_Perc_2024=Pool_row$Shade_Perc_2024,
#        FG_ID=B.250_row$key_fangraphs,
#        BR_ID=B.250_row$key_bbref,
#        MLB_ID=B.250_row$Player_ID,
#        LastInit=tolower(substr(B.250_row$name_last, 1, 1)),
#        BR_URL=paste0("https://www.baseball-reference.com/players/",
#                      tolower(substr(B.250_row$name_last, 1, 1)),
#                      "/", B.250_row$key_bbref, ".shtml"),
#        FG_URL=paste0("https://www.fangraphs.com/players/",
#                      B.250_row$name_first,"-",B.250_row$name_last,
#                      "/",B.250_row$key_fangraphs, "/stats"),
#        MLB_BPL_URL=paste0("https://baseballsavant.mlb.com/visuals/batter-positioning?playerId=",
#                           B.250_row$Player_ID,
#                           "&teamId=&opponent=&firstBase=0&shift=1&season=2022&attempts=250"))
# }
# 
# ### Effect Estimates & P-Values Table:
# ests_tbl <- function(display_name,statval,targS) {
#   if (display_name==All_token_SC) {
#     SCs_Res_int <- get(paste0("SCs_Results_", gsub("-","_",targS), "_full"))
#     MSPEs_Res_int <- get(paste0("MSPEs_Results_", gsub("-","_",targS), "_full"))
#     Tbl <- SCs_Res_int %>% dplyr::filter(Outcome %in% BStats_Use$stat & 
#                                            Intervention & !(Placebo_Unit)) %>% 
#       dplyr::select(Name_Disp,Shift_Perc_2022,Outcome,Season,Observed,Synthetic,Diff) %>%
#       left_join(MSPEs_Res_int %>% dplyr::select(Name_Disp,Outcome,PVal), by=c("Name_Disp","Outcome")) %>%
#       dplyr::rename(Player=Name_Disp, `2022 Shifts (%)`=Shift_Perc_2022, `Observed Value`=Observed, 
#                     `Synthetic Control Value`=Synthetic,
#                     `Effect Estimate`=Diff, `Placebo P-Value`=PVal) %>%
#       dplyr::mutate(across(.cols=-c("Player","Outcome","Season"),
#                            .fns=~format(round(.x, digits=3), digits=3, nsmall=3)))
#     if (statval==All_token_SC) {
#       Tbl %>% dplyr::arrange(desc(`2022 Shifts (%)`),Player,Season,Outcome)
#     } else {
#       Tbl %>% dplyr::filter(Outcome==statval) %>% dplyr::arrange(desc(`2022 Shifts (%)`),Player,Season)
#     }
#   } else {
#     SCs_Res_int <- get(paste0("SCs_Results_",gsub("-","_",targS)))
#     MSPEs_Res_int <- get(paste0("MSPEs_Results_",gsub("-","_",targS)))
#     Tbl <- SCs_Res_int %>% dplyr::filter(Outcome %in% BStats_Use$stat & Name_Disp==display_name & Intervention) %>% 
#       dplyr::select(Name_Disp,Outcome,Season,Observed,Synthetic,Diff) %>%
#       left_join(MSPEs_Res_int %>% dplyr::select(Name_Disp,Outcome,PVal), by=c("Name_Disp","Outcome")) %>%
#       dplyr::rename(Player=Name_Disp,
#                     `Observed Value`=Observed, `Synthetic Control Value`=Synthetic,
#                     `Effect Estimate`=Diff, `Placebo P-Value`=PVal) %>%
#       dplyr::mutate(across(.cols=-c("Player","Outcome","Season"),
#                            .fns=~format(round(.x, digits=3), digits=3, nsmall=3)))
#     if (statval==All_token_SC) {
#       Tbl %>% dplyr::arrange(Player,Season,Outcome)
#     } else {
#       Tbl %>% dplyr::filter(Outcome==statval) %>% dplyr::arrange(Player,Season)
#     }
#   }
# }
# 
# DID_tbl <- function(statval) {
#   if (statval==All_token_DID) {
#     Tbl <- TwoByTwo %>% pivot_longer(cols=-c("Batter"), names_to=c("Outcome","Year"), 
#                                      names_sep="_", values_to="Value") %>% 
#       pivot_wider(id_cols=c("Outcome","Batter"), names_from="Year", values_from="Value") %>%
#       dplyr::rename(`Average, 2022`=`2022`,
#                     `Average, 2023`=`2023`,
#                     `Average, 2024`=`2024`,
#                     "Diff., 2023 \U2212 2022"=`Diff-2023`,
#                     "Diff., 2024 \U2212 2023"=`Diff-2024`) %>%
#       dplyr::arrange(Outcome) %>%
#       dplyr::mutate(across(.cols=-c("Outcome","Batter"),
#                            .fns=~format(round(.x, digits=3), digits=3, nsmall=3)))
#   } else {
#     Tbl <- TwoByTwo %>% dplyr::select(c("Batter",starts_with(statval)))
#     colnames(Tbl) <- c("Batter Handedness",
#                        paste(rep(statval, each=3),
#                              c("2022","2023","2024","Diff., 2023 \U2212 2022","Diff., 2024 \U2212 2023"),
#                              sep=" "))
#     Tbl <- Tbl %>% dplyr::mutate(across(.cols=-c("Batter Handedness"),
#                                         .fns=~format(round(.x, digits=3), digits=3, nsmall=3)))
#   }
#   return(Tbl)
# }
# 
# wts_tbl <- function(display_name,statval,targS) {
#   load(file=paste0("res/Players-SC-", targS, "-full",
#                    "/Player-SC-", display_name, ".Rda"))
#   Tbl <- Weights_Unit %>% dplyr::rename(`Control Player`=unit,
#                                         `wOBA Weight`=wOBA_weight,
#                                         `OBP Weight`=OBP_weight,
#                                         `OPS Weight`=OPS_weight)
#   if (statval==All_token_SC) {
#     statval <- BStats_Use$stat
#   }
#   Tbl %>% dplyr::select(c("Control Player",paste(statval,"Weight", sep=" "))) %>%
#     arrange(across(-c("Control Player"), desc)) %>%
#     dplyr::mutate(across(.cols=-c("Control Player"),
#                          .fns=~paste0(format(round(.x*100, digits=2), digits=2, nsmall=2),"%")))
# }

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
                                 plotOutput("AllProp", width="95%"))
                 # accordion_panel("All Analyses: Trends",
                 #                 plotOutput("AllTrend", width="95%")),
                 # accordion_panel("Selected Analysis: Trend",
                 #                 plotOutput("SelTrend", width="95%")),
                 # accordion_panel("All Analyses: Table",
                 #                 DT::dataTableOutput("AllTbl")),
                 # accordion_panel("Selected Analysis: Table",
                 #                 DT::dataTableOutput("SelTbl"))
               )
             )
    )
  )
)

## Server:
server <- function(input, output) {
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
  
  # output$tbl1 <- DT::renderDataTable({
  #   validate(
  #     need(file.exists(paste0("res/Players-SC-", input$TargetSeason, "-full",
  #                             "/Player-SC-",input$InName,".Rda")),
  #          message="This target season selection is not available for this player.")
  #   )
  #   DT::datatable(ests_tbl(input$InName, input$InStat, input$TargetSeason), 
  #                 options = list(paging = FALSE,
  #                                searching = FALSE))
  # })
  
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
      Cols <- ColNames[ColNames %in% unique(Dat$Category)]
    } else {
      Cols <- ColNames[Cols_Spec]
    }
    
    Pop <- Dat %>% dplyr::filter(Analysis==Numerators[1]) %>%
      dplyr::select(Analysis,Category,`Population Proportion`) %>%
      dplyr::mutate(Analysis=input$denom) %>%
      rename(Proportion=`Population Proportion`)
    
    Props <- Dat %>% dplyr::select(-c("Population Proportion")) %>%
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
  
  # output$plot1 <- renderPlot({
  #   Target <- input$InName
  #   targS <- input$TargetSeason
  #   validate(
  #     need(file.exists(paste0("res/Players-SC-", targS, "-full", 
  #                             "/Player-SC-", Target, ".Rda")),
  #          message="This target season selection is not available for this player.")
  #   )
  #   load(file=paste0("res/Players-SC-", targS, "-full", 
  #                    "/Player-SC-",Target,".Rda"))
  #   if (input$InStat==All_token_SC) {
  #       SC_data <- get(paste0("SCs_Results_",gsub("-","_",targS))) %>% 
  #         dplyr::filter(Outcome==BStats_Use$stat[1] & 
  #                         (Name_Disp %in% c(Target, Weights_Unit$unit)) & 
  #                         (Season %in% unique(SCs$Season)))
  #       plot_SC_ests(BStats_Use$stat[1], SC_data,
  #                    LegName=NULL,
  #                    LegVar="Placebo_Unit",
  #                    LegLabs=c(paste0("Target Player: ",Target),
  #                              "Placebo Players (2022 Shift Rate \U2264 15%)"), 
  #                    LegBreaks=c(FALSE,TRUE),
  #                    LegCols=brewer.pal(3, "Dark2")[c(3,1)], 
  #                    LegAlpha=c(1,0.5), 
  #                    LegLTY=c("solid","longdash"), 
  #                    title=paste0("SCM estimates for ",BStats_Use$stat[1]," for ",Target,
  #                                 " and placebos"),
  #                    LW=1.2,
  #                    tagval=NULL) + 
  #         theme(legend.position="bottom",
  #               legend.background=element_rect(fill="white", color="grey50"),
  #               legend.direction="horizontal")
  #   }
  # })
}

## Run the App:
shinyApp(ui = ui, server = server)
