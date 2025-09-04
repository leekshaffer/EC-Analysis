library(tidyverse)
library(patchwork)

source("R/00-key_values.R")
Numerators <- c("EC", "Senate", "House")
Denominators <- c("Population", "Pop: Without DC", "Pop: With PR")

# Trend Plots
CensusYrs <- c(2000, 2010, 2020)
Scale_House <- c(0.970, 1.025)
Scale_Senate <- c(0.55, 2.20)
Scale_EC <- c(0.9, 1.2)

Dat_Trend <- function(Name, Years=CensusYrs, Type="Census") {
  for (denom in Denominators) {
    assign(x=paste("Tbl", denom, sep="_"),
           value=NULL)
  }
  for (Year in Years) {
    load(paste0("res/",Type,"/",Name,"_",Year,".Rda"))
    for (denom in Denominators) {
      assign(x=paste("Tbl", denom, sep="_"),
             value=bind_rows(get(paste("Tbl", denom, sep="_")),
                             get(paste(Name, Year, "Res", sep="_"))[[denom]] %>% 
                               dplyr::mutate(Year=Year)))
    }
  }
  
  Dat_List <- list()
  for (denom in Denominators) {
    Dat_List[[denom]] <- get(paste("Tbl", denom, sep="_"))
  }
  return(Dat_List)
}

Viz_Trend <- function(Dat_List, denom="Population", 
                         Log=TRUE, 
                         Facets="none", scales="fixed") {
  Dat <- Dat_List[[denom]]
  
  ## Paused as of 9/4 11:38
  if (Facets=="vertical") {
    plot <- ggplot(Dat, mapping=aes(x=Year, y=Weight, 
                                    group=interaction(Category,Numerator), 
                                    linetype=Category, 
                                    color=Category)) +
      geom_point() +
      geom_line() +
      theme_bw() + 
      facet_wrap(facets=~Numerator, ncol=1,
                 scales=scales) +
      theme(legend.position="bottom",
            legend.title=element_blank()) 
    if (length(unique(Dat$Category)) > 3) {
      plot <- plot +
      guides(linetype=guide_legend(nrow=2, byrow=TRUE),
             color=guide_legend(nrow=2, byrow=TRUE))
    }
  } else if (Facets=="horizontal") {
    plot <- ggplot(Dat, mapping=aes(x=Year, y=Weight, 
                                    group=interaction(Category,Numerator), 
                                    linetype=Category, 
                                    color=Category)) +
      geom_point() +
      geom_line() +
      theme_bw() + 
      facet_wrap(facets=~Numerator, nrow=1,
                 scales=scales) +
      theme(legend.position="bottom",
            legend.title=element_blank())
  } else {
    plot <- ggplot(Dat, mapping=aes(x=Year, y=Weight, 
                                    group=interaction(Category,Numerator), 
                                    shape=Numerator, linetype=Numerator, 
                                    color=Category)) +
      geom_point() +
      geom_line() +
      theme_bw()
  }
  if (Log) {
    plot <- plot + scale_y_log10()
  }
  return(plot)
}

for (name in Names) {
  colorders <- ColOrders[[name]]
  if (paste(name, "Addl", sep="_") %in% names(ColOrders)) {
    colorders <- c(colorders, ColOrders[[paste(name, "Addl", sep="_")]])
  }
  Trend_Dat <- Dat_Trend(Name=name,
                         Years=CensusYrs,
                         ColNames=colorders)
  x <- Viz_Trend(Trend_Dat,
                 Log=TRUE, Facets="vertical",
                 scales="free")
  ggsave(filename=paste0("figs/Trend_",name,".png"),
         plot=x,
         device=png,
         width=4, height=6, units="in", dpi=300)
  # save(x, file=paste0("int/TrendPlot_",name,".Rda"))
  
  for (num in Numerators) {
    p1 <- ggplot(Trend_Dat %>% dplyr::filter(Numerator==num),
                 mapping=aes(x=Year, y=Weight,
                             group=Category,
                             linetype=Category,
                             color=Category)) +
      geom_point() +
      geom_line() +
      theme_bw() +
      scale_x_continuous(breaks=CensusYrs, minor_breaks=NULL) +
      scale_y_log10(limits=get(paste("Scale", num, sep="_"))) +
      theme(legend.position="bottom",
            legend.title=element_blank()) +
      labs(subtitle=num)
    if (num=="House") {
      p1 <- p1 + labs(title=Titles[name])
    }
    if (length(unique(Trend_Dat$Category)) > 3) {
      if (length(unique(Trend_Dat$Category)) > 6) {
        p1 <- p1 +
          guides(linetype=guide_legend(nrow=3, byrow=TRUE),
                 color=guide_legend(nrow=3, byrow=TRUE))
      } else {
        p1 <- p1 +
          guides(linetype=guide_legend(nrow=2, byrow=TRUE),
                 color=guide_legend(nrow=2, byrow=TRUE))
      }
    }
    assign(x=paste(name, num, sep="_"),
           value=p1)
  }
  p_col <- (get(paste(name, "House", sep="_")) / 
    get(paste(name, "Senate", sep="_"))/ 
    get(paste(name, "EC", sep="_"))) + 
    plot_layout(guides="collect") & theme(legend.position="bottom")
  assign(x=paste(name, "Plot", "Col", sep="_"),
         value = p_col)
}

p_big <- NULL
for (name in Names) {
  p_big <- p_big | get(paste(name, "Plot", "Col", sep="_"))
}
ggsave(filename="figs/Trend_Plot_Full.png",
       plot=p_big,
       width=9, height=6.5, units="in", dpi=300, device=png)

p_med <- NULL
for (name in Names[c(1,4,5)]) {
  p_med <- p_med | get(paste(name, "Plot", "Col", sep="_"))
}
ggsave(filename="figs/Trend_Plot_Three.png",
       plot=p_med,
       width=8.8, height=5.8, units="in", dpi=300, device=png)

# Proportion Plots
Plot_Props <- function(Name, Year=2020, ColNames=NULL,
                       Props_Use=Numerators,
                       Denom="Population") {
  load(paste0("res/",Name,"_",Year,".Rda"))
  Props <- get(paste(Name, Year, "Res", sep="_"))$Proportions
  if (is.null(ColNames)) {
    Cols <- colnames(Props)[!(colnames(Props) %in% c("Analysis","Total_Prop"))]
  } else {
    Cols <- paste(ColNames, "Prop", sep="_")
  }
  Props <- Props %>%
    dplyr::select(c("Analysis",all_of(Cols))) %>%
    dplyr::filter(Analysis %in% c(Props_Use,Denom)) %>%
    pivot_longer(cols=Cols, 
                 names_to=c("Category","Hold"), names_sep="_",
                 values_to="Proportion") 
  Wts <- get(paste(Name, Year, "Res", sep="_"))$Weights %>%
    dplyr::filter(Numerator %in% Props_Use, 
                  Denominator==Denom) %>%
    pivot_longer(cols=all_of(ColNames),
                 names_to="Category",
                 values_to="Weight") %>%
    dplyr::rename(Analysis=Numerator) %>%
    dplyr::select(Analysis,Category,Weight)
  Props_Viz <- Props %>% left_join(Wts, by=join_by(Analysis,Category)) %>%
    dplyr::mutate(Analysis=factor(Analysis,
                                  levels=c(Props_Use,Denom), 
                                  labels=c(Props_Use,Denom)))
  if (is.null(ColNames)) {
    Props_Viz <- Props_Viz %>%
      dplyr::arrange(Analysis, desc(Category))
  } else {
    Props_Viz <- Props_Viz %>%
      dplyr::mutate(Category=factor(Category, levels=ColNames, labels=ColNames)) %>%
      dplyr::arrange(Analysis, Category)
  }
  Props_Viz <- Props_Viz %>%
    bind_cols(Props_Viz %>% group_by(Analysis) %>%
                dplyr::reframe(CS=cumsum(Proportion)-Proportion) %>%
                dplyr::select(CS)) %>%
    dplyr::mutate(Position=CS+Proportion/2,
                  Percentage=paste0(format(round(Proportion*100, digits=2),
                                           digits=2, nsmall=2), "%"),
                  Label=if_else(is.na(Weight), Percentage,
                                paste(Percentage, paste("Wt:", format(round(Weight, digits=2), 
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
         title=paste0("Population Proportions by ",Titles[Name])) +
    # guides(fill=guide_legend(reverse=TRUE)) +
    theme(legend.position="bottom")
  if (length(Cols) > 4) {
    plot <- plot + 
      guides(fill=guide_legend(nrow=2, byrow=TRUE))
                               # reverse=TRUE))
  }
  
  ggsave(filename=paste0("figs/Props_",Name,".png"),
         plot=plot,
         device=png, width=6, height=3, units="in", dpi=300)
}

for (name in Names) {
  Plot_Props(name, Year=2020, ColNames=ColOrders[[name]])
}

# Tables of Weights and Excess Pops:

Table <- function(Name, Year=2020, ColNames=NULL,
                  Props_Use=Numerators,
                  Denom="Population") {
  load(paste0("res/",Name,"_",Year,".Rda"))
  LX <- get(paste(Name, Year, "Res", sep="_"))
  Cols <- colnames(LX$Relative)
  if (is.null(ColNames)) {
    Cols <- Cols[!(Cols %in% c("Numerator","Denominator"))]
  } else {
    Cols <- ColNames[ColNames %in% Cols]
  }
  tbl <- LX$Proportions %>% 
    dplyr::filter(Analysis %in% c(Denom,Props_Use)) %>%
    pivot_longer(cols=paste(Cols, "Prop", sep="_"),
                 names_to=c("Category", "Hold"), names_sep="_",
                 values_to="Population Proportion") %>%
    dplyr::select(-c("Total_Prop","Hold"))
  
  AbsW <- LX$Weights %>% dplyr::filter(Numerator %in% Props_Use,
                                       Denominator==Denom) %>%
    dplyr::select(all_of(c("Numerator",Cols))) %>%
    pivot_longer(cols=Cols, names_to="Category", values_to="Abs. Weights")
  
  RelW <- LX$Relative %>% dplyr::filter(Numerator %in% Props_Use,
                                       Denominator==Denom) %>%
    dplyr::select(c("Numerator",all_of(Cols))) %>%
    pivot_longer(cols=Cols, names_to="Category", values_to="Rel. Weights")
  
  Exc <- LX$Excess %>% dplyr::filter(Numerator %in% Props_Use,
                                     Denominator==Denom) %>%
    dplyr::select(c("Numerator",all_of(Cols))) %>%
    pivot_longer(cols=Cols, names_to="Category", values_to="Excess Pop.")
  
  tbl_full <- tbl %>% dplyr::filter(Analysis==Denom) %>% 
    dplyr::rename(Denominator=Analysis) %>%
    right_join(tbl %>% dplyr::filter(Analysis %in% Props_Use) %>%
                 dplyr::rename(Proportion="Population Proportion",
                               Numerator=Analysis),
               by=join_by(Category)) %>%
    right_join(AbsW, by=join_by(Category,Numerator)) %>%
    right_join(RelW, by=join_by(Category,Numerator)) %>%
    right_join(Exc, by=join_by(Category,Numerator)) %>%
    dplyr::mutate(Analysis=factor(Numerator,
                                  levels=Props_Use,
                                  labels=Props_Use),
                  Category=factor(Category,
                                  levels=Cols,
                                  labels=Cols)) %>%
    dplyr::select(-c("Numerator","Denominator")) %>%
    dplyr::select(Analysis,Category,everything()) %>%
    dplyr::arrange(desc(Analysis),Category)
  
  return(tbl_full)
}

Table_All <- function(Name, Year=2020, ColNames=NULL,
                      Props_Use=Numerators) {
  Tbl_List <- list()
  for (denom in Denominators) {
    Tbl_List[[denom]] <- Table(Name, Year, ColNames, Props_Use, Denom=denom)
  }
  assign(paste(Name, Year, "Res", sep="_"),
         value=Tbl_List)
  save(list=paste(Name, Year, "Res", sep="_"),
       file=paste0("res/",Name,"_",Year,".Rda"))
}