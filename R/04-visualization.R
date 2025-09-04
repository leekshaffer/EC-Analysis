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

Trend_Viz <- function(Name, Years=CensusYrs, Type="Census",
                      denom="Population", 
                      Log=TRUE, Facets="none", scales="fixed") {
  
  Dat <- NULL

  for (Year in Years) {
    load(paste0("res/",Type,"/",Name,"_",Year,".Rda"))
    Dat <- bind_rows(Dat,
                     get(paste(Name, Year, "Res", sep="_"))[[denom]] %>%
                       dplyr::mutate(Year=Year))
  }
    
  if (Facets=="vertical") {
    plot <- ggplot(Dat, mapping=aes(x=Year, y=`Abs. Weight`, 
                                    group=interaction(Category,Analysis), 
                                    linetype=Category, 
                                    color=Category)) +
      geom_point() +
      geom_line() +
      theme_bw() + 
      facet_wrap(facets=~Analysis, ncol=1,
                 scales=scales) +
      theme(legend.position="bottom",
            legend.title=element_blank()) 
    if (length(unique(Dat$Category)) > 3) {
      plot <- plot +
      guides(linetype=guide_legend(nrow=2, byrow=TRUE),
             color=guide_legend(nrow=2, byrow=TRUE))
    }
  } else if (Facets=="horizontal") {
    plot <- ggplot(Dat, mapping=aes(x=Year, y=`Abs. Weight`, 
                                    group=interaction(Category,Analysis), 
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
    plot <- ggplot(Dat, mapping=aes(x=Year, y=`Abs. Weight`, 
                                    group=interaction(Category,Analysis), 
                                    shape=Analysis, linetype=Analysis, 
                                    color=Category)) +
      geom_point() +
      geom_line() +
      theme_bw()
  }
  if (Log) {
    plot <- plot + scale_y_log10(name="Weight")
  } else {
    plot <- plot + scale_y_continuous(name="Weight")
  }
  return(list(Dat=Dat,
              plot=plot))
}

for (Name in Names) {
  p_fac <- Trend_Viz(Name, Years=CensusYrs, Type="Census",
                    denom="Population",
                    Log=TRUE, Facets="vertical", scales="free")
  
  ggsave(filename=paste0("figs/Census/Trend_",Name,".png"),
         plot=p_fac$plot,
         device=png,
         width=4, height=6, units="in", dpi=300)
  
  Trend_Dat <- p_fac$Dat
  
  for (num in rev(Numerators)) {
    p1 <- ggplot(Trend_Dat %>% dplyr::filter(Analysis==num),
                 mapping=aes(x=Year, y=`Abs. Weight`,
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
    # if (num==rev(Numerators)[1]) {
    #   p1 <- p1 + labs(title=Titles[Name])
    # }
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
    assign(x=paste(Name, num, sep="_"),
           value=p1)
  }
  
  p_col <- ((get(paste(Name, "House", sep="_")) + labs(title=Titles[Name])) / 
    get(paste(Name, "Senate", sep="_"))/ 
    get(paste(Name, "EC", sep="_"))) + 
    plot_layout(guides="collect") & theme(legend.position="bottom")
  assign(x=paste(Name, "Plot", "Col", sep="_"),
         value = p_col)
}

p_big <- NULL
for (Name in Names) {
  p_big <- p_big | get(paste(Name, "Plot", "Col", sep="_"))
}
ggsave(filename="figs/Census/Trend_Plot_Full.png",
       plot=p_big,
       width=12, height=7, units="in", dpi=300, device=png)

p_med <- NULL
for (Name in Names[c(1,4,5)]) {
  p_med <- p_med | get(paste(Name, "Plot", "Col", sep="_"))
}
ggsave(filename="figs/Census/Trend_Plot_Three.png",
       plot=p_med,
       width=8.8, height=5.8, units="in", dpi=300, device=png)

# Proportion Plots
Plot_Props <- function(Name, Year=2020, Type="Census",
                       denom="Population",
                       Cols=NULL) {
  load(paste0("res/",Type,"/",Name,"_",Year,".Rda"))
  Dat <- get(paste(Name, Year, "Res", sep="_"))[[denom]]
  
  ColNames <- ColOrders[[Name]]
  if (is.null(Cols)) {
    Cols <- ColNames[ColNames %in% unique(Dat$Category)]
  }
  
  Pop <- Dat %>% dplyr::filter(Analysis==Numerators[1]) %>%
    dplyr::select(Analysis,Category,`Population Proportion`) %>%
    dplyr::mutate(Analysis=denom) %>%
    rename(Proportion=`Population Proportion`)
  
  Props <- Dat %>% dplyr::select(-c("Population Proportion")) %>%
    bind_rows(Pop) %>%
    dplyr::filter(Category %in% Cols) %>%
    dplyr::mutate(Analysis=factor(Analysis, levels=c(Numerators,denom))) %>%
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
         title=paste0("Population Proportions by ",Titles[Name])) +
    # guides(fill=guide_legend(reverse=TRUE)) +
    theme(legend.position="bottom")
  if (length(Cols) > 4) {
    plot <- plot + 
      guides(fill=guide_legend(nrow=2, byrow=TRUE))
                               # reverse=TRUE))
  }
  
  ggsave(filename=paste0("figs/",Type,"/Props_",Name,".png"),
         plot=plot,
         device=png, width=6, height=3, units="in", dpi=300)
}

for (Name in Names) {
  Plot_Props(Name, Year=2020, Type="Census",
             denom="Population", Cols=NULL)
}
