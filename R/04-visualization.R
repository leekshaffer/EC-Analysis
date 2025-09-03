library(tidyverse)
library(patchwork)

CensusYrs <- c(2000, 2010, 2020)
Scale_House <- c(0.970, 1.025)
Scale_Senate <- c(0.55, 2.20)
Scale_EC <- c(0.9, 1.2)

Names <- c("Pop_RE", "Pop_AgeCat", "Pop_Sex", 
           "Pop_UR", "HH_RO")
Titles <- c("Race/Ethnicity", "Age Category", "Sex", 
            "Urban/Rural", "Housing Status")
names(Titles) <- Names

Dat_Trend <- function(Name, Years, Cols=NULL) {
  Weights <- NULL
  for (year in Years) {
    load(paste0("res/",Name,"_",year,".Rda"))
    Weights <- bind_rows(Weights,
                         get(paste(Name,year,"Res", sep="_"))$Weights %>%
                           mutate(Year=year))
  }
  Trend_Dat <- Weights %>% dplyr::filter(Numerator %in% c("House", "Senate", "EC"),
                                         Denominator %in% c("Population"))
  if (is.null(Cols)) {
    Cols <- colnames(Trend_Dat)[!(colnames(Trend_Dat) %in% c("Numerator","Denominator","Total_Prop","Year"))]
  }
  Dat <- Trend_Dat %>% dplyr::select(c("Numerator", "Year", all_of(Cols))) %>% 
    pivot_longer(cols=Cols, names_to=c("Category","Hold"), names_sep="_",
                 values_to="Weight")
  return(Dat)
}

Viz_Trend <- function(Dat, 
                         Log=TRUE, 
                         Facets="none", scales="fixed") {
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
  Trend_Dat <- Dat_Trend(Name=name,
                         Years=CensusYrs)
  x <- Viz_Trend(Trend_Dat,
                 Log=TRUE, Facets="vertical",
                 scales="free")
  ggsave(filename=paste0("figs/Trend_",name,".png"),
         plot=x,
         device=png,
         width=4, height=6, units="in", dpi=300)
  save(x, file=paste0("int/TrendPlot_",name,".Rda"))
  
  for (num in c("House", "Senate", "EC")) {
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
ggsave(filename="figs/Trend_Plot.png",
       plot=p_big,
       width=14, height=6, units="in", dpi=300, device=png)
