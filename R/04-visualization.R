library(tidyverse)
library(patchwork)

# Basic Info

Names <- c("Pop_RE", "Pop_AgeCat", "Pop_Sex", 
           "Pop_UR", "HH_RO")
Titles <- c("Race/Ethnicity", "Age Category", "Sex", 
            "Urban/Rural", "Housing Status")
names(Titles) <- Names

Numerators <- c("House", "Senate", "EC")

ColOrders <- list(Pop_RE=c("White", "Hispanic", "Black", "Asian", "Multiple", "AIAN", "NHOPI", "Other"),
                  Pop_AgeCat=c("0-17", "18-39", "40-64", "65+"),
                  Pop_Sex=c("Female", "Male"),
                  Pop_UR=c("Urban", "Rural"),
                  HH_RO=c("Renter", "Owner: Mortgage", "Owner: Clear"),
                  Pop_UR_Trend_Addl=c("Urbanized Area", "Urban Cluster"),
                  HH_RO_Trend_Addl=c("Owner"))

# Trend Plots
CensusYrs <- c(2000, 2010, 2020)
Scale_House <- c(0.970, 1.025)
Scale_Senate <- c(0.55, 2.20)
Scale_EC <- c(0.9, 1.2)

Dat_Trend <- function(Name, Years, ColNames=NULL) {
  Weights <- NULL
  for (year in Years) {
    load(paste0("res/",Name,"_",year,".Rda"))
    Weights <- bind_rows(Weights,
                         get(paste(Name,year,"Res", sep="_"))$Weights %>%
                           mutate(Year=year))
  }
  Trend_Dat <- Weights %>% dplyr::filter(Numerator %in% c("House", "Senate", "EC"),
                                         Denominator %in% c("Population"))
  if (is.null(ColNames)) {
    ColNames <- colnames(Trend_Dat)[!(colnames(Trend_Dat) %in% c("Numerator","Denominator","Total","Year"))]
  }
  Dat <- Trend_Dat %>% dplyr::select(c("Numerator", "Year", all_of(ColNames))) %>% 
    pivot_longer(cols=ColNames, names_to="Category",
                 values_to="Weight") %>%
    dplyr::mutate(Category=factor(Category,
                  levels=ColNames, labels=ColNames))
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
  colorders <- ColOrders[[name]]
  if (paste(name, "Trend", sep="_") %in% names(ColOrders)) {
    colorders <- ColOrders[[paste(name, "Trend", sep="_")]]
  } else if (paste(name, "Trend", "Addl", sep="_") %in% names(ColOrders)) {
    colorders <- c(colorders, ColOrders[[paste(name, "Trend", "Addl", sep="_")]])
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
  save(x, file=paste0("int/TrendPlot_",name,".Rda"))
  
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
       width=14, height=6, units="in", dpi=300, device=png)

# Proportion Plots
Props_Use <- c("EC", "Senate", "House", "Population")

Plot_Props <- function(Name, Year=2020, ColNames=NULL) {
  load(paste0("res/",Name,"_",Year,".Rda"))
  Props <- get(paste(Name, Year, "Res", sep="_"))$Proportions
  if (is.null(ColNames)) {
    Cols <- colnames(Props)[!(colnames(Props) %in% c("Analysis","Total_Prop"))]
  } else {
    Cols <- paste(ColNames, "Prop", sep="_")
  }
  Props <- Props %>%
    dplyr::select(c("Analysis",all_of(Cols))) %>%
    dplyr::filter(Analysis %in% Props_Use) %>%
    dplyr::mutate(Analysis=factor(Analysis,
                                  levels=Props_Use, labels=Props_Use)) %>%
    pivot_longer(cols=Cols, 
                 names_to=c("Category","Hold"), names_sep="_",
                 values_to="Proportion") 
  if (is.null(ColNames)) {
    Props <- Props %>%
      dplyr::arrange(Analysis, desc(Category))
  } else {
    Props <- Props %>%
      dplyr::mutate(Category=factor(Category, levels=ColNames, labels=ColNames)) %>%
      dplyr::arrange(Analysis, Category)
  }
  Props <- Props %>%
    bind_cols(Props %>% group_by(Analysis) %>%
                dplyr::reframe(CS=cumsum(Proportion)-Proportion) %>%
                dplyr::select(CS)) %>%
    dplyr::mutate(Position=CS+Proportion/2,
                  Percentage=paste0(format(round(Proportion*100, digits=2),
                                           digits=2, nsmall=2), "%"),
                  PercLab=if_else(Proportion < 0.05,
                                  NA_character_, Percentage))
  plot <- ggplot(Props) +
    geom_col(mapping=aes(y=Analysis, fill=Category,
                         x=Proportion),
             position = position_stack(reverse=TRUE)) +
    geom_text(mapping=aes(x=Position,
                          y=Analysis,
                          label=PercLab),
               hjust="center",
              size=2.1) +
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

Table <- function(Name, Year=2020, ColNames=NULL) {
  load(paste0("res/",Name,"_",Year,".Rda"))
  LX <- get(paste(Name, Year, "Res", sep="_"))
  Cols <- colnames(LX$Relative)
  Cols <- Cols[!(Cols %in% c("Numerator","Denominator"))]
  tbl <- LX$Proportions %>% dplyr::filter(Analysis=="Population") %>%
    dplyr::select(-c("Total_Prop")) %>%
    pivot_longer(cols=paste(Cols, "Prop", sep="_"),
                 names_to=c("Category", "Hold"), names_sep="_",
                 values_to="Population Proportion") %>%
    dplyr::select(-c("Analysis","Hold"))
  
  AbsW <- LX$Weights %>% dplyr::filter(Numerator %in% Numerators,
                                       Denominator=="Population") %>%
    dplyr::select(c("Numerator",all_of(Cols))) %>%
    pivot_longer(cols=Cols, names_to="Category", values_to="W") %>%
    pivot_wider(id_cols=Category, names_from=Numerator, values_from="W",
                names_prefix="Abs. Weights: ")
  
  RelW <- LX$Relative %>% dplyr::filter(Numerator %in% Numerators,
                                       Denominator=="Population") %>%
    dplyr::select(c("Numerator",all_of(Cols))) %>%
    pivot_longer(cols=Cols, names_to="Category", values_to="W") %>%
    pivot_wider(id_cols=Category, names_from=Numerator, values_from="W",
                names_prefix="Rel. Weights: ")
  
  Exc <- LX$Excess %>% dplyr::filter(Numerator %in% Numerators,
                                     Denominator=="Population") %>%
    dplyr::select(c("Numerator",all_of(Cols))) %>%
    pivot_longer(cols=Cols, names_to="Category", values_to="W") %>%
    pivot_wider(id_cols=Category, names_from=Numerator, values_from="W",
                names_prefix="Excess Pop: ")
  
  # %>% dplyr::arrange(desc(`Population Proportion`))
}