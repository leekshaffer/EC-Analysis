library(tidyverse)
library(patchwork)
library(scales)
library(xtable)

source("R/00-key_values.R")

for (Type in c("Census")) {
  for (Name in Names) {
    load(paste0("res/", Type, "/", Name, "_Res.Rda"))
  }
}

# Trend Plots:

Trend_Viz <- function(Name, Type="Census",
                      Log=TRUE, 
                      Fac_Scales="free", Fac_ggsave=TRUE,
                      Col_ggsave=TRUE) {
  ## If _ggsave = TRUE, it will save those plots as pngs
  ## Regardless, it saves .Rdas of the plots
  Full <- get(paste(Name, "Res", sep="_"))
  
  for (Denom in Denominators) {
    Dat <- Full %>% dplyr::filter(Denominator==Denom)
    
    # p_fac <- ggplot(Dat, mapping=aes(x=Year, y=`Abs. Weight`, 
    #                                 group=interaction(Category,Analysis), 
    #                                 linetype=Category, 
    #                                 color=Category)) +
    #   geom_point() +
    #   geom_line() +
    #   theme_bw() + 
    #   facet_wrap(facets=~factor(Analysis, levels=rev(levels(Analysis))), 
    #              # dir="v",
    #              nrow=2, ncol=2,
    #              scales=Fac_Scales) +
    #   theme(legend.position="bottom",
    #         legend.title=element_blank()) +
    #   guides(linetype=guide_legend(nrow=2, byrow=TRUE),
    #          color=guide_legend(nrow=2, byrow=TRUE))
    # 
    # if (Log) {
    #   p_fac <- p_fac + scale_y_log10(name="Weight") +
    #     scale_x_continuous(breaks=CensusYrs, minor_breaks=NULL)
    # } else {
    #   p_fac <- p_fac + scale_y_continuous(name="Weight") +
    #     scale_x_continuous(breaks=CensusYrs, minor_breaks=NULL)
    # }
    
    for (num in Numerators) {
      p1 <- ggplot(Dat %>% dplyr::filter(Analysis==num),
                   mapping=aes(x=Year, y=`Abs. Weight`,
                               group=Category,
                               linetype=Category,
                               color=Category)) +
        geom_point() +
        geom_line() +
        theme_bw() +
        scale_x_continuous(breaks=CensusYrs, minor_breaks=NULL) +
        theme(legend.position="bottom") +
        labs(title=names(Num_Titled)[Num_Titled==num],
             linetype=Titles[Name],
             color=Titles[Name])
      if (length(unique(Dat$Category)) > 3) {
        if (length(unique(Dat$Category)) > 6) {
          p1 <- p1 +
            guides(linetype=guide_legend(nrow=3, byrow=TRUE),
                   color=guide_legend(nrow=3, byrow=TRUE))
        } else {
          p1 <- p1 +
            guides(linetype=guide_legend(nrow=2, byrow=TRUE),
                   color=guide_legend(nrow=2, byrow=TRUE))
        }
      }
      assign(x=paste("p", num, sep="_"),
             value=p1)
    }
    
    p_col <- get(paste("p", Numerators[length(Numerators)], sep="_")) +
      scale_y_log10(name="Weight", limits=Trend_Scales[[num]]) +
      labs(title=names(Num_Titled)[Num_Titled==num],
           color="",
           linetype="") +
      guides(linetype=guide_legend(nrow=3, byrow=TRUE),
             color=guide_legend(nrow=3, byrow=TRUE))
    
    for (num in Numerators[(length(Numerators)-1):1]) {
      p_col <- p_col +
        get(paste("p", num, sep="_")) +
        scale_y_log10(name="Weight", limits=Trend_Scales[[num]]) +
        labs(title=names(Num_Titled)[Num_Titled==num],
             color="",
             linetype="") +
        guides(linetype=guide_legend(nrow=3, byrow=TRUE),
               color=guide_legend(nrow=3, byrow=TRUE))
    }
    p_col <- p_col + 
      plot_layout(ncol=1, guides="collect") & theme(legend.position="bottom")
    
    p_fac <- get(paste("p", Numerators[length(Numerators)], sep="_")) +
      geom_hline(yintercept=1, linetype="dashed", color="gray50") +
      scale_y_log10(name="Weight") +
      guides(linetype=guide_legend(direction="vertical"),
             color=guide_legend(direction="vertical")) +
      labs(tag=paste0(LETTERS[1], ")"))
    j <- 2
    for (num in Numerators[(length(Numerators)-1):1]) {
      p_fac <- p_fac +
        get(paste("p", num, sep="_")) +
        geom_hline(yintercept=1, linetype="dashed", color="gray50") +
        scale_y_log10(name="Weight") +
        guides(linetype=guide_legend(direction="vertical"),
               color=guide_legend(direction="vertical")) +
        labs(tag=paste0(LETTERS[j], ")"))
      j <- j+1
    }
    p_fac <- p_fac + guide_area() +
      plot_layout(ncol=2, nrow=2, byrow=TRUE, guides="collect") & 
      theme(legend.position="bottom")
    
    if (Col_ggsave) {
      ggsave(filename=paste0("figs/",Type,"/",Denom,"/Trend_",Name,"_Col.png"),
             plot=p_col &
               theme(text=element_text(size=10)),
             device=png,
             width=3, height=7, units="in", dpi=300)
    }
    
    if (Fac_ggsave) {
      ggsave(filename=paste0("figs/",Type,"/",Denom,"/Trend_",Name,".png"),
             plot=p_fac,
             device=png,
             width=7, height=6, units="in", dpi=300)
    }
    
    DenNum <- (1:length(Denominators))[Denominators==Denom]
    assign(x=paste("Col", DenNum, Name, sep="_"),
           value=p_col)
    assign(x=paste("Facet", DenNum, Name, sep="_"),
           value=p_fac)
  }

  save(list=paste("Facet", 1:length(Denominators), Name, sep="_"),
       file=paste0("res/",Type,"_Facets/", Name, ".Rda"))
  save(list=paste("Col", 1:length(Denominators), Name, sep="_"),
       file=paste0("res/",Type,"_Cols/", Name, ".Rda"))
}

Trend_Viz_All <- function(Names, Names_Sub=NULL,
                          Type="Census") {
  for (Name in Names) {
    Trend_Viz(Name)
    load(file=paste0("res/",Type,"_Cols/",Name,".Rda"))
  }
  for (Denom in Denominators) {
    DenNum <- (1:length(Denominators))[Denominators==Denom]
    p_big <- NULL
    p_sub <- NULL
    for (Name in Names) {
      p_big <- p_big | get(paste("Col", DenNum, Name, sep="_"))
      if (Name %in% Names_Sub) {
        p_sub <- p_sub | get(paste("Col", DenNum, Name, sep="_"))
      }
    }
    ggsave(filename=paste0("figs/", Type, "/", Denom, "/Trend_All.png"),
           plot=p_big,
           width=12, height=7, units="in", dpi=300, device=png)
    if (!is.null(Names_Sub)) {
      ggsave(filename=paste0("figs/", Type, "/", Denom, "/Trend_Sub.png"),
             plot=p_sub,
             width=8.8, height=5.8, units="in", dpi=300, device=png)
    }
  }
}

Trend_Viz_All(Names, Names_Sub=Names[c(1,4,5)], Type="Census")

# Single-Year and -Analysis Weight Plots:

Plot_Wts <- function(Name, Yr=2020,
                     Num=Numerators[1],
                     Type="Census",
                     Log=TRUE, Wt_ggsave=TRUE) {

  Dat <- get(paste(Name, "Res", sep="_")) %>%
    dplyr::filter(Year==Yr,
                  Analysis==Num) %>%
    dplyr::mutate(Label=format(round(`Abs. Weight`, digits=3),
                               nsmall=3, digits=3))
  
  ColNames <- ColOrders[[Name]]
  Cols <- unique(Dat$Category)
  
  for (Denom in Denominators) {
    plot <- ggplot(Dat %>% dplyr::filter(Denominator==Denom),
           mapping=aes(x=Category, y=`Abs. Weight`,
                       color=Category)) +
      geom_point(size=2.5) +
      geom_text(mapping=aes(label=Label),
                vjust=-1) +
      geom_hline(yintercept=1, color="gray50", linetype="dashed") +
      theme_bw() +
      theme(legend.position="none",
            legend.title=element_blank()) +
      labs(x="Demographic Category",
           title=paste0(names(Num_Titled)[Num_Titled==Num], " Weights by ", Titles[Name]))
    
    if (Log) {
      plot <- plot + scale_y_log10(name="Weight",
                                   expand=expansion(mult=c(0.05, 0.15)))
    } else {
      plot <- plot + scale_y_continuous(name="Weight",
                                        expand=expansion(mult=c(0.05, 0.15)))
    }
    
    if (length(Cols) < length(ColNames)) {
      Cols_Spec <- (1:length(ColNames))[ColNames %in% Cols]
      plot <- plot +
        scale_color_manual(values=hue_pal()(length(ColNames))[Cols_Spec])
    }
    
    DenNum <- (1:length(Denominators))[Denominators==Denom]
    if (Wt_ggsave) {
      ggsave(filename=paste0("figs/",Type,"/",Denom,"/Weights/",
                             Name,"_",Yr,"_",Num,".png"),
             plot=plot + labs(title=""),
             device=png, width=6, height=3, units="in", dpi=300)
    }
    assign(x=paste("Weights", DenNum, 
                   Name, Yr, Num, sep="_"),
           value=plot)
  }
  save(list=paste("Weights", 1:length(Denominators), Name, Yr, Num, sep="_"),
       file=paste0("res/",Type,"_Weights/",
                   Name, "_", Yr, "_", Num, ".Rda"))
}

for (Name in Names) {
  for (Yr in CensusYrs) {
    for (Num in Numerators) {
      Plot_Wts(Name, Yr, Num)
    }
  }
}

# Proportion Plots:

Plot_Props <- function(Name, Yr=2020, 
                       Type="Census", Prop_ggsave=TRUE) {
  ColNames <- ColOrders[[Name]]
  if (paste(Name, Yr, sep="_") %in% names(Cols_Specs)) {
    Cols_Spec <- Cols_Specs[[paste(Name, Yr, sep="_")]]
    Cols <- ColNames[Cols_Spec]
  } else {
    Cols_Spec <- NULL
    Cols <- ColNames
  }
  
  for (Denom in Denominators) {
    Dat <- get(paste(Name, "Res", sep="_")) %>%
      dplyr::filter(Year==Yr, Denominator==Denom)
    
    Pop <- Dat %>% dplyr::filter(Analysis==Numerators[1]) %>%
      dplyr::select(Analysis,Category,`Population Proportion`) %>%
      dplyr::mutate(Analysis="Baseline") %>%
      rename(Proportion=`Population Proportion`)
    
    Props <- Dat %>% dplyr::select(-c("Population Proportion")) %>%
      bind_rows(Pop) %>%
      dplyr::filter(Category %in% Cols) %>%
      dplyr::mutate(Analysis=factor(Analysis, levels=c(Numerators,"Baseline"))) %>%
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
                    Label_Use=if_else(Proportion < 0.08,
                                      "", Label),
                    PercLabel=if_else(Proportion < 0.08,
                                      "", Percentage))
    plot <- ggplot(Props_Viz) +
      geom_col(mapping=aes(y=Analysis, fill=Category,
                           x=Proportion),
               position = position_stack(reverse=TRUE)) +
      geom_text(mapping=aes(x=Position,
                            y=Analysis,
                            label=PercLabel),
                 hjust="center") +
      theme_bw() +
      labs(y="", fill="",
           title="") +
           # title=paste0("Population Proportions by ",Titles[Name])) +
      theme(legend.position="bottom")
    if (length(Cols) > 4) {
      plot <- plot + 
        guides(fill=guide_legend(nrow=2, byrow=TRUE))
                                 # reverse=TRUE))
    }
    if (!is.null(Cols_Spec)) {
      plot <- plot +
        scale_fill_manual(values=hue_pal()(length(ColNames))[Cols_Spec])
    }
    
    if (Prop_ggsave) {
      ggsave(filename=paste0("figs/",Type,"/",Denom,"/Props_",Name,"_",Yr,".png"),
             plot=plot,
             device=png, width=6, height=3, units="in", dpi=300)
    }
    assign(x=paste("Props", (1:length(Denominators))[Denominators==Denom], 
                   Name, Yr, sep="_"),
           value=plot)
  }
    
  save(list=paste("Props", 1:length(Denominators), Name, Yr, sep="_"),
       file=paste0("res/",Type,"_Props/",Name,"_",Yr,".Rda"))
}

for (Name in Names) {
  for (Yr in CensusYrs) {
    Plot_Props(Name, Yr,
               Type="Census", Prop_ggsave=TRUE)
  }
}

## Table Function for Manuscript:

Print_Tbl <- function(Type, Yr, Denom, Name, 
                      Keep_Cols=c("Abs. Weight", "Rel. Weight", "Excess Pop."),
                      KC_Names=c("AW", "RW", "EP")) {
  load(file=paste0("res/", Type, "/", Name, "_Res.Rda"))
  Dat <- get(paste(Name, "Res", sep="_")) %>%
    dplyr::filter(Year==Yr, Denominator==Denom) %>%
    dplyr::mutate(AW=format(round(`Abs. Weight`, digits=3), digits=3, nsmall=3),
                  RW=format(round(`Rel. Weight`, digits=3), digits=3, nsmall=3),
                  EP=format(round(`Excess Pop.`, digits=0), nsmall=0, big.mark=",")) %>%
    dplyr::select(Analysis, Category, all_of(KC_Names)) %>%
    pivot_wider(names_from=Analysis, names_sep="_", names_vary="slowest",
                values_from=all_of(KC_Names))
  print(xtable(Dat), include.rownames=FALSE)
}

Print_Tbl("Census", 2020, "50 States and DC", "Pop_RE")
Print_Tbl("Census", 2020, "50 States and DC", "Pop_AgeCat")
Print_Tbl("Census", 2020, "50 States and DC", "Pop_Sex")
Print_Tbl("Census", 2020, "50 States and DC", "Pop_UR")
Print_Tbl("Census", 2020, "50 States and DC", "HH_RO")

