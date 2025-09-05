library(tidyverse)
library(patchwork)
library(scales)

source("R/00-key_values.R")

# Trend Plots:

Trend_Viz <- function(Name, Type="Census",
                      Log=TRUE, 
                      Fac_Scales="free", Fac_ggsave=TRUE,
                      Col_ggsave=FALSE) {
  ## If _ggsave = TRUE, it will save those plots as pngs
  ## Regardless, it saves .Rdas of the plots
  load(paste0("res/", Type, "/", Name, "_Res.Rda"))
  Full <- get(paste(Name, "Res", sep="_"))
  
  for (Denom in Denominators) {
    Dat <- Full %>% dplyr::filter(Denominator==Denom)
    
    plot <- ggplot(Dat, mapping=aes(x=Year, y=`Abs. Weight`, 
                                    group=interaction(Category,Analysis), 
                                    linetype=Category, 
                                    color=Category)) +
      geom_point() +
      geom_line() +
      theme_bw() + 
      facet_wrap(facets=~factor(Analysis, levels=rev(levels(Analysis))), 
                 dir="v",
                 scales=Fac_Scales) +
      theme(legend.position="bottom",
            legend.title=element_blank()) +
      guides(linetype=guide_legend(nrow=2, byrow=TRUE),
             color=guide_legend(nrow=2, byrow=TRUE))
    
    if (Log) {
      plot <- plot + scale_y_log10(name="Weight") +
        scale_x_continuous(breaks=CensusYrs, minor_breaks=NULL)
    } else {
      plot <- plot + scale_y_continuous(name="Weight") +
        scale_x_continuous(breaks=CensusYrs, minor_breaks=NULL)
    }
    
    if (Fac_ggsave) {
      ggsave(filename=paste0("figs/",Type,"/",Denom,"/Trend_",Name,".png"),
             plot=plot,
             device=png,
             width=4, height=6, units="in", dpi=300)
    }
    
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
        scale_y_log10(limits=Trend_Scales[[num]]) +
        theme(legend.position="bottom",
              legend.title=element_blank()) +
        labs(subtitle=num)
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
    
    p_col <- (get(paste("p", Numerators[length(Numerators)], sep="_")) + labs(title=Titles[Name]))
    for (num in Numerators[(length(Numerators)-1):1]) {
      p_col <- p_col /
        get(paste("p", num, sep="_"))
    }
    p_col <- p_col + 
      plot_layout(guides="collect") & theme(legend.position="bottom")
    
    if (Col_ggsave) {
      ggsave(filename=paste0("figs/",Type,"/",Denom,"/Trend_",Name,"_Col.png"),
             plot=p_col,
             device=png,
             width=4, height=6, units="in", dpi=300)
    }
    
    DenNum <- (1:length(Denominators))[Denominators==Denom]
    assign(x=paste("Col", DenNum, Name, sep="_"),
           value=p_col)
    assign(x=paste("Facet", DenNum, Name, sep="_"),
           value=plot)
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

# Proportion Plots:

Plot_Props <- function(Name, Yr=2020, 
                       Type="Census", Prop_ggsave=TRUE) {
  load(paste0("res/",Type,"/",Name,"_Res.Rda"))
  
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
      dplyr::mutate(Analysis=Denom) %>%
      rename(Proportion=`Population Proportion`)
    
    Props <- Dat %>% dplyr::select(-c("Population Proportion")) %>%
      bind_rows(Pop) %>%
      dplyr::filter(Category %in% Cols) %>%
      dplyr::mutate(Analysis=factor(Analysis, levels=c(Numerators,Denom))) %>%
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
                   Name, sep="_"),
           value=plot)
  }
    
  save(list=paste("Props", 1:length(Denominators), Name, sep="_"),
       file=paste0("res/",Type,"_Props/",Name,"_",Yr,".Rda"))
}

for (Name in Names) {
  for (Yr in CensusYrs) {
    Plot_Props(Name, Yr,
               Type="Census", Prop_ggsave=TRUE)
  }
}
