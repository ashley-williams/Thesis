## to make figures for the thesis
library(tidyverse)
data <- read.csv("index/data/metal_tolerance_OD_cleaned.csv") 
library(ggplot2)
library(ggpubr)
data$factor <- factor(data$factor)
## ggplot

library(gtable)
library(cowplot)
library(grid)

# Function to move the legend
shift_legend <- function(p){
  
  # check if p is a valid object
  if(!"gtable" %in% class(p)){
    if("ggplot" %in% class(p)){
      gp <- ggplotGrob(p) # convert to grob
    } else {
      message("This is neither a ggplot object nor a grob generated from ggplotGrob. Returning original plot.")
      return(p)
    }
  } else {
    gp <- p
  }
  
  # check for unfilled facet panels
  facet.panels <- grep("^panel", gp[["layout"]][["name"]])
  empty.facet.panels <- sapply(facet.panels, function(i) "zeroGrob" %in% class(gp[["grobs"]][[i]]))
  empty.facet.panels <- facet.panels[empty.facet.panels]
  if(length(empty.facet.panels) == 0){
    message("There are no unfilled facet panels to shift legend into. Returning original plot.")
    return(p)
  }
  
  # establish extent of unfilled facet panels (including any axis cells in between)
  empty.facet.panels <- gp[["layout"]][empty.facet.panels, ]
  empty.facet.panels <- list(min(empty.facet.panels[["t"]]), min(empty.facet.panels[["l"]]),
                             max(empty.facet.panels[["b"]]), max(empty.facet.panels[["r"]]))
  names(empty.facet.panels) <- c("t", "l", "b", "r")
  
  # extract legend & copy over to location of unfilled facet panels
  guide.grob <- which(gp[["layout"]][["name"]] == "guide-box")
  if(length(guide.grob) == 0){
    message("There is no legend present. Returning original plot.")
    return(p)
  }
  gp <- gtable_add_grob(x = gp,
                        grobs = gp[["grobs"]][[guide.grob]],
                        t = empty.facet.panels[["t"]],
                        l = empty.facet.panels[["l"]],
                        b = empty.facet.panels[["b"]],
                        r = empty.facet.panels[["r"]],
                        name = "new-guide-box")
  
  # squash the original guide box's row / column (whichever applicable)
  # & empty its cell
  guide.grob <- gp[["layout"]][guide.grob, ]
  if(guide.grob[["l"]] == guide.grob[["r"]]){
    gp <- gtable_squash_cols(gp, cols = guide.grob[["l"]])
  }
  if(guide.grob[["t"]] == guide.grob[["b"]]){
    gp <- gtable_squash_rows(gp, rows = guide.grob[["t"]])
  }
  gp <- gtable_remove_grobs(gp, "guide-box")
  
  return(gp)
}


f <- facet_wrap(~ metal)
l <- geom_line()
lab <- labs(x="Incubation Time (hr)", y= expression(paste(OD [" 580nm"]))) + 
  theme(axis.text.x=element_text(size=10)) 
leg <- labs(color=expression(paste("Concentration")))

# facet
g <- facet_grid(~ metal)
h <- theme(strip.background =element_blank(), strip.text = element_text(face="bold", size = 12))

t <- theme(plot.margin = margin(t = .5, unit = "cm") , plot.title = element_text(size = 12))

## DR13
pdf("index/figure/dr13plot.pdf")
DR13p <- data %>% filter(IsolateID=="DR 13") %>% 
  ggplot(aes(hours, od, group= factor, 
                                color=factor)) + geom_point(size=0.5 ) + stat_summary(aes(group=factor), fun.y = mean, geom = "line") + 
  theme(axis.text.x=element_text(size=10)) +
  labs(x="Incubation Time (hr)", y= expression(paste(OD [" 580nm"]))) + 
  labs(color=expression(paste("Factor"))) +
  facet_wrap(~ Metal, scales = "free") + theme_classic() + h +
  guides(fill = guide_legend(title.position = "top",
                             label.position = "bottom",
                             nrow = 1)) +
  theme(legend.direction = "horizontal")
grid.draw(shift_legend(DR13p))
dev.off()


## DR3.2 
pdf("index/figure/dr32plot.pdf")
DR3.2p <- data %>% filter(IsolateID=="DR 3-2") %>% 
  ggplot(aes(hours, od, group= factor, 
             color=factor)) + geom_point(size=0.5) + stat_summary(aes(group=factor), fun.y = mean, geom = "line") + 
  theme(axis.text.x=element_text(size=10)) +
  labs(x="Incubation Time (hr)", y= expression(paste(OD [" 580nm"]))) + 
  labs(color=expression(paste("Factor"))) +
  facet_wrap(~ Metal, scales = "free") + 
  theme_classic() + h +
  guides(fill = guide_legend(title.position = "top",
                             label.position = "bottom",
                             nrow = 1)) +
  theme(legend.direction = "horizontal")
grid.draw(shift_legend(DR3.2p))
dev.off()


## DR52
pdf("index/figure/dr52plot.pdf")
DR52p <- data %>% filter(IsolateID=="DR 52") %>% 
  ggplot(aes(hours, od, group= factor, 
             color=factor)) + geom_point(size=0.5 ) + stat_summary(aes(group=factor), fun.y = mean, geom = "line") + 
  theme(axis.text.x=element_text(size=10)) +
  labs(x="Incubation Time (hr)", y= expression(paste(OD [" 580nm"]))) + 
  labs(color=expression(paste("Factor"))) +
  facet_wrap(~ Metal, scales = "free") + theme_classic() + h +
  guides(fill = guide_legend(title.position = "top",
                             label.position = "bottom",
                             nrow = 1)) +
  theme(legend.direction = "horizontal")
grid.draw(shift_legend(DR52p))
dev.off()


##SI4
pdf("index/figure/si4plot.pdf")
(si4p<-data %>% filter(IsolateID=="SI 4") %>% 
  ggplot(aes(hours, od, group= factor, 
             color=factor)) + geom_point(size=0.5 ) + stat_summary(aes(group=factor), fun.y = mean, geom = "line") + 
  theme(axis.text.x=element_text(size=10)) +
  labs(x="Incubation Time (hr)", y= expression(paste(OD [" 580nm"]))) + 
  labs(color=expression(paste("Factor"))) +
  facet_wrap(~ Metal, scales = "free") + theme_classic() + h +
  guides(fill = guide_legend(title.position = "top",
                             label.position = "bottom",
                             nrow = 1)) +
  theme(legend.direction = "horizontal") + theme(legend.position="bottom"))
grid.draw(shift_legend(si4p))
dev.off()

## DR5
pdf("index/figure/dr5plot.pdf")
DR5p <- data %>% filter(IsolateID=="DR 5") %>% 
  ggplot(aes(hours, od, group= factor, 
             color=factor)) + geom_point(size=0.5 ) + stat_summary(aes(group=factor), fun.y = mean, geom = "line") + 
  theme(axis.text.x=element_text(size=10)) +
  labs(x="Incubation Time (hr)", y= expression(paste(OD [" 580nm"]))) + 
  labs(color=expression(paste("Factor"))) +
  facet_wrap(~ Metal, scales = "free") + theme_classic() + h +
  guides(fill = guide_legend(title.position = "top",
                             label.position = "bottom",
                             nrow = 1)) +
  theme(legend.direction = "horizontal")
grid.draw(shift_legend(DR5p))
dev.off()

####################
## Phylogeny of isolated stuff - need a csv file with everything
library(janitor)
phy <- read.csv("index/data/phylog.csv")
ggplot(phy, aes(x=Putative.Organism)) +
  geom_bar()


### to lapply through plots
#' @param yvar character name of the y variable
plotserieslines <- function(yvar){
  ggplot(airquality, aes_(x=~Day,y=as.name(yvar))) +
    geom_line() +
    facet_wrap(~Month)
}
lapply(names(airquality[c(1:4)]), plotserieslines)