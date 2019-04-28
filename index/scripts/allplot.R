library(readr)
library(ggplot2)
metal <- read_csv("index/data/metal_tolerance_OD.csv", col_types = cols(X1 = col_skip(), factor = col_character()))

f <- facet_wrap(~ metal)
l <- geom_line()
lab <- labs(x="Incubation Time (hr)", y= expression(paste(OD [" 580nm"]))) + 
  theme(axis.text.x=element_text(size=10))
leg <- labs(color=expression(paste(mu, " ",L^-1)))


plot <- ggplot((metal), aes(hours, od, color = factor)) + geom_vline(xintercept = c(10, 30), linetype = 'dashed', color = 'grey')
meanplot <- plot + stat_summary(aes(group=factor), fun.y = mean, geom = "line")
allplot <- meanplot + facet_grid(IsolateID ~ Metal) 
allplot
