getwd()
setwd("C:/Users/asw/Desktop/MSThesis/Coal ash/Metal Tolerance/R/growth")
library(tidyr)
library(dplyr)
library(broom)
# read all of the gompertz model results
read_plus <- function(flnm) {
  read.csv(flnm) %>% 
    mutate(Isolate = gsub('.{7}$', '', flnm))
}

results <-
  list.files(pattern = "*res.csv") %>% 
  map_df(~read_plus(.))  

##I just renamed the data to remove the 2  
#DR74 <- read.csv("DRp74res_2.csv") %>% 
#  mutate(Isolate = "DRp74") %>% 
#  rbind(results)
#results <- DR74

library(broom)
# analyze the results
Kresults <- results %>% 
  group_by(Isolate, Metal) %>% 
  do(tidy(kruskal.test(K~factor, data = .)))

muresults <- results %>% 
  group_by(Isolate, Metal) %>% 
  do(tidy(kruskal.test(mumax~factor, data = .)))

allresults <- merge(Kresults[,1:4], muresults[,1:4], by=c("Isolate", "Metal")) %>% 
  rename("K.statistic" = "statistic.x", "K.pvalue" = "p.value.x", "mumax.statistic" = "statistic.y", "mumax.pvalue" = "p.value.y" )

allresults %>% group_by(Isolate) %>% tally()



write.csv(allresults, "C:/Users/asw/Desktop/MSThesis/index/data/toleranceTest.csv")
allresults <- read.csv("C:/Users/asw/Desktop/MSThesis/index/data/toleranceTestremoved.csv")


## make a better looking table for the thesis
library(tidyverse)
library(magrittr); requireNamespace("tidyr");
library(tidyr)

Kpvalues <- allresults[,c(1,2,3,5)]

Kp <- Kpvalues %>%
  tidyr::pivot_wider(
    names_from  = c(Metal), # Can accommodate more variables, if needed.
    values_from = c(K.pvalue)
  )
#write.csv(Kp, "C:/Users/asw/Desktop/MSThesis/index/data/kpvalues.csv", row.names = F)
#Kpvalues <- read.csv("C:/Users/asw/Desktop/MSThesis/index/data/kpvalues.csv") %>% mutate_if(is.numeric, round, digits=3)

muvalues <- allresults[,c(1,2,3,7)]
mup <- muvalues %>%
  tidyr::pivot_wider(
    names_from  = c(Metal), # Can accommodate more variables, if needed.
    values_from = c(mumax.pvalue)
  )
#write.csv(mup, "C:/Users/asw/Desktop/MSThesis/index/data/muvalues.csv", row.names = F)
#Kpvalues <- read.csv("C:/Users/asw/Desktop/MSThesis/index/data/kpvalues.csv") %>% mutate_if(is.numeric, round, digits=3)


library(ggpubr)
##compare_means(K ~ factor, data = allresults[which(allresults$Metal=='Cd', allresults$Isolate == 'SI4')]) 
DR24 <- results %>% filter(Metal=='Cd', Isolate=="DR24_2")  %>% 
  compare_means(K~factor, .)
SI4 <- results %>% filter(Metal=='Cd', Isolate=="SI4")  %>% 
  compare_means(K~factor, .)

(DR24test <- pairwise.wilcox.test(DR24$K, DR24$factor,
                    p.adjust.method = "BH"))
DR3.2 <- results %>% filter(Metal=='Se', Isolate=="DR3.2")  
  
(DR3.2test <- pairwise.wilcox.test(DR3.2$mumax, DR3.2$factor,
                                    p.adjust.method = "none"))
compare_means(K~factor, data = DR3.2, ref.group = "0")  

DRp51 <- results %>% filter(Metal=='Se', Isolate=="DRp51")  

compare_means(K~factor, data = DRp51, ref.group = "0")  
pairwise.wilcox.test(DRp51$K, DRp51$factor,
                     p.adjust.method = "BH")


## recheck SI4
si4Kresults <- res %>% 
  group_by(Metal) %>% 
  do(tidy(kruskal.test(K~factor, data = .)))

si4muresults <- res %>% 
  group_by(Metal) %>% 
  do(tidy(kruskal.test(mumax~factor, data = .)))


```{r kpvalue, eval=FALSE, include=FALSE, results='asis'}
library(kableExtra)
library(tidyverse)
read.csv("data/kpvalues.csv") %>% 
  mutate_if(is.numeric, function(x) {
    cell_spec(x, format = "latex", bold = ifelse(x > 0.05, T, F))
  }) %>%
  kable(booktabs=T, caption = "P-values of Kruskall-Wallis test of carrying capacity differences between metal concentration ", align = "l",linesep = "", digits = 3 , format = "latex") %>% 
  row_spec(0, align = "c") %>% 
  kable_styling(latex_options = "striped")
```
