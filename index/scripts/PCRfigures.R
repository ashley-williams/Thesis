sites <- read_csv("data/sites.csv")
ompcr <- read.csv("data/OMpcr.csv") %>% 
  gather(primer, concentration, HgcA:SRB, factor_key=TRUE) %>% 
  mutate(type = case_when(Scheme == "Dredge" ~ "Channel",
                          Scheme == "Ponar" ~ "Channel",
                          TRUE ~ "Shore"))
ompcr$Depth <- as.factor(as.double(ompcr$Depth))
ompcr$Sample <- factor(ompcr$Sample, levels = sites$`Site ID`)
hgcaom <- ompcr %>% filter(type=="Shore", primer == "HgcA")
hgcacompare <- compare_means(concentration ~ Sample, data = hgcaom, ref.group = "D-01") %>% mutate(target = "HgcA")


```{r hgcaomp, include=FALSE}

ggboxplot(hgcaom, x = "Sample", y = "concentration", 
          color = "Sample", add = c("mean_se", "jitter"),
          legend = "none", 
          main = "HgcA Normalized to Organic Matter",
          ylab = ylab, xlab = xlab) +
  stat_compare_means(ref.group = "D-01", label = "p.signif",
                     label.y = c(20010), hide.ns = T, size = fontsize)  
ggsave("figure/hgcaomp.png", height=5.5)
```


```{r ferbomp,  include=FALSE}
ferbom <- ompcr %>% filter(type=="Shore", primer == "FeRB")
ferbcompare <- compare_means(concentration ~ Sample, data = ferbom, ref.group = "D-01") %>% mutate(target = "FeRB") 
ggboxplot(ferbom, x = "Sample", y = "concentration", 
          color = "Sample", add = c("mean_se", "jitter"),
          legend = "none", 
          main = "FeRB Normalized to Organic Matter",
          ylab = ylab, xlab = xlab) +
  stat_compare_means(ref.group = "D-01", label = "p.signif",
                     label.y = max(ferbom$concentration+20), hide.ns = T, size = fontsize) 
ggsave(filename = "figure/ferbomp.png", height=5.5)

```

```{r srbomp, include=FALSE}
srbom <- ompcr %>% filter(type=="Shore", primer == "SRB")
srbcompare <- compare_means(concentration ~ Sample, data = srbom, ref.group = "D-01") %>% mutate(target = "SRB") 
ggboxplot(srbom, x = "Sample", y = "concentration", 
          color = "Sample", add = c("mean_se", "jitter"),
          legend = "none", 
          main = "SRB Normalized to Organic Matter",
          ylab = ylab, xlab = xlab) +
  stat_compare_means(ref.group = "D-01", label = "p.signif",
                     label.y = max(srbom$concentration+0.2), hide.ns = T, size = fontsize)
ggsave(filename = "figure/srbomp.png", height=5.5)
```