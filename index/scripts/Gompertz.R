# Gompertz
library(growthrates)
getwd()
# Files located
setwd("~/Google Drive File Stream/My Drive/Coal ash/Metal Tolerance/R/raw/raw")

# Save to
setwd("~/Google Drive File Stream/My Drive/Coal ash/Metal Tolerance/R/growth")

p <- c(y0 = 0.01, mumax = 0.2, K=0.1)
lower <- c(y0 = 1e-6, mumax = -.2, K=0)
upper <- c(y0 = 0.05, mumax = .6, K=0.5)

#_________________________________________________________________________
# DR+74

DRp74_2 <- read.csv("DRp74_2.csv")
xyplot(od~hours|as.factor(factor) + Metal, data = DRp74_2, groups = Replicate, pch = 16, cex = .5)

allDRp74_2 <- all_growthmodels(od ~ grow_gompertz(hours, parms) | Metal + factor + Replicate,
                               data = DRp74_2, 
                               p = p, lower = lower, upper = upper,
                               log = "y")

allDRp74res_2 <- results(allDRp74_2)

setwd("~/Google Drive File Stream/My Drive/Coal ash/Metal Tolerance/R/growth")

pdf("DRp74res_2.pdf")

xyplot(od~hours|as.factor(factor) + Metal, 
       data = DRp74_2, groups = Replicate, pch = 16, cex = .5)
xyplot(mumax ~ factor|Metal, 
       data = allDRp74res_2, layout = c(7,1), 
       main = "DR+ 74 mumax", type = c("p","a" ))
xyplot(K ~ factor|Metal, 
       data = allDRp74res_2, layout = c(7,1), 
       main = "DR+ 74 Carrying Capacity", type = c("p","a" ))
xyplot(r2 ~ factor|Metal, 
       data = allDRp74res_2, layout = c(7,1), 
       main = "DR+ 74 r2", type = c("p","a" ))
plot(allDRp74_2)

dev.off()

write.csv(allDRp74res_2, "DRp74res_2.csv")


#________________________________________________________________________
#DR 24

setwd("~/Google Drive File Stream/My Drive/Coal ash/Metal Tolerance/R/raw/raw")

DR24_2 <- read.csv("DR24.long.2.csv")

DR24_2fit <- all_growthmodels(od ~ grow_gompertz(hours, parms) | Metal + factor + Replicate,
                               data = DR24_2, 
                               p = p, lower = lower, upper = upper,
                               log = "y")

DR24_2res <- results(DR24_2fit)

setwd("~/Google Drive File Stream/My Drive/Coal ash/Metal Tolerance/R/growth")

pdf("DR24_2res.pdf")
xyplot(od~hours|as.factor(factor) + Metal, 
       data = DR24_2, groups = Replicate, pch = 16, cex = .5, 
       main = "DR 24")
xyplot(mumax ~ factor|Metal, 
       data = DR24_2res, layout = c(6,1), 
       main = "DR 24 mumax", type = c("p","a" ))
xyplot(K ~ factor|Metal, 
       data = DR24_2res, layout = c(6,1), 
       main = "DR 24 Carrying Capacity", type = c("p","a" ))
xyplot(r2 ~ factor|Metal, 
       data = DR24_2res, layout = c(6,1), 
       main = "DR 24 r2", type = c("p","a" ))
plot(DR24_2fit)
dev.off()

write.csv(DR24_2res, "DR24_2res.csv")

#_______________________________________________________________________
#DR 26

setwd("~/Google Drive File Stream/My Drive/Coal ash/Metal Tolerance/R/raw/raw")

DR26 <- read.csv("DR26raw.csv", check.names = F)
DR26.long <- melt(DR26, id=c("Metal", "Isolate ID", "Replicate", "Concentration", "factor"),
                  variable.name = "hours", value.name = "od") 
DR26.long$hours <- as.numeric(as.character(DR26.long$hours))
DR26.long$Concentration <- factor(DR26.long$Concentration)

seDR26 <- read.csv("SE DR 26.csv", check.names = F)
seDR26.long <- melt(seDR26, id=c("Metal", "Isolate ID", "Replicate", "Concentration", "factor"),
                    variable.name = "hours", value.name = "od") 
seDR26.long$hours <- as.numeric(as.character(seDR26.long$hours))
seDR26.long$Concentration <- factor(seDR26.long$Concentration)

write.csv(seDR26.long, "seDR26.long.csv")

#Binding Se to the rest.
DR26 <- rbind(DR26.long, seDR26.long)
write.csv(DR26, "DR26.long.csv")

DR26fit <- all_growthmodels(od ~ grow_gompertz(hours, parms) | Metal + factor + Replicate,
                              data = DR26, 
                              p = p, lower = lower, upper = upper,
                              log = "y")

p <- c(y0 = 0.01, mumax = 0.2, K=0.1)
lower <- c(y0 = 1e-6, mumax = -.2, K=0)
upper <- c(y0 = 0.05, mumax = .6, K=0.5)

DR26res <- results(DR26fit)

setwd("~/Google Drive File Stream/My Drive/Coal ash/Metal Tolerance/R/growth")

pdf("DR26res.pdf")
xyplot(od~hours|as.factor(factor) + Metal, 
       data = DR26, groups = Replicate, pch = 16, cex = .5, 
       main = "DR 26")
xyplot(mumax ~ factor|Metal, 
       data = DR26res, layout = c(7,1), 
       main = "DR 26 mumax", type = c("p","a" ))
xyplot(K ~ factor|Metal, 
       data = DR26res, layout = c(7,1), 
       main = "DR 26 Carrying Capacity", type = c("p","a" ))
xyplot(r2 ~ factor|Metal, 
       data = DR26res, layout = c(7,1), 
       main = "DR 26 r2", type = c("p","a" ))
plot(DR26fit)
dev.off()

write.csv(DR26res, "DR26res.csv")

#______________________________________________________________________________
# DR 3-2

setwd("~/Google Drive File Stream/My Drive/Coal ash/Metal Tolerance/R/raw/raw")

DR3.2raw <- read.csv("DR3.2raw.csv", check.names = F)
DR3.2.long <- melt(DR3.2raw, id=c("Metal", "Isolate ID", "Replicate", "Concentration", "factor"),
                  variable.name = "hours", value.name = "od") 
DR3.2.long$hours <- as.numeric(as.character(DR3.2.long$hours))
DR3.2.long$Concentration <- factor(DR3.2.long$Concentration)

DR3.2rawse <- read.csv("DR3.2rawse.csv", check.names = F)
DR3.2rawse.long <- melt(DR3.2rawse, id=c("Metal", "Isolate ID", "Replicate", "Concentration", "factor"),
                    variable.name = "hours", value.name = "od") 
DR3.2rawse.long$hours <- as.numeric(as.character(DR3.2rawse.long$hours))
DR3.2rawse.long$Concentration <- factor(DR3.2rawse.long$Concentration)

write.csv(DR3.2rawse.long, "DR3.2rawse.long.csv")

#Binding Se to the rest.
DR3.2 <- rbind(DR3.2.long, DR3.2rawse.long)
write.csv(DR3.2, "DR3.2.long.csv")

DR3.2fit <- all_growthmodels(od ~ grow_gompertz(hours, parms) | Metal + factor + Replicate,
                            data = DR3.2, 
                            p = p, lower = lower, upper = upper,
                            log = "y")


DR3.2res <- results(DR3.2fit)

setwd("~/Google Drive File Stream/My Drive/Coal ash/Metal Tolerance/R/growth")

pdf("DR3.2res.pdf")
xyplot(od~hours|as.factor(factor) + Metal, 
       data = DR3.2, groups = Replicate, pch = 16, cex = .5, 
       main = "DR 3-2")
xyplot(mumax ~ factor|Metal, 
       data = DR3.2res, layout = c(7,1), 
       main = "DR 3-2 mumax", type = c("p","a" ))
xyplot(K ~ factor|Metal, 
       data = DR3.2res, layout = c(7,1), 
       main = "DR 3-2 Carrying Capacity", type = c("p","a" ))
xyplot(r2 ~ factor|Metal, 
       data = DR3.2res, layout = c(7,1), 
       main = "DR 3-2 r2", type = c("p","a" ))
plot(DR3.2fit)
dev.off()

write.csv(DR3.2res, "DR3.2res.csv")

#________________________________________________________________________________________
#DR 5 

setwd("~/Google Drive File Stream/My Drive/Coal ash/Metal Tolerance/R/raw/raw")

DR5raw <- read.csv("DR5raw.csv", check.names = F)
DR5.long <- melt(DR5raw, id=c("Metal", "Isolate ID", "Replicate", "Concentration", "factor"),
                   variable.name = "hours", value.name = "od") 
DR5.long$hours <- as.numeric(as.character(DR5.long$hours))
DR5.long$Concentration <- factor(DR5.long$Concentration)

DR5rawse <- read.csv("DR5rawse.csv", check.names = F)
DR5rawse.long <- melt(DR5rawse, id=c("Metal", "Isolate ID", "Replicate", "Concentration", "factor"),
                        variable.name = "hours", value.name = "od") 
DR5rawse.long$hours <- as.numeric(as.character(DR5rawse.long$hours))
DR5rawse.long$Concentration <- factor(DR5rawse.long$Concentration)


#Binding Se to the rest.
DR5 <- rbind(DR5.long, DR5rawse.long)
write.csv(DR5, "DR5.long.csv")

fit <- all_growthmodels(od ~ grow_gompertz(hours, parms) | Metal + factor + Replicate,
                             data = DR5, 
                             p = p, lower = lower, upper = upper,
                             log = "y")


res <- results(fit)

setwd("~/Google Drive File Stream/My Drive/Coal ash/Metal Tolerance/R/growth")

pdf("DR5res.pdf")
xyplot(od~hours|as.factor(factor) + Metal, 
       data = DR5, groups = Replicate, pch = 16, cex = .5, 
       main = "DR 5")
xyplot(mumax ~ factor|Metal, 
       data = res, layout = c(7,1), 
       main = "DR 5 mumax", type = c("p","a" ))
xyplot(K ~ factor|Metal, 
       data = res, layout = c(7,1), 
       main = "DR 5 Carrying Capacity", type = c("p","a" ))
xyplot(r2 ~ factor|Metal, 
       data = res, layout = c(7,1), 
       main = "DR 5 r2", type = c("p","a" ))
plot(fit)
dev.off()

write.csv(res, "DR5res.csv")

#___________________________________________________________________________________

#DR 52 

setwd("~/Google Drive File Stream/My Drive/Coal ash/Metal Tolerance/R/raw/raw")

raw <- read.csv("DR52raw.csv", check.names = F)
long <- melt(raw, id=c("Metal", "Isolate ID", "Replicate", "Concentration", "factor"),
                 variable.name = "hours", value.name = "od") 
long$hours <- as.numeric(as.character(long$hours))
long$Concentration <- factor(long$Concentration)

rawse <- read.csv("DR52rawse.csv", check.names = F)
rawse.long <- melt(rawse, id=c("Metal", "Isolate ID", "Replicate", "Concentration", "factor"),
                      variable.name = "hours", value.name = "od") 
rawse.long$hours <- as.numeric(as.character(rawse.long$hours))
rawse.long$Concentration <- factor(rawse.long$Concentration)


#Binding Se to the rest.

setwd("~/Google Drive File Stream/My Drive/Coal ash/Metal Tolerance/R/growth")

bind <- rbind(long, rawse.long)
write.csv(bind, "DR52.long.csv")

fit <- all_growthmodels(od ~ grow_gompertz(hours, parms) | Metal + factor + Replicate,
                        data = bind, 
                        p = p, lower = lower, upper = upper,
                        log = "y")


res <- results(fit)

pdf("DR52res.pdf")
xyplot(od~hours|as.factor(factor) + Metal, 
       data = bind, groups = Replicate, pch = 16, cex = .5, 
       main = "DR 52")
xyplot(mumax ~ factor|Metal, 
       data = res, layout = c(7,1), 
       main = "DR 52 mumax", type = c("p","a" ))
xyplot(K ~ factor|Metal, 
       data = res, layout = c(7,1), 
       main = "DR 52 Carrying Capacity", type = c("p","a" ))
xyplot(r2 ~ factor|Metal, 
       data = res, layout = c(7,1), 
       main = "DR 52 r2", type = c("p","a" ), ylim = c(.95,1))
plot(fit)
dev.off()

write.csv(res, "DR52res.csv")

#___________________________________________________________________________________
#DR 60 

setwd("~/Google Drive File Stream/My Drive/Coal ash/Metal Tolerance/R/raw/raw")

raw <- read.csv("DR60raw.csv", check.names = F)
long <- melt(raw, id=c("Metal", "Isolate ID", "Replicate", "Concentration", "factor"),
             variable.name = "hours", value.name = "od") 
long$hours <- as.numeric(as.character(long$hours))
long$Concentration <- factor(long$Concentration)

rawse <- read.csv("DR60rawse.csv", check.names = F)
rawse.long <- melt(rawse, id=c("Metal", "Isolate ID", "Replicate", "Concentration", "factor"),
                   variable.name = "hours", value.name = "od") 
rawse.long$hours <- as.numeric(as.character(rawse.long$hours))
rawse.long$Concentration <- factor(rawse.long$Concentration)


#Binding Se to the rest.

setwd("~/Google Drive File Stream/My Drive/Coal ash/Metal Tolerance/R/growth")

bind <- rbind(long, rawse.long)
write.csv(bind, "DR60.long.csv")

p <- c(y0 = 0.01, mumax = 0.2, K=0.5)
lower <- c(y0 = 1e-6, mumax = -.2, K=0)
upper <- c(y0 = 0.05, mumax = .3, K=1)

fit <- all_growthmodels(od ~ grow_gompertz(hours, parms) | Metal + factor + Replicate,
                        data = bind, 
                        p = p, lower = lower, upper = upper,
                        log = "y")


res <- results(fit)

pdf("DR60res.pdf")
xyplot(od~hours|as.factor(factor) + Metal, 
       data = bind, groups = Replicate, pch = 16, cex = .5, 
       main = "DR 60")
xyplot(mumax ~ factor|Metal, 
       data = res, layout = c(7,1), 
       main = "DR 60 mumax", type = c("p","a" ))
xyplot(K ~ factor|Metal, 
       data = res, layout = c(7,1), 
       main = "DR 60 Carrying Capacity", type = c("p","a" ))
xyplot(r2 ~ factor|Metal, 
       data = res, layout = c(7,1), 
       main = "DR 60 r2", type = c("p","a" ), ylim = c(.95,1))
plot(fit)
dev.off()

write.csv(res, "DR60res.csv")
#__________________________________________________________________________________
#DR 76 

setwd("~/Google Drive File Stream/My Drive/Coal ash/Metal Tolerance/R/raw/raw")

raw <- read.csv("DR76raw.csv", check.names = F)
long <- melt(raw, id=c("Metal", "Isolate ID", "Replicate", "Concentration", "factor"),
             variable.name = "hours", value.name = "od") 
long$hours <- as.numeric(as.character(long$hours))
long$Concentration <- factor(long$Concentration)

rawse <- read.csv("DR76rawse.csv", check.names = F)
rawse.long <- melt(rawse, id=c("Metal", "Isolate ID", "Replicate", "Concentration", "factor"),
                   variable.name = "hours", value.name = "od") 
rawse.long$hours <- as.numeric(as.character(rawse.long$hours))
rawse.long$Concentration <- factor(rawse.long$Concentration)


#Binding Se to the rest.

setwd("~/Google Drive File Stream/My Drive/Coal ash/Metal Tolerance/R/growth")

bind <- rbind(long, rawse.long)
write.csv(bind, "DR76.long.csv")

p <- c(y0 = 0.01, mumax = 0.2, K=0.5)
lower <- c(y0 = 1e-6, mumax = -.2, K=0)
upper <- c(y0 = 0.05, mumax = .3, K=1)

fit <- all_growthmodels(od ~ grow_gompertz(hours, parms) | Metal + factor + Replicate,
                        data = bind, 
                        p = p, lower = lower, upper = upper,
                        log = "y")


res <- results(fit)

pdf("DR76res.pdf")
xyplot(od~hours|as.factor(factor) + Metal, 
       data = bind, groups = Replicate, pch = 16, cex = .5, 
       main = "DR 76")
xyplot(mumax ~ factor|Metal, 
       data = res, layout = c(7,1), 
       main = "DR 76 mumax", type = c("p","a" ))
xyplot(K ~ factor|Metal, 
       data = res, layout = c(7,1), 
       main = "DR 76 Carrying Capacity", type = c("p","a" ))
xyplot(r2 ~ factor|Metal, 
       data = res, layout = c(7,1), 
       main = "DR 76 r2", type = c("p","a" ), ylim = c(.95,1))
plot(fit)
dev.off()

write.csv(res, "DR76res.csv")
#__________________________________________________________________________________
#DR 13 

setwd("~/Google Drive File Stream/My Drive/Coal ash/Metal Tolerance/R/raw/raw")

raw <- read.csv("DR13raw.csv", check.names = F)
long <- melt(raw, id=c("Metal", "Isolate ID", "Replicate", "Concentration", "factor"),
             variable.name = "hours", value.name = "od") 
long$hours <- as.numeric(as.character(long$hours))
long$Concentration <- factor(long$Concentration)

rawse <- read.csv("DR13rawse.csv", check.names = F)
rawse.long <- melt(rawse, id=c("Metal", "Isolate ID", "Replicate", "Concentration", "factor"),
                   variable.name = "hours", value.name = "od") 
rawse.long$hours <- as.numeric(as.character(rawse.long$hours))
rawse.long$Concentration <- factor(rawse.long$Concentration)


#Binding Se to the rest.

setwd("~/Google Drive File Stream/My Drive/Coal ash/Metal Tolerance/R/growth")

bind <- rbind(long, rawse.long)
write.csv(bind, "DR13.long.csv")

p <- c(y0 = 0.01, mumax = 0.2, K=0.5)
lower <- c(y0 = 1e-6, mumax = -.2, K=0)
upper <- c(y0 = 0.05, mumax = .3, K=1)

fit <- all_growthmodels(od ~ grow_gompertz(hours, parms) | Metal + factor + Replicate,
                        data = bind, 
                        p = p, lower = lower, upper = upper,
                        log = "y")


res <- results(fit)

pdf("DR13res.pdf")
xyplot(od~hours|as.factor(factor) + Metal, 
       data = bind, groups = Replicate, pch = 16, cex = .5, 
       main = "DR 13")
xyplot(mumax ~ factor|Metal, 
       data = res, layout = c(7,1), 
       main = "DR 13 mumax", type = c("p","a" ))
xyplot(K ~ factor|Metal, 
       data = res, layout = c(7,1), 
       main = "DR 13 Carrying Capacity", type = c("p","a" ))
xyplot(r2 ~ factor|Metal, 
       data = res, layout = c(7,1), 
       main = "DR 13 r2", type = c("p","a" ), ylim = c(.95,1))
plot(fit)
dev.off()

write.csv(res, "DR13res.csv")
#__________________________________________________________________________________
#DR +51

setwd("~/Google Drive File Stream/My Drive/Coal ash/Metal Tolerance/R/raw/raw")

raw <- read.csv("DRp51raw.csv", check.names = F)
long <- melt(raw, id=c("Metal", "Isolate ID", "Replicate", "Concentration", "factor"),
             variable.name = "hours", value.name = "od") 
long$hours <- as.numeric(as.character(long$hours))
long$Concentration <- factor(long$Concentration)

rawse <- read.csv("DRp51rawse.csv", check.names = F)
rawse.long <- melt(rawse, id=c("Metal", "Isolate ID", "Replicate", "Concentration", "factor"),
                   variable.name = "hours", value.name = "od") 
rawse.long$hours <- as.numeric(as.character(rawse.long$hours))
rawse.long$Concentration <- factor(rawse.long$Concentration)


#Binding Se to the rest.

setwd("~/Google Drive File Stream/My Drive/Coal ash/Metal Tolerance/R/growth")

bind <- rbind(long, rawse.long)
write.csv(bind, "DRp51.long.csv")

p <- c(y0 = 0.01, mumax = 0.2, K=0.5)
lower <- c(y0 = 1e-6, mumax = -.2, K=0)
upper <- c(y0 = 0.05, mumax = .3, K=1)

fit <- all_growthmodels(od ~ grow_gompertz(hours, parms) | Metal + factor + Replicate,
                        data = bind, 
                        p = p, lower = lower, upper = upper,
                        log = "y")


res <- results(fit)

pdf("DRp51res.pdf")
xyplot(od~hours|as.factor(factor) + Metal, 
       data = bind, groups = Replicate, pch = 16, cex = .5, 
       main = "DR+ 51")
xyplot(mumax ~ factor|Metal, 
       data = res, layout = c(7,1), 
       main = "DR+ 51 mumax", type = c("p","a" ))
xyplot(K ~ factor|Metal, 
       data = res, layout = c(7,1), 
       main = "DR+ 51 Carrying Capacity", type = c("p","a" ))
xyplot(r2 ~ factor|Metal, 
       data = res, layout = c(7,1), 
       main = "DR+ 51 r2", type = c("p","a" ), ylim = c(.95,1))
plot(fit)
dev.off()

write.csv(res, "DRp51res.csv")
#__________________________________________________________________________________
#DR +72

setwd("~/Google Drive File Stream/My Drive/Coal ash/Metal Tolerance/R/raw/raw")

raw <- read.csv("DR72raw.csv", check.names = F)
long <- melt(raw, id=c("Metal", "Isolate ID", "Replicate", "Concentration", "factor"),
             variable.name = "hours", value.name = "od") 
long$hours <- as.numeric(as.character(long$hours))
long$Concentration <- factor(long$Concentration)

rawse <- read.csv("DRp72rawse.csv", check.names = F)
rawse.long <- melt(rawse, id=c("Metal", "Isolate ID", "Replicate", "Concentration", "factor"),
                   variable.name = "hours", value.name = "od") 
rawse.long$hours <- as.numeric(as.character(rawse.long$hours))
rawse.long$Concentration <- factor(rawse.long$Concentration)


#Binding Se to the rest.

setwd("~/Google Drive File Stream/My Drive/Coal ash/Metal Tolerance/R/growth")

bind <- rbind(long, rawse.long)
write.csv(bind, "DRp72.long.csv")

p <- c(y0 = 0.01, mumax = 0.2, K=0.5)
lower <- c(y0 = 1e-6, mumax = -.2, K=0)
upper <- c(y0 = 0.05, mumax = .3, K=1)

fit <- all_growthmodels(od ~ grow_gompertz(hours, parms) | Metal + factor + Replicate,
                        data = bind, 
                        p = p, lower = lower, upper = upper,
                        log = "y")


res <- results(fit)

pdf("DRp72res.pdf")
xyplot(od~hours|as.factor(factor) + Metal, 
       data = bind, groups = Replicate, pch = 16, cex = .5, 
       main = "DR+ 72")
xyplot(mumax ~ factor|Metal, 
       data = res, layout = c(7,1), 
       main = "DR+ 72 mumax", type = c("p","a" ))
xyplot(K ~ factor|Metal, 
       data = res, layout = c(7,1), 
       main = "DR+ 72 Carrying Capacity", type = c("p","a" ))
xyplot(r2 ~ factor|Metal, 
       data = res, layout = c(7,1), 
       main = "DR+ 72 r2", type = c("p","a" ), ylim = c(.95,1))
plot(fit)
dev.off()

write.csv(res, "DRp72res.csv")
#__________________________________________________________________________________

#NO 14

setwd("~/Google Drive File Stream/My Drive/Coal ash/Metal Tolerance/R/raw/raw")

raw <- read.csv("NO14raw.csv", check.names = F)
long <- melt(raw, id=c("Metal", "Isolate ID", "Replicate", "Concentration", "factor"),
             variable.name = "hours", value.name = "od") 
long$hours <- as.numeric(as.character(long$hours))
long$Concentration <- factor(long$Concentration)

rawse <- read.csv("NO14rawse.csv", check.names = F)
rawse.long <- melt(rawse, id=c("Metal", "Isolate ID", "Replicate", "Concentration", "factor"),
                   variable.name = "hours", value.name = "od") 
rawse.long$hours <- as.numeric(as.character(rawse.long$hours))
rawse.long$Concentration <- factor(rawse.long$Concentration)


#Binding Se to the rest.

setwd("~/Google Drive File Stream/My Drive/Coal ash/Metal Tolerance/R/growth")

bind <- rbind(long, rawse.long)
write.csv(bind, "NO14.long.csv")

p <- c(y0 = 0.01, mumax = 0.2, K=0.5)
lower <- c(y0 = 1e-6, mumax = -.2, K=0)
upper <- c(y0 = 0.05, mumax = .3, K=1)

fit <- all_growthmodels(od ~ grow_gompertz(hours, parms) | Metal + factor + Replicate,
                        data = bind, 
                        p = p, lower = lower, upper = upper,
                        log = "y")


res <- results(fit)

pdf("NO14res.pdf")
xyplot(od~hours|as.factor(factor) + Metal, 
       data = bind, groups = Replicate, pch = 16, cex = .5, 
       main = "NO 14")
xyplot(mumax ~ factor|Metal, 
       data = res, layout = c(7,1), 
       main = "NO 14 mumax", type = c("p","a" ))
xyplot(K ~ factor|Metal, 
       data = res, layout = c(7,1), 
       main = "NO 14 Carrying Capacity", type = c("p","a" ))
xyplot(r2 ~ factor|Metal, 
       data = res, layout = c(7,1), 
       main = "NO 14 r2", type = c("p","a" ), ylim = c(.95,1))
plot(fit)
dev.off()

write.csv(res, "NO14res.csv")
#__________________________________________________________________________________
#NO 17

setwd("~/Google Drive File Stream/My Drive/Coal ash/Metal Tolerance/R/raw/raw")

raw <- read.csv("NO17raw.csv", check.names = F)
long <- melt(raw, id=c("Metal", "Isolate ID", "Replicate", "Concentration", "factor"),
             variable.name = "hours", value.name = "od") 
long$hours <- as.numeric(as.character(long$hours))
long$Concentration <- factor(long$Concentration)

rawse <- read.csv("NO17rawse.csv", check.names = F)
rawse.long <- melt(rawse, id=c("Metal", "Isolate ID", "Replicate", "Concentration", "factor"),
                   variable.name = "hours", value.name = "od") 
rawse.long$hours <- as.numeric(as.character(rawse.long$hours))
rawse.long$Concentration <- factor(rawse.long$Concentration)


#Binding Se to the rest.

setwd("~/Google Drive File Stream/My Drive/Coal ash/Metal Tolerance/R/growth")

bind <- rbind(long, rawse.long)
write.csv(bind, "NO17.long.csv")

p <- c(y0 = 0.01, mumax = 0.2, K=0.5)
lower <- c(y0 = 1e-6, mumax = -.2, K=0)
upper <- c(y0 = 0.05, mumax = .3, K=1)

fit <- all_growthmodels(od ~ grow_gompertz(hours, parms) | Metal + factor + Replicate,
                        data = bind, 
                        p = p, lower = lower, upper = upper,
                        log = "y")


res <- results(fit)

pdf("NO17res.pdf")
xyplot(od~hours|as.factor(factor) + Metal, 
       data = bind, groups = Replicate, pch = 16, cex = .5, 
       main = "NO 17")
xyplot(mumax ~ factor|Metal, 
       data = res, layout = c(7,1), 
       main = "NO 17 mumax", type = c("p","a" ))
xyplot(K ~ factor|Metal, 
       data = res, layout = c(7,1), 
       main = "NO 17 Carrying Capacity", type = c("p","a" ))
xyplot(r2 ~ factor|Metal, 
       data = res, layout = c(7,1), 
       main = "NO 17 r2", type = c("p","a" ), ylim = c(.95,1))
plot(fit)
dev.off()

write.csv(res, "NO17res.csv")
#__________________________________________________________________________________
#NO 22

setwd("~/Google Drive File Stream/My Drive/Coal ash/Metal Tolerance/R/raw/raw")

raw <- read.csv("NO22raw.csv", check.names = F)
long <- melt(raw, id=c("Metal", "Isolate ID", "Replicate", "Concentration", "factor"),
             variable.name = "hours", value.name = "od") 
long$hours <- as.numeric(as.character(long$hours))
long$Concentration <- factor(long$Concentration)

rawse <- read.csv("NO22rawse.csv", check.names = F)
rawse.long <- melt(rawse, id=c("Metal", "Isolate ID", "Replicate", "Concentration", "factor"),
                   variable.name = "hours", value.name = "od") 
rawse.long$hours <- as.numeric(as.character(rawse.long$hours))
rawse.long$Concentration <- factor(rawse.long$Concentration)


#Binding Se to the rest.

setwd("~/Google Drive File Stream/My Drive/Coal ash/Metal Tolerance/R/growth")

bind <- rbind(long, rawse.long)
write.csv(bind, "NO22.long.csv")

p <- c(y0 = 0.01, mumax = 0.2, K=0.5)
lower <- c(y0 = 1e-6, mumax = -.2, K=0)
upper <- c(y0 = 0.05, mumax = .3, K=1)

fit <- all_growthmodels(od ~ grow_gompertz(hours, parms) | Metal + factor + Replicate,
                        data = bind, 
                        p = p, lower = lower, upper = upper,
                        log = "y")


res <- results(fit)

pdf("NO22res.pdf")
xyplot(od~hours|as.factor(factor) + Metal, 
       data = bind, groups = Replicate, pch = 16, cex = .5, 
       main = "NO 22")
xyplot(mumax ~ factor|Metal, 
       data = res, layout = c(7,1), 
       main = "NO 22 mumax", type = c("p","a" ))
xyplot(K ~ factor|Metal, 
       data = res, layout = c(7,1), 
       main = "NO 22 Carrying Capacity", type = c("p","a" ))
xyplot(r2 ~ factor|Metal, 
       data = res, layout = c(7,1), 
       main = "NO 22 r2", type = c("p","a" ), ylim = c(.95,1))
plot(fit)
dev.off()

write.csv(res, "NO22res.csv")
#__________________________________________________________________________________
#SI 2

setwd("~/Google Drive File Stream/My Drive/Coal ash/Metal Tolerance/R/raw/raw")

raw <- read.csv("SI2raw.csv", check.names = F)
long <- melt(raw, id=c("Metal", "Isolate ID", "Replicate", "Concentration", "factor"),
             variable.name = "hours", value.name = "od") 
long$hours <- as.numeric(as.character(long$hours))
long$Concentration <- factor(long$Concentration)

rawse <- read.csv("SI2rawse.csv", check.names = F)
rawse.long <- melt(rawse, id=c("Metal", "Isolate ID", "Replicate", "Concentration", "factor"),
                   variable.name = "hours", value.name = "od") 
rawse.long$hours <- as.numeric(as.character(rawse.long$hours))
rawse.long$Concentration <- factor(rawse.long$Concentration)


#Binding Se to the rest.

setwd("~/Google Drive File Stream/My Drive/Coal ash/Metal Tolerance/R/growth")

bind <- rbind(long, rawse.long)
write.csv(bind, "SI2.long.csv")

p <- c(y0 = 0.01, mumax = 0.2, K=0.5)
lower <- c(y0 = 1e-6, mumax = -.2, K=0)
upper <- c(y0 = 0.05, mumax = .3, K=1)

fit <- all_growthmodels(od ~ grow_gompertz(hours, parms) | Metal + factor + Replicate,
                        data = bind, 
                        p = p, lower = lower, upper = upper,
                        log = "y")


res <- results(fit)

pdf("SI2res.pdf")
xyplot(od~hours|as.factor(factor) + Metal, 
       data = bind, groups = Replicate, pch = 16, cex = .5, 
       main = "SI 2")
xyplot(mumax ~ factor|Metal, 
       data = res, layout = c(7,1), 
       main = "SI 2 mumax", type = c("p","a" ))
xyplot(K ~ factor|Metal, 
       data = res, layout = c(7,1), 
       main = "SI 2 Carrying Capacity", type = c("p","a" ))
xyplot(r2 ~ factor|Metal, 
       data = res, layout = c(7,1), 
       main = "SI 2 r2", type = c("p","a" ), ylim = c(.95,1))
plot(fit)
dev.off()

write.csv(res, "SI2res.csv")
#__________________________________________________________________________________
#SI 4

setwd("~/Google Drive File Stream/My Drive/Coal ash/Metal Tolerance/R/raw/raw")

raw <- read.csv("SI4raw.csv", check.names = F)
long <- melt(raw, id=c("Metal", "Isolate ID", "Replicate", "Concentration", "factor"),
             variable.name = "hours", value.name = "od") 
long$hours <- as.numeric(as.character(long$hours))
long$Concentration <- factor(long$Concentration)



setwd("~/Google Drive File Stream/My Drive/Coal ash/Metal Tolerance/R/growth")
write.csv(long, "SI4.long.csv")

p <- c(y0 = 0.01, mumax = 0.2, K=0.5)
lower <- c(y0 = 1e-6, mumax = -.2, K=0)
upper <- c(y0 = 0.05, mumax = .3, K=1)

fit <- all_growthmodels(od ~ grow_gompertz(hours, parms) | Metal + factor + Replicate,
                        data = long, 
                        p = p, lower = lower, upper = upper,
                        log = "y")


res <- results(fit)

pdf("SI4res.pdf")
xyplot(od~hours|as.factor(factor) + Metal, 
       data = bind, groups = Replicate, pch = 16, cex = .5, 
       main = "SI 4")
xyplot(mumax ~ factor|Metal, 
       data = res, layout = c(6,1), 
       main = "SI 4 mumax", type = c("p","a" ))
xyplot(K ~ factor|Metal, 
       data = res, layout = c(6,1), 
       main = "SI 4 Carrying Capacity", type = c("p","a" ))
xyplot(r2 ~ factor|Metal, 
       data = res, layout = c(6,1), 
       main = "SI 4 r2", type = c("p","a" ), ylim = c(0.8,1))
plot(fit)
dev.off()

write.csv(res, "SI4res.csv")
#__________________________________________________________________________________
