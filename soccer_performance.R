# This script performs the econometrics for the soccer performance paper

# Loading libraries

library(modelsummary)
library(kableExtra)
library(stargazer)
library(sandwich)
library(ggplot2)
library(lmtest)
library(dplyr)
library(AER)

# Project diretory info

direc <-'F:/soccer_performance/'

# Reading in the data

soccer1 <- read.csv(paste(direc, 'data/data_1_.csv', sep = ''))
soccer2 <- read.csv(paste(direc, 'data/data_2_.csv', sep = ''))
soccer3 <- read.csv(paste(direc, 'data/data_3_.csv', sep = ''))
soccer4 <- read.csv(paste(direc, 'data/data_4_.csv', sep = ''))
soccer5 <- read.csv(paste(direc, 'data/data_5_.csv', sep = ''))
soccer6 <- read.csv(paste(direc, 'data/data_6_.csv', sep = ''))
soccer7 <- read.csv(paste(direc, 'data/data_7_.csv', sep = ''))
soccer8 <- read.csv(paste(direc, 'data/data_8_.csv', sep = ''))

soccer1$Years <- rep(1, dim(soccer1)[1])
soccer2$Years <- rep(2, dim(soccer2)[1])
soccer3$Years <- rep(3, dim(soccer3)[1])
soccer4$Years <- rep(4, dim(soccer4)[1])
soccer5$Years <- rep(5, dim(soccer5)[1])
soccer6$Years <- rep(6, dim(soccer6)[1])
soccer7$Years <- rep(7, dim(soccer7)[1])
soccer8$Years <- rep(8, dim(soccer8)[1])

all.data <- rbind(soccer1, soccer2, soccer3, soccer4, soccer5, soccer6, soccer7, soccer8)

b.df <- data.frame()
se.df <- data.frame()
p.df <- data.frame()

# A function for the figures in the main loop

make.y.limits <- function(a.list, b.list) {
  
  a <- min(0, 1.1*min(a.list, b.list))
  b <- max(0, 1.1*max(a.list, b.list))
  y.limits <- c(a,b)
  
  return(y.limits)

}

# Main loop

for (time.frame in 1:8) {
  
  d <- all.data[which(all.data$Years == time.frame),]
  
  # Run RDDs
  
  cep.w1 <- lm(European.Play...Final ~ Treated + Place...Initial, data = d[which(d$Window == 1),])
  cpl.w1 <- lm(Place...Final ~ Treated + Place...Initial + factor(League) + factor(Season), data = d[which(d$Window == 1),])
  cpo.w1 <- lm(Points...Final ~ Treated + Place...Initial + Points...Initial + factor(League) + factor(Season), data = d[which(d$Window == 1),])
  cw.w1 <- lm(Wins...Final ~ Treated + Place...Initial + Wins...Initial + factor(League) + factor(Season), data = d[which(d$Window == 1),])
  cd.w1 <- lm(Draws...Final ~ Treated + Place...Initial + Draws...Initial + factor(League) + factor(Season), data = d[which(d$Window == 1),])
  cl.w1 <- lm(Losses...Final ~ Treated + Place...Initial + Losses...Initial + factor(League) + factor(Season), data = d[which(d$Window == 1),])
  cgs.w1 <- lm(Goals.Scored...Final ~ Treated + Place...Initial + Goals.Scored...Initial + factor(League) + factor(Season), data = d[which(d$Window == 1),])
  cga.w1 <- lm(Goals.Allowed...Final ~ Treated + Place...Initial + Goals.Allowed...Initial + factor(League) + factor(Season), data = d[which(d$Window == 1),])
  cgd.w1 <- lm(Goal.Differential...Final ~ Treated + Place...Initial + Goal.Differential...Initial + factor(League) + factor(Season), data = d[which(d$Window == 1),])
  ccl.w1 <- lm(Champions.League...Final ~ Treated + Place...Initial, data = d[which(d$Window == 1),])
  cel.w1 <- lm(Europa.League...Final ~ Treated + Place...Initial, data = d[which(d$Window == 1),])
  ctf.w1 <- lm(Top.Flight ~ Treated + Place...Initial, data = d[which(d$Window == 1),])
  
  cep.w2 <- lm(European.Play...Final ~ Treated + Place...Initial, data = d[which(d$Window == 2),])
  cpl.w2 <- lm(Place...Final ~ Treated + Place...Initial + Place...Initial + factor(League) + factor(Season), data = d[which(d$Window == 2),])
  cpo.w2 <- lm(Points...Final ~ Treated + Place...Initial + Points...Initial + factor(League) + factor(Season), data = d[which(d$Window == 2),])
  cw.w2 <- lm(Wins...Final ~ Treated + Place...Initial + Wins...Initial + factor(League) + factor(Season), data = d[which(d$Window == 2),])
  cd.w2 <- lm(Draws...Final ~ Treated + Place...Initial + Draws...Initial + factor(League) + factor(Season), data = d[which(d$Window == 2),])
  cl.w2 <- lm(Losses...Final ~ Treated + Place...Initial + Losses...Initial + factor(League) + factor(Season), data = d[which(d$Window == 2),])
  cgs.w2 <- lm(Goals.Scored...Final ~ Treated + Place...Initial + Goals.Scored...Initial + factor(League) + factor(Season), data = d[which(d$Window == 2),])
  cga.w2 <- lm(Goals.Allowed...Final ~ Treated + Place...Initial + Goals.Allowed...Initial + factor(League) + factor(Season), data = d[which(d$Window == 2),])
  cgd.w2 <- lm(Goal.Differential...Final ~ Treated + Place...Initial + Goal.Differential...Initial + factor(League) + factor(Season), data = d[which(d$Window == 2),])
  ccl.w2 <- lm(Champions.League...Final ~ Treated + Place...Initial, data = d[which(d$Window == 2),])
  cel.w2 <- lm(Europa.League...Final ~ Treated + Place...Initial, data = d[which(d$Window == 2),])
  ctf.w2 <- lm(Top.Flight ~ Treated + Place...Initial, data = d[which(d$Window == 2),])
  
  cep.w3 <- lm(European.Play...Final ~ Treated + Place...Initial, data = d[which(d$Window == 3),])
  cpl.w3 <- lm(Place...Final ~ Treated + Place...Initial + Place...Initial + factor(League) + factor(Season), data = d[which(d$Window == 3),])
  cpo.w3 <- lm(Points...Final ~ Treated + Place...Initial + Points...Initial + factor(League) + factor(Season), data = d[which(d$Window == 3),])
  cw.w3 <- lm(Wins...Final ~ Treated + Place...Initial + Wins...Initial + factor(League) + factor(Season), data = d[which(d$Window == 3),])
  cd.w3 <- lm(Draws...Final ~ Treated + Place...Initial + Draws...Initial + factor(League) + factor(Season), data = d[which(d$Window == 3),])
  cl.w3 <- lm(Losses...Final ~ Treated + Place...Initial + Losses...Initial + factor(League) + factor(Season), data = d[which(d$Window == 3),])
  cgs.w3 <- lm(Goals.Scored...Final ~ Treated + Place...Initial + Goals.Scored...Initial + factor(League) + factor(Season), data = d[which(d$Window == 3),])
  cga.w3 <- lm(Goals.Allowed...Final ~ Treated + Place...Initial + Goals.Allowed...Initial + factor(League) + factor(Season), data = d[which(d$Window == 3),])
  cgd.w3 <- lm(Goal.Differential...Final ~ Treated + Place...Initial + Goal.Differential...Initial + factor(League) + factor(Season), data = d[which(d$Window == 3),])
  ccl.w3 <- lm(Champions.League...Final ~ Treated + Place...Initial, data = d[which(d$Window == 3),])
  cel.w3 <- lm(Europa.League...Final ~ Treated + Place...Initial, data = d[which(d$Window == 3),])
  ctf.w3 <- lm(Top.Flight ~ Treated + Place...Initial, data = d[which(d$Window == 3),])
  
  cep.w4 <- lm(European.Play...Final ~ Treated + Place...Initial, data = d[which(d$Window == 4),])
  cpl.w4 <- lm(Place...Final ~ Treated + Place...Initial + Place...Initial + factor(League) + factor(Season), data = d[which(d$Window == 4),])
  cpo.w4 <- lm(Points...Final ~ Treated + Place...Initial + Points...Initial + factor(League) + factor(Season), data = d[which(d$Window == 4),])
  cw.w4 <- lm(Wins...Final ~ Treated + Place...Initial + Wins...Initial + factor(League) + factor(Season), data = d[which(d$Window == 4),])
  cd.w4 <- lm(Draws...Final ~ Treated + Place...Initial + Draws...Initial + factor(League) + factor(Season), data = d[which(d$Window == 4),])
  cl.w4 <- lm(Losses...Final ~ Treated + Place...Initial + Losses...Initial + factor(League) + factor(Season), data = d[which(d$Window == 4),])
  cgs.w4 <- lm(Goals.Scored...Final ~ Treated + Place...Initial + Goals.Scored...Initial + factor(League) + factor(Season), data = d[which(d$Window == 4),])
  cga.w4 <- lm(Goals.Allowed...Final ~ Treated + Place...Initial + Goals.Allowed...Initial + factor(League) + factor(Season), data = d[which(d$Window == 4),])
  cgd.w4 <- lm(Goal.Differential...Final ~ Treated + Place...Initial + Goal.Differential...Initial + factor(League) + factor(Season), data = d[which(d$Window == 4),])
  ccl.w4 <- lm(Champions.League...Final ~ Treated + Place...Initial, data = d[which(d$Window == 4),])
  cel.w4 <- lm(Europa.League...Final ~ Treated + Place...Initial, data = d[which(d$Window == 4),])
  ctf.w4 <- lm(Top.Flight ~ Treated + Place...Initial, data = d[which(d$Window == 4),])
  
  cep.w5 <- lm(European.Play...Final ~ Treated + Place...Initial, data = d[which(d$Window == 5),])
  cpl.w5 <- lm(Place...Final ~ Treated + Place...Initial + Place...Initial + factor(League) + factor(Season), data = d[which(d$Window == 5),])
  cpo.w5 <- lm(Points...Final ~ Treated + Place...Initial + Points...Initial + factor(League) + factor(Season), data = d[which(d$Window == 5),])
  cw.w5 <- lm(Wins...Final ~ Treated + Place...Initial + Wins...Initial + factor(League) + factor(Season), data = d[which(d$Window == 5),])
  cd.w5 <- lm(Draws...Final ~ Treated + Place...Initial + Draws...Initial + factor(League) + factor(Season), data = d[which(d$Window == 5),])
  cl.w5 <- lm(Losses...Final ~ Treated + Place...Initial + Losses...Initial + factor(League) + factor(Season), data = d[which(d$Window == 5),])
  cgs.w5 <- lm(Goals.Scored...Final ~ Treated + Place...Initial + Goals.Scored...Initial + factor(League) + factor(Season), data = d[which(d$Window == 5),])
  cga.w5 <- lm(Goals.Allowed...Final ~ Treated + Place...Initial + Goals.Allowed...Initial + factor(League) + factor(Season), data = d[which(d$Window == 5),])
  cgd.w5 <- lm(Goal.Differential...Final ~ Treated + Place...Initial + Goal.Differential...Initial + factor(League) + factor(Season), data = d[which(d$Window == 5),])
  ccl.w5 <- lm(Champions.League...Final ~ Treated + Place...Initial, data = d[which(d$Window == 5),])
  cel.w5 <- lm(Europa.League...Final ~ Treated + Place...Initial, data = d[which(d$Window == 5),])
  ctf.w5 <- lm(Top.Flight ~ Treated + Place...Initial, data = d[which(d$Window == 5),])
  
  cep.w6 <- lm(European.Play...Final ~ Treated + Place...Initial, data = d[which(d$Window == 6),])
  cpl.w6 <- lm(Place...Final ~ Treated + Place...Initial + Place...Initial + factor(League) + factor(Season), data = d[which(d$Window == 6),])
  cpo.w6 <- lm(Points...Final ~ Treated + Place...Initial + Points...Initial + factor(League) + factor(Season), data = d[which(d$Window == 6),])
  cw.w6 <- lm(Wins...Final ~ Treated + Place...Initial + Wins...Initial + factor(League) + factor(Season), data = d[which(d$Window == 6),])
  cd.w6 <- lm(Draws...Final ~ Treated + Place...Initial + Draws...Initial + factor(League) + factor(Season), data = d[which(d$Window == 6),])
  cl.w6 <- lm(Losses...Final ~ Treated + Place...Initial + Losses...Initial + factor(League) + factor(Season), data = d[which(d$Window == 6),])
  cgs.w6 <- lm(Goals.Scored...Final ~ Treated + Place...Initial + Goals.Scored...Initial + factor(League) + factor(Season), data = d[which(d$Window == 6),])
  cga.w6 <- lm(Goals.Allowed...Final ~ Treated + Place...Initial + Goals.Allowed...Initial + factor(League) + factor(Season), data = d[which(d$Window == 6),])
  cgd.w6 <- lm(Goal.Differential...Final ~ Treated + Place...Initial + Goal.Differential...Initial + factor(League) + factor(Season), data = d[which(d$Window == 6),])
  ccl.w6 <- lm(Champions.League...Final ~ Treated + Place...Initial, data = d[which(d$Window == 6),])
  cel.w6 <- lm(Europa.League...Final ~ Treated + Place...Initial, data = d[which(d$Window == 6),])
  ctf.w6 <- lm(Top.Flight ~ Treated + Place...Initial, data = d[which(d$Window == 6),])
  
  cep.w7 <- lm(European.Play...Final ~ Treated + Place...Initial, data = d[which(d$Window == 7),])
  cpl.w7 <- lm(Place...Final ~ Treated + Place...Initial + Place...Initial + factor(League) + factor(Season), data = d[which(d$Window == 7),])
  cpo.w7 <- lm(Points...Final ~ Treated + Place...Initial + Points...Initial + factor(League) + factor(Season), data = d[which(d$Window == 7),])
  cw.w7 <- lm(Wins...Final ~ Treated + Place...Initial + Wins...Initial + factor(League) + factor(Season), data = d[which(d$Window == 7),])
  cd.w7 <- lm(Draws...Final ~ Treated + Place...Initial + Draws...Initial + factor(League) + factor(Season), data = d[which(d$Window == 7),])
  cl.w7 <- lm(Losses...Final ~ Treated + Place...Initial + Losses...Initial + factor(League) + factor(Season), data = d[which(d$Window == 7),])
  cgs.w7 <- lm(Goals.Scored...Final ~ Treated + Place...Initial + Goals.Scored...Initial + factor(League) + factor(Season), data = d[which(d$Window == 7),])
  cga.w7 <- lm(Goals.Allowed...Final ~ Treated + Place...Initial + Goals.Allowed...Initial + factor(League) + factor(Season), data = d[which(d$Window == 7),])
  cgd.w7 <- lm(Goal.Differential...Final ~ Treated + Place...Initial + Goal.Differential...Initial + factor(League) + factor(Season), data = d[which(d$Window == 7),])
  ccl.w7 <- lm(Champions.League...Final ~ Treated + Place...Initial, data = d[which(d$Window == 7),])
  cel.w7 <- lm(Europa.League...Final ~ Treated + Place...Initial, data = d[which(d$Window == 7),])
  ctf.w7 <- lm(Top.Flight ~ Treated + Place...Initial, data = d[which(d$Window == 7),])
  
  cep.w8 <- lm(European.Play...Final ~ Treated + Place...Initial, data = d[which(d$Window == 8),])
  cpl.w8 <- lm(Place...Final ~ Treated + Place...Initial + Place...Initial + factor(League) + factor(Season), data = d[which(d$Window == 8),])
  cpo.w8 <- lm(Points...Final ~ Treated + Place...Initial + Points...Initial + factor(League) + factor(Season), data = d[which(d$Window == 8),])
  cw.w8 <- lm(Wins...Final ~ Treated + Place...Initial + Wins...Initial + factor(League) + factor(Season), data = d[which(d$Window == 8),])
  cd.w8 <- lm(Draws...Final ~ Treated + Place...Initial + Draws...Initial + factor(League) + factor(Season), data = d[which(d$Window == 8),])
  cl.w8 <- lm(Losses...Final ~ Treated + Place...Initial + Losses...Initial + factor(League) + factor(Season), data = d[which(d$Window == 8),])
  cgs.w8 <- lm(Goals.Scored...Final ~ Treated + Place...Initial + Goals.Scored...Initial + factor(League) + factor(Season), data = d[which(d$Window == 8),])
  cga.w8 <- lm(Goals.Allowed...Final ~ Treated + Place...Initial + Goals.Allowed...Initial + factor(League) + factor(Season), data = d[which(d$Window == 8),])
  cgd.w8 <- lm(Goal.Differential...Final ~ Treated + Place...Initial + Goal.Differential...Initial + factor(League) + factor(Season), data = d[which(d$Window == 8),])
  ccl.w8 <- lm(Champions.League...Final ~ Treated + Place...Initial, data = d[which(d$Window == 8),])
  cel.w8 <- lm(Europa.League...Final ~ Treated + Place...Initial, data = d[which(d$Window == 8),])
  ctf.w8 <- lm(Top.Flight ~ Treated + Place...Initial, data = d[which(d$Window == 8),])
  
  # Calculating robust standard errors
  
  cep.w1 <- coeftest(cep.w1, vcov. = vcovCL, cluster = ~Team)
  cpl.w1 <- coeftest(cpl.w1, vcov. = vcovCL, cluster = ~Team)
  cpo.w1 <- coeftest(cpo.w1, vcov. = vcovCL, cluster = ~Team)
  cw.w1 <- coeftest(cw.w1, vcov. = vcovCL, cluster = ~Team)
  cd.w1 <- coeftest(cd.w1, vcov. = vcovCL, cluster = ~Team)
  cl.w1 <- coeftest(cl.w1, vcov. = vcovCL, cluster = ~Team)
  cgs.w1 <- coeftest(cgs.w1, vcov. = vcovCL, cluster = ~Team)
  cga.w1 <- coeftest(cga.w1, vcov. = vcovCL, cluster = ~Team)
  cgd.w1 <- coeftest(cgd.w1, vcov. = vcovCL, cluster = ~Team)
  ccl.w1 <- coeftest(ccl.w1, vcov. = vcovCL, cluster = ~Team)
  cel.w1 <- coeftest(cel.w1, vcov. = vcovCL, cluster = ~Team)
  ctf.w1 <- coeftest(ctf.w1, vcov. = vcovCL, cluster = ~Team)
  
  cep.w2 <- coeftest(cep.w2, vcov. = vcovCL, cluster = ~Team)
  cpl.w2 <- coeftest(cpl.w2, vcov. = vcovCL, cluster = ~Team)
  cpo.w2 <- coeftest(cpo.w2, vcov. = vcovCL, cluster = ~Team)
  cw.w2 <- coeftest(cw.w2, vcov. = vcovCL, cluster = ~Team)
  cd.w2 <- coeftest(cd.w2, vcov. = vcovCL, cluster = ~Team)
  cl.w2 <- coeftest(cl.w2, vcov. = vcovCL, cluster = ~Team)
  cgs.w2 <- coeftest(cgs.w2, vcov. = vcovCL, cluster = ~Team)
  cga.w2 <- coeftest(cga.w2, vcov. = vcovCL, cluster = ~Team)
  cgd.w2 <- coeftest(cgd.w2, vcov. = vcovCL, cluster = ~Team)
  ccl.w2 <- coeftest(ccl.w2, vcov. = vcovCL, cluster = ~Team)
  cel.w2 <- coeftest(cel.w2, vcov. = vcovCL, cluster = ~Team)
  ctf.w2 <- coeftest(ctf.w2, vcov. = vcovCL, cluster = ~Team)
  
  cep.w3 <- coeftest(cep.w3, vcov. = vcovCL, cluster = ~Team)
  cpl.w3 <- coeftest(cpl.w3, vcov. = vcovCL, cluster = ~Team)
  cpo.w3 <- coeftest(cpo.w3, vcov. = vcovCL, cluster = ~Team)
  cw.w3 <- coeftest(cw.w3, vcov. = vcovCL, cluster = ~Team)
  cd.w3 <- coeftest(cd.w3, vcov. = vcovCL, cluster = ~Team)
  cl.w3 <- coeftest(cl.w3, vcov. = vcovCL, cluster = ~Team)
  cgs.w3 <- coeftest(cgs.w3, vcov. = vcovCL, cluster = ~Team)
  cga.w3 <- coeftest(cga.w3, vcov. = vcovCL, cluster = ~Team)
  cgd.w3 <- coeftest(cgd.w3, vcov. = vcovCL, cluster = ~Team)
  ccl.w3 <- coeftest(ccl.w3, vcov. = vcovCL, cluster = ~Team)
  cel.w3 <- coeftest(cel.w3, vcov. = vcovCL, cluster = ~Team)
  ctf.w3 <- coeftest(ctf.w3, vcov. = vcovCL, cluster = ~Team)
  
  cep.w4 <- coeftest(cep.w4, vcov. = vcovCL, cluster = ~Team)
  cpl.w4 <- coeftest(cpl.w4, vcov. = vcovCL, cluster = ~Team)
  cpo.w4 <- coeftest(cpo.w4, vcov. = vcovCL, cluster = ~Team)
  cw.w4 <- coeftest(cw.w4, vcov. = vcovCL, cluster = ~Team)
  cd.w4 <- coeftest(cd.w4, vcov. = vcovCL, cluster = ~Team)
  cl.w4 <- coeftest(cl.w4, vcov. = vcovCL, cluster = ~Team)
  cgs.w4 <- coeftest(cgs.w4, vcov. = vcovCL, cluster = ~Team)
  cga.w4 <- coeftest(cga.w4, vcov. = vcovCL, cluster = ~Team)
  cgd.w4 <- coeftest(cgd.w4, vcov. = vcovCL, cluster = ~Team)
  ccl.w4 <- coeftest(ccl.w4, vcov. = vcovCL, cluster = ~Team)
  cel.w4 <- coeftest(cel.w4, vcov. = vcovCL, cluster = ~Team)
  ctf.w4 <- coeftest(ctf.w4, vcov. = vcovCL, cluster = ~Team)
  
  cep.w5 <- coeftest(cep.w5, vcov. = vcovCL, cluster = ~Team)
  cpl.w5 <- coeftest(cpl.w5, vcov. = vcovCL, cluster = ~Team)
  cpo.w5 <- coeftest(cpo.w5, vcov. = vcovCL, cluster = ~Team)
  cw.w5 <- coeftest(cw.w5, vcov. = vcovCL, cluster = ~Team)
  cd.w5 <- coeftest(cd.w5, vcov. = vcovCL, cluster = ~Team)
  cl.w5 <- coeftest(cl.w5, vcov. = vcovCL, cluster = ~Team)
  cgs.w5 <- coeftest(cgs.w5, vcov. = vcovCL, cluster = ~Team)
  cga.w5 <- coeftest(cga.w5, vcov. = vcovCL, cluster = ~Team)
  cgd.w5 <- coeftest(cgd.w5, vcov. = vcovCL, cluster = ~Team)
  ccl.w5 <- coeftest(ccl.w5, vcov. = vcovCL, cluster = ~Team)
  cel.w5 <- coeftest(cel.w5, vcov. = vcovCL, cluster = ~Team)
  ctf.w5 <- coeftest(ctf.w5, vcov. = vcovCL, cluster = ~Team)
  
  cep.w6 <- coeftest(cep.w6, vcov. = vcovCL, cluster = ~Team)
  cpl.w6 <- coeftest(cpl.w6, vcov. = vcovCL, cluster = ~Team)
  cpo.w6 <- coeftest(cpo.w6, vcov. = vcovCL, cluster = ~Team)
  cw.w6 <- coeftest(cw.w6, vcov. = vcovCL, cluster = ~Team)
  cd.w6 <- coeftest(cd.w6, vcov. = vcovCL, cluster = ~Team)
  cl.w6 <- coeftest(cl.w6, vcov. = vcovCL, cluster = ~Team)
  cgs.w6 <- coeftest(cgs.w6, vcov. = vcovCL, cluster = ~Team)
  cga.w6 <- coeftest(cga.w6, vcov. = vcovCL, cluster = ~Team)
  cgd.w6 <- coeftest(cgd.w6, vcov. = vcovCL, cluster = ~Team)
  ccl.w6 <- coeftest(ccl.w6, vcov. = vcovCL, cluster = ~Team)
  cel.w6 <- coeftest(cel.w6, vcov. = vcovCL, cluster = ~Team)
  ctf.w6 <- coeftest(ctf.w6, vcov. = vcovCL, cluster = ~Team)
  
  cep.w7 <- coeftest(cep.w7, vcov. = vcovCL, cluster = ~Team)
  cpl.w7 <- coeftest(cpl.w7, vcov. = vcovCL, cluster = ~Team)
  cpo.w7 <- coeftest(cpo.w7, vcov. = vcovCL, cluster = ~Team)
  cw.w7 <- coeftest(cw.w7, vcov. = vcovCL, cluster = ~Team)
  cd.w7 <- coeftest(cd.w7, vcov. = vcovCL, cluster = ~Team)
  cl.w7 <- coeftest(cl.w7, vcov. = vcovCL, cluster = ~Team)
  cgs.w7 <- coeftest(cgs.w7, vcov. = vcovCL, cluster = ~Team)
  cga.w7 <- coeftest(cga.w7, vcov. = vcovCL, cluster = ~Team)
  cgd.w7 <- coeftest(cgd.w7, vcov. = vcovCL, cluster = ~Team)
  ccl.w7 <- coeftest(ccl.w7, vcov. = vcovCL, cluster = ~Team)
  cel.w7 <- coeftest(cel.w7, vcov. = vcovCL, cluster = ~Team)
  ctf.w7 <- coeftest(ctf.w7, vcov. = vcovCL, cluster = ~Team)
  
  cep.w8 <- coeftest(cep.w8, vcov. = vcovCL, cluster = ~Team)
  cpl.w8 <- coeftest(cpl.w8, vcov. = vcovCL, cluster = ~Team)
  cpo.w8 <- coeftest(cpo.w8, vcov. = vcovCL, cluster = ~Team)
  cw.w8 <- coeftest(cw.w8, vcov. = vcovCL, cluster = ~Team)
  cd.w8 <- coeftest(cd.w8, vcov. = vcovCL, cluster = ~Team)
  cl.w8 <- coeftest(cl.w8, vcov. = vcovCL, cluster = ~Team)
  cgs.w8 <- coeftest(cgs.w8, vcov. = vcovCL, cluster = ~Team)
  cga.w8 <- coeftest(cga.w8, vcov. = vcovCL, cluster = ~Team)
  cgd.w8 <- coeftest(cgd.w8, vcov. = vcovCL, cluster = ~Team)
  ccl.w8 <- coeftest(ccl.w8, vcov. = vcovCL, cluster = ~Team)
  cel.w8 <- coeftest(cel.w8, vcov. = vcovCL, cluster = ~Team)
  ctf.w8 <- coeftest(ctf.w8, vcov. = vcovCL, cluster = ~Team)

  # Save results
  
  write.csv(stargazer(cep.w1, cpl.w1, cpo.w1, cw.w1, cd.w1, cl.w1, cgs.w1, cga.w1, cgd.w1, ccl.w1, cel.w1, ctf.w1, type = 'text', omit = c('Season', 'League'), omit.stat = c('f', 'ser')), paste(direc, 'results/window=1__time=', time.frame, '.txt', sep = ''), row.names = FALSE)
  write.csv(stargazer(cep.w2, cpl.w2, cpo.w2, cw.w2, cd.w2, cl.w2, cgs.w2, cga.w2, cgd.w2, ccl.w2, cel.w2, ctf.w2, type = 'text', omit = c('Season', 'League'), omit.stat = c('f', 'ser')), paste(direc, 'results/window=2__time=', time.frame, '.txt', sep = ''), row.names = FALSE)
  write.csv(stargazer(cep.w3, cpl.w3, cpo.w3, cw.w3, cd.w3, cl.w3, cgs.w3, cga.w3, cgd.w3, ccl.w3, cel.w3, ctf.w3, type = 'text', omit = c('Season', 'League'), omit.stat = c('f', 'ser')), paste(direc, 'results/window=3__time=', time.frame, '.txt', sep = ''), row.names = FALSE)
  write.csv(stargazer(cep.w4, cpl.w4, cpo.w4, cw.w4, cd.w4, cl.w4, cgs.w4, cga.w4, cgd.w4, ccl.w4, cel.w4, ctf.w4, type = 'text', omit = c('Season', 'League'), omit.stat = c('f', 'ser')), paste(direc, 'results/window=4__time=', time.frame, '.txt', sep = ''), row.names = FALSE)
  write.csv(stargazer(cep.w5, cpl.w5, cpo.w5, cw.w5, cd.w5, cl.w5, cgs.w5, cga.w5, cgd.w5, ccl.w5, cel.w5, ctf.w5, type = 'text', omit = c('Season', 'League'), omit.stat = c('f', 'ser')), paste(direc, 'results/window=5__time=', time.frame, '.txt', sep = ''), row.names = FALSE)
  write.csv(stargazer(cep.w6, cpl.w6, cpo.w6, cw.w6, cd.w6, cl.w6, cgs.w6, cga.w6, cgd.w6, ccl.w6, cel.w6, ctf.w6, type = 'text', omit = c('Season', 'League'), omit.stat = c('f', 'ser')), paste(direc, 'results/window=6__time=', time.frame, '.txt', sep = ''), row.names = FALSE)
  write.csv(stargazer(cep.w7, cpl.w7, cpo.w7, cw.w7, cd.w7, cl.w7, cgs.w7, cga.w7, cgd.w7, ccl.w7, cel.w7, ctf.w7, type = 'text', omit = c('Season', 'League'), omit.stat = c('f', 'ser')), paste(direc, 'results/window=7__time=', time.frame, '.txt', sep = ''), row.names = FALSE)
  write.csv(stargazer(cep.w8, cpl.w8, cpo.w8, cw.w8, cd.w8, cl.w8, cgs.w8, cga.w8, cgd.w8, ccl.w8, cel.w8, ctf.w8, type = 'text', omit = c('Season', 'League'), omit.stat = c('f', 'ser')), paste(direc, 'results/window=8__time=', time.frame, '.txt', sep = ''), row.names = FALSE)
  
  # Longitudinal plots of change in outcomes over time
  
  cep <- c(cep.w1[2], cep.w2[2], cep.w3[2], cep.w4[2], cep.w5[2], cep.w6[2], cep.w7[2], cep.w8[2])
  cpl <- c(cpl.w1[2], cpl.w2[2], cpl.w3[2], cpl.w4[2], cpl.w5[2], cpl.w6[2], cpl.w7[2], cpl.w8[2])
  cpo <- c(cpo.w1[2], cpo.w2[2], cpo.w3[2], cpo.w4[2], cpo.w5[2], cpo.w6[2], cpo.w7[2], cpo.w8[2])
  cw <- c(cw.w1[2], cw.w2[2], cw.w3[2], cw.w4[2], cw.w5[2], cw.w6[2], cw.w7[2], cw.w8[2])
  cd <- c(cd.w1[2], cd.w2[2], cd.w3[2], cd.w4[2], cd.w5[2], cd.w6[2], cd.w7[2], cd.w8[2])
  cl <- c(cl.w1[2], cl.w2[2], cl.w3[2], cl.w4[2], cl.w5[2], cl.w6[2], cl.w7[2], cl.w8[2])
  cgs <- c(cgs.w1[2], cgs.w2[2], cgs.w3[2], cgs.w4[2], cgs.w5[2], cgs.w6[2], cgs.w7[2], cgs.w8[2])
  cga <- c(cga.w1[2], cga.w2[2], cga.w3[2], cga.w4[2], cga.w5[2], cga.w6[2], cga.w7[2], cga.w8[2])
  cgd <- c(cgd.w1[2], cgd.w2[2], cgd.w3[2], cgd.w4[2], cgd.w5[2], cgd.w6[2], cgd.w7[2], cgd.w8[2])
  ccl <- c(ccl.w1[2], ccl.w2[2], ccl.w3[2], ccl.w4[2], ccl.w5[2], ccl.w6[2], ccl.w7[2], ccl.w8[2])
  cel <- c(cel.w1[2], cel.w2[2], cel.w3[2], cel.w4[2], cel.w5[2], cel.w6[2], cel.w7[2], cel.w8[2])
  ctf <- c(ctf.w1[2], ctf.w2[2], ctf.w3[2], ctf.w4[2], ctf.w5[2], ctf.w6[2], ctf.w7[2], ctf.w8[2])
  
  cepse <- c(cep.w1[dim(cep.w1)[1]*1 + 2], cep.w2[dim(cep.w2)[1]*1 + 2], cep.w3[dim(cep.w3)[1]*1 + 2], cep.w4[dim(cep.w4)[1]*1 + 2], cep.w5[dim(cep.w5)[1]*1 + 2], cep.w6[dim(cep.w6)[1]*1 + 2], cep.w7[dim(cep.w7)[1]*1 + 2], cep.w8[dim(cep.w8)[1]*1 + 2])
  cplse <- c(cpl.w1[dim(cpl.w1)[1]*1 + 2], cpl.w2[dim(cpl.w2)[1]*1 + 2], cpl.w3[dim(cpl.w3)[1]*1 + 2], cpl.w4[dim(cpl.w4)[1]*1 + 2], cpl.w5[dim(cpl.w5)[1]*1 + 2], cpl.w6[dim(cpl.w6)[1]*1 + 2], cpl.w7[dim(cpl.w7)[1]*1 + 2], cpl.w8[dim(cpl.w8)[1]*1 + 2])
  cpose <- c(cpo.w1[dim(cpo.w1)[1]*1 + 2], cpo.w2[dim(cpo.w2)[1]*1 + 2], cpo.w3[dim(cpo.w3)[1]*1 + 2], cpo.w4[dim(cpo.w4)[1]*1 + 2], cpo.w5[dim(cpo.w5)[1]*1 + 2], cpo.w6[dim(cpo.w6)[1]*1 + 2], cpo.w7[dim(cpo.w7)[1]*1 + 2], cpo.w8[dim(cpo.w8)[1]*1 + 2])
  cwse <- c(cw.w1[dim(cw.w1)[1]*1 + 2], cw.w2[dim(cw.w2)[1]*1 + 2], cw.w3[dim(cw.w3)[1]*1 + 2], cw.w4[dim(cw.w4)[1]*1 + 2], cw.w5[dim(cw.w5)[1]*1 + 2], cw.w6[dim(cw.w6)[1]*1 + 2], cw.w7[dim(cw.w7)[1]*1 + 2], cw.w8[dim(cw.w8)[1]*1 + 2])
  cdse <- c(cd.w1[dim(cd.w1)[1]*1 + 2], cd.w2[dim(cd.w2)[1]*1 + 2], cd.w3[dim(cd.w3)[1]*1 + 2], cd.w4[dim(cd.w4)[1]*1 + 2], cd.w5[dim(cd.w5)[1]*1 + 2], cd.w6[dim(cd.w6)[1]*1 + 2], cd.w7[dim(cd.w7)[1]*1 + 2], cd.w8[dim(cd.w8)[1]*1 + 2])
  clse <- c(cl.w1[dim(cl.w1)[1]*1 + 2], cl.w2[dim(cl.w2)[1]*1 + 2], cl.w3[dim(cl.w3)[1]*1 + 2], cl.w4[dim(cl.w4)[1]*1 + 2], cl.w5[dim(cl.w5)[1]*1 + 2], cl.w6[dim(cl.w6)[1]*1 + 2], cl.w7[dim(cl.w7)[1]*1 + 2], cl.w8[dim(cl.w8)[1]*1 + 2])
  cgsse <- c(cgs.w1[dim(cgs.w1)[1]*1 + 2], cgs.w2[dim(cgs.w2)[1]*1 + 2], cgs.w3[dim(cgs.w3)[1]*1 + 2], cgs.w4[dim(cgs.w4)[1]*1 + 2], cgs.w5[dim(cgs.w5)[1]*1 + 2], cgs.w6[dim(cgs.w6)[1]*1 + 2], cgs.w7[dim(cgs.w7)[1]*1 + 2], cgs.w8[dim(cgs.w8)[1]*1 + 2])
  cgase <- c(cga.w1[dim(cga.w1)[1]*1 + 2], cga.w2[dim(cga.w2)[1]*1 + 2], cga.w3[dim(cga.w3)[1]*1 + 2], cga.w4[dim(cga.w4)[1]*1 + 2], cga.w5[dim(cga.w5)[1]*1 + 2], cga.w6[dim(cga.w6)[1]*1 + 2], cga.w7[dim(cga.w7)[1]*1 + 2], cga.w8[dim(cga.w8)[1]*1 + 2])
  cgdse <- c(cgd.w1[dim(cgd.w1)[1]*1 + 2], cgd.w2[dim(cgd.w2)[1]*1 + 2], cgd.w3[dim(cgd.w3)[1]*1 + 2], cgd.w4[dim(cgd.w4)[1]*1 + 2], cgd.w5[dim(cgd.w5)[1]*1 + 2], cgd.w6[dim(cgd.w6)[1]*1 + 2], cgd.w7[dim(cgd.w7)[1]*1 + 2], cgd.w8[dim(cgd.w8)[1]*1 + 2])
  cclse <- c(ccl.w1[dim(ccl.w1)[1]*1 + 2], ccl.w2[dim(ccl.w2)[1]*1 + 2], ccl.w3[dim(ccl.w3)[1]*1 + 2], ccl.w4[dim(ccl.w4)[1]*1 + 2], ccl.w5[dim(ccl.w5)[1]*1 + 2], ccl.w6[dim(ccl.w6)[1]*1 + 2], ccl.w7[dim(ccl.w7)[1]*1 + 2], ccl.w8[dim(ccl.w8)[1]*1 + 2])
  celse <- c(cel.w1[dim(cel.w1)[1]*1 + 2], cel.w2[dim(cel.w2)[1]*1 + 2], cel.w3[dim(cel.w3)[1]*1 + 2], cel.w4[dim(cel.w4)[1]*1 + 2], cel.w5[dim(cel.w5)[1]*1 + 2], cel.w6[dim(cel.w6)[1]*1 + 2], cel.w7[dim(cel.w7)[1]*1 + 2], cel.w8[dim(cel.w8)[1]*1 + 2])
  ctfse <- c(ctf.w1[dim(ctf.w1)[1]*1 + 2], ctf.w2[dim(ctf.w2)[1]*1 + 2], ctf.w3[dim(ctf.w3)[1]*1 + 2], ctf.w4[dim(ctf.w4)[1]*1 + 2], ctf.w5[dim(ctf.w5)[1]*1 + 2], ctf.w6[dim(ctf.w6)[1]*1 + 2], ctf.w7[dim(ctf.w7)[1]*1 + 2], ctf.w8[dim(ctf.w8)[1]*1 + 2])
  
  cepp <- c(cep.w1[dim(cep.w1)[1]*3 + 2], cep.w2[dim(cep.w2)[1]*3 + 2], cep.w3[dim(cep.w3)[1]*3 + 2], cep.w4[dim(cep.w4)[1]*3 + 2], cep.w5[dim(cep.w5)[1]*3 + 2], cep.w6[dim(cep.w6)[1]*3 + 2], cep.w7[dim(cep.w7)[1]*3 + 2], cep.w8[dim(cep.w8)[1]*3 + 2])
  cplp <- c(cpl.w1[dim(cpl.w1)[1]*3 + 2], cpl.w2[dim(cpl.w2)[1]*3 + 2], cpl.w3[dim(cpl.w3)[1]*3 + 2], cpl.w4[dim(cpl.w4)[1]*3 + 2], cpl.w5[dim(cpl.w5)[1]*3 + 2], cpl.w6[dim(cpl.w6)[1]*3 + 2], cpl.w7[dim(cpl.w7)[1]*3 + 2], cpl.w8[dim(cpl.w8)[1]*3 + 2])
  cpop <- c(cpo.w1[dim(cpo.w1)[1]*3 + 2], cpo.w2[dim(cpo.w2)[1]*3 + 2], cpo.w3[dim(cpo.w3)[1]*3 + 2], cpo.w4[dim(cpo.w4)[1]*3 + 2], cpo.w5[dim(cpo.w5)[1]*3 + 2], cpo.w6[dim(cpo.w6)[1]*3 + 2], cpo.w7[dim(cpo.w7)[1]*3 + 2], cpo.w8[dim(cpo.w8)[1]*3 + 2])
  cwp <- c(cw.w1[dim(cw.w1)[1]*3 + 2], cw.w2[dim(cw.w2)[1]*3 + 2], cw.w3[dim(cw.w3)[1]*3 + 2], cw.w4[dim(cw.w4)[1]*3 + 2], cw.w5[dim(cw.w5)[1]*3 + 2], cw.w6[dim(cw.w6)[1]*3 + 2], cw.w7[dim(cw.w7)[1]*3 + 2], cw.w8[dim(cw.w8)[1]*3 + 2])
  cdp <- c(cd.w1[dim(cd.w1)[1]*3 + 2], cd.w2[dim(cd.w2)[1]*3 + 2], cd.w3[dim(cd.w3)[1]*3 + 2], cd.w4[dim(cd.w4)[1]*3 + 2], cd.w5[dim(cd.w5)[1]*3 + 2], cd.w6[dim(cd.w6)[1]*3 + 2], cd.w7[dim(cd.w7)[1]*3 + 2], cd.w8[dim(cd.w8)[1]*3 + 2])
  clp <- c(cl.w1[dim(cl.w1)[1]*3 + 2], cl.w2[dim(cl.w2)[1]*3 + 2], cl.w3[dim(cl.w3)[1]*3 + 2], cl.w4[dim(cl.w4)[1]*3 + 2], cl.w5[dim(cl.w5)[1]*3 + 2], cl.w6[dim(cl.w6)[1]*3 + 2], cl.w7[dim(cl.w7)[1]*3 + 2], cl.w8[dim(cl.w8)[1]*3 + 2])
  cgsp <- c(cgs.w1[dim(cgs.w1)[1]*3 + 2], cgs.w2[dim(cgs.w2)[1]*3 + 2], cgs.w3[dim(cgs.w3)[1]*3 + 2], cgs.w4[dim(cgs.w4)[1]*3 + 2], cgs.w5[dim(cgs.w5)[1]*3 + 2], cgs.w6[dim(cgs.w6)[1]*3 + 2], cgs.w7[dim(cgs.w7)[1]*3 + 2], cgs.w8[dim(cgs.w8)[1]*3 + 2])
  cgap <- c(cga.w1[dim(cga.w1)[1]*3 + 2], cga.w2[dim(cga.w2)[1]*3 + 2], cga.w3[dim(cga.w3)[1]*3 + 2], cga.w4[dim(cga.w4)[1]*3 + 2], cga.w5[dim(cga.w5)[1]*3 + 2], cga.w6[dim(cga.w6)[1]*3 + 2], cga.w7[dim(cga.w7)[1]*3 + 2], cga.w8[dim(cga.w8)[1]*3 + 2])
  cgdp <- c(cgd.w1[dim(cgd.w1)[1]*3 + 2], cgd.w2[dim(cgd.w2)[1]*3 + 2], cgd.w3[dim(cgd.w3)[1]*3 + 2], cgd.w4[dim(cgd.w4)[1]*3 + 2], cgd.w5[dim(cgd.w5)[1]*3 + 2], cgd.w6[dim(cgd.w6)[1]*3 + 2], cgd.w7[dim(cgd.w7)[1]*3 + 2], cgd.w8[dim(cgd.w8)[1]*3 + 2])
  cclp <- c(ccl.w1[dim(ccl.w1)[1]*3 + 2], ccl.w2[dim(ccl.w2)[1]*3 + 2], ccl.w3[dim(ccl.w3)[1]*3 + 2], ccl.w4[dim(ccl.w4)[1]*3 + 2], ccl.w5[dim(ccl.w5)[1]*3 + 2], ccl.w6[dim(ccl.w6)[1]*3 + 2], ccl.w7[dim(ccl.w7)[1]*3 + 2], ccl.w8[dim(ccl.w8)[1]*3 + 2])
  celp <- c(cel.w1[dim(cel.w1)[1]*3 + 2], cel.w2[dim(cel.w2)[1]*3 + 2], cel.w3[dim(cel.w3)[1]*3 + 2], cel.w4[dim(cel.w4)[1]*3 + 2], cel.w5[dim(cel.w5)[1]*3 + 2], cel.w6[dim(cel.w6)[1]*3 + 2], cel.w7[dim(cel.w7)[1]*3 + 2], cel.w8[dim(cel.w8)[1]*3 + 2])
  ctfp <- c(ctf.w1[dim(ctf.w1)[1]*3 + 2], ctf.w2[dim(ctf.w2)[1]*3 + 2], ctf.w3[dim(ctf.w3)[1]*3 + 2], ctf.w4[dim(ctf.w4)[1]*3 + 2], ctf.w5[dim(ctf.w5)[1]*3 + 2], ctf.w6[dim(ctf.w6)[1]*3 + 2], ctf.w7[dim(ctf.w7)[1]*3 + 2], ctf.w8[dim(ctf.w8)[1]*3 + 2])
  
  cepse0 <- cep - cepse
  cplse0 <- cpl - cplse
  cpose0 <- cpo - cpose
  cwse0 <- cw - cwse
  cdse0 <- cd - cdse
  clse0 <- cl - clse
  cgase0 <- cga - cgase
  cgsse0 <- cgs - cgsse
  cgdse0 <- cgd - cgdse
  cclse0 <- ccl - cclse
  celse0 <- cel - celse
  ctfse0 <- ctf - ctfse
  
  cepse1 <- cep + cepse
  cplse1 <- cpl + cplse
  cpose1 <- cpo + cpose
  cwse1 <- cw + cwse
  cdse1 <- cd + cdse
  clse1 <- cl + clse
  cgase1 <- cga + cgase
  cgsse1 <- cgs + cgsse
  cgdse1 <- cgd + cgdse
  cclse1 <- ccl + cclse
  celse1 <- cel + celse
  ctfse1 <- ctf + ctfse
  
  cepx <- as.data.frame(cbind(cep, cepse0, cepse1, rep(0,length(cep))))
  cplx <- as.data.frame(cbind(cpl, cplse0, cplse1, rep(0,length(cpl))))
  cpox <- as.data.frame(cbind(cpo, cpose0, cpose1, rep(0,length(cpo))))
  cwx <- as.data.frame(cbind(cw, cwse0, cwse1, rep(0,length(cw))))
  cdx <- as.data.frame(cbind(cd, cdse0, cdse1, rep(0,length(cd))))
  clx <- as.data.frame(cbind(cl, clse0, clse1, rep(0,length(cl))))
  cgsx <- as.data.frame(cbind(cgs, cgsse0, cgsse1, rep(0,length(cgs))))
  cgax <- as.data.frame(cbind(cga, cgase0, cgase1, rep(0,length(cga))))
  cgdx <- as.data.frame(cbind(cgd, cgdse0, cgdse1, rep(0,length(cgd))))
  cclx <- as.data.frame(cbind(ccl, cclse0, cclse1, rep(0,length(ccl))))
  celx <- as.data.frame(cbind(cel, celse0, celse1, rep(0,length(cel))))
  ctfx <- as.data.frame(cbind(ctf, ctfse0, ctfse1, rep(0,length(ctf))))
  
  png(filename = paste(direc, 'figures/coefs__', time.frame, '.png', sep = ''))
  par(mfrow = c(4,3), mai = c(0.3, 0.7, 0.3, 0.3))
  matplot(ctfx, type = 'l', lty = c('solid', 'dotted', 'dotted', 'solid'), pch = 1, col = c(1,1,1,2), ylab = 'Top Flight', xlab = 'Years', ylim = make.y.limits(ctfse0,ctfse1))
  matplot(cepx, type = 'l', lty = c('solid', 'dotted', 'dotted', 'solid'), pch = 1, col = c(1,1,1,2), ylab = 'European Play', xlab = 'Years', ylim = make.y.limits(cepse0,cepse1))
  matplot(cplx, type = 'l', lty = c('solid', 'dotted', 'dotted', 'solid'), pch = 1, col = c(1,1,1,2), ylab = 'Place', xlab = 'Years', ylim = make.y.limits(cplse0,cplse1))
  matplot(cpox, type = 'l', lty = c('solid', 'dotted', 'dotted', 'solid'), pch = 1, col = c(1,1,1,2), ylab = 'Points', xlab = 'Years', ylim = make.y.limits(cpose0,cpose1))
  matplot(cwx, type = 'l', lty = c('solid', 'dotted', 'dotted', 'solid'), pch = 1, col = c(1,1,1,2), ylab = 'Wins', xlab = 'Years', ylim = make.y.limits(cwse0,cwse1))
  matplot(cdx, type = 'l', lty = c('solid', 'dotted', 'dotted', 'solid'), pch = 1, col = c(1,1,1,2), ylab = 'Draws', xlab = 'Years', ylim = make.y.limits(cdse0,cdse1))
  matplot(clx, type = 'l', lty = c('solid', 'dotted', 'dotted', 'solid'), pch = 1, col = c(1,1,1,2), ylab = 'Loses', xlab = 'Years', ylim = make.y.limits(clse0,clse1))
  matplot(cgsx, type = 'l', lty = c('solid', 'dotted', 'dotted', 'solid'), pch = 1, col = c(1,1,1,2), ylab = 'Goals Scored', xlab = 'Years', ylim = make.y.limits(cgsse0,cgsse1))
  matplot(cgax, type = 'l', lty = c('solid', 'dotted', 'dotted', 'solid'), pch = 1, col = c(1,1,1,2), ylab = 'Goals Allowed', xlab = 'Years', ylim = make.y.limits(cgase0,cgase1))
  matplot(cgdx, type = 'l', lty = c('solid', 'dotted', 'dotted', 'solid'), pch = 1, col = c(1,1,1,2), ylab = 'Goal Difference', xlab = 'Years', ylim = make.y.limits(cgdse0,cgdse1))
  matplot(cclx, type = 'l', lty = c('solid', 'dotted', 'dotted', 'solid'), pch = 1, col = c(1,1,1,2), ylab = 'Champions League', xlab = 'Years', ylim = make.y.limits(cclse0,cclse1))
  matplot(celx, type = 'l', lty = c('solid', 'dotted', 'dotted', 'solid'), pch = 1, col = c(1,1,1,2), ylab = 'Europa League', xlab = 'Years', ylim = make.y.limits(celse0,celse1))
  dev.off()
  
  # Storing data for tables
  
  tmp.b <- cbind(rep(time.frame, length(ctf)), ctf, cep, cpl, cpo, cw, cd, cl, cgs, cga, cgd, ccl, cel)
  tmp.se <- cbind(rep(time.frame, length(ctf)), ctfse, cepse, cplse, cpose, cwse, cdse, clse, cgsse, cgase, cgdse, cclse, celse)
  tmp.p <- cbind(rep(time.frame, length(ctf)), ctfp, cepp, cplp, cpop, cwp, cdp, clp, cgsp, cgap, cgdp, cclp, celp)
  
  b.df <- rbind(b.df, tmp.b)
  se.df <- rbind(se.df, tmp.se)
  p.df <- rbind(p.df, tmp.p)
  
}

# Summary statistics

keepers.outcomes <- c('Window', 'Cutoff', 'Treated', 'Place', 'Points', 'Wins', 'Draws', 'Losses', 'Goals_For', 'Goals_Allowed', 'Goal_Difference', 'Champions.League', 'Europa.League', 'Top.Flight')
new_names <- c('Window', 'Cutoff', 'Treated', 'Place', 'Points', 'Wins', 'Draws', 'Losses', 'Goals For', 'Goals Allowed', 'Goal Difference', 'Champions League', 'Europa League', 'Top Flight')
sumdat <- all.data[,names(all.data) %in% keepers.outcomes]
names(sumdat) <- new_names
sumdat_t <- sumdat[which(sumdat$Treated == 1),]
sumdat_c <- sumdat[which(sumdat$Treated == 0),]

png(filename = paste(direc, 'figures/sumstats.png', sep = ''))
datasummary_skim(sumdat, fmt = '%.3f')
dev.off()

png(filename = paste(direc, 'figures/sumstats_t.png', sep = ''))
datasummary_skim(sumdat_t, fmt = '%.3f')
dev.off()

png(filename = paste(direc, 'figures/sumstats_c.png', sep = ''))
datasummary_skim(sumdat_c, fmt = '%.3f')
dev.off()

# Results tables

b.df <- round(b.df, 3)
se.df <- round(se.df, 3)

p.stars <- function(coefs.input, p.vals.input, ro, co) {
  
  if (p.vals.input[ro,co] < 0.01) {
    
    append <- '$^{***}$'
    
  } else if (p.vals.input[ro,co] < 0.05) {
    
    append <- '$^{**}$'
    
  } else if (p.vals.input[ro,co] < 0.1) {
    
    append <- '$^{*}$'
    
  } else {append <- ''}
  
  gimme <- paste(coefs.input[ro,co], append, sep = '')
  
  return(gimme)
  
}

line.maker.coef <- function(df, pdf, x) {
  
  line.out <- x
  
  for (q in 1:dim(df)[2]) {
    
    val <- p.stars(df, pdf, x, q)
    line.out <- paste(line.out, val, sep = ' & ')
    
  }
  
  line.out <- paste(line.out, '\\', sep = '')
  
  return(line.out)
  
}

line.maker.serr <- function(df, x) {
  
  line.out <- ''
  
  for (q in 2:dim(df)[2]) {
    
    val <- paste('(', round(df[x, q],3), ')', sep = '')
    line.out <- paste(line.out, val, sep = ' & ')
    
  }
  
  line.out <- paste(line.out, '\\', sep = '')
  
  return(line.out)
  
}

for (i in 1:8) {
  
  results.df <- data.frame()
  tmp.b <- b.df[which(b.df$V1 == i), names(b.df) != 'V1']
  tmpbse <- se.df[which(se.df$V1 == i), names(se.df) != 'V1']
  tmp.p <- p.df[which(p.df$V1 == i), names(p.df) != 'V1']
  
  for (j in 1:8) {
    
    line.0 <- line.maker.coef(tmp.b, tmp.p, j)
    line.1 <- line.maker.serr(tmp.se, j)
    
    results.df <- rbind(results.df, line.0, line.1)
    
  }
  
  df.names <- paste('Window', 'Top Flight', 'Euro Play', 'Place', 'Points', 'Wins', 'Draws', 'Losses', 'Goals Scored', 'Goals Allowed', 'Differential', 'Champions L.', 'Europa L.\\\\', sep = ' & ')
  names(results.df) <- df.names
  write.csv(results.df, paste(direc, 'results/treatment_effects_', i, '_.txt', sep = ''), row.names = FALSE)
  
}

