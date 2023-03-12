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

direc <- 'F:/soccer_performance/'

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

# A function for the figures

make.y.limits <- function(a.list) {
  
  a <- min(0, 1.1*min(a.list))
  b <- max(0, 1.1*max(a.list))
  y.limits <- c(a,b)
  
  return(y.limits)

}

# Main loop

for (time.frame in 1:8) {
  
  d <- all.data[which(all.data$Years == time.frame),]
  
  # Run RDDs
  
  cep.w1 <- ivreg(Change.in.European.Play ~ -1 + Treated + Place | . - Treated + Cutoff, data = d[which(d$Window == 1),])
  cpl.w1 <- ivreg(Change.in.Place ~ -1 + Treated + Place | . - Treated + Cutoff, data = d[which(d$Window == 1),])
  cpo.w1 <- ivreg(Change.in.Points ~ -1 + Treated + Place | . - Treated + Cutoff, data = d[which(d$Window == 1),])
  cw.w1 <- ivreg(Change.in.Wins ~ -1 + Treated + Place | . - Treated + Cutoff, data = d[which(d$Window == 1),])
  cd.w1 <- ivreg(Change.in.Draws ~ -1 + Treated + Place | . - Treated + Cutoff, data = d[which(d$Window == 1),])
  cl.w1 <- ivreg(Change.in.Losses ~ -1 + Treated + Place | . - Treated + Cutoff, data = d[which(d$Window == 1),])
  cgs.w1 <- ivreg(Change.in.Goals.Scored ~ -1 + Treated + Place | . - Treated + Cutoff, data = d[which(d$Window == 1),])
  cga.w1 <- ivreg(Change.in.Goals.Allowed ~ -1 + Treated + Place | . - Treated + Cutoff, data = d[which(d$Window == 1),])
  cgd.w1 <- ivreg(Change.in.Goal.Differential ~ -1 + Treated + Place | . - Treated + Cutoff, data = d[which(d$Window == 1),])
  ccl.w1 <- ivreg(Change.in.Champions.League ~ -1 + Treated + Place | . - Treated + Cutoff, data = d[which(d$Window == 1),])
  cel.w1 <- ivreg(Change.in.Europa.League ~ -1 + Treated + Place | . - Treated + Cutoff, data = d[which(d$Window == 1),])
  
  cep.w2 <- ivreg(Change.in.European.Play ~ -1 + Treated + Place | . - Treated + Cutoff, data = d[which(d$Window == 2),])
  cpl.w2 <- ivreg(Change.in.Place ~ -1 + Treated + Place | . - Treated + Cutoff, data = d[which(d$Window == 2),])
  cpo.w2 <- ivreg(Change.in.Points ~ -1 + Treated + Place | . - Treated + Cutoff, data = d[which(d$Window == 2),])
  cw.w2 <- ivreg(Change.in.Wins ~ -1 + Treated + Place | . - Treated + Cutoff, data = d[which(d$Window == 2),])
  cd.w2 <- ivreg(Change.in.Draws ~ -1 + Treated + Place | . - Treated + Cutoff, data = d[which(d$Window == 2),])
  cl.w2 <- ivreg(Change.in.Losses ~ -1 + Treated + Place | . - Treated + Cutoff, data = d[which(d$Window == 2),])
  cgs.w2 <- ivreg(Change.in.Goals.Scored ~ -1 + Treated + Place | . - Treated + Cutoff, data = d[which(d$Window == 2),])
  cga.w2 <- ivreg(Change.in.Goals.Allowed ~ -1 + Treated + Place | . - Treated + Cutoff, data = d[which(d$Window == 2),])
  cgd.w2 <- ivreg(Change.in.Goal.Differential ~ -1 + Treated + Place | . - Treated + Cutoff, data = d[which(d$Window == 2),])
  ccl.w2 <- ivreg(Change.in.Champions.League ~ -1 + Treated + Place | . - Treated + Cutoff, data = d[which(d$Window == 2),])
  cel.w2 <- ivreg(Change.in.Europa.League ~ -1 + Treated + Place | . - Treated + Cutoff, data = d[which(d$Window == 2),])
  
  cep.w3 <- ivreg(Change.in.European.Play ~ -1 + Treated + Place | . - Treated + Cutoff, data = d[which(d$Window == 3),])
  cpl.w3 <- ivreg(Change.in.Place ~ -1 + Treated + Place | . - Treated + Cutoff, data = d[which(d$Window == 3),])
  cpo.w3 <- ivreg(Change.in.Points ~ -1 + Treated + Place | . - Treated + Cutoff, data = d[which(d$Window == 3),])
  cw.w3 <- ivreg(Change.in.Wins ~ -1 + Treated + Place | . - Treated + Cutoff, data = d[which(d$Window == 3),])
  cd.w3 <- ivreg(Change.in.Draws ~ -1 + Treated + Place | . - Treated + Cutoff, data = d[which(d$Window == 3),])
  cl.w3 <- ivreg(Change.in.Losses ~ -1 + Treated + Place | . - Treated + Cutoff, data = d[which(d$Window == 3),])
  cgs.w3 <- ivreg(Change.in.Goals.Scored ~ -1 + Treated + Place | . - Treated + Cutoff, data = d[which(d$Window == 3),])
  cga.w3 <- ivreg(Change.in.Goals.Allowed ~ -1 + Treated + Place | . - Treated + Cutoff, data = d[which(d$Window == 3),])
  cgd.w3 <- ivreg(Change.in.Goal.Differential ~ -1 + Treated + Place | . - Treated + Cutoff, data = d[which(d$Window == 3),])
  ccl.w3 <- ivreg(Change.in.Champions.League ~ -1 + Treated + Place | . - Treated + Cutoff, data = d[which(d$Window == 3),])
  cel.w3 <- ivreg(Change.in.Europa.League ~ -1 + Treated + Place | . - Treated + Cutoff, data = d[which(d$Window == 3),])
  
  cep.w4 <- ivreg(Change.in.European.Play ~ -1 + Treated + Place | . - Treated + Cutoff, data = d[which(d$Window == 4),])
  cpl.w4 <- ivreg(Change.in.Place ~ -1 + Treated + Place | . - Treated + Cutoff, data = d[which(d$Window == 4),])
  cpo.w4 <- ivreg(Change.in.Points ~ -1 + Treated + Place | . - Treated + Cutoff, data = d[which(d$Window == 4),])
  cw.w4 <- ivreg(Change.in.Wins ~ -1 + Treated + Place | . - Treated + Cutoff, data = d[which(d$Window == 4),])
  cd.w4 <- ivreg(Change.in.Draws ~ -1 + Treated + Place | . - Treated + Cutoff, data = d[which(d$Window == 4),])
  cl.w4 <- ivreg(Change.in.Losses ~ -1 + Treated + Place | . - Treated + Cutoff, data = d[which(d$Window == 4),])
  cgs.w4 <- ivreg(Change.in.Goals.Scored ~ -1 + Treated + Place | . - Treated + Cutoff, data = d[which(d$Window == 4),])
  cga.w4 <- ivreg(Change.in.Goals.Allowed ~ -1 + Treated + Place | . - Treated + Cutoff, data = d[which(d$Window == 4),])
  cgd.w4 <- ivreg(Change.in.Goal.Differential ~ -1 + Treated + Place | . - Treated + Cutoff, data = d[which(d$Window == 4),])
  ccl.w4 <- ivreg(Change.in.Champions.League ~ -1 + Treated + Place | . - Treated + Cutoff, data = d[which(d$Window == 4),])
  cel.w4 <- ivreg(Change.in.Europa.League ~ -1 + Treated + Place | . - Treated + Cutoff, data = d[which(d$Window == 4),])
  
  cep.w5 <- ivreg(Change.in.European.Play ~ -1 + Treated + Place | . - Treated + Cutoff, data = d[which(d$Window == 5),])
  cpl.w5 <- ivreg(Change.in.Place ~ -1 + Treated + Place | . - Treated + Cutoff, data = d[which(d$Window == 5),])
  cpo.w5 <- ivreg(Change.in.Points ~ -1 + Treated + Place | . - Treated + Cutoff, data = d[which(d$Window == 5),])
  cw.w5 <- ivreg(Change.in.Wins ~ -1 + Treated + Place | . - Treated + Cutoff, data = d[which(d$Window == 5),])
  cd.w5 <- ivreg(Change.in.Draws ~ -1 + Treated + Place | . - Treated + Cutoff, data = d[which(d$Window == 5),])
  cl.w5 <- ivreg(Change.in.Losses ~ -1 + Treated + Place | . - Treated + Cutoff, data = d[which(d$Window == 5),])
  cgs.w5 <- ivreg(Change.in.Goals.Scored ~ -1 + Treated + Place | . - Treated + Cutoff, data = d[which(d$Window == 5),])
  cga.w5 <- ivreg(Change.in.Goals.Allowed ~ -1 + Treated + Place | . - Treated + Cutoff, data = d[which(d$Window == 5),])
  cgd.w5 <- ivreg(Change.in.Goal.Differential ~ -1 + Treated + Place | . - Treated + Cutoff, data = d[which(d$Window == 5),])
  ccl.w5 <- ivreg(Change.in.Champions.League ~ -1 + Treated + Place | . - Treated + Cutoff, data = d[which(d$Window == 5),])
  cel.w5 <- ivreg(Change.in.Europa.League ~ -1 + Treated + Place | . - Treated + Cutoff, data = d[which(d$Window == 5),])
  
  cep.w6 <- ivreg(Change.in.European.Play ~ -1 + Treated + Place | . - Treated + Cutoff, data = d[which(d$Window == 6),])
  cpl.w6 <- ivreg(Change.in.Place ~ -1 + Treated + Place | . - Treated + Cutoff, data = d[which(d$Window == 6),])
  cpo.w6 <- ivreg(Change.in.Points ~ -1 + Treated + Place | . - Treated + Cutoff, data = d[which(d$Window == 6),])
  cw.w6 <- ivreg(Change.in.Wins ~ -1 + Treated + Place | . - Treated + Cutoff, data = d[which(d$Window == 6),])
  cd.w6 <- ivreg(Change.in.Draws ~ -1 + Treated + Place | . - Treated + Cutoff, data = d[which(d$Window == 6),])
  cl.w6 <- ivreg(Change.in.Losses ~ -1 + Treated + Place | . - Treated + Cutoff, data = d[which(d$Window == 6),])
  cgs.w6 <- ivreg(Change.in.Goals.Scored ~ -1 + Treated + Place | . - Treated + Cutoff, data = d[which(d$Window == 6),])
  cga.w6 <- ivreg(Change.in.Goals.Allowed ~ -1 + Treated + Place | . - Treated + Cutoff, data = d[which(d$Window == 6),])
  cgd.w6 <- ivreg(Change.in.Goal.Differential ~ -1 + Treated + Place | . - Treated + Cutoff, data = d[which(d$Window == 6),])
  ccl.w6 <- ivreg(Change.in.Champions.League ~ -1 + Treated + Place | . - Treated + Cutoff, data = d[which(d$Window == 6),])
  cel.w6 <- ivreg(Change.in.Europa.League ~ -1 + Treated + Place | . - Treated + Cutoff, data = d[which(d$Window == 6),])
  
  cep.w7 <- ivreg(Change.in.European.Play ~ -1 + Treated + Place | . - Treated + Cutoff, data = d[which(d$Window == 7),])
  cpl.w7 <- ivreg(Change.in.Place ~ -1 + Treated + Place | . - Treated + Cutoff, data = d[which(d$Window == 7),])
  cpo.w7 <- ivreg(Change.in.Points ~ -1 + Treated + Place | . - Treated + Cutoff, data = d[which(d$Window == 7),])
  cw.w7 <- ivreg(Change.in.Wins ~ -1 + Treated + Place | . - Treated + Cutoff, data = d[which(d$Window == 7),])
  cd.w7 <- ivreg(Change.in.Draws ~ -1 + Treated + Place | . - Treated + Cutoff, data = d[which(d$Window == 7),])
  cl.w7 <- ivreg(Change.in.Losses ~ -1 + Treated + Place | . - Treated + Cutoff, data = d[which(d$Window == 7),])
  cgs.w7 <- ivreg(Change.in.Goals.Scored ~ -1 + Treated + Place | . - Treated + Cutoff, data = d[which(d$Window == 7),])
  cga.w7 <- ivreg(Change.in.Goals.Allowed ~ -1 + Treated + Place | . - Treated + Cutoff, data = d[which(d$Window == 7),])
  cgd.w7 <- ivreg(Change.in.Goal.Differential ~ -1 + Treated + Place | . - Treated + Cutoff, data = d[which(d$Window == 7),])
  ccl.w7 <- ivreg(Change.in.Champions.League ~ -1 + Treated + Place | . - Treated + Cutoff, data = d[which(d$Window == 7),])
  cel.w7 <- ivreg(Change.in.Europa.League ~ -1 + Treated + Place | . - Treated + Cutoff, data = d[which(d$Window == 7),])
  
  cep.w8 <- ivreg(Change.in.European.Play ~ -1 + Treated + Place | . - Treated + Cutoff, data = d[which(d$Window == 8),])
  cpl.w8 <- ivreg(Change.in.Place ~ -1 + Treated + Place | . - Treated + Cutoff, data = d[which(d$Window == 8),])
  cpo.w8 <- ivreg(Change.in.Points ~ -1 + Treated + Place | . - Treated + Cutoff, data = d[which(d$Window == 8),])
  cw.w8 <- ivreg(Change.in.Wins ~ -1 + Treated + Place | . - Treated + Cutoff, data = d[which(d$Window == 8),])
  cd.w8 <- ivreg(Change.in.Draws ~ -1 + Treated + Place | . - Treated + Cutoff, data = d[which(d$Window == 8),])
  cl.w8 <- ivreg(Change.in.Losses ~ -1 + Treated + Place | . - Treated + Cutoff, data = d[which(d$Window == 8),])
  cgs.w8 <- ivreg(Change.in.Goals.Scored ~ -1 + Treated + Place | . - Treated + Cutoff, data = d[which(d$Window == 8),])
  cga.w8 <- ivreg(Change.in.Goals.Allowed ~ -1 + Treated + Place | . - Treated + Cutoff, data = d[which(d$Window == 8),])
  cgd.w8 <- ivreg(Change.in.Goal.Differential ~ -1 + Treated + Place | . - Treated + Cutoff, data = d[which(d$Window == 8),])
  ccl.w8 <- ivreg(Change.in.Champions.League ~ -1 + Treated + Place | . - Treated + Cutoff, data = d[which(d$Window == 8),])
  cel.w8 <- ivreg(Change.in.Europa.League ~ -1 + Treated + Place | . - Treated + Cutoff, data = d[which(d$Window == 8),])
  
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
  
  # Save results
  
  write.csv(stargazer(cep.w1, cpl.w1, cpo.w1, cw.w1, cd.w1, cl.w1, cgs.w1, cga.w1, cgd.w1, ccl.w1, cel.w1, type = 'text', omit.stat = c('f', 'ser')), paste(direc, 'results/window=1__time=', time.frame, '.txt', sep = ''))
  write.csv(stargazer(cep.w2, cpl.w2, cpo.w2, cw.w2, cd.w2, cl.w2, cgs.w2, cga.w2, cgd.w2, ccl.w2, cel.w2, type = 'text', omit.stat = c('f', 'ser')), paste(direc, 'results/window=2__time=', time.frame, '.txt', sep = ''))
  write.csv(stargazer(cep.w3, cpl.w3, cpo.w3, cw.w3, cd.w3, cl.w3, cgs.w3, cga.w3, cgd.w3, ccl.w3, cel.w3, type = 'text', omit.stat = c('f', 'ser')), paste(direc, 'results/window=3__time=', time.frame, '.txt', sep = ''))
  write.csv(stargazer(cep.w4, cpl.w4, cpo.w4, cw.w4, cd.w4, cl.w4, cgs.w4, cga.w4, cgd.w4, ccl.w4, cel.w4, type = 'text', omit.stat = c('f', 'ser')), paste(direc, 'results/window=4__time=', time.frame, '.txt', sep = ''))
  write.csv(stargazer(cep.w5, cpl.w5, cpo.w5, cw.w5, cd.w5, cl.w5, cgs.w5, cga.w5, cgd.w5, ccl.w5, cel.w5, type = 'text', omit.stat = c('f', 'ser')), paste(direc, 'results/window=5__time=', time.frame, '.txt', sep = ''))
  write.csv(stargazer(cep.w6, cpl.w6, cpo.w6, cw.w6, cd.w6, cl.w6, cgs.w6, cga.w6, cgd.w6, ccl.w6, cel.w6, type = 'text', omit.stat = c('f', 'ser')), paste(direc, 'results/window=6__time=', time.frame, '.txt', sep = ''))
  write.csv(stargazer(cep.w7, cpl.w7, cpo.w7, cw.w7, cd.w7, cl.w7, cgs.w7, cga.w7, cgd.w7, ccl.w7, cel.w7, type = 'text', omit.stat = c('f', 'ser')), paste(direc, 'results/window=7__time=', time.frame, '.txt', sep = ''))
  write.csv(stargazer(cep.w8, cpl.w8, cpo.w8, cw.w8, cd.w8, cl.w8, cgs.w8, cga.w8, cgd.w8, ccl.w8, cel.w8, type = 'text', omit.stat = c('f', 'ser')), paste(direc, 'results/window=8__time=', time.frame, '.txt', sep = ''))
  
  # Longitudinal plots of change in outcomes over time
  
  cep <- c(cep.w1[1], cep.w3[1], cep.w3[1], cep.w4[1], cep.w5[1], cep.w6[1], cep.w7[1], cep.w8[1])
  cpl <- c(cpl.w1[1], cpl.w2[1], cpl.w3[1], cpl.w4[1], cpl.w5[1], cpl.w6[1], cpl.w7[1], cpl.w8[1])
  cpo <- c(cpo.w1[1], cpo.w2[1], cpo.w3[1], cpo.w4[1], cpo.w5[1], cpo.w6[1], cpo.w7[1], cpo.w8[1])
  cw <- c(cw.w1[1], cw.w2[1], cw.w3[1], cw.w4[1], cw.w5[1], cw.w6[1], cw.w7[1], cw.w8[1])
  cd <- c(cd.w1[1], cd.w2[1], cd.w3[1], cd.w4[1], cd.w5[1], cd.w6[1], cd.w7[1], cd.w8[1])
  cl <- c(cl.w1[1], cl.w2[1], cl.w3[1], cl.w4[1], cl.w5[1], cl.w6[1], cl.w7[1], cl.w8[1])
  cgs <- c(cgs.w1[1], cgs.w2[1], cgs.w3[1], cgs.w4[1], cgs.w5[1], cgs.w6[1], cgs.w7[1], cgs.w8[1])
  cga <- c(cga.w1[1], cga.w2[1], cga.w3[1], cga.w4[1], cga.w5[1], cga.w6[1], cga.w7[1], cga.w8[1])
  cgd <- c(cgd.w1[1], cgd.w2[1], cgd.w3[1], cgd.w4[1], cgd.w5[1], cgd.w6[1], cgd.w7[1], cgd.w8[1])
  ccl <- c(ccl.w1[1], ccl.w2[1], ccl.w3[1], ccl.w4[1], ccl.w5[1], ccl.w6[1], ccl.w7[1], ccl.w8[1])
  cel <- c(cel.w1[1], cel.w2[1], cel.w3[1], cel.w4[1], cel.w5[1], cel.w6[1], cel.w7[1], cel.w8[1])
  
  cepx <- as.data.frame(cbind(cep,rep(0,length(cep))))
  cplx <- as.data.frame(cbind(cpl,rep(0,length(cpl))))
  cpox <- as.data.frame(cbind(cpo,rep(0,length(cpo))))
  cwx <- as.data.frame(cbind(cw,rep(0,length(cw))))
  cdx <- as.data.frame(cbind(cd,rep(0,length(cd))))
  clx <- as.data.frame(cbind(cl,rep(0,length(cl))))
  cgsx <- as.data.frame(cbind(cgs,rep(0,length(cgs))))
  cgax <- as.data.frame(cbind(cga,rep(0,length(cga))))
  cgdx <- as.data.frame(cbind(cgd,rep(0,length(cgd))))
  cclx <- as.data.frame(cbind(ccl,rep(0,length(ccl))))
  celx <- as.data.frame(cbind(cel,rep(0,length(cel))))
  
  png(filename = paste(direc, 'figures/coefs__', time.frame, '.png', sep = ''))
  par(mfrow = c(4,3), mai = c(0.3, 0.7, 0.3, 0.3))
  matplot(cepx, type = 'l', pch = 1, col = 1:2, ylab = 'European Play', xlab = 'Years', ylim = make.y.limits(cep))
  matplot(cplx, type = 'l', pch = 1, col = 1:2, ylab = 'Place', xlab = 'Years', ylim = make.y.limits(cpl))
  matplot(cpox, type = 'l', pch = 1, col = 1:2, ylab = 'Points', xlab = 'Years', ylim = make.y.limits(cpo))
  matplot(cwx, type = 'l', pch = 1, col = 1:2, ylab = 'Wins', xlab = 'Years', ylim = make.y.limits(cw))
  matplot(cdx, type = 'l', pch = 1, col = 1:2, ylab = 'Draws', xlab = 'Years', ylim = make.y.limits(cd))
  matplot(clx, type = 'l', pch = 1, col = 1:2, ylab = 'Loses', xlab = 'Years', ylim = make.y.limits(cl))
  matplot(cgsx, type = 'l', pch = 1, col = 1:2, ylab = 'Goals Scored', xlab = 'Years', ylim = make.y.limits(cgs))
  matplot(cgax, type = 'l', pch = 1, col = 1:2, ylab = 'Goals Allowed', xlab = 'Years', ylim = make.y.limits(cga))
  matplot(cgdx, type = 'l', pch = 1, col = 1:2, ylab = 'Goal Difference', xlab = 'Years', ylim = make.y.limits(cgd))
  matplot(cclx, type = 'l', pch = 1, col = 1:2, ylab = 'Champions League', xlab = 'Years', ylim = make.y.limits(ccl))
  matplot(celx, type = 'l', pch = 1, col = 1:2, ylab = 'Europa League', xlab = 'Years', ylim = make.y.limits(cel))
  dev.off()
  
}

# Summary statistics

keepers.outcomes <- c('Window', 'Cutoff', 'Treated', 'Place', 'Points', 'Wins', 'Draws', 'Losses', 'Goals_For', 'Goals_Allowed', 'Goal_Difference', 'Champions.League', 'Europa.League')
new_names <- c('Window', 'Cutoff', 'Treated', 'Place', 'Points', 'Wins', 'Draws', 'Losses', 'Goals For', 'Goals Allowed', 'Goal Difference', 'Champions League', 'Europa League')
sumdat <- all.data[,names(all.data) %in% keepers.outcomes]
names(sumdat) <- new_names
sumdat_t <- sumdat[which(sumdat$Treated == 1),]
sumdat_c <- sumdat[which(sumdat$Treated == 0),]
sumdat_t <- sumdat_t[,which(names(sumdat_t) != 'Treated')]
sumdat_c <- sumdat_c[,which(names(sumdat_c) != 'Treated')]

datasummary_skim(sumdat, fmt = '%.3f')
datasummary_skim(sumdat_t, fmt = '%.3f')
datasummary_skim(sumdat_c, fmt = '%.3f')
