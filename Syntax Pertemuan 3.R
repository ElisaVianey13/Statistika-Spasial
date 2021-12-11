library(spdep)
data=read.table(file.choose(),header=TRUE)
data
attach(data)
b1=as.numeric(data$IPM)
b1
bobot=read.table(file.choose(),header=FALSE)
bobot
bot=as.matrix(bobot)
bot

wbot=mat2listw(bot)
wbot

#IndeksMoran
moran.test(b1,nb2listw(wbot$neighbours,style="W"),alternative="two.sided")

#LISA
localmoran(b1,nb2listw(wbot$neighbours,style="W"),alternative="two.sided")
