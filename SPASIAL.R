library(spdep)
library(readxl)
data <- read_excel("D:/STATISTIKA/MATA KULIAH/KAMPUS MERDEKA/PERTUKARAN MAHASISWA/UNIVERSITAS MULAWARMAN/TUGAS/TUGAS PRAKTIKUM/TUGAS 3  PRAKTIKUM/RLSB.xlsx", 
                   col_types = c("text", "numeric"))
View(data)
data
summary(data)
attach(data)
b1=as.numeric(data$RLS)
b1
bobot=read.table(file.choose(),header=FALSE)
bobot
bot=as.matrix(bobot)
bot

wbot=mat2listw(bot)
wbot

#IndeksMoran
moran.test(b1,nb2listw(wbot$neighbours,style="W"),alternative="two.sided")

#Autokorelasi Geary's 
geary.test(b1,nb2listw(wbot$neighbours,style="W"),alternative="two.sided")

#LISA
localmoran(b1,nb2listw(wbot$neighbours,style="W"),alternative="two.sided")
