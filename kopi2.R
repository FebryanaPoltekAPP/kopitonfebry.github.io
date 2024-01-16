setwd("D:/METOPEL UAS/FEBRY METOPEL")
library(readxl)
library(tidyverse)
library(kableExtra)
read_excel("kopi2.xlsx")
dat <- read_excel("kopi2.xlsx")
kbl(dat) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"))

# regresi
reg1<-lm(ekspor~volt+hargat+kurs+prodt,data=dat)
summary(reg1)

# Plot 
plot(dat$tahun,dat$ekspor,xlab="Tahun",ylab="Nilai FOB Ekspor ")
plot(dat$tahun,dat$volt,xlab="Tahun",ylab="Volume Ekspor (ton)")
plot(dat$tahun,dat$harga,xlab="Tahun",ylab="Harga jual per Ton")
plot(dat$tahun,dat$kurs,xlab="Tahun",ylab="Nilai Tukar RP/USD")
plot(dat$tahun,dat$prodt,xlab="Tahun",ylab="Produksi Kopi  (ton)")

# Plot Error

dat$m<-resid(reg1)
plot(dat$ekspor,dat$m,xlab="Nilai Ekspor Kopi",ylab="error")
abline(h=0) # membuat garis horizontal di y=0

dat$m<-resid(reg1)
plot(dat$volt,dat$m,xlab="Volume Ekspor (ton)",ylab="error")
abline(h=0) # membuat garis horizontal di y=0

dat$m<-resid(reg1)
plot(dat$harga,dat$m,xlab="Harga jual per Ton",ylab="error")
abline(h=0) # membuat garis horizontal di y=0

dat$m<-resid(reg1)
plot(dat$kurs,dat$m,xlab="Nilai Tukar RP/USD",ylab="error")
abline(h=0) # membuat garis horizontal di y=0

dat$m<-resid(reg1)
plot(dat$prodt,dat$m,xlab="Produksi Kopi Indonesia (ton)",ylab="error")
abline(h=0) # membuat garis horizontal di y=0
