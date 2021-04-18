#install.packages("lubridate")
dat <- read.table(textConnection("anae_end,anae_start"), data =Datensatz_Delirium header=TRUE)
#anae_end = ymd_hms(paste(dat[,19])
anae_end = ymd_hms(paste(dat[,19])
anae_start = ymd_hms(paste(dat[,18])
intervall = difftime(anae_end,anae_start,units = "mins")



