
POD_dataset <- read.csv("C:/Users/pushp/Dokoumente/BA/Logist/POD_dataset.csv")
POD_dataset <- read.table(textConnection("POD_dataset$anae_end , POD_dataset$anae_start") ,header = TRUE)

POD_dataset$anae_diff <-strptime( paste(POD_dataset$anae_end, POD_dataset$anae_start), "%Y-%m-%d %H:%M:%S")

#Datensatz_Delirium <- read.table(textConnection("anae_start, anae_end, header=TRUE)

POD_dataset$anae_diff <- as.numeric(difftime(strptime(paste(POD_dataset$anae_end),"%Y-%m-%dT%H:%M:%SZ"), strptime(paste(POD_dataset$anae_start),"%Y-%m-%dT%H:%M:%SZ") , units = "min"))

show(POD_dataset$anae_diff)

intervention <- xtabs(~ POD + intervention, POD_dataset)
hist(intervention)

anae_diff <- xtabs(~ POD + anae_diff POD_dataset)
hist(anae_diff)

#format(.POSIXct(86400*POD_dataset$anae_diff, "UTC"), "%H:%M")
#date1 <- format(as.POSIXct(Sys.Date() + POD_dataset$anae_diff), "%H:%M")
#print(date1)




