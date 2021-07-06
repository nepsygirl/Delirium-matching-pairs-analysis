library (hms)
library (stringr)


                                    
Datesatz_Delirium<- read.csv("C:/Users/pushp/Dokoumente/BA/Datensatz_Delirium.csv")
Datesatz_Delirium$anae_start <- apply (str_split (Datesatz_Delirium$anae_start, "T|Z", simplify = TRUE)[, 1:2], 1,
                             function (t) {return (paste (t[1], t[2], collapse = " "))})
POD_dataset$anae_end <- apply (str_split (Datesatz_Delirium$anae_start, "T|Z", simplify = TRUE)[, 1:2], 1,
                           function (t) {return (paste (t[1], t[2], collapse = " "))})

diff_time <- difftime (strptime (Datesatz_Delirium$anae_start, "%Y-%m-%d %H:%M:%S"), strptime (Datesatz_Delirium$anae_start, "%Y-%m-%d %H:%M:%S") , format(.POSIXct((diff_time), "UTC"), "%H:%M"))
as_hms (diff_time)
show(diff_time)
#format(as.POSIXct(Sys.Date(diff_time), "%H:%M", tz="UTC"))
#format(.POSIXct((diff_time), "UTC"), "%H:%M")
       
