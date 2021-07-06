#calculating Odds Ratio

#mydata <- read.csv("C:Dokoumente/BA/Datensatz_Delirium.csv")
Datensatz_Delirium <- read.csv(file.choose(),header = T)
attach(Datensatz_Delirium)

TAB  <- table(POD , gender)

