set.seed(1)

POD_dataset <- read.csv(file = 'POD_dataset' , sep = ',' , header = T)
summary(POD_dataset)
POD <- table(POD_dataset$POD , POD_dataset$intervention)
POD


#define data
df <- data.frame(team=rep(c('treatment', 'not treatment', each=2)),
                 pos=rep(c('POD', 'No POD'), times=4))
      

#view head of data 
head(df)
tab1 <- table(df$pos, df$team)
tab1

tab <- matrix(rep(2, times=4), ncol=2, byrow=TRUE)

#define column names and row names of matrix
colnames(tab) <- c('Intervention', 'No intervention')
rownames(tab) <- c('POD', 'NO POD')

#convert matrix to table 
tab <- as.table(tab, data = POD_dataset)

#view table 
tab


