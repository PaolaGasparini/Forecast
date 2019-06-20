
setwd("D:/Documents/Paola/R/Progetto_Claudio/new_data")

#setwd("C:/Users/pccity/Documents/R/Progetto_Claudio")
##google trend sara usato prossimamente.
#la mia variabile target è eurogpb
#google <- read.csv("google_trend.csv", header = TRUE)
#googlem <- as.matrix(google[,c(1,2,3,4)] ) 
#googlem_data <-data.frame(googlem)

#1) importo tutte le valute:
AUDJPY <- read.csv("AUDJPY.csv", header = TRUE)
AUDJPYm <- as.matrix(AUDJPY[,c(2,3,4)] ) 
AUDJPYm_data <-data.frame(AUDJPYm)

EURGBP <- read.csv("EURGBP.csv", header = TRUE)
EURGBPm <- as.matrix(EURGBP[,c(2,3,4)] )
EURGBPm_data  <-data.frame(EURGBPm)


AUDUSD <- read.csv("AUDUSD.csv", header = TRUE)
AUDUSDm <- as.matrix(AUDUSD[,c(2,3,4)] ) 
AUDUSDm_data <-data.frame(AUDUSDm)

EURCAD <- read.csv("EURCAD.csv", header = TRUE)
EURCADm <- as.matrix(EURCAD[,c(2,3,4)] )
EURCADm_data  <-data.frame(EURCADm)

EURCHF <- read.csv("EURCHF.csv", header = TRUE)
EURCHFm <- as.matrix(EURCHF[,c(2,3,4)] )
EURCHFm_data  <-data.frame(EURCHFm)

EURJPY <- read.csv("EURJPY.csv", header = TRUE)
EURJPYm <- as.matrix(EURJPY[,c(2,3,4)] )
EURJPYm_data  <-data.frame(EURJPYm)

EURUSD <- read.csv("EURUSD.csv", header = TRUE)
EURUSDm <- as.matrix(EURUSD[,c(2,3,4)] )
EURUSDm_data  <-data.frame(EURUSDm)

GBPUSD <- read.csv("GBPUSD.csv", header = TRUE)
GBPUSDm <- as.matrix(GBPUSD[,c(2,3,4)] )
GBPUSDm_data  <-data.frame(GBPUSDm)

NZDUSD <- read.csv("NZDUSD.csv", header = TRUE)
NZDUSDm <- as.matrix(NZDUSD[,c(2,3,4)] )
NZDUSDm_data  <-data.frame(NZDUSDm)

USDCAD <- read.csv("USDCAD.csv", header = TRUE)
USDCADm <- as.matrix(USDCAD[,c(2,3,4)] )
USDCADm_data  <-data.frame(USDCADm)

USDCHF <- read.csv("USDCHF.csv", header = TRUE)
USDCHFm <- as.matrix(USDCHF[,c(2,3,4)] )
USDCHFm_data  <-data.frame(USDCHFm)

USDJPY <- read.csv("USDJPY.csv", header = TRUE)
USDJPYm <- as.matrix(USDJPY[,c(2,3,4)] )
USDJPYm_data  <-data.frame(USDJPYm)


#2) creo una unica tabella p con tutte le valute
p <-merge(AUDJPYm_data, EURGBPm_data, by=c("X.DTYYYYMMDD.","X.TIME."))
p <-merge(p, AUDUSDm_data, by=c("X.DTYYYYMMDD.","X.TIME."))
p <-merge(p, EURCADm_data, by=c("X.DTYYYYMMDD.","X.TIME."))
p <-merge(p, EURCHFm_data, by=c("X.DTYYYYMMDD.","X.TIME."))
p <-merge(p, EURJPYm_data, by=c("X.DTYYYYMMDD.","X.TIME."))
p <-merge(p, EURUSDm_data, by=c("X.DTYYYYMMDD.","X.TIME."))
p <-merge(p, GBPUSDm_data, by=c("X.DTYYYYMMDD.","X.TIME."))
p <-merge(p, NZDUSDm_data, by=c("X.DTYYYYMMDD.","X.TIME."))
p <-merge(p, USDCADm_data, by=c("X.DTYYYYMMDD.","X.TIME."))
p <-merge(p, USDCHFm_data, by=c("X.DTYYYYMMDD.","X.TIME."))
p <-merge(p, USDJPYm_data, by=c("X.DTYYYYMMDD.","X.TIME."))

#renaming columns
colnames(p)[1] <- "DATE"
colnames(p)[2] <- "TIME"
colnames(p)[3] <- "AUDJPY"
colnames(p)[4] <- "EURGBP"
colnames(p)[5] <- "AUDUSD"
colnames(p)[6] <- "EURCAD"
colnames(p)[7] <- "EURCHF"
colnames(p)[8] <- "EURJPY"
colnames(p)[9] <- "EURUSD"
colnames(p)[10] <- "GBPUSD"
colnames(p)[11] <- "NZDUSD"
colnames(p)[12] <- "USDCAD"
colnames(p)[13] <- "USDCHF"
colnames(p)[14] <- "USDJPY"



Valori_ogni_5min <- function(x) {
  a <-dim(x)
  a <- as.data.frame(a)
  yy<-as.matrix(x[seq(1, a[1,1], 4),1]) #copio colonna2
  return(yy)
}

x <- as.data.frame(p[1])

t <- Valori_ogni_5min(x)

tabella <-array(0,c(dim(t),14))

tabella <-as.data.frame(tabella)

# 3) creo tabella finale chiamata tabella che ha valori delle valute ogni 5 minuti,
#iniziando dal secondo valore 
tabella[1]<- Valori_ogni_5min(as.data.frame(p[1]))
tabella[2]<- Valori_ogni_5min(as.data.frame(p[2]))
tabella[3]<- Valori_ogni_5min(as.data.frame(p[3]))
tabella[4]<- Valori_ogni_5min(as.data.frame(p[4]))
tabella[5]<- Valori_ogni_5min(as.data.frame(p[5]))
tabella[6]<- Valori_ogni_5min(as.data.frame(p[6]))
tabella[7]<- Valori_ogni_5min(as.data.frame(p[7]))
tabella[8]<- Valori_ogni_5min(as.data.frame(p[8]))
tabella[9]<- Valori_ogni_5min(as.data.frame(p[9]))
tabella[10]<- Valori_ogni_5min(as.data.frame(p[10]))
tabella[11]<- Valori_ogni_5min(as.data.frame(p[11]))
tabella[12]<- Valori_ogni_5min(as.data.frame(p[12]))
tabella[13]<- Valori_ogni_5min(as.data.frame(p[13]))
tabella[14]<- Valori_ogni_5min(as.data.frame(p[14]))


colnames(tabella)[1] <- "DATE"
colnames(tabella)[2] <- "TIME"
colnames(tabella)[3] <- "AUDJPY"
colnames(tabella)[4] <- "EURGBP"
colnames(tabella)[5] <- "AUDUSD"
colnames(tabella)[6] <- "EURCAD"
colnames(tabella)[7] <- "EURCHF"
colnames(tabella)[8] <- "EURJPY"
colnames(tabella)[9] <- "EURUSD"
colnames(tabella)[10] <- "GBPUSD"
colnames(tabella)[11] <- "NZDUSD"
colnames(tabella)[12] <- "USDCAD"
colnames(tabella)[13] <- "USDCHF"
colnames(tabella)[14] <- "USDJPY"

##Il nuovo dataset si chiama tabella e ha un valore ogni 5 minuti piu o meno per tutte le valute. 
#Il prossimo step è metterci i 5, 10 minuti, 15 minuti prima di tutte le valute. 
#usando Column_before_Xmin per tutte le valute.

Column_before_Xmin <- function(x,min) {
  colnames(x) <- "test"
  mat_2row_empty <-array(0, c(min/5,1))
  colnames(mat_2row_empty)[1] <- colnames(x)
  y <-rbind(mat_2row_empty,x)
  return(y)
}

#5 min before
tabella[15]<- Column_before_Xmin(tabella[3],5)
colnames(tabella)[15] <- "AUDJPY_5minBefore"
tabella[16]<- Column_before_Xmin(tabella[4],5)
colnames(tabella)[16] <- "EURGBP_5minBefore"
tabella[17]<- Column_before_Xmin(tabella[5],5)
colnames(tabella)[17] <- "AUDUSD_5minBefore"
tabella[18]<- Column_before_Xmin(tabella[6],5)
colnames(tabella)[18] <- "EURCAD_5minBefore"
tabella[19]<- Column_before_Xmin(tabella[7],5)
colnames(tabella)[19] <- "EURCHF_5minBefore"
tabella[20]<- Column_before_Xmin(tabella[8],5)
colnames(tabella)[20] <- "EURJPY_5minBefore"
tabella[21]<- Column_before_Xmin(tabella[9],5)
colnames(tabella)[21] <- "EURUSD_5minBefore"
tabella[22]<- Column_before_Xmin(tabella[10],5)
colnames(tabella)[22] <- "GBPUSD_5minBefore"
tabella[23]<- Column_before_Xmin(tabella[11],5)
colnames(tabella)[23] <- "NZDUSD_5minBefore"
tabella[24]<- Column_before_Xmin(tabella[12],5)
colnames(tabella)[24] <- "USDCAD_5minBefore"
tabella[25]<- Column_before_Xmin(tabella[13],5)
colnames(tabella)[25] <- "USDCHF_5minBefore"
tabella[26]<- Column_before_Xmin(tabella[14],5)
colnames(tabella)[26] <- "USDJPY_5minBefore"


##Tutte le valute 10 minuti prima:

tabella[27]<- Column_before_Xmin(tabella[3],10)
colnames(tabella)[27] <- "AUDJPY_10minBefore"
tabella[28]<- Column_before_Xmin(tabella[4],10)
colnames(tabella)[28] <- "EURGBP_10minBefore"
tabella[29]<- Column_before_Xmin(tabella[5],10)
colnames(tabella)[29] <- "AUDUSD_10minBefore"
tabella[30]<- Column_before_Xmin(tabella[6],10)
colnames(tabella)[30] <- "EURCAD_10minBefore"
tabella[31]<- Column_before_Xmin(tabella[7],10)
colnames(tabella)[31] <- "EURCHF_10minBefore"
tabella[32]<- Column_before_Xmin(tabella[8],10)
colnames(tabella)[32] <- "EURJPY_10minBefore"
tabella[33]<- Column_before_Xmin(tabella[9],10)
colnames(tabella)[33] <- "EURUSD_10minBefore"
tabella[34]<- Column_before_Xmin(tabella[10],10)
colnames(tabella)[34] <- "GBPUSD_10minBefore"
tabella[35]<- Column_before_Xmin(tabella[11],10)
colnames(tabella)[35] <- "NZDUSD_10minBefore"
tabella[36]<- Column_before_Xmin(tabella[12],10)
colnames(tabella)[36] <- "USDCAD_10minBefore"
tabella[37]<- Column_before_Xmin(tabella[13],10)
colnames(tabella)[37] <- "USDCHF_10minBefore"
tabella[38]<- Column_before_Xmin(tabella[14],10)
colnames(tabella)[38] <- "USDJPY_10minBefore"

##Tutte le valute 15 minuti prima:
tabella[39]<- Column_before_Xmin(tabella[3],15)
colnames(tabella)[39] <- "AUDJPY_15minBefore"
tabella[40]<- Column_before_Xmin(tabella[4],15)
colnames(tabella)[40] <- "EURGBP_15minBefore"
tabella[41]<- Column_before_Xmin(tabella[5],15)
colnames(tabella)[41] <- "AUDUSD_15minBefore"
tabella[42]<- Column_before_Xmin(tabella[6],15)
colnames(tabella)[42] <- "EURCAD_15minBefore"
tabella[43]<- Column_before_Xmin(tabella[7],15)
colnames(tabella)[43] <- "EURCHF_15minBefore"
tabella[44]<- Column_before_Xmin(tabella[8],15)
colnames(tabella)[44] <- "EURJPY_15minBefore"
tabella[45]<- Column_before_Xmin(tabella[9],15)
colnames(tabella)[45] <- "EURUSD_15minBefore"
tabella[46]<- Column_before_Xmin(tabella[10],15)
colnames(tabella)[46] <- "GBPUSD_15minBefore"
tabella[47]<- Column_before_Xmin(tabella[11],15)
colnames(tabella)[47] <- "NZDUSD_15minBefore"
tabella[48]<- Column_before_Xmin(tabella[12],15)
colnames(tabella)[48] <- "USDCAD_15minBefore"
tabella[49]<- Column_before_Xmin(tabella[13],15)
colnames(tabella)[49] <- "USDCHF_15minBefore"
tabella[50]<- Column_before_Xmin(tabella[14],15)
colnames(tabella)[50] <- "USDJPY_15minBefore"


#applico moving average a tutte le valute, ma a 2 valori, a 3 valori..fino a 10 valori. 

#Il moving average e' quello dei 5 minuti prima, perche le valute stesse non ce le ho. 

ma <- function(x,n){filter(x,rep(1/n,n), sides=2)}
####
##Moving average con 2 valori

tabella[51] <- ma(tabella[15],2) #moving average con 2 valori.
colnames(tabella)[51] <- "AUDJPY_MA2"
tabella[52] <- ma(tabella[16],2) #moving average con 2 valori.,
colnames(tabella)[52] <- "EURGBP_MA2"
tabella[53] <- ma(tabella[17],2) #moving average con 2 valori.
colnames(tabella)[53] <- "AUDUSD_MA2"
tabella[54] <- ma(tabella[18],2) #moving average con 2 valori.
colnames(tabella)[54] <- "EURCAD_MA2"
tabella[55] <- ma(tabella[19],2) #moving average con 2 valori.
colnames(tabella)[55] <- "EURCHF_MA2"
tabella[56] <- ma(tabella[20],2) #moving average con 2 valori.
colnames(tabella)[56] <- "EURJPY_MA2"
tabella[57] <- ma(tabella[21],2) #moving average con 2 valori.
colnames(tabella)[57] <- "EURUSD_MA2"
tabella[58] <- ma(tabella[22],2) #moving average con 2 valori.
colnames(tabella)[58] <- "GBPUSD_MA2"
tabella[59] <- ma(tabella[23],2) #moving average con 2 valori.
colnames(tabella)[59] <- "NZDUSD_MA2"
tabella[60] <- ma(tabella[24],2) #moving average con 2 valori.
colnames(tabella)[60] <- "USDCAD_MA2"
tabella[61] <- ma(tabella[25],2) #moving average con 2 valori.
colnames(tabella)[61] <- "USDCHF_MA2"
tabella[62] <- ma(tabella[26],2) #moving average con 2 valori.
colnames(tabella)[62] <- "USDJPY_MA2"

#MA con 3 valori
tabella[63] <- ma(tabella[15],3) #moving average con 3 valori.
colnames(tabella)[63] <- "AUDJPY_MA3"
tabella[64] <- ma(tabella[16],3) #moving average con 3 valori.
colnames(tabella)[64] <- "EURGBP_MA3"
tabella[65] <- ma(tabella[17],3) #moving average con 3 valori.
colnames(tabella)[65] <- "AUDUSD_MA3"
tabella[66] <- ma(tabella[18],3) #moving average con 3 valori.
colnames(tabella)[66] <- "EURCAD_MA3"
tabella[67] <- ma(tabella[19],3) #moving average con 3 valori.
colnames(tabella)[67] <- "EURCHF_MA3"
tabella[68] <- ma(tabella[20],3) #moving average con 3 valori.
colnames(tabella)[68] <- "EURJPY_MA3"
tabella[69] <- ma(tabella[21],3) #moving average con 3 valori.
colnames(tabella)[69] <- "EURUSD_MA3"
tabella[70] <- ma(tabella[22],3) #moving average con 3 valori.
colnames(tabella)[70] <- "GBPUSD_MA3"
tabella[71] <- ma(tabella[23],3) #moving average con 3 valori.
colnames(tabella)[71] <- "NZDUSD_MA3"
tabella[72] <- ma(tabella[24],3) #moving average con 3 valori.
colnames(tabella)[72] <- "USDCAD_MA3"
tabella[73] <- ma(tabella[25],3) #moving average con 3 valori.
colnames(tabella)[73] <- "USDCHF_MA3"
tabella[74] <- ma(tabella[26],3) #moving average con 3 valori.
colnames(tabella)[74] <- "USDJPY_MA3"

#MA con 4 valori
tabella[75] <- ma(tabella[15],4) #moving average con 2 valori.
colnames(tabella)[75] <- "AUDJPY_MA4"
tabella[76] <- ma(tabella[16],4) #moving average con 2 valori.
colnames(tabella)[76] <- "EURGBP_MA4"
tabella[77] <- ma(tabella[17],4) #moving average con 2 valori.
colnames(tabella)[77] <- "AUDUSD_MA4"
tabella[78] <- ma(tabella[18],4) #moving average con 2 valori.
colnames(tabella)[78] <- "EURCAD_MA4"
tabella[79] <- ma(tabella[19],4) #moving average con 2 valori.
colnames(tabella)[79] <- "EURCHF_MA4"
tabella[80] <- ma(tabella[20],4) #moving average con 2 valori.
colnames(tabella)[80] <- "EURJPY_MA4"
tabella[81] <- ma(tabella[21],4) #moving average con 2 valori.
colnames(tabella)[81] <- "EURUSD_MA4"
tabella[82] <- ma(tabella[22],4) #moving average con 2 valori.
colnames(tabella)[82] <- "GBPUSD_MA4"
tabella[83] <- ma(tabella[23],4) #moving average con 2 valori.
colnames(tabella)[83] <- "NZDUSD_MA4"
tabella[84] <- ma(tabella[24],4) #moving average con 2 valori.
colnames(tabella)[84] <- "USDCAD_MA4"
tabella[85] <- ma(tabella[25],4) #moving average con 2 valori.
colnames(tabella)[85] <- "USDCHF_MA4"
tabella[86] <- ma(tabella[26],4) #moving average con 2 valori.
colnames(tabella)[86] <- "USDJPY_MA4"

#ma con 5 valori
tabella[87] <- ma(tabella[15],5) #moving average con 2 valori.
colnames(tabella)[87] <- "AUDJPY_MA5"
tabella[88] <- ma(tabella[16],5) #moving average con 2 valori.
colnames(tabella)[88] <- "EURGBP_MA5"
tabella[89] <- ma(tabella[17],5) #moving average con 2 valori.
colnames(tabella)[89] <- "AUDUSD_MA5"
tabella[90] <- ma(tabella[18],5) #moving average con 2 valori.
colnames(tabella)[90] <- "EURCAD_MA5"
tabella[91] <- ma(tabella[19],5) #moving average con 2 valori.
colnames(tabella)[91] <- "EURCHF_MA5"
tabella[92] <- ma(tabella[20],5) #moving average con 2 valori.
colnames(tabella)[92] <- "EURJPY_MA5"
tabella[93] <- ma(tabella[21],5) #moving average con 2 valori.
colnames(tabella)[93] <- "EURUSD_MA5"
tabella[94] <- ma(tabella[22],5) #moving average con 2 valori.
colnames(tabella)[94] <- "GBPUSD_MA5"
tabella[95] <- ma(tabella[23],5) #moving average con 2 valori.
colnames(tabella)[95] <- "NZDUSD_MA5"
tabella[96] <- ma(tabella[24],5) #moving average con 2 valori.
colnames(tabella)[96] <- "USDCAD_MA5"
tabella[97] <- ma(tabella[25],5) #moving average con 2 valori.
colnames(tabella)[97] <- "USDCHF_MA5"
tabella[98] <- ma(tabella[26],5) #moving average con 2 valori.
colnames(tabella)[98] <- "USDJPY_MA5"


#ho applicato  sulla serie di 5 minuti prima con diversi rolling average.
#Non uso la serie stessa perchè il valore corrente diventerà quello target 
#e non voglio costruire una variabile su questo valore che poi non avro




#HO CREATO UN DATABASE CHIAMATO "TABELLA" CON TUTTE LE VARIABILI E LE MEDIE MOBILI, 
#ORA DEVO APPLICARCI LA RETE NEURALE. 




#posso in futuro includere google trends.
###riduco tabella per usare googe trends
###tabella_reduced <-merge(tabella, googlem_data, by=c("X.DTYYYYMMDD.","X.TIME."))

####RIDUCO DATE perche' e' troppo per la mia rete neurale:
tabellar <- tabella[ which(tabella$DATE >20160701), ]

# 6. How about other NAs, should they not be removed as well?
tabellab <-tabella[ which(tabella$DATE >20160701), ]


# 4. Re-scale the data with min-max normalisation.
normal <- function(x) {
  return((x - min(x, na.rm = T)) / (max(x, na.rm = T) - min(x, na.rm = T)))
}

tabellar <- as.data.frame(lapply(tabellar, normal))

tabella.st <-tabellar[colSums(!is.na(tabellar)) > 0]

# 7. Divide data into training (80%) and testing sets (20%).
set.seed(123)
tabella.rows <- sample(nrow(tabella.st), round(0.8*nrow(tabella.st)))

tabella.train <- tabella.st[tabella.rows,]
tabella.test <- tabella.st[-tabella.rows,]
tabella.train <- na.omit(tabella.train)
tabella.test <- na.omit(tabella.test)
# 8. Train the data using neuralnet() from neuralnet package:
install.packages("neuralnet")
library(neuralnet)
?neuralnet
#######

#CREATA TABELLA. 


#choose the names you want:
names <- c('AUDJPY_5minBefore','EURGBP_5minBefore','AUDUSD_5minBefore','EURCAD_5minBefore','EURCHF_5minBefore','EURJPY_5minBefore','EURUSD_5minBefore','GBPUSD_5minBefore','NZDUSD_5minBefore','USDCAD_5minBefore','USDCHF_5minBefore','AUDJPY_10minBefore','EURGBP_10minBefore','AUDUSD_10minBefore','EURCAD_10minBefore','EURCHF_10minBefore','EURJPY_10minBefore','EURUSD_10minBefore','GBPUSD_10minBefore','NZDUSD_10minBefore','USDCAD_10minBefore','USDCHF_10minBefore','AUDJPY_15minBefore','EURGBP_15minBefore','AUDUSD_15minBefore','EURCAD_15minBefore','EURCHF_15minBefore','EURJPY_15minBefore','EURUSD_15minBefore','GBPUSD_15minBefore','NZDUSD_15minBefore','USDCAD_15minBefore','USDCHF_15minBefore','AUDJPY_MA2','EURGBP_MA2','AUDUSD_MA2','EURCAD_MA2','EURCHF_MA2','EURJPY_MA2','EURUSD_MA2','GBPUSD_MA2','NZDUSD_MA2','USDCAD_MA2','USDCHF_MA2','AUDJPY_MA3','EURGBP_MA3','AUDUSD_MA3','EURCAD_MA3','EURCHF_MA3','EURJPY_MA3','EURUSD_MA3','GBPUSD_MA3','NZDUSD_MA3','USDCAD_MA3','USDCHF_MA3','AUDJPY_MA4','EURGBP_MA4','AUDUSD_MA4','EURCAD_MA4','EURCHF_MA4','EURJPY_MA4','EURUSD_MA4','GBPUSD_MA4','NZDUSD_MA4','USDCAD_MA4','USDCHF_MA4','AUDJPY_MA5','EURGBP_MA5','AUDUSD_MA5','EURCAD_MA5','EURCHF_MA5','EURJPY_MA5','EURUSD_MA5','GBPUSD_MA5','NZDUSD_MA5','USDCAD_MA5','USDCHF_MA5')


#to create the input for the neural network
a <- as.formula(paste('EURGBP ~ ' ,paste(names,collapse='+')))


# 11. Adding extra hidden neurons in each layer e.g. 5, 3, 2:
set.seed(123)
system.time(tabella.model <- neuralnet(a, data = tabella.train, stepmax = 1e7, hidden = c(20, 7, 3), linear.output = TRUE, learningrate = 0.5))

#one hidden layer
summary(tabella.model)

# 9. Plot the network topology on the tabella.model:
plot(tabella.model)

predizione <- compute(tabella.model,tabella.test[14:90])
#predizione real values
predizione.RV <- pr.nn$net.result*(max(tabella$EURGBP)-min(tabella$EURGBP))+min(tabella$EURGBP)


test.RV <- (tabella.test$EURGBP)*(max(tabella$EURGBP)-min(tabella$EURGBP))+min(tabella$EURGBP)

EURGBP_5minBefore.RV <- (tabella.test$EURGBP_5minBefore)*(max(tabella$EURGBP)-min(tabella$EURGBP))+min(tabella$EURGBP)


MSE.nn <- sum((test.RV - predizione.RV )^2)/nrow(tabella.test)
test.RV <-data.frame(test.RV)
plot1 <- predizione.RV
plot(plot1,type='l')
plot2 <- ts(test.RV)


plot(plot1,type='l') 
par(new=T)
plot(plot2,type='l',col='blue')


plot(plot1,plot2, type='l')

risultati <-cbind(predizione.RV, test.RV, EURGBP_5minBefore.RV)
printf <- function(...)print(sprintf(...))

b <- predizione.RV[1,1]
printf("il valore di predizione di EURGBP per i prossimi 5 minuti è  %f", b)

save(tabella.model, file = "D:/Documents/Paola/R/Progetto_Claudio/short_data/ReteNeurale.RData")




-----------------------------------------------------------------------------
# 10. Evaluate model performance by:
# a.) generating predictions on the test dataset:

test<-tabella.test[15:98]

outcomes <- compute(tabella.model, test)
str(outcomes)

predizione <- outcomes$net.result
head(predizione, n=20)
#z score only for normal distribution, 
#with normal isation devo poi trasformare indietro.

# b.) correlating predicted income with the actual income in test dataset:
cor(predizione, tabella.test[9])

plot1 <- predizione
plot(plot1,type='l')
plot2 <- ts(tabella.test[9])


plot(plot1,type='l') 
par(new=T)
plot(plot2,type='l',col='green')


plot(plot1,plot2, type='l')



risultati <-cbind(predizione, tabella.test[9])
 ### ?  risultati <- cbind(risultati,tabella.test[14])

############################
############################
########altra rete neurale##
############################
############################


# 11. Adding extra hidden neurons in each layer e.g. 5, 3, 2:
set.seed(123)
system.time(nilt.model2 <- neuralnet(persinc2 ~ rage + rsex + househld + placeliv + highqual, 
                                     data = nilt.train, stepmax = 1e7, hidden = c(20, 7, 3), 
                                     linear.output = TRUE, learningrate = 0.5))

# 12. Plot the network topology on the nilt.model2:
plot(nilt.model2)

# 13. Evaluate nilt.model2 performance by:
# a.) generating predictions on the test dataset:
outcomes2 <- compute(nilt.model2, nilt.test[2:6])
str(outcomes2)

income.pred2 <- outcomes2$net.result

# b.) correlating predicted income (income.pred2) with the actual income in test dataset:
cor(income.pred2, nilt.test$persinc2)

## the model is still not great, even worse than before, it might be because of
## the distribution of the income variable








