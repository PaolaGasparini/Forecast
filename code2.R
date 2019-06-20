
#Script da dare al papà
#Setto cartella dove ci sono i file di tutte le valute e la rete neurale. 

setwd("D:/Documents/Paola/R/Progetto_Claudio/short_data")


#1) importo tutte le valute, in questo case è gia come se fossero i cinque minuti prima, 

install.packages("XLConnect")
library(XLConnect)


EURGBP <- readWorksheet(loadWorkbook("EURGBP.xlsx"),sheet=1)
EURGBPm <- as.matrix(EURGBP[,c(2,3,4)] )
EURGBPm_data  <-data.frame(EURGBPm)

AUDUSD <- readWorksheet(loadWorkbook("AUDUSD.xlsx"),sheet=1)
AUDUSDm <- as.matrix(AUDUSD[,c(2,3,4)] )
AUDUSDm_data  <-data.frame(AUDUSDm)


EURCAD <- readWorksheet(loadWorkbook("EURCAD.xlsx"),sheet=1)
EURCADm <- as.matrix(EURCAD[,c(2,3,4)] )
EURCADm_data  <-data.frame(EURCADm)


EURCHF <- readWorksheet(loadWorkbook("EURCHF.xlsx"),sheet=1)
EURCHFm <- as.matrix(EURCHF[,c(2,3,4)] )
EURCHFm_data  <-data.frame(EURCHFm)


EURJPY <- readWorksheet(loadWorkbook("EURJPY.xlsx"),sheet=1)
EURJPYm <- as.matrix(EURCJPY[,c(2,3,4)] )
EURJPYm_data  <-data.frame(EURJPYm)


EURUSD <- readWorksheet(loadWorkbook("EURUSD.xlsx"),sheet=1)
EURUSDm <- as.matrix(EURCUSD[,c(2,3,4)] )
EURUSDm_data  <-data.frame(EURUSDm)

GBPUSD <- readWorksheet(loadWorkbook("GBPUSD.xlsx"),sheet=1)
GBPUSDm <- as.matrix(GBPUSD[,c(2,3,4)] )
GBPUSDm_data  <-data.frame(GBPUSDm)

NZDUSD <- readWorksheet(loadWorkbook("NZDUSD.xlsx"),sheet=1)
NZDUSDm <- as.matrix(NZDUSD[,c(2,3,4)] )
NZDUSDm_data  <-data.frame(NZDUSDm)


USDCAD<- readWorksheet(loadWorkbook("USDCAD.xlsx"),sheet=1)
USDCADm <- as.matrix(USDCAD[,c(2,3,4)] )
USDCADm_data  <-data.frame(USDCADm)


USDCHF<- readWorksheet(loadWorkbook("USDCHF.xlsx"),sheet=1)
USDCHFm <- as.matrix(USDCHF[,c(2,3,4)] )
USDCHFm_data  <-data.frame(USDCHFm)


USDJPY<- readWorksheet(loadWorkbook("USDJPY.xlsx"),sheet=1)
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



tabella <-p 

#In questo caso le colonne 1-14 sono gia i valori attuali delle valute,
#io voglio predirre il futuro quindi queste faranno anch'esse parte del test set. 

##Il nuovo dataset si chiama tabella e ha un valore ogni 5 minuti piu o meno per tutte le valute. 
#Il prossimo step è metterci i 5, 10 minuti, 15 minuti prima di tutte le valute. 
#usando Column_before_Xmin per tutte le valute.


#creo funzione che calcola i valori di x minuti prima.
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
#tabella[39]<- Column_before_Xmin(tabella[3],15)
#colnames(tabella)[39] <- "AUDJPY_15minBefore"
#tabella[40]<- Column_before_Xmin(tabella[4],15)
#colnames(tabella)[40] <- "EURGBP_15minBefore"
#tabella[41]<- Column_before_Xmin(tabella[5],15)
#colnames(tabella)[41] <- "AUDUSD_15minBefore"
#tabella[42]<- Column_before_Xmin(tabella[6],15)
#colnames(tabella)[42] <- "EURCAD_15minBefore"
#tabella[43]<- Column_before_Xmin(tabella[7],15)
#colnames(tabella)[43] <- "EURCHF_15minBefore"
#tabella[44]<- Column_before_Xmin(tabella[8],15)
#colnames(tabella)[44] <- "EURJPY_15minBefore"
#tabella[45]<- Column_before_Xmin(tabella[9],15)
#colnames(tabella)[45] <- "EURUSD_15minBefore"
#tabella[46]<- Column_before_Xmin(tabella[10],15)
#colnames(tabella)[46] <- "GBPUSD_15minBefore"
#tabella[47]<- Column_before_Xmin(tabella[11],15)
#colnames(tabella)[47] <- "NZDUSD_15minBefore"
#tabella[48]<- Column_before_Xmin(tabella[12],15)
#colnames(tabella)[48] <- "USDCAD_15minBefore"
#tabella[49]<- Column_before_Xmin(tabella[13],15)
#colnames(tabella)[49] <- "USDCHF_15minBefore"
#tabella[50]<- Column_before_Xmin(tabella[14],15)
#colnames(tabella)[50] <- "USDJPY_15minBefore"


#applico moving average a tutte le valute, ma a 2 valori, a 3 valori..fino a 10 valori. 

#Il moving average e' quello dei 5 minuti prima, in questo caso le valute stesse non ce le ho. 

ma <- function(x,n){filter(x,rep(1/n,n), sides=2)}
####
##Moving average con 2 valori
tabella[39] <- ma(tabella[1],2) #moving average con 2 valori.
colnames(tabella)[39] <- "AUDJPY_MA2"
tabella[40] <- ma(tabella[2],2) #moving average con 2 valori.
colnames(tabella)[40] <- "EURGBP_MA2"
tabella[41] <- ma(tabella[3],2) #moving average con 2 valori.
colnames(tabella)[41] <- "AUDUSD_MA2"
tabella[42] <- ma(tabella[4],2) #moving average con 2 valori.
colnames(tabella)[42] <- "EURCAD_MA2"
tabella[43] <- ma(tabella[5],2) #moving average con 2 valori.
colnames(tabella)[43] <- "EURCHF_MA2"
tabella[44] <- ma(tabella[6],2) #moving average con 2 valori.
colnames(tabella)[44] <- "EURJPY_MA2"
tabella[45] <- ma(tabella[7],2) #moving average con 2 valori.
colnames(tabella)[45] <- "EURUSD_MA2"
tabella[46] <- ma(tabella[8],2) #moving average con 2 valori.
colnames(tabella)[46] <- "GBPUSD_MA2"
tabella[47] <- ma(tabella[9],2) #moving average con 2 valori.
colnames(tabella)[47] <- "NZDUSD_MA2"
tabella[48] <- ma(tabella[10],2) #moving average con 2 valori.
colnames(tabella)[48] <- "USDCAD_MA2"
tabella[49] <- ma(tabella[11],2) #moving average con 2 valori.
colnames(tabella)[49] <- "USDCHF_MA2"
tabella[50] <- ma(tabella[12],2) #moving average con 2 valori.
colnames(tabella)[50] <- "USDJPY_MA2"

#MA con 3 valori
tabella[51] <- ma(tabella[1],3) #moving average con 3 valori.
colnames(tabella)[51] <- "AUDJPY_MA3"
tabella[52] <- ma(tabella[2],3) #moving average con 3 valori.
colnames(tabella)[52] <- "EURGBP_MA3"
tabella[53] <- ma(tabella[3],3) #moving average con 3 valori.
colnames(tabella)[53] <- "AUDUSD_MA3"
tabella[54] <- ma(tabella[4],3) #moving average con 3 valori.
colnames(tabella)[54] <- "EURCAD_MA3"
tabella[55] <- ma(tabella[5],3) #moving average con 3 valori.
colnames(tabella)[55] <- "EURCHF_MA3"
tabella[56] <- ma(tabella[6],3) #moving average con 3 valori.
colnames(tabella)[56] <- "EURJPY_MA3"
tabella[57] <- ma(tabella[7],3) #moving average con 3 valori.
colnames(tabella)[57] <- "EURUSD_MA3"
tabella[58] <- ma(tabella[8],3) #moving average con 3 valori.
colnames(tabella)[58] <- "GBPUSD_MA3"
tabella[59] <- ma(tabella[9],3) #moving average con 3 valori.
colnames(tabella)[59] <- "NZDUSD_MA3"
tabella[60] <- ma(tabella[10],3) #moving average con 3 valori.
colnames(tabella)[60] <- "USDCAD_MA3"
tabella[61] <- ma(tabella[11],3) #moving average con 3 valori.
colnames(tabella)[61] <- "USDCHF_MA3"
tabella[62] <- ma(tabella[12],3) #moving average con 3 valori.
colnames(tabella)[62] <- "USDJPY_MA3"

#MA con 4 valori
tabella[63] <- ma(tabella[1],4) #moving average con 2 valori.
colnames(tabella)[63] <- "AUDJPY_MA4"
tabella[64] <- ma(tabella[2],4) #moving average con 2 valori.
colnames(tabella)[64] <- "EURGBP_MA4"
tabella[65] <- ma(tabella[3],4) #moving average con 2 valori.
colnames(tabella)[65] <- "AUDUSD_MA4"
tabella[66] <- ma(tabella[4],4) #moving average con 2 valori.
colnames(tabella)[66] <- "EURCAD_MA4"
tabella[67] <- ma(tabella[5],4) #moving average con 2 valori.
colnames(tabella)[67] <- "EURCHF_MA4"
tabella[68] <- ma(tabella[6],4) #moving average con 2 valori.
colnames(tabella)[68] <- "EURJPY_MA4"
tabella[69] <- ma(tabella[7],4) #moving average con 2 valori.
colnames(tabella)[69] <- "EURUSD_MA4"
tabella[70] <- ma(tabella[8],4) #moving average con 2 valori.
colnames(tabella)[70] <- "GBPUSD_MA4"
tabella[71] <- ma(tabella[9],4) #moving average con 2 valori.
colnames(tabella)[71] <- "NZDUSD_MA4"
tabella[72] <- ma(tabella[10],4) #moving average con 2 valori.
colnames(tabella)[72] <- "USDCAD_MA4"
tabella[73] <- ma(tabella[11],4) #moving average con 2 valori.
colnames(tabella)[73] <- "USDCHF_MA4"
tabella[74] <- ma(tabella[12],4) #moving average con 2 valori.
colnames(tabella)[74] <- "USDJPY_MA4"

#ma con 5 valori
tabella[75] <- ma(tabella[1],5) #moving average con 2 valori.
colnames(tabella)[75] <- "AUDJPY_MA5"
tabella[76] <- ma(tabella[2],5) #moving average con 2 valori.
colnames(tabella)[76] <- "EURGBP_MA5"
tabella[77] <- ma(tabella[3],5) #moving average con 2 valori.
colnames(tabella)[77] <- "AUDUSD_MA5"
tabella[78] <- ma(tabella[4],5) #moving average con 2 valori.
colnames(tabella)[78] <- "EURCAD_MA5"
tabella[79] <- ma(tabella[5],5) #moving average con 2 valori.
colnames(tabella)[79] <- "EURCHF_MA5"
tabella[80] <- ma(tabella[6],5) #moving average con 2 valori.
colnames(tabella)[80] <- "EURJPY_MA5"
tabella[81] <- ma(tabella[7],5) #moving average con 2 valori.
colnames(tabella)[81] <- "EURUSD_MA5"
tabella[82] <- ma(tabella[8],5) #moving average con 2 valori.
colnames(tabella)[82] <- "GBPUSD_MA5"
tabella[83] <- ma(tabella[9],5) #moving average con 2 valori.
colnames(tabella)[83] <- "NZDUSD_MA5"
tabella[84] <- ma(tabella[10],5) #moving average con 2 valori.
colnames(tabella)[84] <- "USDCAD_MA5"
tabella[85] <- ma(tabella[11],5) #moving average con 2 valori.
colnames(tabella)[85] <- "USDCHF_MA5"
tabella[86] <- ma(tabella[12],5) #moving average con 2 valori.
colnames(tabella)[86] <- "USDJPY_MA5"

tabella$USDJPY <- NULL
tabella$USDJPY_5minBefore <- NULL
tabella$USDJPY_10minBefore <- NULL

tabella$USDJPY_MA2 <- NULL
tabella$USDJPY_MA3 <- NULL
tabella$USDJPY_MA4 <- NULL
tabella$USDJPY_MA5 <- NULL

#ho applicato  sulla serie di 5 minuti prima con diversi rolling average.
#Non uso la serie stessa perchè il valore corrente diventerà quello target 
#e non voglio costruire una variabile su questo valore che poi non avro


#HO CREATO UN DATABASE CHIAMATO "TABELLA" CON TUTTE LE VARIABILI E LE MEDIE MOBILI, 
#ORA DEVO APPLICARCI LA RETE NEURALE. 

#il test set sono tutte le variabili che ho creato. Ho eliminato 15min before perchè 
#nel training set avevo meno variabili. 
#Devo eliminare 

test<-tabella[3:86]

outcomes <- compute(tabella.model, test)
#calcolata la predizione di tutte le righe che ci sono nel test set,
#in realtà a me serve solo la prima riga. 
str(outcomes)

predizione <- outcomes$net.result



head(predizione, n=20)

#funzione per stampare:
printf <- function(...)print(sprintf(...))

b <- predizione[1,]


printf("il valore di predizione di EURGBP per i prossimi 5 minuti è  %f", b)

