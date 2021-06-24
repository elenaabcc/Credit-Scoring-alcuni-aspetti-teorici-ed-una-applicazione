rm(list=ls(all=T))  

load("germandataset.RData")

#splittare il dataframe in base alla variabile risposta.
Splitdatacredit= split(dedatacredit, dedatacredit$responsegoodcredit)
splitted_good=Splitdatacredit$"1" #'data.frame':	300 obs. of  21 variables
splitted_bad=Splitdatacredit$"0" #'data.frame':	700 obs. of  21 variables

library(epiDisplay)

#Studio della variabile sex:
  #tabella di frequenza
tab1(dedatacredit$sex ,cum.percent = TRUE)
  #tabella di contingenza
table(dedatacredit$responsegoodcredit ,   dedatacredit$sex)

#-------------------------------------------------------------------------------

#Studio della variabile age:
  #tabella di frequenza
tab1(dedatacredit$age ,cum.percent = TRUE)
      # Moda
getm.ode <- function(e) {
  uniqe <- unique(e)
  uniqe[which.max(tabulate(match(e, uniqe)))]
}
      #Indice di assimmetrica
asymetric_index1= function(x) { 
  ((mean(x) - getm.ode(x))/var(x))}

asymetric_index1(dedatacredit$age)

  #Migliore visualizzazione della distribuzione di frequenza
xAge=dedatacredit$age
propCurve=hist(xAge, col = "#ffd870", xlab = "Età", main = "Curva di distribuzione perla variabile Age")
xfit_Age=seq(min(xAge), max(xAge), length= 40)
yfit_Age=dnorm(xfit_Age,mean=mean(xAge),sd=sd(xAge))
yfit_Age <- yfit_Age*diff(propCurve$mids[1:2])*length(xAge)
lines(xfit_Age, yfit_Age, col="black", lwd=6)

  #tabella di contingenza
table(dedatacredit$age ,   dedatacredit$responsegoodcredit)

  # Tabella di frequenza per i dataset splittati in bad e good
tab1(splitted_bad$age ,cum.percent = TRUE)
tab1(splitted_good$age ,cum.percent = TRUE)

#-------------------------------------------------------------------------------

#Studio della variabile duration:
tab1(dedatacredit$duration ,cum.percent = TRUE)
  #Indice di assimetria
asymetric_index1(dedatacredit$duration)

  #Migliore visualizzazione della distribuzione di frequenza
xduration=dedatacredit$duration
propCurve=hist(xduration, col = "#ffd870", xlab = "Durata del credito", main = "Curva di distribuzione perla variabile Duration")
xfit_duration=seq(min(xduration), max(xduration), length= 40)
yfit_duration=dnorm(xfit_duration,mean=mean(xduration),sd=sd(xduration))
yfit_duration <- yfit_duration*diff(propCurve$mids[1:2])*length(xduration)
lines(xfit_duration, yfit_duration, col="black", lwd=6)


  #tabella di contingenza
table(dedatacredit$duration ,   dedatacredit$responsegoodcredit)

  # Tabella di frequenza per i dataset splittati in bad e good
tab1(splitted_bad$duration ,cum.percent = TRUE)
tab1(splitted_good$duration ,cum.percent = TRUE)

  # Indice di assimetria per i due dataset
asymetric_index1(splitted_bad$duration)
asymetric_index1(splitted_good$duration)

#-------------------------------------------------------------------------------

#Studio della variabile duration:
  #tabella di frequenza
tab1(dedatacredit$purpose ,cum.percent = TRUE)
  #tabella di contingenza
table(dedatacredit$purpose ,   dedatacredit$responsegoodcredit)


#-------------------------------------------------------------------------------

#Studio di più variabili contemporaneamente:

    #Caricamento del dataset in versione numerica
numeric_dedatacredit= read.table("http://archive.ics.uci.edu/ml/machine-learning-databases/statlog/german/german.data-numeric")

colnames(numeric_dedatacredit) = c("account_status", "duration", "credit_history", "purpose", "creditamount", "saving_account", "present_employmentsince", "InstallmentRate", "sex", "other_debtor", 
                                   "present_residencesince", "property", "age", "other_installplans", "housing", "numexisting_credits", 
                                   "job", "numpeople_maintenance", "telephone", "foreign_worker", "responsegoodcredit")

numeric_dedatacredit=numeric_dedatacredit[-rev(seq_len(ncol(numeric_dedatacredit)))-21]

numSplitdatacredit= split(numeric_dedatacredit, numeric_dedatacredit$responsegoodcredit)
numusplitted_bad=numSplitdatacredit$"0" 
numsplitted_good=numSplitdatacredit$"1" 

  #Matrice di correlazione
cormatrix= cor(numeric_dedatacredit)
round(cormatrix,3)

  #correlogram
library(corrplot)
library(RColorBrewer)

corrplot(cormatrix, type="upper", order="hclust",
         col=brewer.pal(n=8, name="PuOr"))

#tabella contingeza varaibile credit history e property
table(dedatacredit$credit_history ,   dedatacredit$property)

