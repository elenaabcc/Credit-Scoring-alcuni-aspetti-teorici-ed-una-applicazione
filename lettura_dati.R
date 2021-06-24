rm(list=ls(all=T))  

dedatacredit = read.table("http://archive.ics.uci.edu/ml/machine-learning-databases/statlog/german/german.data") 


colnames(dedatacredit) = c("account_status", "duration", "credit_history", "purpose", "creditamount", "saving_account", "present_employmentsince", "InstallmentRate", "sex", "other_debtor", 
                           "present_residencesince", "property", "age", "other_installplans", "housing", "numexisting_credits", 
                           "job", "numpeople_maintenance", "telephone", "foreign_worker", "responsegoodcredit")

dedatacredit$responsegoodcredit = dedatacredit$responsegoodcredit - 1 
dedatacredit$responsegoodcredit <- as.factor(dedatacredit$responsegoodcredit)

Splitdatacredit= split(dedatacredit, dedatacredit$responsegoodcredit)
splitted_good=Splitdatacredit$"1" #'data.frame':	300 obs. of  21 variables
splitted_bad=Splitdatacredit$"0" #'data.frame':	700 obs. of  21 variables

# train test
set.seed(400)
test_index <- sample(nrow(dedatacredit),nrow(dedatacredit)*0.70)
datatrain = dedatacredit[test_index,]
datatest = dedatacredit[-test_index,]
# saved 
save.image(file = "germandataset.RData")



