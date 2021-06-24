rm(list=ls(all=T))  


load("germandataset.RData")


# MODELLO COMPLETO (Con tutte le variabili)
myglm=glm(responsegoodcredit~.,data=datatrain,family = "binomial")
summary(myglm)

  # Matrice confusione.
cutoff= 0.5 
prob_myglm=predict(myglm, type = "response")
predicted_myglm=prob_myglm>cutoff
predicted_myglm=as.numeric(predicted_myglm)

confusion_table=table(datatrain$responsegoodcredit, predicted_myglm, dnn = c("Truth", "Predicted"))
confusion_matrix=as.matrix(confusion_table)
confusion_matrix

  # Alcuni indici
sensitivity=  confusion_matrix[1,1]/(confusion_matrix[1,1]+confusion_matrix[2,1]) 
print(sensitivity )

Specificity = confusion_matrix[2,2]/(confusion_matrix[2,2]+confusion_matrix[1,2]) 
print(Specificity)

precision = confusion_matrix[1,1]/(confusion_matrix[1,1]+confusion_matrix[1,2]) 
print(precision)

accuracy =(confusion_matrix[1,1]+confusion_matrix[2,2])/(confusion_matrix[1,1]+confusion_matrix[1,2]+confusion_matrix[2,1]+confusion_matrix[2,2]) 
print(accuracy) 


  # ROC curve e la sua area(AUC)

library(ROCR)
pred_myglm=predict(myglm,  type = "response")
pred_myglm=prediction(pred_myglm, datatrain$responsegoodcredit)


ROC= performance(pred_myglm,"tpr","fpr")
plot(ROC, colorize = T, lwd = 2)
abline(a = 0, b = 1) 


auc = performance(pred_myglm, measure = "auc")
print(auc@y.values)



#SCEGLIERE LE VARIABILI SIGNIFICATIVE:


  #Metodo 1 : Backward stepwise selection
n = length(resid(myglm))
myglm_step=step(myglm, direction = "backward")




myglm_backward=glm(responsegoodcredit ~ account_status + duration + credit_history + 
                other_debtor + purpose + saving_account + sex + InstallmentRate + 
                creditamount + telephone + foreign_worker + other_installplans,
              data=datatrain,family = "binomial")
summary(myglm_backward)

  # matrice confusione.
cutoff= 0.5 
prob_myglm_def=predict(myglm_backward, type = "response")
predicted_myglm_def=prob_myglm_def>cutoff
predicted_myglm_def=as.numeric(predicted_myglm_def)
confusion_table=table(datatrain$responsegoodcredit, predicted_myglm_def, dnn = c("Truth", "Predicted"))
confusion_matrix=as.matrix(confusion_table)
confusion_matrix

  # Alcuni indici

sensitivity=  confusion_matrix[1,1]/(confusion_matrix[1,1]+confusion_matrix[2,1])  
print(sensitivity )

Specificity = confusion_matrix[2,2]/(confusion_matrix[2,2]+confusion_matrix[1,2]) 
print(Specificity)

precision = confusion_matrix[1,1]/(confusion_matrix[1,1]+confusion_matrix[1,2]) 
print(precision)

accuracy =(confusion_matrix[1,1]+confusion_matrix[2,2])/(confusion_matrix[1,1]+confusion_matrix[1,2]+confusion_matrix[2,1]+confusion_matrix[2,2]) # [1] 0.7885714
print(accuracy) 


  # ROC curve e la sua area(AUC)

library(ROCR)
pred_myglm=predict(myglm_backward,  type = "response")
pred_myglm=prediction(pred_myglm, datatrain$responsegoodcredit)


ROC= performance(pred_myglm,"tpr","fpr")
plot(ROC, colorize = T, lwd = 2)
abline(a = 0, b = 1) 


auc = performance(pred_myglm, measure = "auc")
print(auc@y.values)



#metodo 2:  forward stepwiser regression

nullModel=glm(responsegoodcredit ~ 1, data=datatrain,family = "binomial") # creiamo il modello nullo
summary(nullModel)

myglm_Forward=step(nullModel,scope=list(lower=nullModel, upper=myglm), k = 2, direction="forward")




myglm_Forward= glm(responsegoodcredit~ account_status + duration + credit_history + 
                     other_debtor + purpose + saving_account + sex + InstallmentRate + 
                     creditamount + telephone + foreign_worker + other_installplans ,data=datatrain,family = "binomial")


summary(myglm_Forward)


cutoff= 0.5 
prob_myglmforward=predict(myglm_Forward, type = "response")
predicted_myglmforward=prob_myglmforward>cutoff
predicted_myglmforward=as.numeric(predicted_myglmforward)

confusion_table=table(datatrain$responsegoodcredit, predicted_myglmforward, dnn = c("Truth", "Predicted"))
confusion_matrix=as.matrix(confusion_table)
confusion_matrix

sensitivity=  confusion_matrix[1,1]/(confusion_matrix[1,1]+confusion_matrix[2,1])  
print(sensitivity )

Specificity = confusion_matrix[2,2]/(confusion_matrix[2,2]+confusion_matrix[1,2]) 
print(Specificity)

precision = confusion_matrix[1,1]/(confusion_matrix[1,1]+confusion_matrix[1,2]) 
print(precision)

accuracy =(confusion_matrix[1,1]+confusion_matrix[2,2])/(confusion_matrix[1,1]+confusion_matrix[1,2]+confusion_matrix[2,1]+confusion_matrix[2,2]) 
print(accuracy) 


# ROC curve e la sua area(AUC)

library(ROCR)
pred_myglm=predict(myglm_Forward,  type = "response")
pred_myglm=prediction(pred_myglm, datatrain$responsegoodcredit)


ROC= performance(pred_myglm,"tpr","fpr")
plot(ROC, colorize = T, lwd = 2)
abline(a = 0, b = 1) 


auc = performance(pred_myglm, measure = "auc")
print(auc@y.values)

