require(ISLR)
glm_fit = glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume, data=Smarket, family="binomial")

score_pred = predict(glm_fit, newdata=Smarket, type="link") 	### !!! type="link" gives predictions on the scale of the predictors
prob_pred = predict(glm_fit, newdata=Smarket, type="response")  ### !!! type="response" gives predictions on the scale of the response

exp(score_pred)/(1 + exp(score_pred)) == prob_pred

### a function to generate ROC-related quantities
myROC = function(prob, y, n=100)
{
	thresh_seq = seq(from=0, to=1, length=n)
	fp_seq = rep(NA, n)
	tp_seq = rep(NA, n)
	for(i in 1:n)
	{
		thresh = thresh_seq[i]
		fp_seq[i] = sum((y==0) & (prob > thresh))/sum(y==0)
		tp_seq[i] = sum((y==1) & (prob > thresh))/sum(y==1)
	}
	auc = round((sum(diff(fp_seq) * tp_seq[-1]) + sum(diff(fp_seq) * tp_seq[-n]))*(-0.5), digits=2)
	list(fp=fp_seq, tp=tp_seq, auc=auc)
}

roc = myROC(prob=prob_pred, y=ifelse(Smarket$Direction=="Up",1, 0), n=1000)
plot(roc$fp, roc$tp, type="l", xlim=c(0,1), ylim=c(0,1), xlab="false positive", ylab="true positive", main=paste("AUC=",roc$auc,sep=""))
abline(a=0,b=1,lty=2)

train = Smarket$Year < 2005
glm_fit_train = glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume, data=Smarket[train,], family="binomial")
prob_pred = predict(glm_fit_train, newdata=Smarket, type="response")

roc_train = myROC(prob=prob_pred[train], y=ifelse(Smarket$Direction=="Up",1, 0)[train], n=1000)
roc_test = myROC(prob=prob_pred[!train], y=ifelse(Smarket$Direction=="Up",1, 0)[!train], n=1000)
plot(roc_train$fp, roc_train$tp, type="l", xlim=c(0,1), ylim=c(0,1), xlab="false positive", ylab="true positive", col="red")
lines(roc_test$fp, roc_test$tp, col="blue")
legend("topleft",c("train","test"),col=c("red","blue"),lty=1)
abline(a=0,b=1,lty=2)

table(ifelse(prob_pred[train] > 0.5, "Up", "Down"), Smarket$Direction[train])
table(ifelse(prob_pred[!train] > 0.5, "Up", "Down"), Smarket$Direction[!train])