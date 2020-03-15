options(scipen = 999)

#Reading the Data from the CSV file
data = read.csv("//Users//karansehgal//Downloads//CC GENERAL.csv")

# Structure of Dataset
str(data)
#Data saving to another variable to avoid changes in the original data
data1 = data

data1[,"CUST_ID"] = NULL


#Defining User function for Descriptive Statistics
mystats <- function(x) {
  nmiss<-sum(is.na(x))
  a <- x[!is.na(x)]
  m <- mean(a)
  n <- length(a)
  total = n+nmiss
  miss_per = (nmiss/total)*100
  s <- sd(a)
  min <- min(a)
  pctls <-quantile(a,probs=c(0.01,0.1,0.25,0.5,0.75,0.9,0.99))
  pctl_5 = quantile(a,probs = 0.05) 
  pctl_95 = quantile(a,probs = 0.95) 
  max <- max(a)
  uc = pctl_95
  lc = pctl_5
  flag = max>uc|min<lc
  return(c(n=n, nmiss=nmiss,miss_per=miss_per,  mean=m, stdev=s,pctls=pctls,pctl_5=pctl_5, pctl_95=pctl_95,min = min,  max=max,uc = uc,lc=lc,flag = flag))
}

#Finding the Descriptive Statistics for data set having Numeric Variables
stats = apply(data1,2,mystats)
stats = t(stats)
stats = as.data.frame(stats)
View(stats)

write.csv(stats,"stats.csv")


# Defining user defined fuction for outlier treatment
outliers = function(x){
  up = quantile(x,probs= 0.95,na.rm = T)
  down = quantile(x,probs=0.05,na.rm = T) 
  x[x<down] = down
  x[x>up] = up
  x
}

# Treating outliers in the training dataset using the user defined function
data1 = data.frame(apply(data1,2,outliers)) 


#Missing Values Treatment

#Count of missing values column-wise
stats[stats$nmiss>=1,]

#Missing Value Treatment

library(mice)
data1=complete(mice(data1,m=2,method="rf"))

# For Factor Analysis - Correlation Matrix
corrm = cor(data1)

# Scree Plot
library(psych)
scree(corrm,factors = T,pc = T,main = "Scree Plot",hline = NULL,add = F)
eigen(corrm)$values
FA1 = fa(r=corrm,6,rotate = "varimax",fm="ml")
fa_sort1=fa.sort(FA1)
list(fa_sort1)
fat1 = fa_sort1$loadings

Loadings = data.frame(fat1[,])
View(Loadings)

write.csv(Loadings, "loadings1.csv") ### SAVING THE FILE

colnames(data1)

# Important Variables as per Factor Analysis

varimp = c("PURCHASES_INSTALLMENTS_FREQUENCY","PURCHASES_FREQUENCY","INSTALLMENTS_PURCHASES","ONEOFF_PURCHASES","PURCHASES",
           "CASH_ADVANCE_TRX","CASH_ADVANCE_FREQUENCY","CASH_ADVANCE","BALANCE","MINIMUM_PAYMENTS","ONEOFF_PURCHASES_FREQUENCY")

data2 = data1[,varimp]


# Scaling of the Data
data2 = data.frame(scale(data2,scale = T,center = T))


# Using Kmeans for Clustering
cluster_three <- kmeans(data2,3) 
cluster_four <- kmeans(data2,4)
cluster_five <- kmeans(data2,5)
cluster_six <- kmeans(data2,6)


# Adding the clusters with rest of the treated data
data3<-cbind(data1,km_clust_3=cluster_three$cluster,
             km_clust_4=cluster_four$cluster,
             km_clust_5=cluster_five$cluster,
             km_clust_6=cluster_six$cluster   )
str(data3)

#Graph based on k-means - Optional
library(cluster)
clusplot(data2, #dataframe
         cluster_three$cluster, #clusterdata
         color = TRUE, #color
         #shade = TRUE, # Lines in clusters
         lines = 10, # lines connecting centroids
         labels = 1) # Labels clusters and cases



# --------------------------------------  Addition of new variables ---------------------------------------------------#

#Addition of new variables

# Monthly Purchase & Cash Advance
data3$PURCHASE_MONTHLY = data3$PURCHASES/data3$TENURE
data3$CASH_ADVANCE_MONTHLY = data3$CASH_ADVANCE/data3$TENURE

# Purchase by Installments, one off or both
data3$PURCHASE_TYPE_INSTALLMENT = ifelse(data3$INSTALLMENTS_PURCHASES==0,0,1)
data3$PURCHASE_TYPE_ONEOFF = ifelse(data3$ONEOFF_PURCHASES==0,0,1)
data3$PURCHASE_TYPE_BOTH = ifelse(data3$PURCHASE_TYPE_INSTALLMENT==1&data3$PURCHASE_TYPE_ONEOFF==1,1,0)

# Average Amount per purchase, OneOff Purchase, Installments, Cash_Adance
data3$AVG_AMT_PER_PURCHASE = data3$PURCHASES*data3$PURCHASES_FREQUENCY
data3$AVG_AMT_PER_ONEOFF = data3$ONEOFF_PURCHASES*data3$ONEOFF_PURCHASES_FREQUENCY
data3$AVG_AMT_PER_INSTALLMENTS = data3$INSTALLMENTS_PURCHASES*data3$PURCHASES_INSTALLMENTS_FREQUENCY
data3$AVG_AMT_PER_CASHADVANCE = data3$CASH_ADVANCE*data3$CASH_ADVANCE_FREQUENCY

#CREDIT LIMIT
data3$BALNC_CREDITLIMIT = data3$BALANCE/data3$CREDIT_LIMIT

#PAYMENT TO MINIMUM PAYMENTS
data3$PAYMT_MINPAYMT = data3$PAYMENTS/data3$MINIMUM_PAYMENTS
data3 = data.frame(data3)


# ------------------------------------------------ Profiling ---------------------------------------------- #


# Profiling
# Converting clusters into factor variables
data3$km_clust_3=factor(data3$km_clust_3)
data3$km_clust_4=factor(data3$km_clust_4)
data3$km_clust_5=factor(data3$km_clust_5)
data3$km_clust_6=factor(data3$km_clust_6)

# Mean value of different varaibles according to different numbers clusters
library(tables)
profile1<-tabular(1+ BALANCE+BALANCE_FREQUENCY+PURCHASES+ONEOFF_PURCHASES+INSTALLMENTS_PURCHASES+CASH_ADVANCE+PURCHASES_FREQUENCY+
                    CASH_ADVANCE_FREQUENCY+CASH_ADVANCE_TRX+PURCHASES_TRX+CREDIT_LIMIT+PAYMENTS+MINIMUM_PAYMENTS+PRC_FULL_PAYMENT+TENURE+PURCHASE_MONTHLY+                 
                    CASH_ADVANCE_MONTHLY+PURCHASE_TYPE_INSTALLMENT+PURCHASE_TYPE_ONEOFF+PURCHASE_TYPE_BOTH+AVG_AMT_PER_PURCHASE+AVG_AMT_PER_ONEOFF
                  +AVG_AMT_PER_INSTALLMENTS+AVG_AMT_PER_CASHADVANCE+BALNC_CREDITLIMIT+PAYMT_MINPAYMT~ mean +
                    (mean*km_clust_3)+(mean*km_clust_4)+(mean*km_clust_5)+(mean*km_clust_6),
                  data=data3)



profile1<-as.matrix(profile1)
profile1<-data.frame(profile1)
View(profile1)

# Measuring Length of different Clusters
profile2<-tabular(1~length+(length*km_clust_3)+(length*km_clust_4)+(length*km_clust_5)+(length*km_clust_6),
                  data=data3)
profile2<-as.matrix(profile2)
profile2<-data.frame(profile2)


# Converting PURCHASE_TYPE_INSTALLMENT, PURCHASE_TYPE_ONEOFF & PURCHASE_TYPE_BOTH - into factor variables
data3$PURCHASE_TYPE_INSTALLMENT=factor(data3$PURCHASE_TYPE_INSTALLMENT)
data3$PURCHASE_TYPE_ONEOFF=factor(data3$PURCHASE_TYPE_ONEOFF)
data3$PURCHASE_TYPE_BOTH=factor(data3$PURCHASE_TYPE_BOTH)

profile3<-tabular(1+PURCHASE_TYPE_INSTALLMENT+PURCHASE_TYPE_ONEOFF+PURCHASE_TYPE_BOTH~length+(length*km_clust_3)+(length*km_clust_4)+(length*km_clust_5)+(length*km_clust_6),
                  data=data3)

profile3<-as.matrix(profile3)
profile3<-data.frame(profile3)

write.csv(profile1,"profile1.csv",row.names = F)### SAVING THE FILE
write.csv(profile2,"profile2.csv",row.names = F)### SAVING THE FILE
write.csv(profile3,"profile3.csv",row.names = F)### SAVING THE FILE

# --------------------------- End of Clustering Analysis -------------------------------------- #
