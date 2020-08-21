rm(list=ls()) # clear the memory
setwd("C:/Users/angel/Desktop/Project 2")
library("reshape")
library("plm")
library("rpart")
library("zoo")
library("plyr")
library("dplyr")
library("stringr")
library("reshape2")
library("ggplot2")
library("pander")
library("DataCombine")
library("plm")
library("quantmod")
library(cluster)
library(readr)
#install.packages("Rtsne")
library(Rtsne)

# Import the mortgage data:
load("Project 2.Rda")

# Rename the data (matter of preference):
df <- p.mort.dat.annual
rm(p.mort.dat.annual)

df <- pdata.frame(df, index=c("LOAN_ID","year"),
                  stringsAsFactors = F)

# Print the class of the variable:
class(df) 

# 2. Replace NUM_UNIT with MULTI_UNIT dummy:
table(df$NUM_UNIT)
df$MULTI_UN <- 0
tmp <- which(df$NUM_UNIT > 1)
df$MULTI_UN[tmp] <- 1

# 3. Count the number of loans:
print(length(unique(df$LOAN_ID)))

# Compress the data to single loans:
df.annual <-df %>% 
  group_by(LOAN_ID) %>%
  #mutate(def.max = max(def)) %>%
  #dplyr::mutate(n = row_number()) %>%
  mutate(n = row_number()) %>%
  ungroup()

# Print the variable names in df.annual
names(df.annual)

# keep one obs per loan:
tmp <- which(df.annual$n == 1)
df.annual <- df.annual[tmp,]
dim(df.annual)

#sample data
set.seed(123)
index <- sample(1:nrow(df.annual), 10000)
df.annual <- df.annual[index,]

#-------------data cleaning and feature enginnering------------------

#if last upb = NA, it should be replaced by the original amount of the loan
#df.annual$LAST_UPB <- ifelse(is.na(df.annual$LAST_UPB), df.annual$ORIG_AMT, df.annual$LAST_UPB)

#drop CSCORE_B null values
df.annual <- df.annual[complete.cases(df.annual$CSCORE_B),]

#change NUM_BO null values to 1
df.annual$NUM_BO[df.annual$NUM_BO == ""] <- NA
df.annual$NUM_BO[is.na(df.annual$NUM_BO)] <- 1

#replace DTI null value with 45
df.annual$DTI[is.na(df.annual$DTI)] <- 45

#Drop FTHB_FLG unknown values (only 5)
table(df.annual$FTHB_FLG)
df.annual <- df.annual[!(df.annual$FTHB_FLG == "U"),]

#Drop loan purpose unknown values (only 2)
table(df.annual$PURPOSE)
df.annual <- df.annual[!(df.annual$PURPOSE == "U"),]

library(lubridate)
#calculate maturity date - original date
maturity_date <- mdy(df.annual$Maturity.Date)
original_date <- mdy(df.annual$ORIG_DTE)
loan_days <- maturity_date - original_date
loan_month <- round(loan_days/30,0)
df.annual <- cbind(df.annual, loan_month)

#calculate the day difference between the original loan date and first date
first_date <- mdy(df.annual$FRST_DTE)
first_month <- round((first_date - original_date)/30,0)
df.annual <- cbind(df.annual, first_month)


#log money related features
df.annual$ORIG_AMT <- log(df.annual$ORIG_AMT)
#df.annual$LAST_UPB<- log(df.annual$LAST_UPB)
#df.annual$ORIG_VAL <- log(df.annual$ORIG_VAL)
#df.annual$FCE_UPB <- log(df.annual$FCE_UPB)
#df.annual$Fin_UPB <- log(df.annual$Fin_UPB)
#df.annual$F180_UPB <- log(df.annual$F180_UPB)
#df.annual[df.annual$Fin_UPB == "-Inf"] <- 0

#select columns to drop
drop.vars <- c("V1","ORIG_VAL","FCE_UPB","Fin_UPB","F180_UPB","LOAN_ID","MI_PCT","CSCORE_C","MI_TYPE","LPI_DTE","DISP_DT","FCC_DTE","FCC_COST", "PP_COST",
               "AR_COST","IE_COST","TAX_COST","NS_PROCS","CE_PROCS","RMW_PROCS","O_PROCS","NON_INT_UPB",
               "REPCH_FLAG","PRIN_FORG_UPB","modfg_cost", "Maturity.Date", "ORIG_DTE","FRST_DTE",
               "C_modir_cost","C_modfb_cost","lpi2disp","zb2disp","INT_COST","total_expense","total_proceeds",
               "NET_LOSS","NET_SEV","Total_Cost","Tot_Procs","Tot_Liq_Ex","FMOD_DTE","FMOD_UPB",
               "DispYr","MODIR_COST","MODFB_COST","MODTOT_COST", "Monthly.Rpt.Prd", "LAST_DTE", "date", "VinYr",
               "ActYr", "year","n", "MULTI_UN", "Seller.Name", "Servicer.Name", "STATE", "MOD_FLAG", "Product.Type", 
               "Delq.Status","ZB_DTE", "Zero.Bal.Code","TRANSFER_FLAG","Count","LAST_STAT", "FCE_DTE", "F180_DTE",
               "ZIP_3","LAST_UPB","RELOCATION_FLG","Months.To.Legal.Mat","MSA","MODTRM_CHNG", "MODUPB_CHNG", "n.obs","n.year",
               "n.year.max")

df.model <- df.annual[ , !(names(df.annual) %in% drop.vars)]

#change character to numeric
df.model$NUM_BO <- as.numeric(df.model$NUM_BO)
df.model$NUM_UNIT <- as.numeric(df.model$NUM_UNIT)
df.model$loan_month <- as.numeric(df.model$loan_month)
df.model$first_month <- as.numeric(df.model$first_month)
df.model$ORIG_TRM <- as.numeric(df.model$ORIG_TRM)
df.model$Loan.Age <- df.model$Loan.Age / 30

#num_cols <- c("ORIG_CHN", "FTHB_FLG", "PURPOSE","PROP_TYP", 
#              "OCC_STAT", "RELOCATION_FLG", "TRANSFER_FLAG",
#              "ZIP_3", "V1", "MODTRM_CHNG", "MODUPB_CHNG",
#              "Months.To.Legal.Mat", "n.year", "MSA", "Delq.Status",
#              "ZB_DTE", "FCE_DTE", "F180_DTE")

#df.model.num <- df.model[ , !(names(df.model) %in% num_cols)]
#df.model.num.norm <- scale(df.model.num)

library(fastDummies)

df.model.dum <- dummy_cols(df.model, select_columns = c("ORIG_CHN", "FTHB_FLG", "PURPOSE","PROP_TYP", 
                                                        "OCC_STAT"))


cat_cols <- c("ORIG_CHN", "FTHB_FLG", "PURPOSE","PROP_TYP", "OCC_STAT")


#drop remaining categorical features
df.model.all <- df.model.dum[ , !(names(df.model.dum) %in% cat_cols)]
#only numeric features
df.model.num <- df.model[, !(names(df.model) %in% cat_cols)]

#check if there are still characters
str(df.model.all)

#-----------------------------PCA--------------------------------------------------
df.model.pca <- prcomp(df.model.num, scale. = T)
summary(df.model.pca)
df.model.pca$rotation[,1:15]

pcaCharts <- function(x) {
  x.var <- x$sdev ^ 2
  x.pvar <- x.var/sum(x.var)
  print("proportions of variance:")
  print(x.pvar)
  
  par(mfrow=c(2,2))
  plot(x.pvar,xlab="Principal component", ylab="Proportion of variance explained", ylim=c(0,1), type='b')
  plot(cumsum(x.pvar),xlab="Principal component", ylab="Cumulative Proportion of variance explained", ylim=c(0,1), type='b')
  screeplot(x)
  screeplot(x,type="l")
  par(mfrow=c(1,1))
}

par(mar=c(5,5,5,5))
pcaCharts(df.model.pca)


#install.packages("FactoMineR")
library(FactoMineR)

pca.graph <- PCA(df.model.num, scale.unit = TRUE, ncp = 7, ind.sup = NULL, 
                 quanti.sup = NULL, quali.sup = NULL, row.w = NULL, 
                 col.w = NULL, graph = TRUE, axes = c(1,2))


#get the pca transfromed data based on the summary from PCA (10 components)
pca_out=prcomp(df.model.num[,1:7], scale. = T)
df.pca <- pca_out$x


#-----------------------------k-means-----------------------------------------------
# choose k means k
set.seed(123)
wss <- (nrow(df.pca)-1)*sum(apply(df.pca,2,var))
for (i in 2:20) wss[i] <- sum(kmeans(df.pca,
                                     centers=i)$withinss)
par(mar=c(5,5,5,5))
plot(1:20, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares")

#kmeans

km <- kmeans(df.pca,5)
km$centers

par(mar=c(3,3,3,3))
# plot profile plot of centroids
# plot an empty scatter plot
plot(c(0), xaxt = 'n', ylab = "", type = "l", 
     ylim = c(min(km$centers), max(km$centers)), xlim = c(0, 7))
# label x-axes
axis(1, at = c(1:7), labels = c('1','2','3','4','5','6','7'))
# plot centroids
for (i in c(1:5))
  lines(km$centers[i,], lty = i, lwd = 2, col = ifelse(i %in% c(1,3,5), "black", "grey" ))                                                     
# name clusters
text(x = 0.5, y = km$centers[, 1], labels = paste("Cluster", c(1:5)))

library("factoextra")
fviz_cluster(km, df.pca, stand = FALSE, frame = FALSE, geom = "point")

library(rgl)
library(scatterplot3d)
colors.km <- c("blue", "black", "green","red","purple","yellow","grey")
colors.km <- colors.km[as.numeric(km$cluster)]
plot3d(df.pca[,1],df.pca[,2],df.pca[,3],col = colors.km)

scatterplot3d(df.pca[,1],df.pca[,2],df.pca[,3], color = colors.km, angle = 90)
scatterplot3d(df.pca[,1],df.pca[,2],df.pca[,3], color = colors.km, angle = 180)
#-------------------Use numeric data to run dbscan-------------------------
#install.packages("dbscan")
library(dbscan)
kNNdistplot(df.pca, k = 5)

#take eps = 1.1 from the knndistplot

cl <- dbscan(df.pca, eps = 1.1, minPts = 5)

#install.packages("factoextra")
library("factoextra")
fviz_cluster(cl, df.pca, stand = FALSE, frame = FALSE, geom = "point")
#install.packages("rgl")

table(cl$cluster)

plot3d(df.pca[,1],df.pca[,2],df.pca[,3],col = c("grey","dark green","yellow"))
#--------------------------mean shift------------------------------------------------
#install.packages("meanShiftR")
#library(meanShiftR)
#ms <- meanShift(df.pca, trainData = df.pca,
#                algorithm = "linear", kernelType = "NORMAL", alpha = 0, 
#                iterations = 10, epsilon = 1e-08,
#                epsilonCluster = 1e-04, parameters = NULL)

#iter <- 1000
#h <- c(0.5,0.5,0.5,0.5,0.5,0.5,0.5)

result <- meanShift(
  df.pca,
  df.pca,
  bandwidth=h,
  alpha=0,
  iterations = iter
)
meanShiftR_runtime <- (proc.time()-run.time)[3]
max(result$assignment) #80 clusters

#-----------------------------GMM----------------------------------------------------
#install.packages("ClusterR")
#install.packages("matrixStats")
#install.packages("randomcoloR")
library("ClusterR")
library("matrixStats")
library("FIACH")
library("randomcoloR")
library("mixtools")
library("plotGMM")
em <- GMM(df.pca,gaussian_comps = 10, km_iter=10, em_iter=10)

pr.em <- predict_GMM(df.pca,em$centroids,em$covariance_matrices,em$weights)

set.seed(123)
mixmdl <- mixtools::normalmixEM(faithful$waiting, k = 4)
plot_GMM(mixmdl, 4)
#-------------both categorical and numeric: Gower Distance---------------------------
#Compute Gower distance
gower_dist <- daisy(df.model.all, metric = "gower")

sil_width <- c(NA)
for(i in 2:8){  
  pam_fit <- pam(gower_dist, diss = TRUE, k = i)  
  sil_width[i] <- pam_fit$silinfo$avg.width  
}

plot(1:8, sil_width,
     xlab = "Number of clusters",
     ylab = "Silhouette Width")
lines(1:8, sil_width)

k <- 7
pam_fit <- pam(gower_dist, diss = TRUE, k)
pam_results <- df.model %>%
  mutate(cluster = pam_fit$clustering) %>%
  group_by(cluster) %>%
  do(the_summary = summary(.))
pam_results$the_summary

tsne_obj <- Rtsne(gower_dist, is_distance = TRUE)
tsne_data <- tsne_obj$Y %>%
  data.frame() %>%
  setNames(c("X", "Y")) %>%
  mutate(cluster = factor(pam_fit$clustering))
ggplot(aes(x = X, y = Y), data = tsne_data) + geom_point(aes(color = cluster))
