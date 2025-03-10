library(MixGHD)
library(MASS)
library(caret)
library(klaR)
library(dplyr)
library(GGally)
library(stringr)
library(cluster)
library(e1071)



fire <- read.csv("C:/Users/yunxi/OneDrive/Desktop/Data_Group_9.csv")
head(fire)

#********************* EDA & Data preparation *******************************
#Check for missing values
sum(is.na(fire))

fire$Classes <- str_trim(fire$Classes, side="both")
unique(fire$Classes)

fire_subset <- fire[-c(1,2,3,13,15)] # Remove "day", "month", "year", "FWI", "Region"
head(fire_subset)

fire_subset[, -c(10)] <- scale(fire_subset[, -c(10)]) #scale data
fire_subset$Classes <- factor(fire_subset$Classes)
head(fire_subset)
# Pair plot - with categorical variables removed
#fire_subset2 <- fire_subset[,-10]  # Remove "day", "month", "year","Classes" "Region"
ggpairs(fire_subset, aes(colour=as.factor(Classes), alpha=0.4),
        lower=list(continuous="points"), axisLabels="none", switch="both")

# Pie chart
data_pie <- fire %>% 
  group_by(Classes) %>% 
  count() %>% 
  ungroup() %>% 
  mutate(per = n / sum(n)) %>% 
  arrange(desc(Classes))
data_pie$label <- scales::percent(data_pie$per)
ggplot(data=data_pie) +
  geom_bar(aes(x="", y=per, fill=Classes), stat="identity", width=1) +
  coord_polar("y", start=0) + scale_fill_brewer("Classes") +
  theme_void() +
  geom_text(aes(x=1, y = cumsum(per) - per/2, label=label))

# Split data train/test
set.seed(1234)
train.index <-  createDataPartition(fire_subset$Classes,times=1,p=0.70,list=FALSE)
train <- fire_subset[train.index, ]
test  <- fire_subset[-train.index, ]


#********************* Hierarchical Clustering  *******************************
#Single Linkage Hierarchical Clustering:
set.seed(1234)
fire_numeric<-fire[sapply(fire, is.numeric)]
hc1 <- hclust(dist(fire_subset[, -c(10)]), method = "single")
plot(hc1, main = "Algerian forest fires dataset - Single Linkage", xlab = "", sub = "", cex = 0.5)
l1 <- cutree(hc1,2)
single1 <- table(fire[,'Classes'],l1)
classAgreement(single1)$crand #ARI is 0.005081677


#Complete Linkage Hierarchical Clustering:
set.seed(1234)
hc2<-hclust(dist(fire_subset[, -c(10)]), "complete") 
plot(hc2,main="Algerian forest fires dataset--Complete Linkage", xlab="", sub="", cex=.5)
l2 <- cutree(hc2,2)
single2 <- table(fire[,'Classes'],l2)
classAgreement(single2)$crand # ARI is 0.280948

#Average Linkage Clustering:
set.seed(1234)
hc3 <- hclust(dist(fire_subset[, -c(10)]), "average")
plot(hc3,main="Algerian forest fires dataset -- Average Linkage", xlab="", sub="", cex=.5)
l3 <- cutree(hc3,2)
single3 <- table(fire[,'Classes'],l3)
classAgreement(single3)$crand #ARI is 0.02707615

#Ward.D2
set.seed(1234)
D2 <- hclust(dist(fire_subset[, -c(10)]), "ward.D2") 
plot(D2, main="Cluster Dendrogram", xlab="", ylab="Height", sub="")
l4 <- cutree(D2,2)
single4 <- table(fire[,'Classes'],l4)
classAgreement(single4)$crand #ARI is 0.3939026

#********************* k-Medoids  *******************************
fire_subset
set.seed(1234)
fire_kmedoids<-pam(fire_subset[,-10],2)
fire_kmedoids$clustering
fire_kmedoids_2<-table(fire[,"Classes"],fire_kmedoids$clustering)
km_si2 <- silhouette(fire_kmedoids$clustering, dist(fire_subset[,-10]))
plot(km_si2, nmax= 80, cex.names=0.6, col=c("pink", "skyblue"), main = "")

set.seed(1234)
fire_kmedoids<-pam(fire_subset[,-10],3)
fire_kmedoids$clustering
fire_kmedoids_3<-table(fire[,"Classes"],fire_kmedoids$clustering)
km_si3 <- silhouette(fire_kmedoids$clustering, dist(fire_subset[,-10]))
plot(km_si3, nmax= 80, cex.names=0.6, col=c("pink", "skyblue","green"), main = "")

set.seed(1234)
fire_kmedoids<-pam(fire_subset[,-10],4)
fire_kmedoids$clustering
fire_kmedoids_4<-table(fire[,"Classes"],fire_kmedoids$clustering)
km_si4 <- silhouette(fire_kmedoids$clustering, dist(fire_subset[,-10]))
plot(km_si4, nmax= 80, cex.names=0.6, col=c("pink", "skyblue","green","yellow"), main = "")

set.seed(1234)
fire_kmedoids<-pam(fire_subset[,-10],5)
fire_kmedoids$clustering
fire_kmedoids_5<-table(fire[,"Classes"],fire_kmedoids$clustering)
km_si5 <- silhouette(fire_kmedoids$clustering, dist(fire_subset[,-10]))
plot(km_si5, nmax= 80, cex.names=0.6, col=c("pink", "skyblue","green","yellow","gray"), main = "")

par(mfrow=c(1,4))
plot(km_si2, nmax= 80, cex.names=0.6, col=c("pink", "skyblue"), main = "")
plot(km_si3, nmax= 80, cex.names=0.6, col=c("pink", "skyblue","green"), main = "")
plot(km_si4, nmax= 80, cex.names=0.6, col=c("pink", "skyblue","green","yellow"), main = "")
plot(km_si5, nmax= 80, cex.names=0.6, col=c("pink", "skyblue","green","yellow","gray"), main = "")

classAgreement(fire_kmedoids_2)$crand


#********************* Logistic Regression  *******************************
fire.lg <- fire_subset
fire.lg$y <- ifelse(fire$Classes=="not fire",0,1)
set.seed(1234)
train.index.lg <-  createDataPartition(fire.lg$y,times=1,p=0.70,list=FALSE)
train.lg <- fire.lg[train.index, ]
test.lg  <- fire.lg[-train.index, ]

fire_full_glm <- glm(y ~ ., data = train.lg, family = binomial(link = "logit")) #Forward Selection
summary(fire_full_glm)

fire_glm <- glm(y ~ Temperature+DC, data = train.lg, family = binomial(link = "logit")) #Forward Selection
summary(fire_glm)
G<-235.62-144.72 #null deviance - residual deviance
G
pchisq(G ,171-169,lower.tail=FALSE)  # p-value=1.825222e-20, reject HO(fitted model as good as null)

coefficients <- coef(fire_glm)
odds_ratios <- exp(coefficients)
odds_ratios

predicted_probs <- predict(fire_glm, newdata = test.lg, type = "response")
predicted_classes <- ifelse(predicted_probs > 0.5, 1, 0) 

accuracy <- mean(predicted_classes == test.lg$y)
print(paste("Accuracy on test set:", round(accuracy * 100, 2), "%"))

lg.tab <- table(predicted_classes, test.lg$y) #missclassification table
1-(sum(diag(lg.tab)) / sum(lg.tab)) #MCR=0.1666667


#********************* LDA&QDA  *******************************
# LDA, full model
set.seed(1234)
lda_fire <- lda(x=fire_subset[,-10],grouping=fire_subset[,10],subset=train.index)
lda_fire
pred_fire_lda <- predict(lda_fire,fire_subset[-train.index,-10])$class
# Classification Table
fire_lda_tab = table(fire_subset[-train.index,10],pred_fire_lda)
fire_lda_tab
# MCR = 0.08333333
1-(sum(diag(fire_lda_tab)) / sum(fire_lda_tab))
# Accuracy
sum(diag(fire_lda_tab))/sum(fire_lda_tab)

# Plot LDA
partimat(fire_subset[,-10], grouping=fire_subset[,10], subset=train.index, method ="lda",nplots.vert=1,nplots.hor=3)

# LDA, reduced model
# Using only variables chosen by forward selection
# Temperature, DC
set.seed(1234)
lda_fire_reduce <- lda(x=fire_subset[,c("Temperature","DC")],grouping=fire_subset[,10],subset=train.index)
lda_fire_reduce
pred_fire_lda_reduce <- predict(lda_fire_reduce,fire_subset[-train.index,c("Temperature","DC")])$class
fire_lda_tab_reduce = table(fire_subset[-train.index,10],pred_fire_lda_reduce)
fire_lda_tab_reduce
1-(sum(diag(fire_lda_tab_reduce)) / sum(fire_lda_tab_reduce))


# QDA
set.seed(1234)
qda_fire <- qda(x=fire_subset[,-10],grouping=fire_subset[,10],subset=train.index)
qda_fire
pred_fire_qda <- predict(qda_fire,fire_subset[-train.index,-10])$class
fire_qda_tab=table(fire_subset[-train.index,10],pred_fire_qda)
fire_qda_tab
# MCR = 0.05555556
1-(sum(diag(fire_qda_tab))/sum(fire_qda_tab))
# Accuracy
sum(diag(fire_qda_tab))/sum(fire_qda_tab)

# Plot QDA
partimat(fire_subset[,-10], grouping=fire_subset[,10], subset=train.index, method = "qda",
         nplots.vert=1,nplots.hor=3)

# QDA, reduced model
# Using only variables chosen by forward selection
# Temperature, DC
set.seed(1234)
qda_fire_reduce <- qda(x=fire_subset[,c("Temperature","DC")],grouping=fire_subset[,10],subset=train.index)
qda_fire_reduce
pred_fire_qda_reduce <- predict(qda_fire_reduce,fire_subset[-train.index,c("Temperature","DC")])$class
fire_qda_tab_reduce = table(fire_subset[-train.index,10],pred_fire_qda_reduce)
fire_qda_tab_reduce
1-(sum(diag(fire_qda_tab_reduce)) / sum(fire_qda_tab_reduce))

