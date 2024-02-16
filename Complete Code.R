library(DataExplorer)#introduce
#library(ggplot)
library(egg) #ggarange function
library(MASS)#lda
library(class)
library(bestglm)#bestglm
library(glmnet)#ridge and lasso
library(gam) #gam
library(tree) #tree
library(gbm) #gbm
library(randomForest) #forest
library(boot)#cv.glm
library(ggplot2)


#LOAD DATA
dat <- read.table("SouthGermanCredit.asc", header=TRUE) 

## dat contains numbers for all variables.
## variables duration, amount and age are truly quantitative
## variables installment_rate, present_residence and number_credits are
### quantitative in the data, but are in fact discretized scores for 
### an underlying quantitative variable
### and are thus stored as ordered factors below
## variable people_liable is quantitative in the data but is in fact 
### a binarized score (less 0 to 2 versus 3 or more)
### and is thus stored as a factor below
## all the numeric values (=level codes) 
### for the categorical variables 
### (including the discretized quantitative variables), 
### are the P2 scores from Häußler (1979) 
### which can be directly used in credit scoring (larger=better).
### (Exceptions have been corrected in the raw data, 
###     which implies that columns pers and gastarb have 
###     entries opposite to those in Open Data LMU (2010)
###     and the GermanCredit data from the UCI ML Repo.)

## variable names from Fahrmeir/Hamerle book
nam_fahrmeirbook <- colnames(dat)

### assign levels 
### level assignment can be sanity-checked 
### with Table 2.1 from the Fahrmeir/Hamerle book, 
###     which gives proportions separated for good and bad credit risks.
### That table is provided with by Open Data LMU 
###     (https://doi.org/10.5282/ubm/data.23)
###     together with a German language version of the data set.
### A corresponding table for the English language data is produced 
###     below for the final data (levels ordered by increasing code).
### Level labels have been taken from package evtree, except for 
###     the variable telephone (where the yes level has been made more detailed)
###     and those variables that were quantitative and do not have level labels
###     in evtree.

nam_evtree <- c("status", "duration", "credit_history", "purpose", "amount", 
                "savings", "employment_duration", "installment_rate",
                "personal_status_sex", "other_debtors",
                "present_residence", "property",
                "age", "other_installment_plans",
                "housing", "number_credits",
                "job", "people_liable", "telephone", "foreign_worker",
                "credit_risk")
names(dat) <- nam_evtree

## make factors for all except the numeric variables
## make sure that even empty level of factor purpose = verw (dat[[4]]) is included
for (i in setdiff(1:21, c(2,4,5,13)))
  dat[[i]] <- factor(dat[[i]])
## factor purpose
dat[[4]] <- factor(dat[[4]], levels=as.character(0:10))

## assign level codes
## make intrinsically ordered factors into class ordered 
levels(dat$credit_risk) <- c("bad", "good")
levels(dat$status) = c("no checking account",
                       "... < 0 DM",
                       "0<= ... < 200 DM",
                       "... >= 200 DM / salary for at least 1 year")
## "critical account/other credits elsewhere" was
## "critical account/other credits existing (not at this bank)",
levels(dat$credit_history) <- c(
  "delay in paying off in the past",
  "critical account/other credits elsewhere",
  "no credits taken/all credits paid back duly",
  "existing credits paid back duly till now",
  "all credits at this bank paid back duly")
levels(dat$purpose) <- c(
  "others",
  "car (new)",
  "car (used)",
  "furniture/equipment",
  "radio/television",
  "domestic appliances",
  "repairs",
  "education", 
  "vacation",
  "retraining",
  "business")
levels(dat$savings) <- c("unknown/no savings account",
                         "... <  100 DM", 
                         "100 <= ... <  500 DM",
                         "500 <= ... < 1000 DM", 
                         "... >= 1000 DM")
levels(dat$employment_duration) <- 
  c(  "unemployed", 
      "< 1 yr", 
      "1 <= ... < 4 yrs",
      "4 <= ... < 7 yrs", 
      ">= 7 yrs")
dat$installment_rate <- ordered(dat$installment_rate)
levels(dat$installment_rate) <- c(">= 35", 
                                  "25 <= ... < 35",
                                  "20 <= ... < 25", 
                                  "< 20")
levels(dat$other_debtors) <- c(
  "none",
  "co-applicant",
  "guarantor"
)
## female : nonsingle was female : divorced/separated/married
##    widowed females are not mentioned in the code table
levels(dat$personal_status_sex) <- c(
  "male : divorced/separated",
  "female : non-single or male : single",
  "male : married/widowed",
  "female : single")
dat$present_residence <- ordered(dat$present_residence)
levels(dat$present_residence) <- c("< 1 yr", 
                                   "1 <= ... < 4 yrs", 
                                   "4 <= ... < 7 yrs", 
                                   ">= 7 yrs")
## "building soc. savings agr./life insurance", 
##    was "building society savings agreement/life insurance"
levels(dat$property) <- c(
  "unknown / no property",
  "car or other",
  "building soc. savings agr./life insurance", 
  "real estate"
)
levels(dat$other_installment_plans) <- c(
  "bank",
  "stores",
  "none"
)
levels(dat$housing) <- c("for free", "rent", "own")
dat$number_credits <- ordered(dat$number_credits)
levels(dat$number_credits) <- c("1", "2-3", "4-5", ">= 6")
## manager/self-empl./highly qualif. employee  was
##   management/self-employed/highly qualified employee/officer
levels(dat$job) <- c(
  "unemployed/unskilled - non-resident",
  "unskilled - resident",
  "skilled employee/official",
  "manager/self-empl./highly qualif. employee"
)
levels(dat$people_liable) <- c("3 or more", "0 to 2")
levels(dat$telephone) <- c("no", "yes (under customer name)")
levels(dat$foreign_worker) <- c("yes", "no")

## checks against fahrmeir table
tabs <- 
  list(status = round(100*prop.table(xtabs(~status+credit_risk, dat),2),2),
       credit_history = round(100*prop.table(xtabs(~credit_history+credit_risk, dat),2),2),
       purpose = round(100*prop.table(xtabs(~purpose+credit_risk, dat),2),2),
       savings = round(100*prop.table(xtabs(~savings+credit_risk, dat),2),2),
       employment_duration = round(100*prop.table(xtabs(~employment_duration+credit_risk, dat),2),2),
       installment_rate = round(100*prop.table(xtabs(~installment_rate+credit_risk, dat),2),2),
       personal_status_sex = round(100*prop.table(xtabs(~personal_status_sex+credit_risk, dat),2),2),
       other_debtors = round(100*prop.table(xtabs(~other_debtors+credit_risk, dat),2),2),
       present_residence = round(100*prop.table(xtabs(~present_residence+credit_risk, dat),2),2),
       property = round(100*prop.table(xtabs(~property+credit_risk, dat),2),2),
       other_installment_plans = round(100*prop.table(xtabs(~other_installment_plans+credit_risk, dat),2),2),
       housing = round(100*prop.table(xtabs(~housing+credit_risk, dat),2),2),
       number_credits = round(100*prop.table(xtabs(~number_credits+credit_risk, dat),2),2),
       job = round(100*prop.table(xtabs(~job+credit_risk, dat),2),2),
       people_liable = round(100*prop.table(xtabs(~people_liable+credit_risk, dat),2),2),
       telephone = round(100*prop.table(xtabs(~telephone+credit_risk, dat),2),2),
       foreign_worker = round(100*prop.table(xtabs(~foreign_worker+credit_risk, dat),2),2))

## variables for which a tab entry is available
## (all except 2, 5 and 13)
tabwhich <- setdiff(1:20, c(2,5,13))


set.seed(10)

german_credit <- dat
attach(german_credit)

#Data Exploration
head(german_credit,5)
str(german_credit)

metadata<-t(introduce(german_credit))
colnames(metadata)<-"Info"

#xtable(metadata) for latex
plot_intro(german_credit,title = "South German Credit Data Integrity")

#Plots for Continuous Variables 
#duration
g1 <- ggplot(german_credit, aes(x = credit_risk, y = duration, fill = credit_risk)) +
  geom_boxplot() +
  stat_summary(fun=mean, geom="point", shape=18, size=3, color="black", position=position_dodge(0.75)) +
  theme(legend.position = "none")
g1.2 <- ggplot(german_credit, aes(sample = duration)) +
  stat_qq() + geom_qq_line(color = "red", linetype = "dashed") + labs(x = "Theoretical",y = "Sample")
#amount
g1.3<-ggplot(german_credit, aes(x = credit_risk, y = amount, fill = credit_risk)) + geom_boxplot() +
  stat_summary(fun=mean, geom="point", shape=18, size=3, color="black", position=position_dodge(0.75)) +
  theme(legend.position = "none")
g1.4 <- ggplot(german_credit, aes(sample = amount)) +
  stat_qq() + geom_qq_line(color = "red", linetype = "dashed") + labs(x = "Theoretical",y = "Sample")
#AGE
g1.5<-ggplot(german_credit, aes(x =credit_risk, y = age, fill = credit_risk)) + 
  geom_boxplot() + stat_summary(fun=mean, geom="point", shape=18, size=3, color="black", position=position_dodge(0.75)) +
  theme(legend.position = "none")
g1.6 <- ggplot(german_credit, aes(sample = age)) +
  stat_qq() + geom_qq_line(color = "red", linetype = "dashed") + labs(x = "Theoretical",y = "Sample")

ggarrange(g1,g1.3,g1.5, g1.2,g1.4,g1.6,
          labels = c("Class Distribution", "","", "Normal Probability Plot","", ""),
          ncol = 3, nrow = 2)

#Plots for Categorical and Binary Variables
g3<-ggplot(german_credit, aes(status, ..count..)) + geom_bar(aes(fill = credit_risk), position = "dodge") 

g4<-ggplot(german_credit, aes(credit_history, ..count..)) + geom_bar(aes(fill = credit_risk), position = "dodge") 

g5<-ggplot(german_credit, aes(purpose, ..count..)) + geom_bar(aes(fill = credit_risk), position = "dodge") 

g6<-ggplot(german_credit, aes(savings, ..count..)) + geom_bar(aes(fill = credit_risk), position = "dodge") 

g7<-ggplot(german_credit, aes(employment_duration, ..count..)) + geom_bar(aes(fill = credit_risk), position = "dodge")

g8<-ggplot(german_credit, aes(installment_rate, ..count..)) + geom_bar(aes(fill = credit_risk), position = "dodge")

g9<-ggplot(german_credit, aes(personal_status_sex, ..count..)) + geom_bar(aes(fill = credit_risk), position = "dodge") 

g10<-ggplot(german_credit, aes(other_debtors, ..count..)) + geom_bar(aes(fill = credit_risk), position = "dodge") 

g11<-ggplot(german_credit, aes(present_residence, ..count..)) + geom_bar(aes(fill = credit_risk), position = "dodge") 

g12<-ggplot(german_credit, aes(property, ..count..)) + geom_bar(aes(fill = credit_risk), position = "dodge") 

g13<-ggplot(german_credit, aes(other_installment_plans, ..count..)) + geom_bar(aes(fill = credit_risk), position = "dodge") 

g14<-ggplot(german_credit, aes(housing, ..count..)) + geom_bar(aes(fill = credit_risk), position = "dodge") 

g15<-ggplot(german_credit, aes(number_credits, ..count..)) + geom_bar(aes(fill = credit_risk), position = "dodge") 

g16<-ggplot(german_credit, aes(job, ..count..)) + geom_bar(aes(fill = credit_risk), position = "dodge") 

g17<-ggplot(german_credit, aes(people_liable, ..count..)) + geom_bar(aes(fill = credit_risk), position = "dodge") 

g18<-ggplot(german_credit, aes(telephone, ..count..)) + geom_bar(aes(fill = credit_risk), position = "dodge") 

g19<-ggplot(german_credit, aes(foreign_worker, ..count..)) + geom_bar(aes(fill = credit_risk), position = "dodge") 

#Plots
ggarrange(g3, g4, g5, labels = c("3.1: status ", "3.2: credit_history","3.3: purpose"),ncol = 1, nrow = 3)
ggarrange(g6, g7, g8, labels = c("4.1 savings ", "4.2: employment_duration","4.3: installment_rate"),ncol = 1, nrow = 3)
ggarrange(g9, g10, g11, labels = c("5.1: personal_status_sex ", "5.2: other_debtors","5.3: present_residence"),ncol = 1, nrow = 3)
ggarrange(g12, g13, g14, labels = c("6.1: property ", "6.2: other_installment_plans","6.3: housing"),ncol = 1, nrow = 3)
ggarrange(g15, g16, g17, labels = c("7.1: number_credits ", "7.2: job","7.3: people_liable"),ncol = 1, nrow = 3)
ggarrange(g18, g19, labels = c("8.1: telephone ", "8.2: foreign_worker"),ncol = 1, nrow = 2)

#Excluding
#Table to inspect which to exclude 
factor_variable_names <- names(Filter(is.factor, german_credit))
# Create a list to store proportions
proportions_list <- list()
# Loop through the factor variables and populate the list
for (variable in factor_variable_names) {
  proportions <- prop.table(table(german_credit[, variable])) * 100
  proportions_list[[variable]] <- proportions
}
proportions_list
# Print the resulting list

#Exclude : 
#personal_status_sex #Drop for data issues
#foreign_worker #proportions
#telephone #almost everyone now will have 
#other_debtors #
#People liable
#other_installment_plans#

german_credit <- german_credit[, !(names(german_credit) %in% c("personal_status_sex", "foreign_worker", "telephone", "other_debtors","people_liable","other_installment_plans"))]

#Amend
#Housing 
german_credit$renting <- ifelse(german_credit$housing %in% c("for free", "own"), "no", "yes")
german_credit <- german_credit[, !(names(german_credit) %in% "housing")] # Remove the original "housing" variable
german_credit$renting <- factor(german_credit$renting, levels = c("yes", "no")) # Convert the new "renting" variable to a factor
summary(german_credit$renting)

#Number of credits join 4-5 and >6 and just make it >= 6 
levels(german_credit$number_credits) <- ifelse(levels(german_credit$number_credits) %in% c("2-3","4-5", ">= 6"),
                                               ">=2",
                                               levels(german_credit$number_credits))

# Assuming 'job' is a factor variable
levels(german_credit$job) <- ifelse(levels(german_credit$job) %in% c("unemployed/unskilled - non-resident", "unskilled - resident"),
                                    "unskilled/Unemployed",
                                    levels(german_credit$job))

#Education 
sum(german_credit$purpose == "education") #remove educaiton as purpose
levels(german_credit$purpose) <- ifelse(levels(german_credit$purpose) %in% c("education", "others"),
                                        "others",
                                        levels(german_credit$purpose))

str(german_credit)
#Join domestic appliances and tv/radio and furniture / equipment
levels(german_credit$purpose) <- ifelse(levels(german_credit$purpose) %in% c("domestic appliances", "radio/television"),
                                        "domestic appliances/radio/television",
                                        levels(german_credit$purpose))
#Join business and training
levels(german_credit$purpose) <- ifelse(levels(german_credit$purpose) %in% c("business", "retraining"),
                                        "business/retraining",
                                        levels(german_credit$purpose))

levels(german_credit$purpose) <- ifelse(levels(german_credit$purpose) %in% c("vacation"),
                                        "others",
                                        levels(german_credit$purpose))

#Savings Join 100< 500 and 500 < 1000
levels(german_credit$savings) <- ifelse(levels(german_credit$savings) %in% c("100 <= ... <  500 DM", "500 <= ... < 1000 DM"),
                                        "100 <= ... < 1000 DM",
                                        levels(german_credit$savings))

german_credit <- german_credit[, c("credit_risk", setdiff(names(german_credit), "credit_risk"))]

str(german_credit)

### Part 1 ### 
train <- sample(1:1000,700)

gc_train <- german_credit[train,]
gc_test <- german_credit[-train,]

response_train <- gc_train$credit_risk
response_test <- gc_test$credit_risk

#since data has assymettric distribution, set cutt-off probability to 30%

#Logistic Regression
log.fit <- glm(credit_risk ~., data = gc_train, family = binomial)
summary(log.fit)

log.prob <- predict(log.fit, gc_test, type = "response") #Calculate probabilities

log.pred <- rep("bad", length(response_test))
log.pred[log.prob > 0.3] <- "good"
table(log.pred, response_test)
mean(log.pred != response_test) # overall error rate = 0.27

#L00CV
cost.fn <- function(r, pi = 0) mean((r==1 & pi<0.3) | (r==0 & pi>0.3))
cv.glm(gc_train,log.fit,cost=cost.fn)$delta[1] # Log LOOCV = 0.255 #Might be on full dataset

#Part 2
#LDA
lda.fit <- lda(credit_risk ~ ., data = gc_train)

lda.prob <- predict(lda.fit , gc_test)

# For a 0.3 cut-off
lda.pred <- rep("bad",length(response_test))
lda.pred[lda.prob$posterior[,2] > 0.3] <- "good"
mean(lda.pred != response_test) # error rate = 0.28

#LDA default prior probability 
mean(lda.prob$class != response_test) #0.27

#QDA
qda.fit <- qda(credit_risk ~., data = gc_train)
qda.fit
qda.prob <- predict(qda.fit , gc_test)

# For a 0.3 cut-off
qda.pred <- rep("bad",length(response_test))
qda.pred[qda.prob$posterior[,2] > 0.3] <- "good"
table(qda.pred,response_test)
mean(qda.pred != response_test) # error rate = 0.29

mean(qda.prob$class != response_test)#0.3

#KNN
attach(german_credit)
train.X=cbind(status,duration,credit_history,purpose,amount,savings,employment_duration,installment_rate,present_residence,property,age,number_credits,job,renting)[train,]
test.X=cbind(status,duration,credit_history,purpose,amount,savings,employment_duration,installment_rate,present_residence,property,age,number_credits,job,renting)[-train,]

set.seed(10)
# Set the range of k values to test
k_values <- seq(10, 200, by = 5)

# Create a data frame to store the results
results <- data.frame(k = k_values, error_rate = rep(NA, length(k_values)))

# Define the control parameters for cross-validation
ctrl <- trainControl(method = "cv",   # Use cross-validation
                     number = 10)      # Number of folds

# Train the kNN model using cross-validation
KNN <- train(x = train.X,
             y = response_train,
             method = "knn",
             trControl = ctrl,
             tuneGrid = data.frame(k = k_values))

# Store the cross-validated error rates in the results data frame
results$error_rate <- 1 - KNN$results$Accuracy

# Plot the results with k on the x-axis and error_rate on the y-axis
plot(results$k, results$error_rate, type = "b", pch = 19, col = "blue",
     xlab = "k", ylab = "Error Rate", main = "Cross-Validated Error Rates for Different Values of k")

# Add a vertical line at the optimal k
abline(v = results$k[which.min(results$error_rate)], col = "red", lty = 2)

set.seed(10)
knn.pred1 <- knn(train=train.X, test=test.X, cl=response_train, k = 40)
table(knn.pred1, response_test)
mean(knn.pred1 != response_test) # Error rate =  0.2966


#Part 5
#BIC
bestlogits <- bestglm(rev(gc_train), IC = "BIC",family = binomial)
summary(bestlogits$BestModel) #this is logit regression of best model 
bestlog.prob <- predict(bestlogits$BestModel, rev(gc_test), type = "response")

bestlogits$Subsets

bestlog.pred <- rep("bad", length(response_test))
bestlog.pred[bestlog.prob > 0.3] <- "good"
table(bestlog.pred, response_test)
mean(bestlog.pred !=  response_test) # overall error rate = 0.29666

plot(x = 1:15, y = bestlogits$Subsets$BIC, xlab = "Number of Variables", ylab = "BIC", main = "BIC", pch = 19, col = "blue")
abline(h = min(bestlogits$Subsets$BIC), col = "red", lty = 2)# Adding a line to highlight the minimum value
legend("top", legend = "Minimum Value", col = "red", lty = 2, cex = 0.8,) # Adding a line to highlight the minimum value

#AIC
bestaic <- bestglm(rev(gc_train),IC="AIC",family = binomial)
summary(bestaic$BestModel)
bestaic.prob <-  predict(bestaic$BestModel, rev(gc_test), type = "response")

bestaic.pred <- rep("bad", length(response_test))
bestaic.pred[bestaic.prob > 0.3] <- "good"
table(bestaic.pred, response_test)
mean(bestaic.pred !=  response_test) # overall error rate = 0.33

plot(x = 1:15, y = bestaic$Subsets$AIC, xlab = "Number of Variables", ylab = "AIC", main = "AIC", pch = 19, col = "blue")
abline(h = min(bestaic$Subsets$AIC), col = "red", lty = 2)# Adding a line to highlight the minimum value
legend("top", legend = "Minimum Value", col = "red", lty = 2, cex = 0.8,) # Adding a line to highlight the minimum value

#Part 5 
#ridge
set.seed(1)
x <- model.matrix(credit_risk~.,data = gc_train)[,-1]
y <- response_train
newx <- model.matrix(credit_risk~.,data = gc_test)[,-1]

grid <- 10^seq(10, -2, length = 100)

ridge.mod <- glmnet(x,y,alpha = 0,family = "binomial",lambda = grid) #ridge
ridge.cv <- cv.glmnet(x,y,alpha = 0,family = "binomial",lambda = grid,type.measure = "class")
plot(ridge.cv)

ridge.bestlam <- ridge.cv$lambda.min
ridge.bestlam
ridge.pred <- predict(ridge.mod, s = ridge.bestlam, newx = newx,type = 'class')

table(ridge.pred, response_test)
mean(ridge.pred != response_test)#0.285

#1SE
ridge.1se <- ridge.cv$lambda.1se
ridge.1se
ridge.pred2 <- predict(ridge.mod, s = ridge.1se, newx = newx,type = 'class')
table(ridge.pred2, response_test)
mean(ridge.pred2 != response_test)#0.33

#Lasso
set.seed(10)
lasso.mod <- glmnet(x, y, alpha = 1, lambda = grid,family = 'binomial')
lasso.cv <- cv.glmnet(x, y, alpha = 1, lambda = grid,family='binomial',type.measure = "class")

plot(lasso.cv)

lasso.bestlam <- lasso.cv$lambda.min
lasso.bestlam #0.01
lasso.pred <- predict(lasso.mod, s = lasso.bestlam, newx = newx,type = 'class')
table(lasso.pred, response_test)
mean(lasso.pred != response_test)#0.2866

# Most sparse lasso model with 1 se
lasso.bestlam2 <- lasso.cv$lambda.1se
lasso.bestlam2 #0.017
lasso.pred2 <- predict(lasso.mod, s = lasso.bestlam2, newx = newx,type = 'class')
mean(lasso.pred2 != response_test)#0.34

#GAM
selected_columns <- names(bestaic$BestModels)[as.logical(bestaic$BestModels[1,])]
selected_columns <- selected_columns[selected_columns != "Criterion"]
n <- selected_columns

gamtrain <- gc_train
gamtest <- gc_test
gamtrain$credit_risk <- ifelse(gamtrain$credit_risk == "bad", 0, 1)
gamtest$credit_risk <- ifelse(gamtest$credit_risk == "bad", 0, 1)

cont<-c("duration","credit_risk","age")
f <- as.formula(paste("I(credit_risk) ~ ns(duration) +ns(amount)+", paste(n[!n %in% cont], collapse = " + ")))

logit.gam <- gam(formula= f, data = gamtrain, family =binomial)

summary(logit.gam)

gam.prob <- predict(logit.gam,newdata = gamtest, type = "response")

gam.pred <- rep("bad", length(response_test))
gam.pred[gam.prob > 0.3] <- "good"
table(gam.pred, response_test)
mean(gam.pred != response_test) # overall error rate = 0.286

#Part 7
#Tree 
set.seed(10)
tree.credit <- tree(credit_risk ~ ., data = gc_train)
summary(tree.credit)
plot(tree.credit)
text(tree.credit, pretty = 0)

tree.pred <- predict(tree.credit, newdata = gc_test,type = 'class')
table(tree.pred,response_test)
mean(tree.pred != response_test) #0.3


cv.tree <- cv.tree(tree.credit, FUN = prune.misclass)

par(mfrow = c(1, 2)) #graphs
plot(cv.tree$size, cv.tree$dev, type = "b") 
plot(cv.tree$k, cv.tree$dev, type = "b")


par(mfrow = c(1, 1)) #graphs
prune.tree.credit <- prune.misclass(tree.credit, best = 6)#pruned tree
plot(prune.tree.credit)
text(prune.tree.credit, pretty = 0)

tree.pred2 <- predict(prune.tree.credit, gc_test, type = "class")
table(tree.pred2, response_test)
mean(tree.pred2 != response_test) #0.3


#boost
boost.credit <- gbm(credit_risk ~., gamtrain, distribution = "bernoulli", n.trees = 500, interaction.depth = 4)
summary(boost.credit,las=2)

#Tune it
# Define ranges for tuning parameters
n_trees_grid <- seq(100, 1000, by = 100)
interaction_depth_grid <- seq(2, 6, by = 1)
shrinkage_grid <- seq(0.01, 0.2, by = 0.01)

# Initialize a 3D array to store test errors for each combination
test.error <- array(NA, dim = c(length(n_trees_grid), length(interaction_depth_grid), length(shrinkage_grid)))

set.seed(10)
# Loop over each combination of parameters
for (i in 1:length(n_trees_grid)) {
  for (j in 1:length(interaction_depth_grid)) {
    for (k in 1:length(shrinkage_grid)) {
      # Train a boosted model with the current combination
      boost.credit <- gbm(
        credit_risk ~ .,
        data = gamtrain,
        distribution = "bernoulli",
        n.trees = n_trees_grid[i],
        interaction.depth = interaction_depth_grid[j],
        shrinkage = shrinkage_grid[k]
      )
      
      # Make predictions on the test set
      pred.credit.test <- predict(boost.credit, gamtest, n.trees = n_trees_grid[i], type = "response")
      
      # Calculate test error (assuming binary classification)
      test.error[i, j, k] <- mean((pred.credit.test - gamtest$credit_risk)^2)
    }
  }
}

# Find the indices of the minimum test error
min_error_indices <- which(test.error == min(test.error), arr.ind = TRUE)
best_n_trees <- n_trees_grid[min_error_indices[1, 1]]
best_interaction_depth <- interaction_depth_grid[min_error_indices[1, 2]]
best_shrinkage <- shrinkage_grid[min_error_indices[1, 3]]

set.seed(10)
best.boost <- gbm(credit_risk ~ ., data = gamtrain, distribution = "bernoulli", n.trees = best_n_trees, interaction.depth = best_interaction_depth, shrinkage = best_shrinkage)
summary(best.boost,las=2)

boost_prob <- predict(best.boost,gamtest, n.trees = best_n_trees, type = "response")#predict with best model one 
best.boost.pred <- rep("bad", length(response_test))
best.boost.pred[boost_prob > 0.3] <- "good"
table(best.boost.pred, response_test)
mean(best.boost.pred !=  response_test) # overall error rate = 0.28

#Random Forest
set.seed(10)
credit.rf <- randomForest(as.factor(credit_risk)~., data = gc_train,mtry=sqrt(ncol(gc_train)-1),ntree=500)
credit.rf
#tuned
mtry_values <- seq(1, sqrt(ncol(gc_train) - 1), by = 0.1)  #range of mtry
test.error_rf <- rep(-1, length(mtry_values))  #Initialize a vector to store test errors for each mtry
set.seed(10)
# Loop over each mtry value
for (i in 1:length(mtry_values)) {
  # Train a random forest model with the current mtry value
  credit.rf <- randomForest(as.factor(credit_risk) ~ ., data = gc_train, mtry = mtry_values[i], ntree = 500)
  
  # Make predictions on the test set
  pred.credit.test_rf <- predict(credit.rf, gc_test, type = "response")
  
  test.error_rf[i] <- mean(pred.credit.test_rf != gc_test$credit_risk)
  
}

# Plot the test errors for each mtry value
plot(mtry_values, test.error_rf, type = "b", xlab = "mtry", ylab = "Test MSE (Random Forest)")

# Find the optimal mtry value and corresponding test error
best_mtry_rf <- mtry_values[which.min(test.error_rf)]
mse_rf <- min(test.error_rf)

best_rf <- randomForest(as.factor(credit_risk)~., data = gc_train,mtry=best_mtry_rf,ntree=500)
best_rf.pred <- predict(best_rf,newdata = gc_test,type='response')
table(best_rf.pred,response_test)
mean(best_rf.pred!= response_test) #0.256

#Bagging

credit.bag <- randomForest(as.factor(credit_risk)~., data = gc_train,mtry=ncol(gamtrain)-1,ntree=500,importance = TRUE)
set.seed(10)

#tuning
num_trees <- seq(100, 1000, by = 100) # Adjust the range and step size as needed
# Initialize a vector to store test errors for each number of trees
test.error_bagging <- rep(-1, length(num_trees))
# Loop over each number of trees
for (i in 1:length(num_trees)) {
  # Train a bagged model with the current number of trees
  bag.credit <- randomForest(as.factor(credit_risk) ~ ., data = gc_train,mtry=ncol(gc_train)-1, ntree = num_trees[i],importance = TRUE)
  
  # Make predictions on the test set
  pred.credit.test_bagging <- predict(bag.credit, gc_test,type = "response")
  
  # Calculate test error (assuming binary classification)
  test.error_bagging[i] <- mean(pred.credit.test_bagging != gc_test$credit_risk)
}
# Plot the test errors for each number of trees
plot(num_trees, test.error_bagging, type = "b", xlab = "Number of Trees", ylab = "Test MSE (Bagging)")
# Find the optimal number of trees and corresponding test error
best_num_trees_bagging <- num_trees[which.min(test.error_bagging)]
mse_bagging <- min(test.error_bagging)

best.bag <- randomForest(as.factor(credit_risk)~., data = gc_train,mtry=ncol(gc_train)-1,ntree=best_num_trees_bagging,importance = TRUE)

best.bag.pred <- predict(best.bag,newdata = gc_test,type='response')
table(best.bag.pred,response_test)
mean(best.bag.pred!= response_test) #0.24

credit.bag <- randomForest(as.factor(credit_risk)~., data = gc_train,mtry=ncol(gamtrain)-1,ntree=500,importance = TRUE)
set.seed(10)
#tuning
num_trees <- seq(100, 1000, by = 100) # Adjust the range and step size as needed
# Initialize a vector to store test errors for each number of trees
test.error_bagging <- rep(-1, length(num_trees))
# Loop over each number of trees
for (i in 1:length(num_trees)) {
  # Train a bagged model with the current number of trees
  bag.credit <- randomForest(as.factor(credit_risk) ~ ., data = gc_train,mtry=ncol(gc_train)-1, ntree = num_trees[i],importance = TRUE)
  
  # Make predictions on the test set
  pred.credit.test_bagging <- predict(bag.credit, gc_test,type = "response")
  
  # Calculate test error (assuming binary classification)
  test.error_bagging[i] <- mean(pred.credit.test_bagging != gc_test$credit_risk)
}
# Plot the test errors for each number of trees
plot(num_trees, test.error_bagging, type = "b", xlab = "Number of Trees", ylab = "Test MSE (Bagging)")
# Find the optimal number of trees and corresponding test error
best_num_trees_bagging <- num_trees[which.min(test.error_bagging)]
mse_bagging <- min(test.error_bagging)


best.bag <- randomForest(as.factor(credit_risk)~., data = gc_train,mtry=ncol(gc_train)-1,ntree=best_num_trees_bagging,importance = TRUE)

best.bag.pred <- predict(best.bag,newdata = gc_test,type='response')
table(best.bag.pred,response_test)
mean(best.bag.pred!= response_test) #0.24




#Conclusion #Statistics
calculate_profit <- function(predictions,method) {
  # Create confusion matrix
  confusion_matrix <- table(predictions, response_test)
  
  # Calculate proportions
  proportion_good_good <- confusion_matrix[4] / sum(confusion_matrix)
  proportion_bad_good <- confusion_matrix[2] / sum(confusion_matrix)
  
  # Calculate per-unit profit
  per_unit_profit <- proportion_good_good * 0.35 + proportion_bad_good * (-1)
  
  #Calculate average amount for applications predicted good
  mean_approved_amount<- mean(subset(data.frame(predictions,gc_test$amount), predictions == 'good')$gc_test.amount)
  
  #calculate per applicant profit =
  per_applicant_profit <- mean_approved_amount * per_unit_profit
  
  #calculate overall profit for test data
  profit = per_applicant_profit * (confusion_matrix[2]+confusion_matrix[4])
  
  #calculate test mse 
  test_mse = (confusion_matrix[2]+confusion_matrix[3])/sum(confusion_matrix)
  
  #calculate opportunity cost / profit missed
  opportunity.cost = per_applicant_profit * confusion_matrix[3]
  
  result_table <- data.frame(
    Prediction_Method = method,
    Test_MSE = test_mse,
    Per_Unit_Profit = per_unit_profit,
    Per_Applicant_Profit = per_applicant_profit,
    Mean_Approved_Amount = mean_approved_amount,
    Profit_Overall = profit,
    Opportunity_Cost = opportunity.cost
  )
  return(result_table)
}

all_results <- rbind(
  calculate_profit(log.pred, "Full Logistic Model"),
  calculate_profit(lda.pred, "LDA Classifier, Threshold = 0.3"),
  calculate_profit(lda.prob$class, "LDA Classifier, threshold = Prior Prob"),
  calculate_profit(qda.prob$class, "QDA, threshold = prior"),
  calculate_profit(qda.pred, "QDA, threshold = 0.3"),
  calculate_profit(knn.pred1, "KNN, K = 20"),
  calculate_profit(bestlog.pred, "Subset Selection: BIC"),
  calculate_profit(bestaic.pred, "Subset Selection: AIC"),
  calculate_profit(ridge.pred, "Ridge: best lambda"),
  calculate_profit(ridge.pred2, "Ridge: best lambda, 1se"),
  calculate_profit(lasso.pred, "Lasso: best lambda"),
  calculate_profit(lasso.pred2, "Sparse Lasso: best lambda with 1se"),
  calculate_profit(gam.pred, "GAM: Logistic Model"),
  calculate_profit(tree.pred, "Classification Tree"),
  calculate_profit(tree.pred2, "Pruned Classification Tree"),
  calculate_profit(best.boost.pred, "Boosting, tuned"),
  calculate_profit(best.bag.pred, "Bagging, Best number of trees"),
  calculate_profit(best_rf.pred, "Random Forest: best mtry")
)
all_results
