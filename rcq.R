#----- Retrieving data frames -----
setwd("/home/hp/Dropbox/drinkless/csvs/sites/default/files/gwiaa_export/")

# Retrieving csvs with client_id column
files  <- list.files() #listing files 
element  <- as.logical(rep(1, length(files))) #creating object to store elements

for (i in 1:length(files)) {
  data <- read.csv(files[[i]])
  dataNames  <- names(data)
  element[i]  <- is.element("Client_Id", dataNames) | is.element("Client_id", dataNames) | is.element("client_id", dataNames) | is.element("client_ID", dataNames) | is.element("UID", dataNames)
  
}    
report  <- data.frame(files, element)
csvwId  <- subset(report, element == TRUE)
varNames  <- gsub("export_|.csv", "", csvwId$files) # picking goodnames for dataframes

for (j in 1:length(csvwId$files)) {
  dfname  <- varNames[j]
  assign(dfname, read.csv(as.character(csvwId[j, 1]), na.strings = c("NA","")))
  
}

#---- RCQ Validation ------

# Libraries
library(psych)
library(RColorBrewer)
library(sp)
library(car)
library(MBESS)
library(semPlot)
library(lattice)
library(ltm)
library(mirt)


#------ PREPARING DATA  -------

# Merging dataframes with addicional data - demographics + audit

## Putting same names in different data frames
names(audit)[names(audit)=="client_ID"] <- "client_id"
names(registration_form)[names(registration_form)=="client_ID"] <- "client_id"

## Merging data frames using "client_id" as key
rcqMerged  <- merge(audit, registration_form, by.x="client_id", by.y="client_id", all= TRUE) # creating temporary data.frame
rcqFinal  <- merge(rcqMerged, rcq_motivation, by.x="client_id", by.y="client_ID", all = TRUE)
rm(rcqMerged) # removing the temporary dataframe

## Select just valid cases for RCQ
rcq  <- subset(rcqFinal, rcqFinal$rcq_1 != "NA" & rcqFinal$rcq_2 != "NA" & rcqFinal$rcq_1 != "NA" & rcqFinal$Research == "Sim")
rcq$sex <-  as.numeric(rcq$sex)
rcq$Timestamp.x <- as.Date(rcq$Timestamp.x, "%d.%m.%Y")


## Final dataframe
rcq  <- rcq[, c("client_id", "Timestamp.x", "audit_1", "audit_2", "audit_3", "audit_4", "audit_5", "audit_6", "audit_7", "audit_8", "audit_9", "audit_10", "Result", "sex", "age", "education", "Province", "País", "Work.situation", "rcq_1", "rcq_2", "rcq_3","rcq_4","rcq_5","rcq_6","rcq_7","rcq_8","rcq_9","rcq_10","rcq_11","rcq_12","rcq_a","rcq_b","rcq_c","rcq_outcome")]


## Save final data.frame as csv file
write.csv(rcq, "rcq.csv")

#------ DATA ANALYSIS -------

## Open dataframe
rcq  <- read.csv("rcq.csv")
describe(rcq)

##---- Transform and compute vars ---------

### Audit Zones
rcq$auditRec[rcq$Result <= 7]  <-  "Low Risk"
rcq$auditRec[rcq$Result > 7 &  rcq$Result <= 15]  <-  "Risky"
rcq$auditRec[rcq$Result > 15 &  rcq$Result <= 19]  <-  "High-Risk"
rcq$auditRec[rcq$Result > 20]  <-  "Dependency"
rcq$auditRec  <- as.factor(rcq$auditRec) # transforming var as a factor

### Sex
rcq$sex <- factor(rcq$sex, labels=c("Female","Male"))

### Education
rcq$education <- factor(rcq$education, labels=c("High School Comp","High School Incomp", "Elementary", "Graduate", "College", "College Incomp."))

### Work situation
rcq$Work.situation  <- factor(rcq$Work.situation, labels=c("Não", "Sim"))

### Province to Region
# Southeast
rcq$region[rcq$Province == "Esp?rito Santo" | rcq$Province == "Minas Gerais" | rcq$Province == "Rio de Janeiro" | rcq$Province == "S?o Paulo"]  <- "Southeast"
# South
rcq$region[rcq$Province == "Paran?" | rcq$Province == "Santa Catarina" | rcq$Province == "Rio Grande do Sul"]  <- "South"
# Mid-west
rcq$region[rcq$Province == "Mato Grosso" | rcq$Province == "Mato Grosso do Sul" | rcq$Province == "Goi?s" | rcq$Province == "Distrito Federal"]  <- "Midwest"
# Northeast
rcq$region[rcq$Province == "Bahia" | rcq$Province == "Sergipe" | rcq$Province == "Pernambuco" | rcq$Province == "Piau?" | rcq$Province == "Rio Grande do Norte" |  rcq$Province == "Para?ba" | rcq$Province == "Cear?" |  rcq$Province == "Alagoas" | rcq$Province == "Maranh?o"]  <- "Northeast"
# North
rcq$region[rcq$Province == "Acre" | rcq$Province == "Amazonas" | rcq$Province == "Rond?nia" | rcq$Province == "Roraima" | rcq$Province == "Amap?" | rcq$Province == "Tocantins" ]  <- "North"
# NA
rcq$region[rcq$Province == "0"]  <- NA

### RCQ
#computing using WHO criteria
rcq$scorePc  <- rcq$rcq_1 + rcq$rcq_5 + rcq$rcq_10 + rcq$rcq_12
rcq$scoreC <- rcq$rcq_3 + rcq$rcq_4 + rcq$rcq_8 + rcq$rcq_9
rcq$scoreA  <- rcq$rcq_2 + rcq$rcq_6 + rcq$rcq_7 + rcq$rcq_11

##---- Descriptives ---------

# Timestamp
range(rcq$Timestamp.x)

# Age -
describe(rcq$age)
by(rcq$age, rcq$sex, summary)
bwplot(~age|sex*education, data=rcq)

# Sex - 
round(prop.table(table(rcq$sex)),3)
by(rcq$sex, rcq$education, summary)

# Education - 
tableEducation  <- sort(table(rcq$education), decreasing=TRUE)
cbind(round(prop.table(tableEducation),3))

# Work situation
cbind(round(prop.table(table(rcq$Work.situation)),3))
by(rcq$Result, rcq$Work.situation, summary)

# Region
cbind(round(prop.table(sort(table(rcq$region), decreasig=TRUE)),3))

# Audit Score
cbind(round(prop.table(sort(table(rcq$auditRec), decreasig=TRUE)),3))
boxplot(Result ~sex, data=rcq)
bwplot(~Result|sex*education, data=rcq)

# RCQ classification vs. Audit
by(rcq$Result, rcq$rcq_outcome, summary)
bwplot(~Result|rcq_outcome, data=rcq)


##---- Psychometric properties  ---------

# Descriptive statistics for questionnaire
describe(rcq[,20:31])

# KMO Kaiser-Meyer-Olkin Measure of Sampling Adequacy
# Function by G. Jay Kerns, Ph.D., Youngstown State University (http://tolstoy.newcastle.edu.au/R/e2/help/07/08/22816.html)
kmo = function( data ){
  library(MASS) 
  X <- cor(as.matrix(data)) 
  iX <- ginv(X) 
  S2 <- diag(diag((iX^-1)))
  AIS <- S2%*%iX%*%S2                      # anti-image covariance matrix
  IS <- X+AIS-2*S2                         # image covariance matrix
  Dai <- sqrt(diag(diag(AIS)))
  IR <- ginv(Dai)%*%IS%*%ginv(Dai)         # image correlation matrix
  AIR <- ginv(Dai)%*%AIS%*%ginv(Dai)       # anti-image correlation matrix
  a <- apply((AIR - diag(diag(AIR)))^2, 2, sum)
  AA <- sum(a) 
  b <- apply((X - diag(nrow(X)))^2, 2, sum)
  BB <- sum(b)
  MSA <- b/(b+a)                        # indiv. measures of sampling adequacy
  AIR <- AIR-diag(nrow(AIR))+diag(MSA)  # Examine the anti-image of the correlation matrix. That is the  negative of the partial correlations, partialling out all other variables.
  kmo <- BB/(AA+BB)                     # overall KMO statistic
  # Reporting the conclusion 
  if (kmo >= 0.00 && kmo < 0.50){test <- 'The KMO test yields a degree of common variance unacceptable for FA.'} 
  else if (kmo >= 0.50 && kmo < 0.60){test <- 'The KMO test yields a degree of common variance miserable.'} 
  else if (kmo >= 0.60 && kmo < 0.70){test <- 'The KMO test yields a degree of common variance mediocre.'} 
  else if (kmo >= 0.70 && kmo < 0.80){test <- 'The KMO test yields a degree of common variance middling.' } 
  else if (kmo >= 0.80 && kmo < 0.90){test <- 'The KMO test yields a degree of common variance meritorious.' }
  else { test <- 'The KMO test yields a degree of common variance marvelous.' }
  
  ans <- list( overall = kmo,
               report = test,
               individual = MSA,
               AIS = AIS,
               AIR = AIR )
  return(ans)
}
kmo(rcq[20:31])

# Barlett test of homogeneity
bartlett.test(rcq[20:31])

# Defining factors
fa.parallel.poly(rcq[20:31], fm="pa") # yields 2 components
VSS(rcq[20:31], rotate="varimax") # yields 2 components


# Principal components analysis 
## Original scale

### Model
pca <- principal(rcq[20:31], nfactors = 2, rotate = "varimax")
### Summary
pcResult  <- print.psych(pca, cut = 0.3, sort = FALSE) # data revealed that item 5 was not related with any factors. Thus, I removed it and perform the model again.

## Scale with item 5 dropped
rcq11  <- rcq[, c("rcq_1", "rcq_2", "rcq_3", "rcq_4", "rcq_6", "rcq_7", "rcq_8", "rcq_9", "rcq_10", "rcq_11", "rcq_12")]
pca <- principal(rcq11, nfactors = 2, rotate = "varimax")
### Summary
print.psych(pca, cut = 0.3)

# Summing factors
## Action
rcq$scoreA  <- (rcq$rcq_2 + rcq$rcq_6 + rcq$rcq_7 + rcq$rcq_11) / 4 

## Contemplation
rcq$rcq_1 <- Recode(rcq$rcq_1, "1='5' ; 2='4' ; 3 = '3'; 3 = '3'; 4 = '2'; 5 = '1'")
rcq$rcq_10 <- Recode(rcq$rcq_10, "1='5' ; 2='4' ; 3 = '3'; 3 = '3'; 4 = '2'; 5 = '1'")
rcq$rcq_12 <- Recode(rcq$rcq_12, "1='5' ; 2='4' ; 3 = '3'; 3 = '3'; 4 = '2'; 5 = '1'")

rcq$scoreC  <- (rcq$rcq_1 + rcq$rcq_10 + rcq$rcq_12 + rcq$rcq_3 + rcq$rcq_4 + rcq$rcq_8 + rcq$rcq_9) / 7

# PreCon + Con
preo  <- rcq[, c("rcq_1", "rcq_3", "rcq_4", "rcq_8", "rcq_9", "rcq_10", "rcq_12")]

# 11 items RCQ
alpha(rcq11) # alpha .88

#subscales
alpha(preo)
alpha(action)

# Comparing subscales scores
rcor.test(rcq[,c("scoreC","scoreA")], p.adjust= TRUE, method="kendall", p.adjust.method="bonferroni")
describe(rcq[,c("scoreC","scoreA")])


#  ORIGINAL COMPARISIONS ----
# Creating proflies based on Heather et al. (1991)
rcq$heather
rcq$heather[rcq$rcqA  == rcq$rcqC & rcq$rcqA != rcq$rcqPc & rcq$rcqPc != rcq$rcqC]    <- "Tie A-C"
rcq$heather[rcq$rcqA  == rcq$rcqPc & rcq$rcqA != rcq$rcqC & rcq$rcqPc != rcq$rcqC]    <- "Tie A-Pc"
rcq$heather[rcq$rcqPc == rcq$rcqC & rcq$rcqPc != rcq$rcqA & rcq$rcqPc != rcq$rcqA]    <- "Tie C-Pc"
rcq$heather[rcq$rcqPc == rcq$rcqC & rcq$rcqPc == rcq$rcqA]    <- "Triple Tie"
rcq$heather[rcq$rcqA  > rcq$rcqC & rcq$rcqA > rcq$rcqPc]    <- "Action"
rcq$heather[rcq$rcqC  > rcq$rcqA & rcq$rcqC > rcq$rcqPc]    <- "Contemplation"
rcq$heather[rcq$rcqPc > rcq$rcqC & rcq$rcqPc > rcq$rcqA]    <- "Pre Contemplation"

## Heather with correction
rcq$heatherC[rcq$rcqA  >= rcq$rcqC & rcq$rcqA >= rcq$rcqPc]    <- "Action"
rcq$heatherC[rcq$rcqC  > rcq$rcqA & rcq$rcqC > rcq$rcqPc]    <- "Contemplation"
rcq$heatherC[rcq$rcqPc > rcq$rcqC & rcq$rcqPc > rcq$rcqA]    <- "Pre Contemplation"

cbind(table(rcq$heatherC))

# Creating profiles based on Rollnick et al. (1992) adapted for the 3 point subscales
rcq$profile[rcq$rcqPc > 9 & rcq$rcqC > 9 & rcq$rcqA > 9 ]  <- "A (+ + +)"
rcq$profile[rcq$rcqPc > 9 & rcq$rcqC > 9 & rcq$rcqA <= 9 ]  <- "B (+ + –)"
rcq$profile[rcq$rcqPc > 9 & rcq$rcqC <= 9 & rcq$rcqA > 9 ]  <- "C (+ – +)"
rcq$profile[rcq$rcqPc > 9 & rcq$rcqC <= 9 & rcq$rcqA <= 9 ]  <- "D (+ – –)"
rcq$profile[rcq$rcqPc <= 9 & rcq$rcqC > 9 & rcq$rcqA > 9 ]  <- "E (– + +)"
rcq$profile[rcq$rcqPc <= 9 & rcq$rcqC > 9 & rcq$rcqA <= 9 ]  <- "F (– + –)"
rcq$profile[rcq$rcqPc <= 9 & rcq$rcqC <= 9 & rcq$rcqA > 9 ]  <- "G (– – +)"
rcq$profile[rcq$rcqPc <= 9 & rcq$rcqC <= 9 & rcq$rcqA <= 9 ]  <- "H (– – –)"



#- IRT PLAYGROUND -----
## Subscales Graded Response Model

## Checking unidimensionality
### 1 Factor

#### preo
m1f  <- mirt(preo, 1, rotate="oblimin")
summary(m1f)
residuals(m1f)

#### Action
m1f  <- mirt(action, 1, rotate="oblimin")
summary(m1f)
plot(m1f)

itemplot(m1f, 7, 'trace')

residuals(m1f, type="LDG2")
coef(m1f)

# factor scores
scores  <- fscores(m1f, full.scores = TRUE)

summary(scores$)


## Pre contemplation
preo2p  <- grm(preo); preo1p  <- grm(preo, constrained = T) # Comparing models - one parameter vs. 2 parameter
### Comparing, using BIC and AIC as criteria. (Less is better)
anova(preo1p, preo2p) # 2 parameters model (preo2p) seems to fit data better

### Plotting Item Characteristic Curves
plot(preo2p, type= "IIC", col = brewer.pal(4,"Dark2"), legend= TRUE) 
plot(preo2p, type= "ICC", col = brewer.pal(4,"Dark2"), legend= TRUE)
plot(preo2p, type= "OCCu", col = brewer.pal(4,"Dark2"), legend= TRUE)

### Coeficients
coef(preo2p)

## Contemplation
c2Par  <- grm(shortC); c1Par  <- grm(shortC, constrained = T) # Comparing models - one parameter vs. 2 parameter
### Comparing, using BIC and AIC as criteria. (Less is better)
anova(pc1Par, pc2Par) # 2 parameters model (pc2Par) seems to fit data better

### Plotting Item Characteristic Curves
plot(c2Par, type= "IIC", col = brewer.pal(4,"Dark2"), legend= TRUE) 
plot(c2Par, type= "ICC", col = brewer.pal(4,"Dark2"), legend= TRUE)
plot(c2Par, type= "OCCu", col = brewer.pal(4,"Dark2"), legend= TRUE)

### Coeficients
coef(c2Par)

## Action
a2Par  <- grm(shortA); a1Par  <- grm(shortA, constrained = T) # Comparing models - one parameter vs. 2 parameter
### Comparing, using BIC and AIC as criteria. (Less is better)
anova(a1Par, a2Par) # 2 parameters model (pc2Par) seems to fit data better

### Plotting Item Characteristic Curves
plot(a2Par, type= "IIC", col = brewer.pal(4,"Dark2"), legend= TRUE) 
plot(a2Par, type= "ICC", col = brewer.pal(4,"Dark2"), legend= TRUE)
plot(a2Par, type= "OCCu", col = brewer.pal(4,"Dark2"), legend= TRUE)

### Coeficients
coef(a2Par)

# Scoring subscales
scorePre  <- factor.scores(pc2Par, resp.patterns=shortPc)
scoreContemplation  <- factor.scores(c2Par, resp.patterns=shortC)
scoreAction  <- factor.scores(a2Par, resp.patterns=shortA)


boxplot(scorePre$score.dat$z1 ~ rcq$auditRec)
boxplot(scoreContemplation$score.dat$z1 ~ rcq$auditRec)
boxplot(scoreAction$score.dat$z1 ~ rcq$auditRec)

by(scoreContemplation$score.dat$z1, rcq$auditRec, mean)


# Thetas Scores
plot(scoreAction$score.dat$z1, rcq$rcqA, type = "p", xlab = "Thetas", ylab = "Soma bruta da escala")
cor(scoreAction$score.dat$z1, rcq$rcqA)

plot(scoreContemplation$score.dat$z1, rcq$rcqC, type = "p", xlab = "Thetas", ylab = "Soma bruta da escala")
cor(scoreContemplation$score.dat$z1, rcq$rcqC)

plot(scorePre$score.dat$z1, rcq$rcqPc, type = "p", xlab = "Thetas", ylab = "Soma bruta da escala")
cor(scorePre$score.dat$z1, rcq$rcqPc)



cbind(table(rcq$profile))

