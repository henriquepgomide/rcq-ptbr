#----- Retrieving data frames -----
setwd("drinkless/csvs/sites/default/files/gwiaa_export/")

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
library(ggplot2)
library(RColorBrewer)
library(xtable)
library(sp)
library(car)
library(MBESS)
library(semPlot)
library(ltm)
library(lattice)


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
rcqValid  <- subset(rcqFinal, rcqFinal$rcq_1 != "NA" & rcqFinal$rcq_2 != "NA" & rcqFinal$rcq_1 != "NA" & rcqFinal$Research == "Sim")
rcqValid$sex <-  as.numeric(rcqValid$sex)


## Final dataframe
rcqValid  <- rcqValid[, c("client_id", "audit_1", "audit_2", "audit_3", "audit_4", "audit_5", "audit_6", "audit_7", "audit_8", "audit_9", "audit_10", "Result", "sex", "age", "education", "Province", "País", "Work.situation", "rcq_1", "rcq_2", "rcq_3","rcq_4","rcq_5","rcq_6","rcq_7","rcq_8","rcq_9","rcq_10","rcq_11","rcq_12","rcq_a","rcq_b","rcq_c","rcq_outcome")]


## Save final data.frame as csv file
write.csv(rcqValid, "rcq.csv")

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

##---- Descriptives ---------

# Age - 
by(rcq$age, rcq$sex, summary)
bwplot(~age|sex*education, data=rcq)

# Sex - 
table(rcq$sex)
by(rcq$sex, rcq$education, summary)


# Education - 
tableEducation  <- sort(table(rcq$education), decreasing=TRUE)
round(cbind(tableEducation/length(rcq$education)),2)

# Audit Score
boxplot(Result ~sex, data=rcq)
bwplot(~Result|sex*education, data=rcq)

# Work situation
by(rcq$Result, rcq$Work.situation, summary)

# RCQ classification vs. Audit
by(rcq$Result, rcq$rcq_outcome, summary)
bwplot(~Result|rcq_outcome, data=rcq)


# Audit - 
# Barplot 
tableAudit  <- table(rcqValid$auditRec) # creating table
xtable(tableAudit, caption="Participants consumption zones") # table 4 LaTeX
round(cbind(tableAudit/length(rcqValid$auditRec)),2) # simply % table
barplot(tableAudit, xlim=c(0,7), ylim=c(0,300), space=0.1, beside=TRUE, col= brewer.pal(4, "Accent"), cex.names=.95) # barplot

# Province
## importing map
brasil  <- url("http://biogeo.ucdavis.edu/data/gadm2/R/BRA_adm1.RData")
print(load(brasil))
close(brasil)

## plotting data on the map
freq <- c(0,8,0,3,44,27,36,18,11,11,9,10,70,3,7,30,17,4,57,7,32,0,1,16,128,2,1)
cutFreq  <- cut(freq, 5)
levels(cutFreq)  <- c("<25", "25 - 51", "52-77", "78-102", ">102")
gadm$freq <- cutFreq
col  <- brewer.pal(5, "BuPu")
spplot(gadm, "freq", col.regions=col, col=grey(.9), border="white", par.settings = list(axis.line = list(col = 'transparent')))

# work situation
tableWork  <- table(rcqValid$Work.situation)
xtable(tableWork)

##---- Psychometric properties  ---------

# ---- WHO version ----

#computing using WHO criteria
rcqValid$scorePc  <- rcqValid$rcq_1 + rcqValid$rcq_5 + rcqValid$rcq_10 + rcqValid$rcq_12
rcqValid$scoreC <- rcqValid$rcq_3 + rcqValid$rcq_4 + rcqValid$rcq_8 + rcqValid$rcq_9
rcqValid$scoreA  <- rcqValid$rcq_2 + rcqValid$rcq_6 + rcqValid$rcq_7 + rcqValid$rcq_11

# Descriptive statistics for questionnaire
sapply(rcqValid[,34:45], summary)

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

kmo(rcqValid[34:45])

# Barlett test of homogeneity
bartlett.test(rcqValid[34:45])

# Correlations among itens
rcorr(as.matrix(rcqValid[50:52]), type="pearson")
round(sapply(rcqValid[,46:48], mean),2)


# Defining factors
fa.parallel(rcqValid[34:45], fm="pa") # yields 2 components
VSS(rcqValid[34:45], rotate="varimax") # yields 2 components
# Cattel's Scree
fa.parallel(rcqValid[34:45], fm="ml", fa="pc") # 2 components

# Principal components analysis
pca <- principal(rcqValid[34:45], nfactors = 2, rotate = "varimax")

# Observing loadings
print.psych(pca, cut = 0.4, sort = FALSE) # data revealed that item 5 was not related with any factors. As a possible solution, we decided to remove 1 item for each subscale. Therefore, we removed 3 items: 2,5,8.

#----- Short version ----
shortRcq  <- rcqValid[, c("rcq_1", "rcq_3","rcq_4","rcq_6","rcq_7","rcq_9","rcq_10","rcq_11", "rcq_12")]

#subscales
shortPc  <- rcqValid[, c("rcq_1", "rcq_10","rcq_12")]
shortC  <- rcqValid[, c("rcq_3","rcq_4","rcq_9")]
shortA  <- rcqValid[, c("rcq_6","rcq_7","rcq_11")]

# Defining factors
fa.parallel(shortRcq, fm="pa") # yields 2 components
VSS(shortRcq, rotate="varimax") # yields 2 components
# Cattel's Scree
fa.parallel(shortRcq, fm="ml", fa="pc") # 2 components

## Principal components analysis
pcaShort <- principal(shortRcq, nfactors = 2, rotate = "varimax")
print.psych(pcaShort, cut=0.3)

# Cronbach's alpha New Version
#main scale
alpha(shortRcq) # alpha .85
#subscales
alpha(shortPc) # pre-contemplation - alpha .81
alpha(shortC)  # contemplation - alpha .81
alpha(shortA) # action - alpha .80

# Summing factors
rcqValid$rcqPc  <- rcqValid$rcq_1 + rcqValid$rcq_10 + rcqValid$rcq_12
rcqValid$rcqC  <- rcqValid$rcq_3 + rcqValid$rcq_4 + rcqValid$rcq_9
rcqValid$rcqA  <- rcqValid$rcq_6 + rcqValid$rcq_7 + rcqValid$rcq_11

# Comparing subscales scores
cor(rcqValid)
rcor.test(rcqValid[,51:53], p.adjust= TRUE, p.adjust.method="bonferroni")


# Creating proflies based on Heather et al. (1991)
rcqValid$heather
rcqValid$heather[rcqValid$rcqA  == rcqValid$rcqC & rcqValid$rcqA != rcqValid$rcqPc & rcqValid$rcqPc != rcqValid$rcqC]    <- "Tie A-C"
rcqValid$heather[rcqValid$rcqA  == rcqValid$rcqPc & rcqValid$rcqA != rcqValid$rcqC & rcqValid$rcqPc != rcqValid$rcqC]    <- "Tie A-Pc"
rcqValid$heather[rcqValid$rcqPc == rcqValid$rcqC & rcqValid$rcqPc != rcqValid$rcqA & rcqValid$rcqPc != rcqValid$rcqA]    <- "Tie C-Pc"
rcqValid$heather[rcqValid$rcqPc == rcqValid$rcqC & rcqValid$rcqPc == rcqValid$rcqA]    <- "Triple Tie"
rcqValid$heather[rcqValid$rcqA  > rcqValid$rcqC & rcqValid$rcqA > rcqValid$rcqPc]    <- "Action"
rcqValid$heather[rcqValid$rcqC  > rcqValid$rcqA & rcqValid$rcqC > rcqValid$rcqPc]    <- "Contemplation"
rcqValid$heather[rcqValid$rcqPc > rcqValid$rcqC & rcqValid$rcqPc > rcqValid$rcqA]    <- "Pre Contemplation"

## Heather with correction
rcqValid$heatherC[rcqValid$rcqA  >= rcqValid$rcqC & rcqValid$rcqA >= rcqValid$rcqPc]    <- "Action"
rcqValid$heatherC[rcqValid$rcqC  > rcqValid$rcqA & rcqValid$rcqC > rcqValid$rcqPc]    <- "Contemplation"
rcqValid$heatherC[rcqValid$rcqPc > rcqValid$rcqC & rcqValid$rcqPc > rcqValid$rcqA]    <- "Pre Contemplation"

cbind(table(rcqValid$heatherC))

# Creating profiles based on Rollnick et al. (1992) adapted for the 3 point subscales
rcqValid$profile[rcqValid$rcqPc > 9 & rcqValid$rcqC > 9 & rcqValid$rcqA > 9 ]  <- "A (+ + +)"
rcqValid$profile[rcqValid$rcqPc > 9 & rcqValid$rcqC > 9 & rcqValid$rcqA <= 9 ]  <- "B (+ + –)"
rcqValid$profile[rcqValid$rcqPc > 9 & rcqValid$rcqC <= 9 & rcqValid$rcqA > 9 ]  <- "C (+ – +)"
rcqValid$profile[rcqValid$rcqPc > 9 & rcqValid$rcqC <= 9 & rcqValid$rcqA <= 9 ]  <- "D (+ – –)"
rcqValid$profile[rcqValid$rcqPc <= 9 & rcqValid$rcqC > 9 & rcqValid$rcqA > 9 ]  <- "E (– + +)"
rcqValid$profile[rcqValid$rcqPc <= 9 & rcqValid$rcqC > 9 & rcqValid$rcqA <= 9 ]  <- "F (– + –)"
rcqValid$profile[rcqValid$rcqPc <= 9 & rcqValid$rcqC <= 9 & rcqValid$rcqA > 9 ]  <- "G (– – +)"
rcqValid$profile[rcqValid$rcqPc <= 9 & rcqValid$rcqC <= 9 & rcqValid$rcqA <= 9 ]  <- "H (– – –)"

cbind(table(rcqValid$profile))


#- IRT -----
## Subscales Graded Response Model

## Pre contemplation
pc2Par  <- grm(shortPc); pc1Par  <- grm(shortPc, constrained = T) # Comparing models - one parameter vs. 2 parameter
### Comparing, using BIC and AIC as criteria. (Less is better)
anova(pc1Par, pc2Par) # 2 parameters model (pc2Par) seems to fit data better

### Plotting Item Characteristic Curves
plot(pc2Par, type= "IIC", col = brewer.pal(4,"Dark2"), legend= TRUE) 
plot(pc2Par, type= "ICC", col = brewer.pal(4,"Dark2"), legend= TRUE)
plot(pc2Par, type= "OCCu", col = brewer.pal(4,"Dark2"), legend= TRUE)

### Coeficients
coef(pc2Par)

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


boxplot(scorePre$score.dat$z1 ~ rcqValid$auditRec)
boxplot(scoreContemplation$score.dat$z1 ~ rcqValid$auditRec)
boxplot(scoreAction$score.dat$z1 ~ rcqValid$auditRec)

by(scoreContemplation$score.dat$z1, rcqValid$auditRec, mean)


# Thetas Scores
plot(scoreAction$score.dat$z1, rcqValid$rcqA, type = "p", xlab = "Thetas", ylab = "Soma bruta da escala")
cor(scoreAction$score.dat$z1, rcqValid$rcqA)

plot(scoreContemplation$score.dat$z1, rcqValid$rcqC, type = "p", xlab = "Thetas", ylab = "Soma bruta da escala")
cor(scoreContemplation$score.dat$z1, rcqValid$rcqC)

plot(scorePre$score.dat$z1, rcqValid$rcqPc, type = "p", xlab = "Thetas", ylab = "Soma bruta da escala")
cor(scorePre$score.dat$z1, rcqValid$rcqPc)

#- CFA Confirmatory factor analysis ----
library(lavaan)

# specify the model
rcq.model <- 'contemplation =~ rcq_3 + rcq_4 + rcq_9 + rcq_1 + rcq_10 + rcq_12
              action   =~ rcq_6 + rcq_7 + rcq_11 '

# fit the model
fit <- cfa(rcq.model, data=rcqValid)

inspect(fit,"cov.lv")

# display summary output
summary(fit, fit.measures=TRUE)

est  <- parameterEstimates(fit, ci = FALSE, standardized = TRUE)
subset(est, op == "=~")

# display
semPaths(fit, title = FALSE, curvePivot = TRUE, whatLabels="stand", residuals=TRUE, intercepts=TRUE, layout="tree2")
