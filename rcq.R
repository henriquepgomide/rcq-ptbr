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

#---- LIBRARIES ------

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


### Write dataframe

write.csv(rcq, "rcq_df.csv" )

## --- OPEN DATA ----

rcq  <- read.csv("rcq_df.csv")


##---- Descriptives ---------

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


# full RCQ

# EFA ----

# Descriptive statistics for questionnaire
describe(fullRcq)

# KMO = .89
KMO(fullRcq)

# Barlett test of homogeneity; K²=169.92; p < 0.001
bartlett.test(fullRcq) 

# Factor analysis

## 1-factor model
rcq1factor  <- rcq[,21:32]

# Recode preContemplation into Contemplation
rcq1factor$rcq_1 <- Recode(rcq1factor$rcq_1, "1='5' ; 2='4' ; 3 = '3'; 3 = '3'; 4 = '2'; 5 = '1'")
rcq1factor$rcq_5 <- Recode(rcq1factor$rcq_5, "1='5' ; 2='4' ; 3 = '3'; 3 = '3'; 4 = '2'; 5 = '1'")
rcq1factor$rcq_10 <- Recode(rcq1factor$rcq_10, "1='5' ; 2='4' ; 3 = '3'; 3 = '3'; 4 = '2'; 5 = '1'")
rcq1factor$rcq_12 <- Recode(rcq1factor$rcq_12, "1='5' ; 2='4' ; 3 = '3'; 3 = '3'; 4 = '2'; 5 = '1'")

## fa with 1 factor
fa1 <- fa.poly(rcq1factor, nfactors = 1)
print(fa1, cut = .3)

## 2-factor model
rcq2factor  <- rcq[,21:32]

## fa with 3 factor
fa2 <- fa.poly(rcq2factor, nfactors = 2)
print(fa2, cut = .3)

## 3-factor model
rcq3factor  <- rcq[,21:32]

## fa with 3 factor
fa3 <- fa.poly(rcq3factor, nfactors = 3)
print(fa3, cut = .3)


### CFA ----

library(lavaan)

# 1-Factor Model

RCQ.1f.MODEL  <- ' # Latent variables
                   readiness       =~ rcq_1 + rcq_2 + rcq_3 + rcq_4
                                   + rcq_5 + rcq_6 + rcq_7 + rcq_8
                                   + rcq_9 + rcq_10 + rcq_11 + rcq_12'

cfa1f  <- cfa(RCQ.1f.MODEL,  data = rcq1factor)



# 2-Factor Model 
RCQ.2f.MODEL  <- ' # Latent variables
                   Con   =~ rcq_1 + rcq_3 + rcq_3 + rcq_5 + 
                            rcq_8 + rcq_9 + rcq_10 + rcq_12                                   
                   Action =~ rcq_2 + rcq_6 + rcq_7 + rcq_11                 
                '

cfa2f  <- cfa(RCQ.2f.MODEL,  data = rcq1factor)



# 3-Factor Model with Correlation

RCQ.3fr.MODEL  <- '# Latent variables                  
                  Con       =~ rcq_1 + rcq_5 + rcq_10  + rcq_12
                  Precont   =~ rcq_3 + rcq_4 + rcq_8 + rcq_9
                  Action    =~ rcq_2 + rcq_6 + rcq_7 + rcq_11

                  # Factor variances
                  Con      ~~ Precont
                  Action   ~~ Con
                  Precont  ~~ Action

                                  
                '

cfa3fr  <- cfa(RCQ.3fr.MODEL,  data = rcq3factor)


# 3-Factor Model with no Correlation

RCQ.3f.MODEL  <- '# Latent variables                  
                  Con       =~ rcq_1 + rcq_5 + rcq_10 + rcq_12
                  Precont   =~ rcq_3 + rcq_4 + rcq_8 + rcq_9
                  Action    =~ rcq_2 + rcq_6 + rcq_7 + rcq_11              
                                  
                '

cfa3f  <- cfa(RCQ.3f.MODEL,  data = rcq3factor, orthogonal=TRUE)


# Comparing Models
## Anova
anova(cfa1f, cfa2f, cfa3f, cfa3fr)

## Summary
summary(cfa1f, standardized=TRUE, fit.measures=TRUE, rsq=TRUE, modindices=TRUE)  #  CFI = .850; TLI = .808; RMSEA = .145; SRMR = .119 
summary(cfa2f, standardized=TRUE, fit.measures=TRUE, rsq=TRUE, modindices=TRUE)  #  CFI = .850; TLI = .808; RMSEA = .145; SRMR = .119 
summary(cfa3f, standardized=TRUE, fit.measures=TRUE, rsq=TRUE, modindices=TRUE)  #  CFI = .850; TLI = .808; RMSEA = .145; SRMR = .119 
summary(cfa3fr, standardized=TRUE, fit.measures=TRUE, rsq=TRUE, modindices=TRUE) #  CFI = .856; TLI = .813; RMSEA = .143; SRMR = .120

## Model Loadings
Est <- parameterEstimates(cfa3fr, ci = FALSE, standardized = TRUE)
subset(Est, op == "=~")

## Modification indices
MI <- modificationIndices(cfa3fr)
subset(MI, mi > 10)

fitMeasures(cfa3fr, fit.measures = c("chisq", "df", "cfi", "rmsea", "rmsea.ci.lower", "rmsea.ci.upper"))
fitMeasures(cfa1f, fit.measures = c("chisq", "df", "cfi", "rmsea", "rmsea.ci.lower", "rmsea.ci.upper"))
fitMeasures(cfa3f, fit.measures = c("chisq", "df", "cfi", "rmsea", "rmsea.ci.lower", "rmsea.ci.upper"))

# Graphing solution
library(qgraph)
qgraph(cfa3f, layout = "tree", titles = FALSE)

# Summing factors
## Action
rcq$scoreA  <- (rcq$rcq_2 + rcq$rcq_6 + rcq$rcq_7 + rcq$rcq_11) / 4 

## Contemplation
rcq$rcq_1 <- Recode(rcq$rcq_1, "1='5' ; 2='4' ; 3 = '3'; 3 = '3'; 4 = '2'; 5 = '1'")
rcq$rcq_5 <- Recode(rcq$rcq_5, "1='5' ; 2='4' ; 3 = '3'; 3 = '3'; 4 = '2'; 5 = '1'")
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

