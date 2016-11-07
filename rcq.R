#---- LIBRARIES ------

# Libraries
library(psych)
library(lavaan)
library(sp)
library(car)
library(lattice)

## --- OPEN DATA ----

rcq  <- read.csv("rcq_df.csv")
rcq <- subset(rcq, rcq$auditRec != "Low Risk")

##---- Descriptives ---------

# Split data frames for EFA and CFA
set.seed(12345)
rcq_rand <- rcq[order(runif(408)), ]
rcqEfa  <- rcq_rand[1:204, ]   # Exploratory factor analysis data
rcqCfa  <- rcq_rand[205:408, ] # Confirmatory factor analysis data

# Age -
## Full Sample
describe(rcq$age)
by(rcq$age, rcq$sex, summary)
bwplot(~age|sex*education, data=rcq)
describe(rcqEfa$age) # EFA Sample
describe(rcqCfa$age) # EFA Sample

# Sex - 
round(prop.table(table(rcq$sex)),3) # Full sample
round(prop.table(table(rcqEfa$sex)),3) # EFA
round(prop.table(table(rcqCfa$sex)),3) # CFA

# Education - 
## Full sample
tableEducation  <- sort(table(rcq$education), decreasing=TRUE)
cbind(round(prop.table(tableEducation),3))
## EFA sample
tableEducationEFA  <- sort(table(rcqEfa$education), decreasing=TRUE)
cbind(round(prop.table(tableEducationEFA),3))
## CFA sample
tableEducationCFA  <- sort(table(rcqCfa$education), decreasing=TRUE)
cbind(round(prop.table(tableEducationCFA),3))

# Work situation
cbind(round(prop.table(table(rcq$Work.situation)),3))    ## Full Scale 
cbind(round(prop.table(table(rcqEfa$Work.situation)),3)) ## EFA
cbind(round(prop.table(table(rcqCfa$Work.situation)),3)) ## CFA

# Region
cbind(round(prop.table(sort(table(rcq$region), decreasig=TRUE)),3)) ## Full scale
cbind(round(prop.table(sort(table(rcqEfa$region), decreasig=TRUE)),3)) ## EFA
cbind(round(prop.table(sort(table(rcqCfa$region), decreasig=TRUE)),3)) ## CFA

# Audit Score
cbind(round(prop.table(sort(table(rcq$auditRec), decreasig=TRUE)),3))*100 ## full Scale
cbind(round(prop.table(sort(table(rcqEfa$auditRec), decreasig=TRUE)),3))*100 ## EFA
cbind(round(prop.table(sort(table(rcqCfa$auditRec), decreasig=TRUE)),3))*100 ## CFA

boxplot(Result ~sex, data=rcq)A
bwplot(~Result|sex*education, data=rcq)

# RCQ classification vs. Audit
by(rcq$Result, rcq$rcq_outcome, summary)
bwplot(~Result|rcq_outcome, data=rcq)


##---- Psychometric properties  ---------
# Exploratory Factor Analysis ----
rcqEfa_complete  <- rcqEfa[,22:33] # Select just the RCQ items
      
# Descriptive statistics for questionnaire
describe(rcqEfa_complete)

# KMO = .89
KMO(rcqEfa_complete)

# Barlett test of homogeneity; K²=169.92; p < 0.001
bartlett.test(rcqEfa_complete)

# Parallel Analysis with polychoric correlations and minimal residuals method
fa.parallel(rcqEfa_complete, fm="minres", fa="fa", cor="poly")

efaModel0 <- fa.poly(rcqEfa_complete, nfactors = 3,  fm="minres")
print(efaModel0, cut = .3)

# Very simple structure
VSS(rcqEfa_complete)

# Factor analysis

## 1-factor model
rcq1factor  <- rcqEfa_complete

# Recode preContemplation into Contemplation
rcq1factor$rcq_1 <- Recode(rcq1factor$rcq_1, "1='5' ; 2='4' ; 3 = '3'; 3 = '3'; 4 = '2'; 5 = '1'")
rcq1factor$rcq_5 <- Recode(rcq1factor$rcq_5, "1='5' ; 2='4' ; 3 = '3'; 3 = '3'; 4 = '2'; 5 = '1'")
rcq1factor$rcq_10 <- Recode(rcq1factor$rcq_10, "1='5' ; 2='4' ; 3 = '3'; 3 = '3'; 4 = '2'; 5 = '1'")
rcq1factor$rcq_12 <- Recode(rcq1factor$rcq_12, "1='5' ; 2='4' ; 3 = '3'; 3 = '3'; 4 = '2'; 5 = '1'")

## fa with 1 factor
fa1 <- fa.poly(rcq1factor, nfactors = 1,  fm="minres")
print(fa1, cut = .3)

## 2-factor model
rcq2factor  <- rcqEfa_complete
fa2 <- fa.poly(rcq2factor, nfactors = 2, rotate="oblimin", fm="minres")
print(fa2, cut = .3)

## 3-factor model
rcq3factor  <- rcqEfa_complete

## fa with 3 factor
fa3 <- fa.poly(rcq3factor, nfactors = 3)
print(fa3, cut = .3)

### Reliability ----
alpha(rcq2factor)

# 1st factor
alpha(rcq2factor[,c(1,3,4,5,8,9,10,12)])

# 2nd factor
alpha(rcq2factor[,-c(1,3,4,5,8,9,10,12)])

### CFA ----
rcqCfa_complete  <- rcqCfa[,22:33]

# 1-factor
cfa.rcq1f  <- rcqCfa_complete 

# Recode preContemplation into Contemplation
cfa.rcq1f$rcq_1 <- Recode(cfa.rcq1f$rcq_1, "1='5' ; 2='4' ; 3 = '3'; 3 = '3'; 4 = '2'; 5 = '1'")
cfa.rcq1f$rcq_5 <- Recode(cfa.rcq1f$rcq_5, "1='5' ; 2='4' ; 3 = '3'; 3 = '3'; 4 = '2'; 5 = '1'")
cfa.rcq1f$rcq_10 <- Recode(cfa.rcq1f$rcq_10, "1='5' ; 2='4' ; 3 = '3'; 3 = '3'; 4 = '2'; 5 = '1'")
cfa.rcq1f$rcq_12 <- Recode(cfa.rcq1f$rcq_12, "1='5' ; 2='4' ; 3 = '3'; 3 = '3'; 4 = '2'; 5 = '1'")


# 1-Factor Model
RCQ.1f.MODEL  <- ' # Latent variables
                   readiness       =~ rcq_1 + rcq_2 + rcq_3 + rcq_4
                                   + rcq_5 + rcq_6 + rcq_7 + rcq_8
                                   + rcq_9 + rcq_10 + rcq_11 + rcq_12'

cfa1f  <- cfa(RCQ.1f.MODEL,  data = cfa.rcq1f)

# 2-Factor Model 
RCQ.2f.MODEL  <- ' # Latent variables
                   Con   =~ rcq_1 + rcq_3 + rcq_4 + rcq_5 + 
                            rcq_8 + rcq_9 + rcq_10 + rcq_12                                   
                   Action =~ rcq_2 + rcq_6 + rcq_7 + rcq_11                 
                '

cfa2f  <- cfa(RCQ.2f.MODEL,  data = rcqCfa_complete)

# 3-Factor Model with Correlation
RCQ.3fr.MODEL  <- '# Latent variables                  
                  Con       =~ rcq_1 + rcq_5 + rcq_10 + rcq_12
                  Precont   =~ rcq_3 + rcq_4 + rcq_8 + rcq_9
                  Action    =~ rcq_2 + rcq_6 + rcq_7 + rcq_11                 
                '

cfa3fr  <- cfa(RCQ.3fr.MODEL,  data = rcqCfa_complete)

# 3-Factor Model with no Correlation
RCQ.3f.MODEL  <- '# Latent variables                  
                  Con       =~ rcq_1 + rcq_5 + rcq_10 + rcq_12
                  Precont   =~ rcq_3 + rcq_4 + rcq_8 + rcq_9
                  Action    =~ rcq_2 + rcq_6 + rcq_7 + rcq_11                                                
                '

cfa3f  <- cfa(RCQ.3f.MODEL,  data = rcqCfa_complete, orthogonal=TRUE)


## Summary
summary(cfa1f, standardized=TRUE, fit.measures=TRUE, rsq=TRUE, modindices=TRUE)
summary(cfa2f, standardized=TRUE, fit.measures=TRUE, rsq=TRUE, modindices=TRUE)
summary(cfa3f, standardized=TRUE, fit.measures=TRUE, rsq=TRUE, modindices=TRUE)


# 1-Factor Improved Reduced with the best loadings
RCQ.1fi.MODEL  <- ' # Latent variables
                   readiness       =~ rcq_1 + rcq_4 + rcq_8 +  rcq_10'

cfa1fi  <- cfa(RCQ.1fi.MODEL,  data = rcq1factor)

summary(cfa1fi, standardized=TRUE, fit.measures=TRUE, rsq=TRUE, modindices=TRUE)



# Fit measures
rbind(
round(fitMeasures(cfa1fi, fit.measures = c("chisq", "df", "cfi", "rmsea", "rmsea.ci.lower", "rmsea.ci.upper", "bic", "aic")),3) , 
round(fitMeasures(cfa2f, fit.measures = c("chisq", "df", "cfi", "rmsea", "rmsea.ci.lower", "rmsea.ci.upper", "bic", "aic")),3) , 
round(fitMeasures(cfa1f, fit.measures = c("chisq", "df", "cfi", "rmsea", "rmsea.ci.lower", "rmsea.ci.upper", "bic", "aic")),3),
round(fitMeasures(cfa3f, fit.measures = c("chisq", "df", "cfi", "rmsea", "rmsea.ci.lower", "rmsea.ci.upper", "bic", "aic")),3)
)

## Anova
anova(cfa1f, cfa2f, cfa1fi, cfa3f)

# Graphing solution
library(qgraph)
qgraph(cfa1fi, layout = "tree", titles = FALSE)

# Alpha
alpha(cfa.rcq1f[,c(1,4,8,10)])

# 2-Factor Model 
RCQ.2fb.MODEL  <- ' # Latent variables
Con   =~ rcq_1 + rcq_3 + rcq_3 +  
rcq_8 + rcq_9 + rcq_10 + rcq_12                                   
Action =~ rcq_6 + rcq_7 + rcq_11                 
'

cfa2fb  <- cfa(RCQ.2fb.MODEL,  data = rcq1factor)

summary(cfa2fb, standardized=TRUE, fit.measures=TRUE, rsq=TRUE, modindices=TRUE)


# Final Scale sum
sumrcq  <- (rcq[,c("rcq_1","rcq_4","rcq_8","rcq_10")])

# Recode 1 to 10
sumrcq$rcq_1 <- Recode(sumrcq$rcq_1, "1='2'  ; 2='1'  ; 3 = '0'; 4 = '-1'; 5 = '-2'")
sumrcq$rcq_4 <- Recode(sumrcq$rcq_4, "1='-2' ; 2='-1' ; 3 = '0'; 4 = ' 1'; 5 = ' 2'")
sumrcq$rcq_8 <- Recode(sumrcq$rcq_8, "1='-2' ; 2='-1' ; 3 = '0'; 4 = ' 1'; 5 = ' 2'")
sumrcq$rcq_10 <- Recode(sumrcq$rcq_10, "1='2'; 2='1'  ; 3 = '0'; 4 = '-1'; 5 = '-2'")

# Sum vari
sumrcq$sum  <- sumrcq$rcq_1 + sumrcq$rcq_4 + sumrcq$rcq_8 + sumrcq$rcq_10

# summary
describe(sumrcq$sum)

