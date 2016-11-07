#------ RETRIEVE DATA  -------

# Merging dataframes with addicional data - demographics + audit

## Putting same names in different data frames
names(audit)[names(audit)=="client_ID"] <- "client_id"
names(registration_form)[names(registration_form)=="client_ID"] <- "client_id"

## Merging data frames using "client_id" as key
rcqMerged  <- merge(audit, registration_form, by.x="client_id", by.y="client_id", all= TRUE) # create temporary data.frame
rcqFinal  <- merge(rcqMerged, rcq_motivation, by.x="client_id", by.y="client_ID", all = TRUE)
rm(rcqMerged) # removing the temporary dataframe

## Convert  timestamp to R date format
rcqFinal$Timestamp.x <- as.Date(rcqFinal$Timestamp.x, "%d.%m.%Y")

## Subset data from the period 2013-10-21 until  2014-03-11
rcq  <- subset(rcqFinal, rcqFinal$Timestamp.x >= "2013-10-21" & rcqFinal$Timestamp.x <= "2014-03-11")

## Select just valid cases for RCQ
rcq  <- subset(rcq, rcq$rcq_1 != "NA" & rcq$rcq_2 != "NA" & rcq$rcq_1 != "NA" & rcq$Research == "Sim")

## Final dataframe
rcq  <- rcq[, c("client_id", "Timestamp.x", "audit_1", "audit_2", "audit_3", "audit_4", "audit_5", "audit_6", "audit_7", "audit_8", "audit_9", "audit_10", "Result", "sex", "age", "education", "Province", "País", "Work.situation", "rcq_1", "rcq_2", "rcq_3","rcq_4","rcq_5","rcq_6","rcq_7","rcq_8","rcq_9","rcq_10","rcq_11","rcq_12","rcq_a","rcq_b","rcq_c","rcq_outcome")]


## Save final data.frame as csv file
setwd("~/rcq-ptbr/")
write.csv(rcq, "rcq.csv")

#------ PREPARE PAPER DATA -------

## Open dataframe 
rcq  <- read.csv("rcq.csv")

##---- Transform and compute vars ---------

### Audit Zones
rcq$auditRec[rcq$Result <= 7]  <-  "Low Risk"
rcq$auditRec[rcq$Result > 7 &  rcq$Result < 20]  <-  "Hazardous/harmful risk"
rcq$auditRec[rcq$Result >= 20]  <-  "Dependency"
rcq$auditRec  <- as.factor(rcq$auditRec) # transforming var as a factor

### Sex
rcq$sex <- factor(rcq$sex, labels=c("Female","Male"))

### Education
rcq$education <- factor(rcq$education, labels=c("High School Comp","High School Incomp", "Elementary", "Graduate", "College", "College Incomp."))

### Work situation
rcq$Work.situation  <- factor(rcq$Work.situation, labels=c("No", "Yes"))

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