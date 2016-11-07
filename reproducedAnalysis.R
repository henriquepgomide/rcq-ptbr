#  ANALYSIS PERFORMED BY PREVIOUS STUDIES ----
# This script is experimental and incomplete. Use it at your own risk.

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