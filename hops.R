hops <- read.csv("Hospital Costs.csv")
head(hops)
dim(hops)


# To find the age category of people who frequent the hospital
summary(as.factor(hops$AGE))
# Output: AGE = 0


#To find age category of people who have maximum expenditure
library(dplyr)
df1 <- summarise(group_by(hops, AGE), TotChgAge = sum(TOTCHG))
arrange(df1, desc(TotChgAge))[1,]
# Output: AGE = 0


# To find the diagnosis-related group that has maximum hospitalization
df2 <- summarise(group_by(hops, APRDRG), TotLos = sum(LOS))
arrange(df2, desc(TotLos))[1,]
# Output: 640


# To find the diagnosis-related group that has maximum expenditure
df3 <- summarise(group_by(hops, APRDRG), TotChgGroup = sum(TOTCHG))
arrange(df3, desc(TotChgGroup))[1,]
# Output: 640 


# To analyze if the race of the patient is related to the hospitalization costs
aov1 <- aov(TOTCHG ~ RACE, hops)
# Ho: The race of the patient is not related to the hospitalization costs
# Ha: The race of the patient is related to the hospitalization costs
alpha <- 0.05
p_value <- summary(aov1)[[1]]["Pr(>F)"][1,]
p_value < alpha #False
# Output: We do not reject our null hypothesis and hence can make sure that there is no malpractice


# To analyze the severity of the hospital costs by age and gender
aov2 <- aov(TOTCHG ~ AGE + FEMALE, hops)
summary(aov2)
# Output: Both age and gender are significant to the hospital costs and hence affect the same

 
# To find if the length of stay can be predicted from age, gender, and race
hops$FEMALE <- as.factor(hops$FEMALE)
hops$RACE <- as.factor(hops$RACE)
model1 <- lm(LOS ~ AGE + FEMALE + RACE, hops)
summary(model1)
# Output: Age, gender, and race are not significant to the length of stay and thus cannot predict the same


# To find the variable that mainly affects hospital costs
model2 <- lm(TOTCHG ~ ., hops)
summary(model2)
# Output: Age, Length of stay and All Patient Refined Diagnosis Related Groups mainly affect the hospital costs












