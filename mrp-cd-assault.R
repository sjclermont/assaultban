library(arm)
library(foreign)
library(plyr)

#read in poll
poll.data <- read.csv("C:/Documents and Settings/Stephen Clermont/My Documents/Personal/R Work/MRP/CCES Short-1.csv") 
head(poll.data)
#read in Census data
census <- read.csv("C:/Documents and Settings/Stephen Clermont/My Documents/Personal/R Work/MRP/poststratification 2016.csv")

#Create index variables

  #At level of megapoll

poll.data$race.male <- (poll.data$.Sex *3) + poll.data$.Race #1: Women 18-24, 2: Women White, 3: Women NW, 4: Men 18-24, 5: Men White, 6: Men NW
poll.data$sex.age <- 5 * (poll.data$.Sex) + poll.data$.Age #Ten categories Women 18-24,25-34,35-44,45-64,65+ Men 18-24,25-34,35-44,45-64,65+
poll.data$sex.educ <- 3 * (poll.data$.Sex) + poll.data$.Educ #Six categories Women HS,SC,Coll Men HS,SC,Coll
poll.data$age.educ <- 3 * (poll.data$.Age -1) + poll.data$.Educ #15 categories 18-24 HS,SC,Coll 25-34 HS,SC,Coll 35-44 HS,SC,Coll 45-64 HS,SC,Coll 65+ HS,SC,Coll
poll.data$race.educ <- 3 * (poll.data$.Race -1) + poll.data$.Educ #Nine categories 18-24 HS,SC,Coll White HS,SC,Coll NonWhite HS,SC,Coll

  #Census file level (same coding as above for all variables)

census$race.male <- (census$.Sex *3) + census$.Race #1: Women 18-24, 2: Women White, 3: Women NW, 4: Men 18-24, 5: Men White, 6: Men NW
census$sex.age <- 5 * (census$.Sex) + census$.Age #Ten categories Women 18-24,25-34,35-44,45-64,65+ Men 18-24,25-34,35-44,45-64,65+
census$sex.educ <- 3 * (census$.Sex) + census$.Educ #Six categories Women HS,SC,Coll Men HS,SC,Coll
census$age.educ <- 3 * (census$.Age -1) + census$.Educ #15 categories 18-24 HS,SC,Coll 25-34 HS,SC,Coll 35-44 HS,SC,Coll 45-64 HS,SC,Coll 65+ HS,SC,Coll
census$race.educ <- 3 * (census$.Race -1) + census$.Educ #Nine categories 18-24 HS,SC,Coll White HS,SC,Coll NonWhite HS,SC,Coll


#run individual-level opinion model

individual.model <- glmer(formula = BanAssault ~ (1|race.male) + (1|sex.age) 
  + (1|sex.educ) + (1|age.educ) + (1|.CD),
  data=poll.data, weight=weight, family=binomial(link="logit"))
display(individual.model)

#examine random effects and standard errors for race-female
ranef(individual.model)$race.male
se.ranef(individual.model)$race.male

#create a prediction for each cell in Census data
cellpred <- invlogit(fixef(individual.model)["(Intercept)"]
            +ranef(individual.model)$race.male[census$race.male,1]
            +ranef(individual.model)$sex.age[census$sex.age,1]
            +ranef(individual.model)$sex.educ[census$sex.educ,1]
            +ranef(individual.model)$age.educ[census$age.educ,1]
            +ranef(individual.model)$.CD[census$.CD,1]   
               )

cellpredweighted <- cellpred * census$cpercent.pct

#calculates the percent within each precinct(weighted average of responses)
pctpred <- 100* as.vector(tapply(cellpredweighted,census$.CD,sum))
pctpred 

write.csv(pctpred, "C:/Documents and Settings/Stephen Clermont/My Documents/Personal/R Work/MRP/Assault-SupportF.csv")

