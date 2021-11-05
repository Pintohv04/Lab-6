
#Jennifer,Joaquin,Lauren, and Hugo)
# Econometric 
# Lab#6 
#Professor Kevin Foster
# 11/5/2021


#  we will usingf logit and probit models with a dependent varaiable from 0 to 1 First, we load both datas  Acs_2017_ny and household_pulse  for do observation and transimed into factor, which wil not treat dummy vaiables as continuous variables. also we will analyze what factor affecting vaccination  in the West 

attach(acs2017_ny)
model_v1 <- lm(INCWAGE ~ AGE)
detach()

# we will using diferent models, where will be include wages,age and education 

model_v2 <- lm(acs2017_ny$INCWAGE ~ acs2017_ny$AGE)

model_v3 <- lm(INCWAGE ~ AGE, data = acs2017_ny)


model_logit1 <- glm(vaxx ~ EEDUC,
family = binomial, data = Household_Pulse_data)

## Creating subset group.

Household_Pulse_data$vaxx <- (Household_Pulse_data$RECVDVACC == "yes got vaxx")
is.na(Household_Pulse_data$vaxx) <- which(Household_Pulse_data$RECVDVACC == "NA") 

table(Household_Pulse_data$vaxx,Household_Pulse_data$EEDUC)

summary(Household_Pulse_data$vaxx,Household_Pulse_data$EEDUC)
summary(as.numeric(Household_Pulse_data$vaxx,Household_Pulse_data$EEDUC))

# we will be focous in the west, get the  predicted outcome of vacinnation  rate 

pick_use1 <- (Household_Pulse_data$REGION == "West")
dat_use1 <- subset(Household_Pulse_data, pick_use1)


dat_use1$RECVDVACC <- droplevels(dat_use1$RECVDVACC) 



model_logit1 <- glm(vaxx ~ EEDUC + MS + GENID_DESCRIBE,
                    family = binomial, data = dat_use1)
summary(model_logit1)

#Creating subset group.

#At this stage we will look at different subsets including gender,race, sexual orientation, to get a predictible outcome about vaccination. We belive that gender won't affect vaccination,however, edducation is one of the major factors for people to get vaccination.  

d_ph <- data.frame(model.matrix(~ dat_use1$PUBHLTH))
d_race <- data.frame(model.matrix(~ dat_use1$RRACE))
d_x <- data.frame(model.matrix(~ dat_use1$SEXUAL_ORIENTATION))
d_anx <- data.frame(model.matrix(~ dat_use1$ANXIOUS))


model_probit1 <- glm(vaxx ~ EEDUC + MS + GENID_DESCRIBE,
family = binomial (link = 'probit'), data = dat_use1)
summary(model_probit1)

# the result,shows us that people with college degree,and advance college degree 
# have significal advance in vaccination rate compare with some hs and hs diploma.

#The Regressional Model reveals that race  is not statistically significant. Thus, other races does not effects on vacination rate . Meanwhile, all other variables are statistically significant  
# The regression Models reveals a nagative effect of other marrital status 




