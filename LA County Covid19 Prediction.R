##################################
# Covid19 Challenge: LMU MSBA Team
##################################


#################  Introduction ################# 
# This script is used for predicting the Covid19 infected and deceased cases
# in LA county, the predictions will then be used as the based of the masurement
# of hazard that Covid19 imposed to all heigborhoods in LA county


#################  setup the working environment ################# 
# please change the path accroding to you preference  
setwd("E:/OneDrive - lmu.edu/Covid19 GRMDS Challenge")
# solve the ordinary differential equation system in compartmental model 
library(deSolve)
# data manipulaiton and visulization
library(tidyverse)
library(ggplot2)
# parallal computation for fine turning the model parameters
library(foreach)
library(doParallel)
library(parallel)
library(readr)





################# Covid19 cases/deaths data for LA county ################# 
# cases: total infected cases
# deaths: total deceased cases


# load LA Country covid19 case data from LA Times
# https://raw.githubusercontent.com/datadesk/california-coronavirus-data/master/latimes-county-totals.csv
Covid_19 <- read_csv(url('https://raw.githubusercontent.com/datadesk/california-coronavirus-data/master/latimes-county-totals.csv'))

# Note that the total populaiton of LA county is 10039107

LA <- Covid_19 %>% filter(county == "Los Angeles")
LA$date <- as.Date(LA$date, format = c("%Y-%m-%d"))

# filter the date until the data is stable and less noisy
initial_date <- c("2020-3-24")
LA <- LA %>% filter(date >= initial_date)
colnames(LA) <- c("date", "county", "fips", "case_total", "death_total", "case_increase", "death_increase")

# very important: making the data assending in date, so that the order of data could match you prediction 
LA <- LA %>% arrange(date)

# separate the case and death datasets
#LA_Case <- LA %>% select(date, confirmed_cases, new_confirmed_cases)
#colnames(LA_Case) <- c("date", "case_total", "case_increase")
#LA_Death <- LA %>% select(date, deaths, new_deaths)
#colnames(LA_Death) <- c("date", "death_total", "death_increase")


# if you use JHU data and downloaded the data to your local
#LA <- read.csv("LA County.csv")
#LA <- LA[, c(1, 3, 4, 5)]
#LA$Date <- as.Date(LA$Date, format = c("%m/%d/%Y"))

# transfer the table from long to wide
# for infected cases
#LA_Case <- LA %>% filter(Case_Type == "Confirmed") %>% select(-1)
#colnames(LA_Case) <- c("case_total", "case_increase", "date")
# for deceased cases
#LA_Death <- LA %>% filter(Case_Type == "Deaths") %>% select(-1)
#colnames(LA_Death) <- c("death_total", "death_increase", "date")
# combind the two datasets
#LA <- left_join(LA_Case, LA_Death, by = c("date"))
#LA <- LA %>% arrange(date)
 



# plot the raw case/death data
ggplot(LA, aes(x = date, y = death_total)) + geom_point() + ggtitle("LA Daily Total Death")
ggplot(LA, aes(x = date, y = case_total)) + geom_point() + ggtitle("LA Daily Total Case")
ggplot(LA, aes(x = date, y = death_increase)) + geom_point() + ggtitle("LA Daily Death Increase")
ggplot(LA, aes(x = date, y = case_increase)) + geom_point() + ggtitle("LA Daily Case Increase")



################# Setup the Ordinary Derivative Function #################
##### Model description
# Model: SEIR with variable reproduction number across time
# 
# parameters: gamma (inverse of latency period), 
#             sigma (inversse of infectious period) , 
#             mu (death rate in unit infectious period), 
#             r(reproduction number) 
#             fatigue (which is the derivative of r, capturing the shutdown fatigue)
#             beta (transmission rate) is derived from the fact that r = beta/sigma 
#             refrence for the priors of the parameter values: https://gabgoh.github.io/COVID/index.html

##### Variable reproduction number
# core idea: 
#        fine tune the reproduction number at different stages to fit the reality
#        that states can be lock-down and gradual reopening 
#
# lockdown fatigue: 
#        note that at lock-down stage, people could disobey the shelter-at-home order 
#        especially the lockdown has lasted a long period 
#        thus, we add a fatigue factor as the derivative of reprodutoin number to capture its dynamics. 
#
# base reproduction number and fatigue rates
#        in the model, we have two base reproduction number (r0, r_t1) for two stages: 
#        locakdown (intital date - May 8), reopen (May 9 - now)
#        and each base reproduction number has its own fatigue factor (f0, f_t1) 
# 


##### Ordinary differential equation system in SEIR
# Here we leverage the "ode" function in the deSolve package for efficiency
# per the setting of ode function ,the input differential equation system
# must be in the format (t, state, parameters)
#      t: time length,
#      state: initial values of the state variables
#      parameters: the parameters in the equation system

# please refer to our report for the mathmetical representation of the equation system 
Covid_Dyn <- function (t, state, parameters) {
  with(as.list(c(state,parameters)),             
       {
         N <- S + E + I + R
         beta <- r * sigma
         # flow diagram or derivatives
         dS  <-  - S/N * beta * I                
         dE <- S/N * beta * I - gamma * E        
         dI <- gamma * E - sigma * I             
         dR <- (1 - mu) * sigma * I           
         dDead <- mu * sigma * I              
         dOnset <- gamma * E
         dr <- fatigue
         dfatigue <- 0
         # collect all dirivatives
         dx <- c(dS, dE, dI, dR, dDead, dOnset, dr, dfatigue)
         list(dx)
       }
  )
}

################# Test the SEIR Simultion #################
# before we fit the SEIR model to observed data
# we want to test the simulation first


###### Setup the time schedule of LA county 
# total time length in days
t <- nrow(LA)     
times <- seq(1, t, by = 1)
# https://covid19.ca.gov/stay-home-except-for-essential-needs/
# March 19 shelter at home in effect
# May 8 the order is released

# t1 donotes the day in which LA county starts reopen 
t1 <- as.numeric(as.Date("2020-05-08") - as.Date("2020-03-24"))


######### Test the Simulation 

###### initialize the values of state parameters
# state: S, E, I, R, N
# according to the data on the initial date:
# N: total population of LA county
N <- 10039107                 
# E0: initial value of exposed group 
#     (half of the total cases of 2020-3-24 in the LA county data) 
E0 <- 325
# I0: initial value of infecious group 
#     (half of the total cases of 2020-3-24 in the LA county data) 
I0 <- 325
# Dead0: initial value of deceased group
#       (from the death number of 2020-3-24 in the LA county data)
Dead0 <- 11

######  Inital values of parameters that we will learn
# base reproduction number in lockdown period
r0 <- 1
# lock-down fatigue in lockdown period
f0 <- 0.005
# base reproduction number in reopening period
r_t1 <- 1.5
# lock-down fatigue in reopening period
f_t1 <- 0.006

# start values of state variable 
xstart <- c(S = N - E0 - I0, E = E0,  
            I = I0, R = Dead0,
            Dead = Dead0, Onset = 0, r = r0, fatigue = f0) 

# start values of parameters
par <- c(0.2, 0.2, 0.2, r0, f0, r_t1, f_t1)
par <- c(gamma = par[1], sigma = par[2],
         mu = par[3], r = par[4], fatigue = par[5], r_t1 = par[6], f_t1 = par[7]) 

# set up the intervention schedule: scenarios of changing from lockdown to reopen 
intervention <- data.frame(var = c("r","fatigue"), time = c(t1),
                       value = c(r_t1, f_t1), method = "rep")       

# run the simulation
simu <- as.data.frame(ode(y = xstart, times = times, 
                         func = Covid_Dyn, parms = par,
                         events = list(data = intervention))) 

###### Simulation check point 
#      see if the simulation unrolled as expected
#      especial the shift from lock-down to reopen



################# Fine-tune the SEIR model with observed data #################
##### Core idea
#     define a Loss function, calculating the mean squared error between the simulated number and ground truth
#     fine-tune the model parameters by minimize the loss

##### Method of fine-tuning: grid search
#     we tried to use search algorithms like BFGS, but the algorithms are always trapped in local minimal
#     generating nonsense parameters
#     we therefore swithc to grid search method
#     using previous reserach to set ranges of parameter values
#     then generating combinations of those values
#     then simulate with those combinations in training and validate datasets
#     and lastly, select those satisfactory combinations to apply to the test dataset

##### Parameter Pool: par_grid
# refrence for the priors of the parameter values: https://gabgoh.github.io/COVID/index.html
gamma <- seq(0.15, 0.25, by = 0.03)
sigma <- seq(0.1, 0.5, by = 0.2)
mu <- seq(0.05, 0.15, by = 0.015)
r <- seq(0.8, 2, by = 0.04)
fatigue <- seq(0.001, 0.01, by = 0.002)
r_t1 <- seq(0.8, 2, by = 0.04)
f_t1 <- seq(0.001, 0.02, by = 0.003)

# generate the value combinations
par_grid <- expand.grid(gamma, sigma, mu, r, fatigue, r_t1, f_t1)
colnames(par_grid) <- c("gamma", "sigma", "mu", "r", "fatigue", "r_t1", "f_t1")

# check dataset size in MB
object.size(par_grid)/1024/1024


########### Loss Total Function ############
###### split the train, valid, and test datasts
# 80% obs for training
train_len <- round(nrow(LA) * 0.8)
# 14% obs for validating
valid_len <- round(nrow(LA) * 0.14)

# thus we will have 7 obs for testing 
# which is consistent with many COVID19 predictive models that test predictions for one week


#####  define the Loss_Total function:
#      it is called loss total becasue it will calculate the loss in training, valid, and test datasets
#      note that the input argument is only one scaler par_id 
#      par_id extract one parameter vector from parameter pool(par_grid) 
#      the parameter vector has seven elevements
#          five for the covid_dyn function:  
#              gamma, sigma, mu, r (lock-down stage), fatigue (lock-down stage)
#          the two extras are r_t1 and f_t1 for intervention schedule:
#              r_t1 and f_t1 the new reproduction number and fatigue in reopenning stage

Loss_Total <- function (par_id) {
    # pull one parameter vector from paramter pool
    par <- as.vector(par_grid[par_id, ])
    
    # Note that we need to use parallel computation for grid searching 
    # to make the coding task easy later, 
    # we put all the small environmental variables in the function itself
    # so that they do not need to explictly export to each cluster 
    t1 <- 45
    train_len <- round(nrow(LA) * 0.8)
    valid_len <- round(nrow(LA) * 0.14)
    N <- 10039107                 
    E0 <- 325
    I0 <- 325
    Dead0 <- 11
    
    
    # starting values of state variable
    xstart <- c(S = N - E0 - I0, E = E0,  
              I = I0, R = Dead0,
              Dead = Dead0, Onset = 0, unlist(par[4]), unlist(par[5])) 
    
    # time schedule of the simulation  
    times <- seq(1:nrow(LA))
    
    # order of elements in the parameter vector
    #    c(gamma = par[1], sigma = par[2],
    #      mu = par[3], r = par[4], fatigue = par[5], r_t1 = par[6], f_t1 = par[7]) 
    
    # intervention schedule
    intervention <- data.frame(var = c("r","fatigue"), time = c(t1),
                             value = c(unlist(par[6]), unlist(par[7])), method = "rep")      
    
    # run the simulation   
    simu <- as.data.frame(ode(y = xstart, times = times, 
                            func = Covid_Dyn, parms = par[1:5],
                            events = list(data = intervention))) 

    # note that we defined 4 different loss fucntion
    # 1: mean squared error (MSE) of predicted total deaths only
    # 2. MSE of predicted deaths and predicted infected cases
    # 3. MSE of predicted deaths and absolute error (ABS) of predicted infected cases
    #        since infected cases is of a much larger magnitude compared with deaths
    #        using ABS helps to balance the focus on the two losses
    # 4. MSE of predicted infected cases only
    
    # MSE of deaths
    error1 <- (simu$Dead - LA$death_total)^2
    loss_train1 <- mean(error1[1: train_len])
    loss_valid1 <- mean(error1[(train_len + 1): (train_len + valid_len)])
    loss_test1 <- mean(error1[(train_len + valid_len + 1): nrow(LA)])   
  
    # MSE of (deaths + cases)
    error2 <- (simu$Dead - LA$death_total)^2 + (N - simu$S - LA$case_total)^2
    loss_train2 <- mean(error2[1: train_len])
    loss_valid2 <- mean(error2[(train_len + 1): (train_len + valid_len)])
    loss_test2 <- mean(error2[(train_len + valid_len + 1): nrow(LA)])   

    # MSE of daths + ABS(cases)
    error3 <- (simu$Dead - LA$death_total)^2 + abs(N - simu$S - LA$case_total)
    loss_train3 <- mean(error3[1: train_len])
    loss_valid3 <- mean(error3[(train_len + 1): (train_len + valid_len)])
    loss_test3 <- mean(error3[(train_len + valid_len + 1): nrow(LA)])   
  
    # MSE of case
    error4 <- (N - simu$S - LA$case_total)^2
    loss_train4 <- mean(error4[1: train_len])
    loss_valid4 <- mean(error4[(train_len + 1): (train_len + valid_len)])
    loss_test4 <- mean(error4[(train_len + valid_len + 1): nrow(LA)])   
    
    return(c(loss_train1, loss_valid1, loss_test1, 
             loss_train2, loss_valid2, loss_test2, 
             loss_train3, loss_valid3, loss_test3, 
             loss_train4, loss_valid4, loss_test4))
}


##### Test the Loss_Total function 
Loss_Total(1)

# test a sapply function
test <- sapply(c(1:1000), Loss_Total, simplify = T)



########### Parallel Computation ############
# select the number of cores/clusters of the task
# please adjust the number of cores accordingly
cl <- makeCluster(25)

# Export necessary datasets, functions, and packages to each cluster 
clusterExport(cl, "LA")
clusterExport(cl, "par_grid")
clusterExport(cl, "Covid_Dyn")
clusterEvalQ(cl, library(deSolve))


# parSapply is just Parallal version Sapply
ptm <- proc.time()
loss_temp <- parSapply(cl, c(1:nrow(par_grid)), Loss_Total, simplify = T)
proc.time() - ptm
# stop the clusters after computing
stopCluster(cl)

# loss_mat is the matrix for all the loss infomation
loss_mat <- t(loss_temp)
object.size(loss_mat)
colnames(loss_mat) <- c("loss_train1", "loss_valid1", "loss_test1", 
                        "loss_train2", "loss_valid2", "loss_test2",
                        "loss_train3", "loss_valid3", "loss_test3", 
                        "loss_train4", "loss_valid4", "loss_test4")

loss_mat <- as.data.frame(loss_mat)

# match the loss info back to the corresponding parameter vector
# par_loss is the dataset we will work on for paramter selection and prediction
par_loss <- cbind(par_grid, loss_mat)
par_loss$id <- c(1:nrow(par_loss))

# save the results
write.csv(par_loss, "par_loss.csv")


########### Simulate with the selected parameters ############
# before we move to select those good performance parameter combinations
# let's first define a function for making prediction based on those combinations
Predict <- function (par) {
  # note that the input of Predict function is 
  # one a vector of parameters from par_loss dataset, not a par_id
  # therefore, we will use apply function with Predict later.

  t1 <- 45
  N <- 10039107                 
  E0 <- 325
  I0 <- 325
  Dead0 <- 11
  times <- seq(1, nrow(LA), by = 1)
    
  xstart <- c(S = N - E0 - I0, E = E0,  
              I = I0, R = Dead0,
              Dead = Dead0, Onset = 0, unlist(par[4]), unlist(par[5])) 
  
  intervention <- data.frame(var = c("r","fatigue"), time = c(t1),
                             value = c(unlist(par[6]), unlist(par[7])), method = "rep")      
  simu <- as.data.frame(ode(y = xstart, times = times, 
                            func = Covid_Dyn, parms = par[1:5],
                            events = list(data = intervention))) 
  return(simu)
}

# test the prediction function 
Predict(par_loss[1,c("gamma", "sigma", "mu", "r", "fatigue", "r_t1", "f_t1")])


########### Select the models(parameter combinations) ############
###### prevent overfitting
#       to prevent overfitting, 
#       we first choose the top 10000 models with smallest loss in training dataset
#       from those models, we choice 10 with the smallest loss in validate dataset
#       comparing with directly choosing 10 models with the smallest 
#       sum of training loss and validate loss
#       our approach has better performence in test dataset
#       of course, the 10000 and 10 are the hyperparameters we can work on later

# par_group are the selected models
###### Selecting diffrent loss function
#       Note that, in the two par_group codes below, we could choose to select models 
#       acoording to different loss functions by change the suffix (1-4) of the loss_train and loss_valid

####### Models for predicting deceased casese
# model for predicting deaths using loss function 1 
par_group <- par_loss %>% top_n(-10000, loss_train1) %>% top_n(-10, loss_valid1)


# later we will calculate the weighted average of the predictions from the selected models
# the weights is negative related to their prediction loss
par_group <- par_group %>% 
  mutate(loss = loss_train1 + loss_valid1) %>% arrange(desc(loss)) %>% 
  mutate(rank = 1:n(),weight = rank/sum(1:10))
# save the selected model 
write.csv(par_group,"deaths_model.csv")


####### Models for predicting infected casese
# mdoel fro predicting infected cases using loss function 4
par_group <- par_loss %>% top_n(-10000, loss_train4) %>% top_n(-10, loss_valid4)


par_group <- par_group %>% 
  mutate(loss = loss_train4 + loss_valid4) %>% arrange(desc(loss)) %>% 
  mutate(rank = 1:n(), weight = rank/sum(1:10))


write.csv(par_group,"cases_model.csv")



###### Making predictions with the selected model 
prediction_temp <- apply(par_group[,c("gamma", "sigma", "mu", "r", "fatigue", "r_t1", "f_t1")], 1, Predict)

# the output of our apply function is a list of matrix
# we need to merge it into one matrix
prediction <- bind_rows(prediction_temp, .id = "par_group")

# add weights info to the prediction
prediction$weight <- rep(par_group$weight, each = nrow(LA))

###### calculate the predicted total infected and deceased cases
# N is total population in LA county
# S is the suspectible group, thus N - S is our prediction on total infected cases
# note that we calculate both weighted average, and the 95% confidence interval 
# with lower end is 2.5% and higher end at 97.5%  
N <- 10039107
prediction <- prediction %>% group_by(time) %>% mutate(Positve = N - S) %>% 
  summarize(dead = sum(Dead * weight), 
            dead_low = quantile(Dead, 0.025),
            dead_high = quantile(Dead, 0.975),
            positive = sum(Positve * weight), 
            positive_low = quantile(Positve, 0.025),
            positive_high = quantile(Positve, 0.975))

# now we calculate the predicted daily incrases of infected and deceased cases
prediction <- cbind(prediction, LA) %>% arrange(date) %>% 
  mutate(dead_increase = dead - lag(dead), positive_increase = positive - lag(positive))

##### generate the benchmark predction 
# bence mark prediction is to use the previous week( or 7 days)'s average as prediciton for current week (or 7 days)
# here we generate the benchmark predictions of total infected cases and deaths, and their daily increase

prediction <- prediction %>% 
  mutate(week_id = c(rep(c(1: (nrow(LA)%/%7)), each = 7), rep(nrow(LA)%/%7 + 1, nrow(LA)%%7))) %>% group_by(week_id) %>% 
  mutate(bench_case_temp = mean(case_increase), bench_death_temp = mean(death_increase)) %>% 
  ungroup() %>% arrange(date) %>% 
  mutate(bench_case_increase = lag(bench_case_temp, 7), 
         bench_death_increase = lag(bench_death_temp, 7)) %>% 
  mutate(bench_case_increase = ifelse(is.na(bench_case_increase), 0, bench_case_increase),
         bench_death_increase = ifelse(is.na(bench_death_increase), 0, bench_death_increase)) %>%
  mutate(bench_case_total = cumsum(bench_case_increase),
         bench_death_total = cumsum(bench_death_increase),
         bench_case_total = bench_case_total + prediction[7,"case_total"],
         bench_death_total = bench_death_total + prediction[7,"death_total"])


prediction[c(1:7),c("bench_case_increase", "bench_case_total",
                     "bench_death_increase", "bench_death_total")] <-NA  
         
##### Summarize the model performance
# two types of losses for our model and benchmark model: 
# mean square error in all data and test data 
loss_summary <- prediction %>% summarise(my_loss_death_total = mean((dead - death_total)^2),
                                         bench_loss_death_total = mean((bench_death_total - death_total)^2, na.rm = T),
                                         my_loss_case_total = mean((positive - case_total)^2),
                                         bench_loss_case_total = mean((bench_case_total - case_total)^2, na.rm = T),
                                         my_loss_death_total_test = mean(c((dead - death_total)^2)[-c(1:66)]),
                                         bench_loss_death_total_test = mean(c((bench_death_total - death_total)^2)[-c(1:66)], na.rm = T),
                                         my_loss_case_total_test = mean(c((positive - case_total)^2)[-c(1:66)]),
                                         bench_loss_case_total_test = mean(c((bench_case_total - case_total)^2)[-c(1:66)], na.rm = T))

view(loss_summary)

####### save the model fitting

write.csv(prediction, "deaths_fit.csv")
write.csv(loss_summary, "deaths_fit_summary.csv")

write.csv(prediction, "cases_fit.csv")
write.csv(loss_summary, "cases_fit_summary.csv")

####### visulize the prediction
# black: groud truth; blue: our prediction; red: benchmark prediction
ggplot(prediction, aes(x = date, y = death_total)) + geom_point() +
  geom_point(aes(y = dead), color = "blue") +
  geom_point(aes(y = bench_death_total), color = "red") +
  ggtitle("Predicted Total Deaths")

ggplot(prediction, aes(x = date, y = death_increase)) + geom_line() +
  geom_point(aes(y = dead_increase), color = "blue") +
  geom_point(aes(y = bench_death_increase), color = "red") +
  ggtitle("Predicted Daily Deaths")

sqrt(loss_summary[5])

ggplot(prediction, aes(x = date, y = case_total)) + geom_point() +
  geom_point(aes(y = positive), color = "blue") +
  geom_point(aes(y = bench_case_total), color = "red") +
  ggtitle("Predicted Total Infected Cases")

ggplot(prediction, aes(x = date, y = case_increase)) + geom_line() +
  geom_point(aes(y = positive_increase), color = "blue") +
  geom_point(aes(y = bench_case_increase), color = "red") +
  ggtitle("Predicted Daily Infected Cases")

sqrt(loss_summary[7])


########### Make Final Predictons to the Future ###########
# Note that our model is trained and tested on data until June-4-2020
# we than make prediction for one week later

# predicting death
par_group <- par_loss %>% top_n(-10000, (loss_train1 + loss_valid1)) %>% top_n(-10, loss_test1)
par_group <- par_group %>% 
  mutate(loss = loss_train1 + loss_valid1 + loss_test1) %>% arrange(desc(loss)) %>% 
  mutate(rank = 1:n(), weight = rank/sum(1:10))
write.csv(par_group,"deaths_model.csv")

# predicting confirmed cases
par_group <- par_loss %>% top_n(-10000, (loss_train4 + loss_valid4)) %>% top_n(-10, loss_test4) 

par_group <- par_group %>% 
  mutate(loss = loss_train4 + loss_valid4 + loss_test4) %>% arrange(desc(loss)) %>% 
  mutate(rank = 1:n(), weight = rank/sum(1:10))
write.csv(par_group,"cases_model.csv")

Predict_New <- function (par, n) {
  # n is the number of days we want to predict for the future 
  
  t1 <- 45
  N <- 10039107                 
  E0 <- 325
  I0 <- 325
  Dead0 <- 11
  times <- seq(1, (nrow(LA) + n), by = 1)
  
  xstart <- c(S = N - E0 - I0, E = E0,  
              I = I0, R = Dead0,
              Dead = Dead0, Onset = 0, unlist(par[4]), unlist(par[5])) 
  
  intervention <- data.frame(var = c("r","fatigue"), time = c(t1),
                             value = c(unlist(par[6]), unlist(par[7])), method = "rep")      
  simu <- as.data.frame(ode(y = xstart, times = times, 
                            func = Covid_Dyn, parms = par[1:5],
                            events = list(data = intervention))) 
  return(simu)
}


prediction_temp <- apply(par_group[,c("gamma", "sigma", "mu", "r", "fatigue", "r_t1", "f_t1")], 
                         n = 7, MARGIN = 1, FUN = Predict_New)

# the output of our apply function is a list of matrix
# we need to merge it into one matrix
prediction <- bind_rows(prediction_temp, .id = "par_group")

# add weights info to the prediction
# note 80 is nrow(LA) + n
prediction$weight <- rep(par_group$weight, each = nrow(LA) + 7)

###### calculate the predicted total infected and deceased cases
# N is total population in LA county
# S is the suspectible group, thus N - S is our prediction on total infected cases
# note that we calculate both weighted average, and the 95% confidence interval 
# with lower end is 2.5% and higher end at 97.5%  
N <- 10039107
prediction_new <- prediction %>% group_by(time) %>% mutate(Positve = N - S) %>% 
  summarize(dead = sum(Dead * weight), 
            dead_low = quantile(Dead, 0.025),
            dead_high = quantile(Dead, 0.975),
            positive = sum(Positve * weight), 
            positive_low = quantile(Positve, 0.025),
            positive_high = quantile(Positve, 0.975)) %>% tail(7)

###### save the prediction
write.csv(prediction_new, "deaths_prediction.csv")
write.csv(prediction_new, "cases_prediction.csv")
