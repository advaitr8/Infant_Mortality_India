# rm(list = ls())
# install.packages("rstan")
library (dplyr)
library (rstan)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())
#install.packages("broom")
library(broom)
#Load the data
list.files()
df <- read.csv("imr_exp_revised_final.csv",
               header = TRUE,
               skip = 2)
str(df)

#Drop Andaman and Nicobar & Dadra NH & Daman and Diu & Lakshadweep & Chandigarh and 2003
df <- df[df$Year != 2003,]
str(df)
df <- df[df$State != "Andaman & Nicobar Islands"
         & df$State != "Dadra & Nagar Haveli"
         & df$State != "Daman & Diu"
         & df$State != "Lakshadweep"
         & df$State != "Chandigarh",]
str(df)

unique(df$State)

# #Fix names of Orissa, Jammu Kashmir and Chattisgarh
# df$State <- as.character ( df$State )
# df$State[ df$State == " Odisha "] <- " Orissa "
# df$State [ df$State == " Jammu  &  Kashmir "] <- " Jammu  and  Kashmir "
# df$State [ df$State == " Chhatisgarh "] <- "Chhattisgarh "

#read in population data
list.files()
pop <- read.csv("population.csv",
                header = T)
str(pop)

#attaching population to the data frame
ind <- merge(df,pop)
str(ind)

# Function for projecting population up or down
pop.func <- function (year, pop2011){
  pop <- NULL
  pop <- ifelse ( year > 2011 ,
                  pop2011 *(1.0184^( year - 2011)) ,
                  ifelse ( year < 2011,
                           pop2011 *((1/1.0184)^(2011 - year )),
                           pop2011 ))
  return (pop)
}
#Add column of projected population numbers
ind$population <- pop.func ( ind$Year , ind$X2011pop )
# Remove old population numbers
str(ind)
ind <- ind [,-8]
str(ind)

##
#Put all currency in Ruppee
ind$exp.rup <- NULL
for (i in 1: length (ind [ ,1])){
  ind$exp.rup[i] <- ifelse(ind$Units [i] == "Lakh",
                           ind$Health.Exp[i]*100000 ,
                           ind$Health.Exp[i]*1000000)
  }
  
#
#Get spending per capita
ind$percap <- ind$exp.rup/ ind$population
str(ind)

#
# Create ratio of female to male IMR
ind$fm.imr <- ind$Female / ind$Male
str(ind)

# Regress IMR ratio on health exp
summary(lm(fm.imr ~ percap ,
            data = ind ))

# Sort by year
ind <- ind[ order ( ind$Year ),]
# Create state id
ind <- within (ind , state.id <- match (State , unique ( State )))
# Create Year id
ind <- within (ind , year.id <- match (Year , unique ( Year )))

str(ind)
################################################
################# PLOTS #######################
################################################
#These plots need to be before the lag is created and 2005 is dropped
#I doubt this plot is useful
# Plot per cap health exp by state over time -----
par( mfrow = c(5, 6),
     mar = c(1.5 , 1.5 , 1, 1),
     cex = 0.6 ,
     las = 1)
for (i in 1:30){
  plot ( ind$Year [ ind$state.id == i],
         ind$percap [ ind$state.id == i],
         type = "l",
         ylim = c(0, 3000) ,
         yaxt = "n",
         xaxt = "n",
         xlab = " year ",
         main = paste ( unique ( ind$State [ ind$state.id == i])) ,
         cex.main = 1)
  axis (1, at = c(2005 , 2012) ,
        lwd = 0.5 ,
        cex.axis = 0.8)
  axis (2, at = c(0, 3000) ,
        lwd = 0.5 ,
        cex.axis = 0.6)
  title ( ylab = "exp",
          line =0, cex.lab= .6)
  title ( xlab = "yr",
          line =0, cex.lab= .6)
}


dev.off()
#To view IMR across states in 2005 and 2014 ----
state <- ind$State[1:30]
c1 <- ind$Person[ind$Year == 2005]
c2 <- ind$Person[ind$Year == 2014]
plot514_df <- data.frame(state,c1,c2)
plot514_df <- plot514_df[order(plot514_df$c1, decreasing = F),]
plot514_df
#2005 IMR
dots_05 <- plot514_df$c1
#2014 IMR
dots_14 <- plot514_df$c2

#Plot
par(mar = c(4,6,2.5,3),
    col = "gray")
plot(dots_05,1:30,
     pch = 16,
     cex = 0.8,
     col = "white",
     yaxt = 'n',
     xaxt = 'n',
     ylab = "",
     xlab = "",
     cex.lab = 0.7,
     xlim = c(0,80),
     main = "IMR in 2005 and 2014",
     cex.main = 0.8)
abline(h = c(1:30), lty = 3)
points(dots_05,1:30,
       pch = 1,
       cex = 0.8,
       col = "black")
points(dots_14,1:30,
       pch = 16,
       cex = 0.8,
       col = "black")
axis(2, 
     at = pretty(1:30,n = 30),
     labels = paste(plot514_df$state),
     las = 2, 
     lwd.tick = 0.5,
     cex.axis = 0.7, 
     col = "gray",
     tick = T)
axis(1, 
     at = (seq(0,80,length.out = 5)),
     labels = (seq(0,80,length.out = 5)), 
     col.ticks = "gray", 
     lwd.tick = 0.5, 
     cex.axis = 0.7,
     col = "gray")

title(xlab="Infant Mortality Rate", 
      line=2, cex.lab=0.8)
legend('bottomright', 
       legend = c("2014", "2005"), 
       col = "black",
       pch = c(16,1),
       cex = 0.7,
       bty = 'n',
       text.col = "black")
dev.off()
#View a trend of IMR across states from 2005 to 2014 ----
par( mfrow = c(5, 6),
     mar = c(2 , 2 , 1, 1),
     cex = 0.6 ,
     las = 1,
     col = "gray")
for (i in 1:30){
  plot ( ind$Year [ ind$state.id == i],
         ind$Person [ ind$state.id == i],
         type = "l",
         ylim = c(0, 80) ,
         yaxt = "n",
         xaxt = "n",
         xlab = "n",
         main = paste ( unique ( ind$State [ ind$state.id == i])) ,
         cex.main = 1,
         col = "black")
  axis (1, at = c(2005 , 2014) ,
        lwd = 0.5 ,
        cex.axis = 0.8,
        col = "gray")
  axis (2, at = c(0, 80) ,
        lwd = 0.5 ,
        cex.axis = 0.6,
        col = "gray")
  title ( ylab = "IMR",
          line =0, cex.lab= .8)
  title ( xlab = "year",
          line =0, cex.lab= .8)
}


dev.off()
#Gender-wise IMR by state in the time period 2005 - 2012
#View a trend of male and female deaths across states from 2005 to 2012 ----
par( mfrow = c(5, 6),
     mar = c(2 , 2 , 1, 1),
     cex = 0.6 ,
     las = 1,
     col = "gray")
for (i in 1:30){
  plot ( ind$Year[ind$state.id == i],
         ind$Male[ind$state.id == i],
         type = "l",
         ylim = c(0, 85) ,
         yaxt = "n",
         xaxt = "n",
         xlab = "n",
         main = paste(unique(ind$State[ind$state.id == i])) ,
         cex.main = 1,
         col = "blue",
         xlim = c(2005,2014))
  lines(ind$Year[ind$state.id == i],
        ind$Female[ind$state.id == i],
        col = "red")
  axis (1, at = c(2005 , 2014) ,
        lwd = 0.5 ,
        cex.axis = 0.8,
        col = "gray")
  axis (2, at = c(0, 80) ,
        lwd = 0.5 ,
        cex.axis = 0.6,
        col = "gray")
  title ( ylab = "IMR",
          line =0, cex.lab= .8)
  title ( xlab = "year",
          line =0, cex.lab= .8)
 legend ('topright', legend = c("F", "M"),
          col = c("red" , "blue"),
          lty = c(1, 1),
          bty = 'n',
          cex = 0.5,
          text.col = "black")
}






#Female to Male IMR, >1 more female deaths, <1 more male deaths ----
par( mfrow = c(5, 6),
     mar = c(2 , 2 , 1, 1),
     cex = 0.6 ,
     las = 1,
     col = "gray")
for (i in 1:30){
  plot ( ind$Year [ ind$state.id == i],
         ind$fm.imr[ ind$state.id == i],
         type = "l",
         ylim = c(0.4, 2.5) ,
         yaxt = "n",
         xaxt = "n",
         xlab = "n",
         main = paste ( unique ( ind$State [ ind$state.id == i])) ,
         cex.main = 1,
         col = "black")
  axis (1, at = c(2005 , 2014) ,
        lwd = 0.5 ,
        cex.axis = 0.8,
        col = "gray")
  axis (2, at = c(0.4,1,2.5) ,
        lwd = 0.5 ,
        cex.axis = 0.6,
        col = "gray")
  title ( ylab = "IMR Ratio",
          line =0, cex.lab= .6)
  title ( xlab = "year",
          line =0, cex.lab= .6)
  abline(h = 1, lty = 3)
}









# #Create a lagged health expenditure column

###Re order by state id for lags to work
#creating "ind_temp" so as to not mess with the original data frame "ind"
ind_temp <- ind[order(ind$state.id),]
ind_temp
ind_temp <- ind_temp %>%
  group_by(State) %>%
  mutate(lag.exp = lag(percap, order_by = State))
#check if it works
cbind(ind_temp$State, 
      ind_temp$Year, 
      ind_temp$year.id, 
      ind_temp$percap, 
      ind_temp$lag.exp)
#turns out it does, now add ind_temp$lag.exp to ind
ind <- ind[order(ind$state.id),]
ind$lag.exp <- ind_temp$lag.exp
ind
#Drop NA in Year 2005
str(ind)
ind <- ind[complete.cases(ind),]
str(ind)

#

ind <- ind[order(ind$Year),]
cbind(ind$State, 
      ind$Year, 
      ind$year.id, 
      ind$percap, 
      ind$lag.exp,
      ind$population,
      ind$exp.rup)


################################################
################# MODELS #######################
################################################


###Preparing variables for Stan Model
#years start at 2006 so re-code year.id
year_id <- ind$year.id - 1
state_id <- ind$state.id
# imr <- ind$Person
imr_ratio <- ind$fm.imr
percap_exp <- ind$lag.exp
N <- length(ind$Person)

#
#
#
#

#
#
#
#

#
#
#
#
#Model 1
#log(imr) ~  normal(state_int +
                    # time_int +
                    # beta_exp*log(percap_exp(lagged)),
                    # sigma^2)
#running Model 1 ----
#first run model_1 for females and save results
imr <- ind$Female
stanc("model_1.stan")
fit_1_f <- stan("model_1.stan",
              data = list("N",
                          "year_id",
                          "state_id",
                          "imr",
                          "percap_exp"),
              iter = 2000,
              chains = 4)
#second run model_1 for males and save results
imr <- ind$Male
stanc("model_1.stan")
fit_1_m <- stan("model_1.stan",
                data = list("N",
                            "year_id",
                            "state_id",
                            "imr",
                            "percap_exp"),
                iter = 2000,
                chains = 4)

#---- 
#Model 1 - Plots
#extracting the results into dataframes
tidy_1_f <- tidy(fit_1_f, conf.int = T, conf.level = 0.50)
tidy_1_m <- tidy(fit_1_m, conf.int = T, conf.level = 0.50)
str(tidy_1_f)

female_pred_1 <- tidy_1_f[77:346,2]
female_low_1 <- tidy_1_f[77:346,4]
female_hi_1 <- tidy_1_f[77:346,5]

male_pred_1 <- tidy_1_m[77:346,2]
male_low_1 <- tidy_1_m[77:346,4]
male_hi_1 <- tidy_1_m[77:346,5]

#female posterior predictive check ----
par(mfcol = c(1,2),
    col = "gray")
plot(exp(female_pred_1), 
     ind$Female,
     pch = 16,
     cex = 0.7,
     col = "black",
     xaxt = 'n',
     xlab = "predicted female IMR",
     yaxt = 'n',
     ylab = "actual female IMR",
     cex.lab = 0.8,
     main = "Posterior Predictive Checks - Female IMR",
     cex.main = 0.8,
     xlim = c(0,80),
     ylim = c(0,80))
axis (1, at = seq(0,80, length.out = 9),
      lwd = 0.5 ,
      cex.axis = 0.8,
      col = "gray")
axis (2, at = seq(0,80, length.out = 9) ,
      lwd = 0.5 ,
      cex.axis = 0.8,
      col = "gray")
abline(0,1,
       col = "black",
       lty = 1)
arrows(exp(female_pred_1),
       ind$Female,
       exp(female_low_1),
       ind$Female,
       col = "gray",
       length = 0)
arrows(exp(female_pred_1),
       ind$Female,
       exp(female_hi_1),
       ind$Female,
       col = "gray",
       length = 0)
points(exp(female_pred_1), 
       ind$Female,
       pch = 16,
       cex = 0.7,
       col = "black")
legend ('topleft', legend = c("Data", "50% Interval"),
        col = c("black  " , "darkgrey") , pch=c(16,3),
        lty = c(0, 1),
        bty = 'n',
        cex = 0.7,
        text.col = "black")

#male posterior predictive check ----


plot(exp(male_pred_1), 
     ind$Male,
     pch = 16,
     cex = 0.7,
     col = "black",
     xaxt = 'n',
     xlab = "predicted male IMR",
     yaxt = 'n',
     ylab = "actual male IMR",
     cex.lab = 0.8,
     main = "Posterior Predictive Checks - Male IMR",
     cex.main = 0.8,
     xlim = c(0,80),
     ylim = c(0,80))
axis (1, at = seq(0,80, length.out = 9),
      lwd = 0.5 ,
      cex.axis = 0.8,
      col = "gray")
axis (2, at = seq(0,80, length.out = 9) ,
      lwd = 0.5 ,
      cex.axis = 0.8,
      col = "gray")
abline(0,1,
       col = "black",
       lty = 1)
arrows(exp(male_pred_1),
       ind$Male,
       exp(male_low_1),
       ind$Male,
       col = "gray",
       length = 0)
arrows(exp(male_pred_1),
       ind$Male,
       exp(male_hi_1),
       ind$Male,
       col = "gray",
       length = 0)
points(exp(male_pred_1), 
       ind$Male,
       pch = 16,
       cex = 0.7,
       col = "black")
legend ('topleft', legend = c("Data", "50% Interval"),
        col = c("black  " , "darkgrey") , pch=c(16,3),
        lty = c(0, 1),
        bty = 'n',
        cex = 0.7,
        text.col = "black")






#Posterior predictive checks in a grid for model 1 ----
ppc <- data.frame(ind$State,
                  ind$state.id,
                  ind$Year,
                  female_pred_1,
                  female_low_1,
                  female_hi_1,
                  male_pred_1,
                  male_low_1,
                  male_hi_1)
ppc$female <- ind$Female
ppc$male <- ind$Male

#Female grid PPC - model 1
par( mfrow = c(5, 6),
     mar = c(2 , 2 , 1, 1),
     cex = 0.6 ,
     las = 1,
     col = "gray")
for (i in 1:30){
  plot ( ppc$ind.Year[ppc$ind.state.id == i],
         ppc$female[ppc$ind.state.id == i],
         type = "l",
         ylim = c(0, 80) ,
         yaxt = "n",
         xaxt = "n",
         xlab = "n",
         main = paste ( unique ( ppc$ind.State[ ppc$ind.state.id == i])) ,
         cex.main = 1,
         col = "black")
  lines(ppc$ind.Year[ppc$ind.state.id == i],
        exp(ppc$female_pred_1[ppc$ind.state.id == i]),
        col = "pink")
  lines(ppc$ind.Year[ppc$ind.state.id == i],
        exp(ppc$female_low_1[ppc$ind.state.id == i]),
        col = "pink",
        lty = 2)
  lines(ppc$ind.Year[ppc$ind.state.id == i],
        exp(ppc$female_hi_1[ppc$ind.state.id == i]),
        col = "pink",
        lty = 2)
  axis (1, at = c(2006 , 2014) ,
        lwd = 0.5 ,
        cex.axis = 0.8,
        col = "gray")
  axis (2, at = c(0, 80) ,
        lwd = 0.5 ,
        cex.axis = 0.6,
        col = "gray")
  title ( ylab = "IMR",
          line =0, cex.lab= .8)
  title ( xlab = "year",
          line =0, cex.lab= .8)
}

#Male grid PPC - model 1 
par( mfrow = c(5, 6),
     mar = c(2 , 2 , 1, 1),
     cex = 0.6 ,
     las = 1,
     col = "gray")
for (i in 1:30){
  plot ( ppc$ind.Year[ppc$ind.state.id == i],
         ppc$male[ppc$ind.state.id == i],
         type = "l",
         ylim = c(0, 80) ,
         yaxt = "n",
         xaxt = "n",
         xlab = "n",
         main = paste ( unique ( ppc$ind.State[ ppc$ind.state.id == i])) ,
         cex.main = 1,
         col = "black")
  lines(ppc$ind.Year[ppc$ind.state.id == i],
        exp(ppc$male_pred_1[ppc$ind.state.id == i]),
        col = "lightblue")
  lines(ppc$ind.Year[ppc$ind.state.id == i],
        exp(ppc$male_low_1[ppc$ind.state.id == i]),
        col = "lightblue",
        lty = 2)
  lines(ppc$ind.Year[ppc$ind.state.id == i],
        exp(ppc$male_hi_1[ppc$ind.state.id == i]),
        col = "lightblue",
        lty = 2)
  axis (1, at = c(2006 , 2014) ,
        lwd = 0.5 ,
        cex.axis = 0.8,
        col = "gray")
  axis (2, at = c(0, 80) ,
        lwd = 0.5 ,
        cex.axis = 0.6,
        col = "gray")
  title ( ylab = "IMR",
          line =0, cex.lab= .8)
  title ( xlab = "year",
          line =0, cex.lab= .8)
}
#
#
#
#time intercept for model 1 ----
par(mar = c(2.5,2.4,2,2),
    mfcol = c(1,2),
    col = "gray")
plot(exp(tidy_1_f[33:39,2]),
     seq(2006,2012),
     pch = 16,
     col = "black",
     main = "Time intercepts for Females - Model 1",
     cex.main = 0.8,
     xlab = NA,
     xaxt = 'n',
     ylab = NA,
     yaxt = 'n',
     xlim = c(0,20))
abline (h = c(2006:2012), lty = 3)
arrows(exp(tidy_1_f[33:39,2]),
       seq(2006,2012),
       exp(tidy_1_f[33:39,4]),
       seq(2006,2012),
       col = "gray",
       length = 0,
       lwd = 2)
arrows(exp(tidy_1_f[33:39,2]),
       seq(2006,2012),
       exp(tidy_1_f[33:39,5]),
       seq(2006,2012),
       col = "gray",
       length = 0,
       lwd = 2)
axis (2, at = seq(2006 , 2012) ,
      lwd = 0.5 ,
      cex.axis = 0.6,
      col = "gray",
      las = 2)
axis (1, seq(0,20, length.out = 5) ,
      lwd = 0.5 ,
      cex.axis = 0.6,
      col = "gray")
points(exp(tidy_1_f[33:39,2]),
       seq(2006,2012),
       pch = 16,
       col = "black")
legend (14,2011.8,
        legend = c("Data", "50% Interval"),
        col = c("black  " , "gray") ,
        pch=c(16,3),
        lty = c(0, 1),
        bty = 'n',
        cex = 0.5,
        text.col = "black")
plot(exp(tidy_1_m[33:39,2]),
     seq(2006,2012),
     pch = 16,
     col = "black",
     main = "Time intercepts for Males - Model 1",
     cex.main = 0.8,
     xlab = NA,
     xaxt = 'n',
     ylab = NA,
     yaxt = 'n',
     xlim = c(0,20))
abline (h = c(2006:2012), lty = 3)
arrows(exp(tidy_1_m[33:39,2]),
       seq(2006,2012),
       exp(tidy_1_m[33:39,4]),
       seq(2006,2012),
       col = "gray",
       length = 0,
       lwd = 2)
arrows(exp(tidy_1_m[33:39,2]),
       seq(2006,2012),
       exp(tidy_1_m[33:39,5]),
       seq(2006,2012),
       col = "gray",
       length = 0,
       lwd = 2)
axis (2, 
      at = seq(2006, 2012) ,
      lwd = 0.5 ,
      cex.axis = 0.6,
      col = "gray",
      las = 2)
axis (1, 
      seq(0,20, length.out = 5) ,
      lwd = 0.5 ,
      cex.axis = 0.6,
      col = "gray")
points(exp(tidy_1_m[33:39,2]),
       seq(2006,2012),
       pch = 16,
       col = "black")
legend (14,2011.8,
        legend = c("Data", "50% Interval"),
        col = c("black  " , "gray") ,
        pch=c(16,3),
        lty = c(0, 1),
        bty = 'n',
        cex = 0.5,
        text.col = "black")

#
#
#State intercepts for model 1 ----
par(mar = c(2,6,2,1),
    mfcol = c(2,1),
    col = "gray")
plot(exp(tidy_1_f[1:30,2]),
     seq(1,30),
     pch = 16,
     col = "black",
     main = "State intercepts for Females",
     cex.main = 0.6,
     xlab = NA,
     xaxt = 'n',
     ylab = NA,
     yaxt = 'n',
     xlim = c(0,80),
     cex = 0.7)
abline (h = c(1:30), lty = 3)
arrows(exp(tidy_1_f[1:30,2]),
       seq(1,30),
       exp(tidy_1_f[1:30,4]),
       seq(1,30),
       col = "gray",
       length = 0,
       lwd = 2)
arrows(exp(tidy_1_f[1:30,2]),
       seq(1,30),
       exp(tidy_1_f[1:30,5]),
       seq(1,30),
       col = "gray",
       length = 0,
       lwd = 2)
axis(2, 
     at = pretty(1:30,n = 30),
     labels = paste(ind$State[1:30]),
     las = 2, 
     lwd.tick = 0.5,
     cex.axis = 0.5, 
     col = "gray",
     tick = T)
axis (1, seq(0,80, length.out = 5) ,
      lwd = 0.5 ,
      cex.axis = 0.6,
      col = "gray")
points(exp(tidy_1_f[1:30,2]),
       seq(1,30),
       pch = 16,
       col = "black",
       cex = 0.7)
legend ('bottomright',
        legend = c("Data", "50% Interval"),
        col = c("black  " , "gray") ,
        pch=c(16,3),
        lty = c(0, 1),
        # bty = 'n',
        cex = 0.5,
        text.col = "black")
# title ( xlab = "year",
#         line =0, cex.lab= .8)
plot(exp(tidy_1_m[1:30,2]),
     seq(1,30),
     pch = 16,
     col = "black",
     main = "State intercepts for Males",
     cex.main = 0.6,
     xlab = NA,
     xaxt = 'n',
     ylab = NA,
     yaxt = 'n',
     xlim = c(0,80),
     cex = 0.7)
abline (h = c(1:30), lty = 3)
arrows(exp(tidy_1_m[1:30,2]),
       seq(1,30),
       exp(tidy_1_m[1:30,4]),
       seq(1,30),
       col = "gray",
       length = 0,
       lwd = 2)
arrows(exp(tidy_1_m[1:30,2]),
       seq(1,30),
       exp(tidy_1_m[1:30,5]),
       seq(1,30),
       col = "gray",
       length = 0,
       lwd = 2)
axis(2, 
     at = pretty(1:30,n = 30),
     labels = paste(ind$State[1:30]),
     las = 2, 
     lwd.tick = 0.5,
     cex.axis = 0.5, 
     col = "gray",
     tick = T)
axis (1, 
      seq(0,80, length.out = 5) ,
      lwd = 0.5 ,
      cex.axis = 0.6,
      col = "gray")
points(exp(tidy_1_m[1:30,2]),
       seq(1,30),
       pch = 16,
       col = "black",
       cex = 0.7)
legend ('bottomright',
        legend = c("Data", "50% Interval"),
        col = c("black  " , "gray") ,
        pch=c(16,3),
        lty = c(0, 1),
        # bty = 'n',
        cex = 0.5,
        text.col = "black")
# title ( xlab = "year",
#         line =0, cex.lab= .8)
#
#
#state intercept df
state_int <- data.frame("state_name" = ind$State[1:30],
                          "fem1" = tidy_1_f[1:30,4],
                          "fem2" = tidy_1_f[1:30,2],
                          "fem3" = tidy_1_f[1:30,5],
                          "mal1" = tidy_1_m[1:30,4],
                          "mal2" = tidy_1_m[1:30,2],
                          "mal3" = tidy_1_m[1:30,5])

par(col = "gray",
    mar = c(2.5,6,1,1))
plot(seq(1,60),
     seq(1,60),
     pch = 16,
     col = "white",
     main = "State intercepts - Model 1",
     cex.main = 0.6,
     xlab = NA,
     xaxt = 'n',
     ylab = NA,
     yaxt = 'n',
     xlim = c(0,35),
     cex = 0.7)
abline(h = seq(1,60), 
       col = "gray",
       lty = 3)
arrows(exp(state_int$fem2),
       seq(1,60, by = 2),
       exp(state_int$fem1),
       seq(1,60, by = 2),
       col = "pink",
       length = 0,
       lwd = 2)
arrows(exp(state_int$fem2),
       seq(1,60, by = 2),
       exp(state_int$fem3),
       seq(1,60, by = 2),
       col = "pink",
       length = 0,
       lwd = 2)
arrows(exp(state_int$mal2),
       seq(2,60, by = 2),
       exp(state_int$mal1),
       seq(2,60, by = 2),
       col = "lightblue",
       length = 0,
       lwd = 2)
arrows(exp(state_int$mal2),
       seq(2,60, by = 2),
       exp(state_int$mal3),
       seq(2,60, by = 2),
       col = "lightblue",
       length = 0,
       lwd = 2)
points(exp(state_int$fem2),
       seq(1,60, by = 2),
       pch = 16,
       col = "red",
       cex = 0.7)
points(exp(state_int$mal2),
       seq(2,60, by = 2),
       pch = 16,
       col = "blue",
       cex = 0.7)
axis(2, 
     at = seq(1.5,59.5, by = 2),
     labels = paste(state_int$state_name),
     las = 2, 
     lwd.tick = 0.5,
     cex.axis = 0.7, 
     col = "gray",
     tick = T)
axis (1, 
      seq(0,35, length.out = 8) ,
      lwd = 0.5 ,
      cex.axis = 0.6,
      col = "gray")
legend ('bottomright',
        legend = c("F", "F - 50% Interval", "M","M - 50% Interval"),
        col = c("red","pink","blue", "lightblue") ,
        pch=c(16,3,16,3),
        lty = c(0, 1, 0, 1),
        # bty = 'n',
        cex = 0.5,
        text.col = "black",
        box.col = "gray")




#Plotting the marginal posterior distribution FOR MODEL 1 ----
ext_1_f <- extract(fit_1_f)
ext_1_m <- extract(fit_1_m)
par( mfrow = c(5, 6),
     mar = c(2 , 1 , 1, 1),
     cex = 0.6 ,
     las = 1,
     col = "gray")
for(i in 1:30){
  plot(density(ext_1_f$beta_exp[,i]),
       main = paste ( unique ( ind$State [ ind$state.id == i])),
       cex.main = 0.8,
       xlab = NA,
       ylab = NA,
       xaxt = 'n',
       yaxt = 'n',
       col = "red",
       xlim = c(-0.45,0.45),
       ylim = c(0,11),
       bty = 'n')
  lines(density(ext_1_m$beta_exp[,i]),
        col = "blue")
  abline(v = 0, lty = 2)
  axis (1, at = c(-0.45, 0, 0.45) ,
        lwd = 0.5 ,
        cex.axis = 0.6,
        col = "gray")
  legend ('topright', legend = c("F", "M"),
          col = c("red" , "blue"),
          lty = c(1, 1),
          bty = 'n',
          cex = 0.5,
          text.col = "black")
}
#----
#
#
#

#
#
#
#

#
#
#
#
#----
#Model 2 ----
stanc("model_2.stan")
imr <- ind$fm.imr
fit_2 <- stan("model_2.stan",
                  data = list("N",
                              "imr",
                              "percap_exp",
                              "state_id",
                              "year_id"),
                  iter = 2000,
                  chains = 4)
print(fit_2)
gamma_fit <- tidy(fit_2,conf.int = T, conf.level = 0.95 ) 
str(gamma_fit)
imr_ratio_pred <- gamma_fit[425:634,2]
imr_ratio_low <- gamma_fit[425:634,4]
imr_ratio_hi <- gamma_fit[425:634,5]

#WRITE MODEL 2 FORMULATION HERE
#model 2 - posterior predictive check ----
x <- c(1:210)
par(col = "gray")
plot(imr,
     pch = 16,
     cex = 0.7,
     ylim = c(0,5),
     yaxt = 'n',
     xaxt = 'n',
     ylab = "Ratio of Female to Male IMR",
     cex.lab = 0.7,
     main = "Model 2 - Posterior Predictive Checks",
     cex.main = 0.8)
arrows(x,
       imr_ratio_pred,
       x,
       imr_ratio_low,
       length = 0,
       col = "gray")
arrows(x,
       imr_ratio_pred,
       x,
       imr_ratio_low,
       length = 0,
       col = "gray")
arrows(x,
       imr_ratio_pred,
       x,
       imr_ratio_hi,
       length = 0,
       col = "gray")
points(imr,
       pch = 16,
       cex = 0.7,
       col = "black")
points(imr_ratio_pred,
       pch = 1,
       cex = 0.7,
       col = "black")
axis (1, at = seq(0,200, length.out = 5),
      lwd = 0.5 ,
      cex.axis = 0.8,
      col = "gray")
axis (2, at = seq(0,5, length.out = 6) ,
      lwd = 0.5 ,
      cex.axis = 0.8,
      col = "gray")
legend ('topleft', 
        legend = c("Data","Prediction", "50% Interval"),
        col = c("black","black", "darkgrey") , 
        pch=c(16,1,3),
        lty = c(0,0, 1),
        bty = 'n',
        cex = 0.7,
        text.col = "black")

#----
#
#
#
#

#
#
#
#

#
#
#
#

#Model 3 ----
##imr ~  normal(state_int +
                    # time_int +
                    # beta_exp*percap_exp(lagged),
                    # sigma^2)
#running Model 3 ----
#first run model_3 for females and save results
imr <- ind$Female
percap_exp <- ind$lag.exp
stanc("model_3.stan")
fit_3_f <- stan("model_3.stan",
                data = list("N",
                            "year_id",
                            "state_id",
                            "imr",
                            "percap_exp"),
                iter = 2000,
                chains = 4)

#second run model_3 for males and save results
imr <- ind$Male
stanc("model_3.stan")
fit_3_m <- stan("model_3.stan",
                data = list("N",
                            "year_id",
                            "state_id",
                            "imr",
                            "percap_exp"),
                iter = 2000,
                chains = 4)

#
#
#
#Marginal posteriors for Model 3 ---- 
ext_3_f <- extract(fit_3_f)
ext_3_m <- extract(fit_3_m)
par( mfrow = c(5, 6),
     mar = c(2 , 1 , 1, 1),
     cex = 0.6 ,
     las = 1,
     col = "gray")
for(i in 1:30){
  plot(density(ext_3_m$beta_exp[,i]),
       main = paste(unique(ind$State[ind$state.id == i])),
       cex.main = 0.8,
       xlab = NA,
       ylab = NA,
       xaxt = 'n',
       yaxt = 'n',
       col = "blue",
       bty = 'n',
       ylim = c(0,190),
       xlim = c(-0.025,0.025))
  lines(density(ext_3_f$beta_exp[,i]),
        col = "red")
  abline(v = 0, lty = 2)
  axis (1, at = c(-0.025, 0, 0.025) ,
        lwd = 0.5 ,
        cex.axis = 0.6,
        col = "gray")
  legend ('topleft', legend = c("F", "M"),
          col = c("red" , "blue"),
          lty = c(1, 1),
          bty = 'n',
          cex = 0.5,
          text.col = "black")
}




#
#extracting the results into dataframes
tidy_3_f <- tidy(fit_3_f, conf.int = T, conf.level = 0.50)
tidy_3_m <- tidy(fit_3_m, conf.int = T, conf.level = 0.50)
str(tidy_3_f)

female_pred_3 <- tidy_1_f[75:284,2]
female_low_3 <- tidy_1_f[75:284,4]
female_hi_3 <- tidy_1_f[75:284,5]
#female posterior predictive check ----
par(mfcol = c(1,2),
    col = "gray")
plot(exp(female_pred_3), 
     ind$Female,
     pch = 16,
     cex = 0.7,
     col = "black",
     xaxt = 'n',
     xlab = "predicted female IMR",
     yaxt = 'n',
     ylab = "actual female IMR",
     cex.lab = 0.8,
     main = "Model 3 - Posterior Predictive Checks - Female IMR",
     cex.main = 0.8,
     xlim = c(0,80),
     ylim = c(0,80))
axis (1, at = seq(0,80, length.out = 9),
      lwd = 0.5 ,
      cex.axis = 0.8,
      col = "gray")
axis (2, at = seq(0,80, length.out = 9) ,
      lwd = 0.5 ,
      cex.axis = 0.8,
      col = "gray")
abline(0,1,
       col = "black",
       lty = 1)
arrows(exp(female_pred_3),
       ind$Female,
       exp(female_low_3),
       ind$Female,
       col = "gray",
       length = 0)
arrows(exp(female_pred_3),
       ind$Female,
       exp(female_hi_3),
       ind$Female,
       col = "gray",
       length = 0)
points(exp(female_pred_3), 
       ind$Female,
       pch = 16,
       cex = 0.7,
       col = "black")
legend ('topleft', legend = c("Data", "50% Interval"),
        col = c("black  " , "darkgrey") , pch=c(16,3),
        lty = c(0, 1),
        bty = 'n',
        cex = 0.7,
        text.col = "black")

#male posterior predictive check ----
male_pred_3 <- tidy_1_m[75:284,2]
male_low_3 <- tidy_1_m[75:284,4]
male_hi_3 <- tidy_1_m[75:284,5]


plot(exp(male_pred_3), 
     ind$Male,
     pch = 16,
     cex = 0.7,
     col = "black",
     xaxt = 'n',
     xlab = "predicted male IMR",
     yaxt = 'n',
     ylab = "actual male IMR",
     cex.lab = 0.8,
     main = "Model 3 - Posterior Predictive Checks - Male IMR",
     cex.main = 0.8,
     xlim = c(0,80),
     ylim = c(0,80))
axis (1, at = seq(0,80, length.out = 9),
      lwd = 0.5 ,
      cex.axis = 0.8,
      col = "gray")
axis (2, at = seq(0,80, length.out = 9) ,
      lwd = 0.5 ,
      cex.axis = 0.8,
      col = "gray")
abline(0,1,
       col = "black",
       lty = 1)
arrows(exp(male_pred_3),
       ind$Male,
       exp(male_low_3),
       ind$Male,
       col = "gray",
       length = 0)
arrows(exp(male_pred_3),
       ind$Male,
       exp(male_hi_3),
       ind$Male,
       col = "gray",
       length = 0)
points(exp(male_pred_3), 
       ind$Male,
       pch = 16,
       cex = 0.7,
       col = "black")
legend ('topleft', legend = c("Data", "50% Interval"),
        col = c("black  " , "darkgrey") , pch=c(16,3),
        lty = c(0, 1),
        bty = 'n',
        cex = 0.7,
        text.col = "black")



#
#


