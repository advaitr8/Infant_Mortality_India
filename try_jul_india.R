df <- read.csv("https://raw.githubusercontent.com/julbautista/India-Project/master/India_Data.csv", header = T, skip = 2)

#Drop Andaman and Nicobar & Dadra NH & Daman and Diu & Lakshadweep & Chandigarh and 2003
df <- df[df$Year != 2003,]
df <- df[df$State != "Andaman & Nicobar Islands"
         & df$State != "Dadra & Nagar Haveli"
         & df$State != "Daman & Diu" 
         & df$State != "Lakshadweep"
         & df$State != "Chandigarh",]

df$State <- as.character(df$State)

df$State[df$State == "Odisha"] <- "Orissa"
df$State[df$State == "Chhatisgarh"] <- "Chhattisgarh"

#Read in population data---------------------
pop <- read.csv("https://raw.githubusercontent.com/julbautista/India-Project/master/population.csv", header = TRUE)

#Remove relevant states for population---------------------
pop <- pop[pop$State != "Andaman and Nicobar Islands"
           & pop$State != "Dadra and Nagar Haveli"
           & pop$State != "Daman and Diu" 
           & pop$State != "Lakshadweep"
           & pop$State != "Chandigarh",]

#Merge population to dataframe---------------------
ind <- merge(df, pop)

#Function for projecting population up or down---------------------
pop.func <- function(year, pop2011){
  pop <- NULL
  pop <- ifelse(year > 2011,
                pop2011*(1.0184^(year - 2011)),
                ifelse(year < 2011, 
                       pop2011*((1/1.0184)^(2011 - year)), 
                       pop2011))
  return(pop)
}

#Add column of projected population numbers---------------------
ind$population <- pop.func(ind$Year, ind$X2011pop)

#Remove old population numbers---------------------
ind <- ind[,-8]
rm(df,pop)

#Put all currency in Ruppee---------------------
ind$exp.rup <- NULL
for(i in 1:length(ind[,1])){
  ind$exp.rup[i] <- ifelse(ind$Units[i] == "Lakh",
                           ind$Health.Exp[i]*100000,
                           ind$Health.Exp[i]*1000000)
}

#Get spending per capita---------------------
ind$percap <- ind$exp.rup/ind$population

#Create ratio of female to male IMR---------------------
ind$fm.imr <- ind$Female/ind$Male

#Regress IMR diff on health exp---------------------
#plot(lm(fm.imr ~ percap, data = ind))

#Sort by year---------------------
ind <- ind[order(ind$Year),]

#Create state id---------------------
ind <- within(ind, state.id <- match(State, unique(State)))

#Create Year id---------------------
ind <- within(ind, year.id <- match(Year, unique(Year)))

#create lagged per cap variable---------------------
ind1 <- ind %>% group_by(state.id) %>% mutate(lag.exp = c(NA, percap[-length(percap)]))

#Get rid of 2005---------------------
ind1 <- ind1[ind1$Year != 2005,]
####################################################

#Run the same model on per cap exp lagged---------------------
year_id <- ind1$year.id - 1
state_id <- ind1$state.id
imr <- log(ind1$Female)
percap <- log(ind1$lag.exp)
N <- length(imr)

stanc("jul_try.stan")
fit2 <- stan("jul_try.stan",
             data = list("year_id", "state_id", "N", "imr","percap"),
             iter = 2000, chains = 4)
