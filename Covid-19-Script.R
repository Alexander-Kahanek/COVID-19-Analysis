### INFO YOU CAN CHANGE #########
COUNTRY = "Spain" #### Country you are looking for
DAYS = 100 ####### Number of days(ish) you want to predict 
TRIALS = 1000 #### Number of TRIALS to run
SPLIT = 20 #### How many previous days you want to predict off
FOLDER = "raw1" ######## Folder for the data
################
## HOW TO RUN: CTRL + A, then run as a section.
##
## BY: Alexander Kahanek
################

options(stringsAsFactors=FALSE)
library(plyr)
library(dplyr)
library(reshape2)
library(highcharter)

#function for grabbing status from my filenames
getStatus<- function(x){
  strsplit(x, "-")[[1]] %>%
    last() %>%
    gsub(pattern="\\.csv", replacement="")
}

#function created for adding an active status
createActive <- function(x){
  dcast(Country.Region + Date ~ Status,
        data=x, value.var="Value") %>%
    mutate(Active = Confirmed - (Deaths + Recovered)) %>% 
    melt(id = c("Country.Region", "Date"))
}

#function used to convert a dataset from long to wide
convert <- function(x){ dcast(Country + Date ~ Status,
                              data=x, value.var="Value")}

split_to_country <- function(data, country){
  #take data and SPLIT by country
  #first function for model
  country_data <- data %>% 
    subset(Country == country) %>% #getting italy
    convert() %>% #function converts to wide format
    subset(select = c(Country, Date, Deaths, Confirmed)) %>% 
    mutate( #adding columns for per day change, and rate per day
      Deaths_Per_Day =  Deaths - lag(Deaths, k=1),
      Deaths_Rate_Change =  (Deaths - lag(Deaths, k=1))/lag(Deaths, k=1),
      Mortality_Rate = Deaths/Confirmed
    )
  
  #cleaning data for further analysis
  country_data <- country_data %>% 
    subset(select = c(Date, Deaths, Deaths_Per_Day, Deaths_Rate_Change)) %>% 
    subset(!is.na(Deaths_Per_Day)) %>% 
    mutate(
      Deaths_PD = Deaths_Per_Day,
      Deaths_RC = Deaths_Rate_Change
    ) %>% 
    subset(select = c(Date, Deaths, Deaths_PD, Deaths_RC))
  return (country_data)
}

split_for_graph <- function(data, split){
  data <- data %>% 
    subset(!is.na(D_RC_C))
  
  #take only the bottom quarter
  data <- data[ceiling(nrow(data)-(split-1)) : nrow(data),]
  
  mu = data %>% 
    subset(select = Deaths_PD) %>% 
    as.matrix() %>% 
    mean()
  
  sd = data %>% 
    subset(select = Deaths_PD) %>% 
    as.matrix() %>% 
    sd()
  
  #getting rid of outliers in the deaths per day
  data <- data %>% 
    subset(mu-sd*3 < Deaths_PD & Deaths_PD < mu+sd*3)
  
  mu <- data %>% 
    subset(select = D_RC_C) %>% 
    as.matrix() %>% 
    mean()
  
  sd <- data %>% 
    subset(select = D_RC_C) %>% 
    as.matrix() %>% 
    sd()
  
  #getting rid of outliers in the change of the rate of change in deaths
  data <- data %>% 
    subset(mu-sd*3 < D_RC_C & D_RC_C < mu+sd*3)
  
  return (data)
}

get_dr_c <- function(data){
  #Calulating the change in our rates of change
  data <- data %>% 
    mutate(
      D_RC_C = Deaths_RC - lag(Deaths_RC,k=1)
    )
  return (data)
}

### functions used inside function below
get_rc <- function(death_rc_n1, change){
  #rc = lag death_rc + change
  rc=death_rc_n1+change
  return(rc)
}

get_pd <- function(deaths_n1, deaths_rc){
  #lag death*roc = pd
  pd = (deaths_rc*deaths_n1)
  return(ceiling(pd))
}

get_death <- function(lag_death = lag_death, dpd){
  #lag death + death per dat = next cumulative death
  death = lag_death+dpd
  return(ceiling(death))
}

get_random <- function(n,mu, sd){
  rand =0
  for (i in 1:n){
    i=i*11 #this is done to get vast variances in our seeds
    set.seed(i)
    rand <- rand + rnorm(1, mu, sd)
  }
  rand <- rand/n
  return(rand)
}

### function used for prediction model
prediction_model <- function(data, trials, og_d, days, country){
  #set.seed(r_seed)
  
  mu <- data %>% #grabbing first mean
    subset(select = D_RC_C) %>% 
    as.matrix() %>% 
    mean()
  
  sd <- data %>% #grabbing firs standard deviation
    subset(select = D_RC_C) %>% 
    as.matrix() %>% 
    sd()
  
  pred <- get_random(trials,mu,sd) #getting a random prediction
  
  temp <- data
  
  
  for (i in 1:days){ #loop for the days we want
    curr_d = max(temp[temp$D_RC_C != 0,"Date"])
    
    temp <- temp %>% #adding our prediction, taking off the top value
      bind_rows(data.frame(Date = curr_d+1, Deaths = 0, Deaths_PD = 0, Deaths_RC = 0, D_RC_C = pred[1])) %>% 
      slice(-1)
    
    data <- temp %>% #adding our prediction to the overall data
      bind_rows(data.frame(Date = curr_d+1, Deaths = 0, Deaths_PD = 0, Deaths_RC = 0, D_RC_C = pred[1])) %>% 
      slice(1) %>% 
      bind_rows(data)
    
    mu <- data %>% #grabbing the new mean
      subset(select = D_RC_C) %>% 
      as.matrix() %>% 
      mean()
    
    sd <- data %>% #grabbing the new standard deviation
      subset(select = D_RC_C) %>% 
      as.matrix() %>% 
      sd()
    
    pred <- get_random(trials,mu,sd) #grabbing the new prediction!
  } #repeat
  
  data <- data %>% #combining our data back to our real data
    slice(-1) %>% 
    bind_rows(og_d) %>% 
    arrange(Date)
  
  data <- data[!duplicated(data$Date), ] #ended up with duplicates, lets get rid of those
  
  #taking the last row where our deaths aren't 0
  temp <- data %>% 
    subset(Deaths != 0) %>% 
    slice(n())
  
  # adding that row to our data
  temp <- data %>% 
    subset(Deaths == 0) %>% 
    bind_rows(temp) %>% 
    arrange(Date)
  
  
  #Here is where I get our values based on our predictions!
  for (i in 2:nrow(temp)){
    #getting roc
    temp[i,4] <- get_rc(temp[i-1,4], temp[i,5])
    
    #getting deaths per day
    temp[i,3] <- get_pd(temp[i-1,2], temp[i,4])
    
    #getting cumulative deaths
    temp[i,2] <- get_death(temp[i-1,2], temp[i,3])
  }
  
  
  #Here is where we need to fix our data, we ended up getting negative deaths per day after a certain time, and this started making out cumulative deaths decrease. This is intended, as we need these deaths per day values, but we need to swap it back to negative!
  
  #Another thing to note, is I changed the cumulative deaths to match our decreasing deaths per day, essentially this fixes our model to reflect the real world, as people cannot become undead!
  bool = 1
  for (i in 2:nrow(temp)){
    
    if (temp[i,3] < 0){
      temp[i,3] <- temp[i,3]*-1
      temp[i,2] <- temp[i-1,2] + (2*temp[i,3]) #adjust from previous calculations using negative numbers
    }
    else{
      temp[i,2] <- temp[i-1,2] + temp[i,3] #recalculate, this is incase if bounces beteween negative and positive
    }
  }
  
  temp <- temp %>% 
    mutate(
      Country = country
    )
  
  return (temp)
}


###########################################
### This is where I start cleaning the data
files <- list.files(FOLDER, full.names = TRUE)

raw <- files %>% #read in files
  lapply(function(x){
    read.csv(x) %>% 
      mutate(
        Date = as.Date(Date, "%m/%d/%Y"),
        Status = getStatus(x) #add status
      )
  }) %>% 
  bind_rows() %>% #combine files
  subset(!(Value==0) )

raw <- raw %>% #combine countries into one
  group_by(Country.Region, Date, Status) %>% 
  summarise(
    Value = sum(Value)
  )

raw <- raw %>% #get global stats
  group_by(Date, Status) %>% 
  summarise(
    Value = sum(Value)
  ) %>% 
  ungroup() %>% 
  mutate(
    Country.Region = "Global"
  ) %>% 
  bind_rows(raw) %>% rename(
    Country = Country.Region,
  )

#########GETTING EVERYTHING
temp <- raw %>% 
  split_to_country(COUNTRY) %>% 
  get_dr_c()

og <- temp
og <- og %>% 
  mutate(
    Country = COUNTRY
  )

temp <- temp %>% 
  split_for_graph(SPLIT) %>% 
  prediction_model(trials = TRIALS, og_d = og, days = DAYS, country = COUNTRY)


predictions <- temp %>% 
  subset(Deaths_PD != 0) %>% 
  slice(-n()) %>% 
  arrange(Date) %>%
  slice(-1) %>% 
  bind_rows(og) %>% 
  arrange(Date) %>% 
  subset(select = c(Country, Date, Deaths, Deaths_PD, Deaths_RC, D_RC_C))

rm(list=setdiff(ls(), list("predictions", "raw", "COUNTRY")))

data_date <- raw %>% 
  subset(select = Date) %>% 
  as.matrix() %>% 
  max()

predictions %>% 
  hchart(type = "line", hcaes(x=Date, y=Deaths), color = "red", name = "Total Deaths") %>% 
  hc_add_theme(hc_theme_flat()) %>% 
  hc_title(text = COUNTRY,
           align = "left") %>% 
  hc_subtitle(text = "Using the prediction model",
              align = "left") %>% 
  hc_yAxis(labels = list(format = "{value}"), title = list(text = "Cumulative Number of Deaths")) %>% 
  hc_xAxis(title = list(text = "Date"),
           plotBands = list(
             list(
               label = list(text = "This is the Predicted Data"),
               color = "lightblue",
               from = datetime_to_timestamp(as.Date(data_date)),
               to = datetime_to_timestamp(as.Date('2030-01-01'))
             )))%>% 
  hc_legend(align = "left")

