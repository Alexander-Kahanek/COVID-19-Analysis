### INFO YOU CAN CHANGE #########
COUNTRY = "Italy" #### Country you are looking for
DAYS = 6 ####### Number of days(ish) you want to predict 
TRIALS = 100 #### Number of TRIALS to run
SPLIT = 3 #### How many previous days you want to predict off
FOLDER = "raw" ######## Folder for the data
########
# SAVE DATA
SAVE = 1
FILE = "predictions/split-%s-italy-23.csv"
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
  if(rc <0){
    rc =0
  }
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

### function used for prediction model
prediction_model <- function(data, days, trials, split){
  
  curr_d = max(data[data$D_RC_C != 0,"Date"])
  
  for (n in 1:trials){
    temp <- data %>% 
      subset(select = c(Date, D_RC_C))

    for (i in 1:days){
      
      mu <- temp %>% #grabbing mean
        subset(select = D_RC_C) %>% 
        slice(nrow(temp)-(nrow(temp)-split):nrow(temp)+i) %>% 
        as.matrix() %>% 
        mean()
      
      sd <- temp %>% #grabbing standard deviation
        subset(select = D_RC_C) %>% 
        slice(nrow(temp)-(nrow(temp)-split):nrow(temp)+i) %>% 
        as.matrix() %>% 
        sd()
      
      set.seed(i*13*n)
      temp <- temp %>% 
        bind_rows(data.frame(Date = as.Date(data_date, format = "%Y-%m-%d")+i, D_RC_C = rnorm(1, mu, sd))) %>% 
        mutate(
          Trial = n
        )
    }
    
    if (n == 1){
      trial_data <- temp
    }
    else{
      trial_data <- temp %>% 
        bind_rows(trial_data)
    }
  }
  
  
  return (trial_data)
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

data_date <- raw %>% 
  subset(select = Date) %>% 
  as.matrix() %>% 
  max()
#########GETTING EVERYTHING
predictions <- raw %>% 
  split_to_country(COUNTRY) %>% 
  get_dr_c()

og <- predictions
og <- og %>% 
  mutate(
    Country = COUNTRY
  )

predictions <- predictions %>% 
  split_for_graph(SPLIT) %>% 
  prediction_model(days = DAYS, trials = TRIALS, split = SPLIT) %>% 
  group_by(Trial, Date) %>% 
  slice(n()) %>% 
  ungroup() %>% 
  mutate(
    Deaths = NA,
    Deaths_PD = NA,
    Deaths_RC = NA,
    Country = COUNTRY
  )

for (d in predictions$Date){
  predictions[predictions$Date == d,"Deaths"] <- og[og$Date == d,"Deaths"]
  predictions[predictions$Date == d,"Deaths_PD"] <- og[og$Date == d,"Deaths_PD"]
  predictions[predictions$Date == d,"Deaths_RC"] <- og[og$Date == d,"Deaths_RC"]
}

for (i in 1:TRIALS){
  for (d in predictions$Date){
    if (d > as.Date(data_date, format = "%Y-%m-%d")){
      #getting roc
      predictions[predictions$Date == d & predictions$Trial == i,6] <- get_rc(predictions[predictions$Date == d-1 & predictions$Trial == i,6], predictions[predictions$Date == d & predictions$Trial == i,2])
      
      #getting deaths per day
      predictions[predictions$Date == d & predictions$Trial == i,5] <- get_pd(predictions[predictions$Date == d-1 & predictions$Trial == i,4], predictions[predictions$Date == d & predictions$Trial == i,6])
      
      #getting cumulative deaths
      predictions[predictions$Date == d & predictions$Trial == i,4] <- get_death(predictions[predictions$Date == d-1 & predictions$Trial == i,4], predictions[predictions$Date == d & predictions$Trial == i,5])
    }
  }
}

predictions <- predictions %>% 
  mutate(
    Deaths_RC = abs(Deaths_RC)
  )

##########################
## PLOTTING
hc <- highchart() 

pn <- predictions %>% 
  subset(Date >= data_date)

for (i in 1:TRIALS){
  hc <- hc %>% 
    hc_add_series(name = paste("Trial", as.character(i), sep=" "), data = pn[pn$Trial == i,c("Deaths", "Date")], type = "line", hcaes(x=Date, y=Deaths), showInLegend = F)
}
hc <- hc %>% 
  hc_xAxis(type = "datetime", dateTimeLabelFormats = list(day = '%d/%b')) %>% 
  hc_add_theme(hc_theme_flat()) %>% 
  hc_title(text = "Italys' projected Deaths",
           align = "left") %>% 
  hc_subtitle(text = "Using my prediction model",
              align = "left") %>% 
  hc_yAxis(labels = list(format = "{value}"), title = list(text = "Cumulative Number of Deaths")) %>% 
  hc_xAxis(title = list(text = "Date"),
           plotBands = list(
             list(
               label = list(text = "This is the Predicted Data"),
               color = "lightblue",
               from = datetime_to_timestamp(as.Date(data_date)+1),
               to = datetime_to_timestamp(as.Date('2025-06-02'))
             )))%>% 
  hc_legend(align = "left") %>% 
  hc_add_series(name = "Actual Data", data = og, type = "line", hcaes(x=Date, y=Deaths), color = "black")

hc

# SAVING DATA
if (SAVE == 1){
  FILENAME = sprintf(FILE, SPLIT)
  
  predictions <- predictions %>% 
    mutate(Split = SPLIT) %>% 
    subset(Date >= data_date) %>% 
    write.csv(FILENAME)
}


