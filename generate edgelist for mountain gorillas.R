#Dispersal analysis
#excluding immigrants and the first week of data after their migration (June 1-8th)

library(brms)
library(igraph)
knitr::opts_chunk$set(echo = TRUE)
library(lubridate)
library(stringr)
library(magrittr)
library(readxl)
library(dplyr)
library(tidyr)
library(bayestestR)
library(data.table)

# Get the data ready to use in the model
data <- read.csv("raw_data_pre_post.csv", header = TRUE) # this is the raw data but i manually added pre and post (migration) and removed the rows from 01.06.2018-08.06.2018 (included)

# Substitute affiliation_provider or affiliation_receiver by affiliation, since direction of the behaviour does not matter
data$Behaviour[data$Behaviour=="Affiliation_provider" | data$Behaviour=="Affiliation_receiver"] <- "Affiliation"

# Substitute during for after in the Period columns, so that we only have "before" or "not before"
data$Period[data$Period=="During"] <- "After"

# We create a new copy of the data to get at the behavioural data. We keep the original copy to be able to count the number of focal follows of each individual.
data.AR <- data 

# Exclude Mituno, Nyampazi and NyB. These were the individuals that immigrated. We exclude them because we want to see the effect of their immigrant in the pre-existing relationships.
data.AR <- subset(data.AR, ID != "Mituno" & ID != "Nyampazi" & ID !="NyB") 

# For ease of computation, we remove all possible rows that don't matter (e.g., rows in which there is noone at AR)
data.AR <- subset(data.AR, AR > 0)  # Exclude all AR == 0

# Separate AR.individual in one row per individual in AR
data.AR <- data.AR %>% separate_longer_delim(AR.individual, delim = "+") # Separate all words separated by "+" into two rows

# Remove all rows of AR.individual that are not from one of the individuals we are focusing on
data.AR <- subset(data.AR,
                  AR.individual== "Buzinza"  |
                  AR.individual== "Kabukojo"  |
                  AR.individual== "Kabunga"  |
                  AR.individual== "Kalembezi"  |
                  AR.individual== "Kanyindo"  |
                  AR.individual== "Kanywani"  |
                  AR.individual== "Kibande"  |
                  AR.individual== "Muyana"  |
                  AR.individual== "Ruterana"  |
                  AR.individual== "KibB"  |
                  AR.individual== "MuyB"  |
                  AR.individual== "RutB")

# Keep only the columns we need 
data.AR <- select(data.AR, Code, Period, ID, Behaviour, AR.individual, Migration)

# Remove the duplicated rows - since we incorporated Code, now we remove the duplicated columns so that we are measuring the "number of focals in which this dyad was observed in proximity while doing this behaviour"
data.AR<- data.AR[!duplicated(data.AR), ]

# Extract all possible dyads
ids<-data.frame(unique(data.AR$ID)) # Extract the name of all individuals
ids<-paste(ids$unique.data.AR.ID., sep = ", ", collapse = NULL) # Extract the names from column to a string
all.pairs = t(combn(ids,2)) # Combine all possible pairs of ids

#####
### Here we start building the final dataset to fill it up as we go

## Pre migration and before tourists

df.1<-data.frame(id.1=all.pairs[,1],
                          id.2=all.pairs[,2],
                          migration="Pre",
                          period="Before",
                          behaviour="Affiliation",
                          nr.focals=NA,
                          total.AR=NA)

for(j in 1:nrow(df.1)){ # The loop goes through each dyad in this dataset
  
  # Sum the number of focals of each member of the dyad
  ## The total number of focals by id.1 - here we go back to the original data file, before we filtered it
  id.1<- subset(data, Migration==df.1$migration[1] & Period==df.1$period[1] & ID==df.1$id.1[j]) 
  focals.id1 = length(unique(id.1$Code))
  ## The total number of focals by id.2
  id.2<- subset(data, Migration==df.1$migration[1] & Period==df.1$period[1] & ID==df.1$id.2[j]) 
  focals.id2 = length(unique(id.2$Code))
  ## The sum of the two individuals
  df.1$nr.focals[j] = focals.id1 + focals.id2

  # Calculate total number of AR by the dyad
  ### Number of times id.2 was recorded as AR of focal of id.1
  AR.id.1<- subset(data.AR, Migration==df.1$migration[1] & Period==df.1$period[1] & Behaviour==df.1$behaviour[1] & ID==df.1$id.1[j] & AR.individual==df.1$id.2[j]) 
  AR.id1<- nrow(AR.id.1)
  ### Number of times id.1 was recorded as AR of focal of id.2
  AR.id.2<- subset(data.AR, Migration==df.1$migration[1] & Period==df.1$period[1] & Behaviour==df.1$behaviour[1] & ID==df.1$id.1[j] & AR.individual==df.1$id.1[j]) 
  AR.id2<- nrow(AR.id.2)
  ### Sum the number of AR by the dyad
  df.1$total.AR[j]=AR.id1+AR.id2
}


df.2<-data.frame(id.1=all.pairs[,1],
                                id.2=all.pairs[,2],
                                migration="Pre",
                                period="Before",
                                behaviour="Feeding",
                                nr.focals=NA,
                                total.AR=NA)
for(j in 1:nrow(df.2)){ # The loop goes through each dyad in this dataset
  
  # Sum the number of focals of each member of the dyad
  ## The total number of focals by id.1 - here we go back to the original data file, before we filtered it
  id.1<- subset(data, Migration==df.2$migration[1] & Period==df.2$period[1] & ID==df.2$id.1[j]) 
  focals.id1 = length(unique(id.1$Code))
  ## The total number of focals by id.2
  id.2<- subset(data, Migration==df.2$migration[1] & Period==df.2$period[1] & ID==df.2$id.2[j]) 
  focals.id2 = length(unique(id.2$Code))
  ## The sum of the two individuals
  df.2$nr.focals[j] = focals.id1 + focals.id2
  
  # Calculate total number of AR by the dyad
  ### Number of times id.2 was recorded as AR of focal of id.1
  AR.id.1<- subset(data.AR, Migration==df.2$migration[1] & Period==df.2$period[1] & Behaviour==df.2$behaviour[1] & ID==df.2$id.1[j] & AR.individual==df.2$id.2[j]) 
  AR.id1<- nrow(AR.id.1)
  ### Number of times id.1 was recorded as AR of focal of id.2
  AR.id.2<- subset(data.AR, Migration==df.2$migration[1] & Period==df.2$period[1] & Behaviour==df.2$behaviour[1] & ID==df.2$id.1[j] & AR.individual==df.2$id.1[j]) 
  AR.id2<- nrow(AR.id.2)
  ### Sum the number of AR by the dyad
  df.2$total.AR[j]=AR.id1+AR.id2
}

df.3<-data.frame(id.1=all.pairs[,1],
                                id.2=all.pairs[,2],
                                migration="Pre",
                                period="Before",
                                behaviour="Inactive",
                                nr.focals=NA,
                                total.AR=NA)
for(j in 1:nrow(df.3)){ # The loop goes through each dyad in this dataset
  
  # Sum the number of focals of each member of the dyad
  ## The total number of focals by id.1 - here we go back to the original data file, before we filtered it
  id.1<- subset(data, Migration==df.3$migration[1] & Period==df.3$period[1] & ID==df.3$id.1[j]) 
  focals.id1 = length(unique(id.1$Code))
  ## The total number of focals by id.2
  id.2<- subset(data, Migration==df.3$migration[1] & Period==df.3$period[1] & ID==df.3$id.2[j]) 
  focals.id2 = length(unique(id.2$Code))
  ## The sum of the two individuals
  df.3$nr.focals[j] = focals.id1 + focals.id2
  
  # Calculate total number of AR by the dyad
  ### Number of times id.2 was recorded as AR of focal of id.1
  AR.id.1<- subset(data.AR, Migration==df.3$migration[1] & Period==df.3$period[1] & Behaviour==df.3$behaviour[1] & ID==df.3$id.1[j] & AR.individual==df.3$id.2[j]) 
  AR.id1<- nrow(AR.id.1)
  ### Number of times id.1 was recorded as AR of focal of id.2
  AR.id.2<- subset(data.AR, Migration==df.3$migration[1] & Period==df.3$period[1] & Behaviour==df.3$behaviour[1] & ID==df.3$id.1[j] & AR.individual==df.3$id.1[j]) 
  AR.id2<- nrow(AR.id.2)
  ### Sum the number of AR by the dyad
  df.3$total.AR[j]=AR.id1+AR.id2
}

df.4<-data.frame(id.1=all.pairs[,1],
                                id.2=all.pairs[,2],
                                migration="Pre",
                                period="Before",
                                behaviour="Scratch",
                                nr.focals=NA,
                                total.AR=NA)
for(j in 1:nrow(df.4)){ # The loop goes through each dyad in this dataset
  
  # Sum the number of focals of each member of the dyad
  ## The total number of focals by id.1 - here we go back to the original data file, before we filtered it
  id.1<- subset(data, Migration==df.4$migration[1] & Period==df.4$period[1] & ID==df.4$id.1[j]) 
  focals.id1 = length(unique(id.1$Code))
  ## The total number of focals by id.2
  id.2<- subset(data, Migration==df.4$migration[1] & Period==df.4$period[1] & ID==df.4$id.2[j]) 
  focals.id2 = length(unique(id.2$Code))
  ## The sum of the two individuals
  df.4$nr.focals[j] = focals.id1 + focals.id2
  
  # Calculate total number of AR by the dyad
  ### Number of times id.2 was recorded as AR of focal of id.1
  AR.id.1<- subset(data.AR, Migration==df.4$migration[1] & Period==df.4$period[1] & Behaviour==df.4$behaviour[1] & ID==df.4$id.1[j] & AR.individual==df.4$id.2[j]) 
  AR.id1<- nrow(AR.id.1)
  ### Number of times id.1 was recorded as AR of focal of id.2
  AR.id.2<- subset(data.AR, Migration==df.4$migration[1] & Period==df.4$period[1] & Behaviour==df.4$behaviour[1] & ID==df.4$id.1[j] & AR.individual==df.4$id.1[j]) 
  AR.id2<- nrow(AR.id.2)
  ### Sum the number of AR by the dyad
  df.4$total.AR[j]=AR.id1+AR.id2
}

df.5<-data.frame(id.1=all.pairs[,1],
                                id.2=all.pairs[,2],
                                migration="Pre",
                                period="Before",
                                behaviour="Self-directed",
                                nr.focals=NA,
                                total.AR=NA)
for(j in 1:nrow(df.5)){ # The loop goes through each dyad in this dataset
  
  # Sum the number of focals of each member of the dyad
  ## The total number of focals by id.1 - here we go back to the original data file, before we filtered it
  id.1<- subset(data, Migration==df.5$migration[1] & Period==df.5$period[1] & ID==df.5$id.1[j]) 
  focals.id1 = length(unique(id.1$Code))
  ## The total number of focals by id.2
  id.2<- subset(data, Migration==df.5$migration[1] & Period==df.5$period[1] & ID==df.5$id.2[j]) 
  focals.id2 = length(unique(id.2$Code))
  ## The sum of the two individuals
  df.5$nr.focals[j] = focals.id1 + focals.id2
  
  # Calculate total number of AR by the dyad
  ### Number of times id.2 was recorded as AR of focal of id.1
  AR.id.1<- subset(data.AR, Migration==df.5$migration[1] & Period==df.5$period[1] & Behaviour==df.5$behaviour[1] & ID==df.5$id.1[j] & AR.individual==df.5$id.2[j]) 
  AR.id1<- nrow(AR.id.1)
  ### Number of times id.1 was recorded as AR of focal of id.2
  AR.id.2<- subset(data.AR, Migration==df.5$migration[1] & Period==df.5$period[1] & Behaviour==df.5$behaviour[1] & ID==df.5$id.1[j] & AR.individual==df.5$id.1[j]) 
  AR.id2<- nrow(AR.id.2)
  ### Sum the number of AR by the dyad
  df.5$total.AR[j]=AR.id1+AR.id2
}

df.6<-data.frame(id.1=all.pairs[,1],
                                id.2=all.pairs[,2],
                                migration="Pre",
                                period="Before",
                                behaviour="Vigilant",
                                nr.focals=NA,
                                total.AR=NA)
for(j in 1:nrow(df.6)){ # The loop goes through each dyad in this dataset
  
  # Sum the number of focals of each member of the dyad
  ## The total number of focals by id.1 - here we go back to the original data file, before we filtered it
  id.1<- subset(data, Migration==df.6$migration[1] & Period==df.6$period[1] & ID==df.6$id.1[j]) 
  focals.id1 = length(unique(id.1$Code))
  ## The total number of focals by id.2
  id.2<- subset(data, Migration==df.6$migration[1] & Period==df.6$period[1] & ID==df.6$id.2[j]) 
  focals.id2 = length(unique(id.2$Code))
  ## The sum of the two individuals
  df.6$nr.focals[j] = focals.id1 + focals.id2
  
  # Calculate total number of AR by the dyad
  ### Number of times id.2 was recorded as AR of focal of id.1
  AR.id.1<- subset(data.AR, Migration==df.6$migration[1] & Period==df.6$period[1] & Behaviour==df.6$behaviour[1] & ID==df.6$id.1[j] & AR.individual==df.6$id.2[j]) 
  AR.id1<- nrow(AR.id.1)
  ### Number of times id.1 was recorded as AR of focal of id.2
  AR.id.2<- subset(data.AR, Migration==df.6$migration[1] & Period==df.6$period[1] & Behaviour==df.6$behaviour[1] & ID==df.6$id.1[j] & AR.individual==df.6$id.1[j]) 
  AR.id2<- nrow(AR.id.2)
  ### Sum the number of AR by the dyad
  df.6$total.AR[j]=AR.id1+AR.id2
}

df.7<-data.frame(id.1=all.pairs[,1],
                                id.2=all.pairs[,2],
                                migration="Pre",
                                period="Before",
                                behaviour="Play",
                                nr.focals=NA,
                                total.AR=NA)
for(j in 1:nrow(df.7)){ # The loop goes through each dyad in this dataset
  
  # Sum the number of focals of each member of the dyad
  ## The total number of focals by id.1 - here we go back to the original data file, before we filtered it
  id.1<- subset(data, Migration==df.7$migration[1] & Period==df.7$period[1] & ID==df.7$id.1[j]) 
  focals.id1 = length(unique(id.1$Code))
  ## The total number of focals by id.2
  id.2<- subset(data, Migration==df.7$migration[1] & Period==df.7$period[1] & ID==df.7$id.2[j]) 
  focals.id2 = length(unique(id.2$Code))
  ## The sum of the two individuals
  df.7$nr.focals[j] = focals.id1 + focals.id2
  
  # Calculate total number of AR by the dyad
  ### Number of times id.2 was recorded as AR of focal of id.1
  AR.id.1<- subset(data.AR, Migration==df.7$migration[1] & Period==df.7$period[1] & Behaviour==df.7$behaviour[1] & ID==df.7$id.1[j] & AR.individual==df.7$id.2[j]) 
  AR.id1<- nrow(AR.id.1)
  ### Number of times id.1 was recorded as AR of focal of id.2
  AR.id.2<- subset(data.AR, Migration==df.7$migration[1] & Period==df.7$period[1] & Behaviour==df.7$behaviour[1] & ID==df.7$id.1[j] & AR.individual==df.7$id.1[j]) 
  AR.id2<- nrow(AR.id.2)
  ### Sum the number of AR by the dyad
  df.7$total.AR[j]=AR.id1+AR.id2
}

## Pre migration and after tourists

df.8<-data.frame(id.1=all.pairs[,1],
                               id.2=all.pairs[,2],
                               migration="Pre",
                               period="After",
                               behaviour="Affiliation",
                               nr.focals=NA,
                               total.AR=NA)
for(j in 1:nrow(df.8)){ # The loop goes through each dyad in this dataset
  
  # Sum the number of focals of each member of the dyad
  ## The total number of focals by id.1 - here we go back to the original data file, before we filtered it
  id.1<- subset(data, Migration==df.8$migration[1] & Period==df.8$period[1] & ID==df.8$id.1[j]) 
  focals.id1 = length(unique(id.1$Code))
  ## The total number of focals by id.2
  id.2<- subset(data, Migration==df.8$migration[1] & Period==df.8$period[1] & ID==df.8$id.2[j]) 
  focals.id2 = length(unique(id.2$Code))
  ## The sum of the two individuals
  df.8$nr.focals[j] = focals.id1 + focals.id2
  
  # Calculate total number of AR by the dyad
  ### Number of times id.2 was recorded as AR of focal of id.1
  AR.id.1<- subset(data.AR, Migration==df.8$migration[1] & Period==df.8$period[1] & Behaviour==df.8$behaviour[1] & ID==df.8$id.1[j] & AR.individual==df.8$id.2[j]) 
  AR.id1<- nrow(AR.id.1)
  ### Number of times id.1 was recorded as AR of focal of id.2
  AR.id.2<- subset(data.AR, Migration==df.8$migration[1] & Period==df.8$period[1] & Behaviour==df.8$behaviour[1] & ID==df.8$id.1[j] & AR.individual==df.8$id.1[j]) 
  AR.id2<- nrow(AR.id.2)
  ### Sum the number of AR by the dyad
  df.8$total.AR[j]=AR.id1+AR.id2
}

df.9<-data.frame(id.1=all.pairs[,1],
                              id.2=all.pairs[,2],
                              migration="Pre",
                              period="After",
                              behaviour="Feeding",
                              nr.focals=NA,
                              total.AR=NA)
for(j in 1:nrow(df.9)){ # The loop goes through each dyad in this dataset
  
  # Sum the number of focals of each member of the dyad
  ## The total number of focals by id.1 - here we go back to the original data file, before we filtered it
  id.1<- subset(data, Migration==df.9$migration[1] & Period==df.9$period[1] & ID==df.9$id.1[j]) 
  focals.id1 = length(unique(id.1$Code))
  ## The total number of focals by id.2
  id.2<- subset(data, Migration==df.9$migration[1] & Period==df.9$period[1] & ID==df.9$id.2[j]) 
  focals.id2 = length(unique(id.2$Code))
  ## The sum of the two individuals
  df.9$nr.focals[j] = focals.id1 + focals.id2
  
  # Calculate total number of AR by the dyad
  ### Number of times id.2 was recorded as AR of focal of id.1
  AR.id.1<- subset(data.AR, Migration==df.9$migration[1] & Period==df.9$period[1] & Behaviour==df.9$behaviour[1] & ID==df.9$id.1[j] & AR.individual==df.9$id.2[j]) 
  AR.id1<- nrow(AR.id.1)
  ### Number of times id.1 was recorded as AR of focal of id.2
  AR.id.2<- subset(data.AR, Migration==df.9$migration[1] & Period==df.9$period[1] & Behaviour==df.9$behaviour[1] & ID==df.9$id.1[j] & AR.individual==df.9$id.1[j]) 
  AR.id2<- nrow(AR.id.2)
  ### Sum the number of AR by the dyad
  df.9$total.AR[j]=AR.id1+AR.id2
}

df.10<-data.frame(id.1=all.pairs[,1],
                              id.2=all.pairs[,2],
                              migration="Pre",
                              period="After",
                              behaviour="Inactive",
                              nr.focals=NA,
                              total.AR=NA)
for(j in 1:nrow(df.10)){ # The loop goes through each dyad in this dataset
  
  # Sum the number of focals of each member of the dyad
  ## The total number of focals by id.1 - here we go back to the original data file, before we filtered it
  id.1<- subset(data, Migration==df.10$migration[1] & Period==df.10$period[1] & ID==df.10$id.1[j]) 
  focals.id1 = length(unique(id.1$Code))
  ## The total number of focals by id.2
  id.2<- subset(data, Migration==df.10$migration[1] & Period==df.10$period[1] & ID==df.10$id.2[j]) 
  focals.id2 = length(unique(id.2$Code))
  ## The sum of the two individuals
  df.10$nr.focals[j] = focals.id1 + focals.id2
  
  # Calculate total number of AR by the dyad
  ### Number of times id.2 was recorded as AR of focal of id.1
  AR.id.1<- subset(data.AR, Migration==df.10$migration[1] & Period==df.10$period[1] & Behaviour==df.10$behaviour[1] & ID==df.10$id.1[j] & AR.individual==df.10$id.2[j]) 
  AR.id1<- nrow(AR.id.1)
  ### Number of times id.1 was recorded as AR of focal of id.2
  AR.id.2<- subset(data.AR, Migration==df.10$migration[1] & Period==df.10$period[1] & Behaviour==df.10$behaviour[1] & ID==df.10$id.1[j] & AR.individual==df.10$id.1[j]) 
  AR.id2<- nrow(AR.id.2)
  ### Sum the number of AR by the dyad
  df.10$total.AR[j]=AR.id1+AR.id2
}

df.11<-data.frame(id.1=all.pairs[,1],
                               id.2=all.pairs[,2],
                               migration="Pre",
                               period="After",
                               behaviour="Scratch",
                               nr.focals=NA,
                               total.AR=NA)
for(j in 1:nrow(df.11)){ # The loop goes through each dyad in this dataset
  
  # Sum the number of focals of each member of the dyad
  ## The total number of focals by id.1 - here we go back to the original data file, before we filtered it
  id.1<- subset(data, Migration==df.11$migration[1] & Period==df.11$period[1] & ID==df.11$id.1[j]) 
  focals.id1 = length(unique(id.1$Code))
  ## The total number of focals by id.2
  id.2<- subset(data, Migration==df.11$migration[1] & Period==df.11$period[1] & ID==df.11$id.2[j]) 
  focals.id2 = length(unique(id.2$Code))
  ## The sum of the two individuals
  df.11$nr.focals[j] = focals.id1 + focals.id2
  
  # Calculate total number of AR by the dyad
  ### Number of times id.2 was recorded as AR of focal of id.1
  AR.id.1<- subset(data.AR, Migration==df.11$migration[1] & Period==df.11$period[1] & Behaviour==df.11$behaviour[1] & ID==df.11$id.1[j] & AR.individual==df.11$id.2[j]) 
  AR.id1<- nrow(AR.id.1)
  ### Number of times id.1 was recorded as AR of focal of id.2
  AR.id.2<- subset(data.AR, Migration==df.11$migration[1] & Period==df.11$period[1] & Behaviour==df.11$behaviour[1] & ID==df.11$id.1[j] & AR.individual==df.11$id.1[j]) 
  AR.id2<- nrow(AR.id.2)
  ### Sum the number of AR by the dyad
  df.11$total.AR[j]=AR.id1+AR.id2
}

df.12<-data.frame(id.1=all.pairs[,1],
                              id.2=all.pairs[,2],
                              migration="Pre",
                              period="After",
                              behaviour="Self-directed",
                              nr.focals=NA,
                              total.AR=NA)
for(j in 1:nrow(df.12)){ # The loop goes through each dyad in this dataset
  
  # Sum the number of focals of each member of the dyad
  ## The total number of focals by id.1 - here we go back to the original data file, before we filtered it
  id.1<- subset(data, Migration==df.12$migration[1] & Period==df.12$period[1] & ID==df.12$id.1[j]) 
  focals.id1 = length(unique(id.1$Code))
  ## The total number of focals by id.2
  id.2<- subset(data, Migration==df.12$migration[1] & Period==df.12$period[1] & ID==df.12$id.2[j]) 
  focals.id2 = length(unique(id.2$Code))
  ## The sum of the two individuals
  df.12$nr.focals[j] = focals.id1 + focals.id2
  
  # Calculate total number of AR by the dyad
  ### Number of times id.2 was recorded as AR of focal of id.1
  AR.id.1<- subset(data.AR, Migration==df.12$migration[1] & Period==df.12$period[1] & Behaviour==df.12$behaviour[1] & ID==df.12$id.1[j] & AR.individual==df.12$id.2[j]) 
  AR.id1<- nrow(AR.id.1)
  ### Number of times id.1 was recorded as AR of focal of id.2
  AR.id.2<- subset(data.AR, Migration==df.12$migration[1] & Period==df.12$period[1] & Behaviour==df.12$behaviour[1] & ID==df.12$id.1[j] & AR.individual==df.12$id.1[j]) 
  AR.id2<- nrow(AR.id.2)
  ### Sum the number of AR by the dyad
  df.12$total.AR[j]=AR.id1+AR.id2
}

df.13<-data.frame(id.1=all.pairs[,1],
                             id.2=all.pairs[,2],
                             migration="Pre",
                             period="After",
                             behaviour="Vigilant",
                             nr.focals=NA,
                             total.AR=NA)
for(j in 1:nrow(df.13)){ # The loop goes through each dyad in this dataset
  
  # Sum the number of focals of each member of the dyad
  ## The total number of focals by id.1 - here we go back to the original data file, before we filtered it
  id.1<- subset(data, Migration==df.13$migration[1] & Period==df.13$period[1] & ID==df.13$id.1[j]) 
  focals.id1 = length(unique(id.1$Code))
  ## The total number of focals by id.2
  id.2<- subset(data, Migration==df.13$migration[1] & Period==df.13$period[1] & ID==df.13$id.2[j]) 
  focals.id2 = length(unique(id.2$Code))
  ## The sum of the two individuals
  df.13$nr.focals[j] = focals.id1 + focals.id2
  
  # Calculate total number of AR by the dyad
  ### Number of times id.2 was recorded as AR of focal of id.1
  AR.id.1<- subset(data.AR, Migration==df.13$migration[1] & Period==df.13$period[1] & Behaviour==df.13$behaviour[1] & ID==df.13$id.1[j] & AR.individual==df.13$id.2[j]) 
  AR.id1<- nrow(AR.id.1)
  ### Number of times id.1 was recorded as AR of focal of id.2
  AR.id.2<- subset(data.AR, Migration==df.13$migration[1] & Period==df.13$period[1] & Behaviour==df.13$behaviour[1] & ID==df.13$id.1[j] & AR.individual==df.13$id.1[j]) 
  AR.id2<- nrow(AR.id.2)
  ### Sum the number of AR by the dyad
  df.13$total.AR[j]=AR.id1+AR.id2
}

df.14<-data.frame(id.1=all.pairs[,1],
                              id.2=all.pairs[,2],
                              migration="Pre",
                              period="After",
                              behaviour="Play",
                              nr.focals=NA,
                              total.AR=NA)
for(j in 1:nrow(df.14)){ # The loop goes through each dyad in this dataset
  
  # Sum the number of focals of each member of the dyad
  ## The total number of focals by id.1 - here we go back to the original data file, before we filtered it
  id.1<- subset(data, Migration==df.14$migration[1] & Period==df.14$period[1] & ID==df.14$id.1[j]) 
  focals.id1 = length(unique(id.1$Code))
  ## The total number of focals by id.2
  id.2<- subset(data, Migration==df.14$migration[1] & Period==df.14$period[1] & ID==df.14$id.2[j]) 
  focals.id2 = length(unique(id.2$Code))
  ## The sum of the two individuals
  df.14$nr.focals[j] = focals.id1 + focals.id2
  
  # Calculate total number of AR by the dyad
  ### Number of times id.2 was recorded as AR of focal of id.1
  AR.id.1<- subset(data.AR, Migration==df.14$migration[1] & Period==df.14$period[1] & Behaviour==df.14$behaviour[1] & ID==df.14$id.1[j] & AR.individual==df.14$id.2[j]) 
  AR.id1<- nrow(AR.id.1)
  ### Number of times id.1 was recorded as AR of focal of id.2
  AR.id.2<- subset(data.AR, Migration==df.14$migration[1] & Period==df.14$period[1] & Behaviour==df.14$behaviour[1] & ID==df.14$id.1[j] & AR.individual==df.14$id.1[j]) 
  AR.id2<- nrow(AR.id.2)
  ### Sum the number of AR by the dyad
  df.14$total.AR[j]=AR.id1+AR.id2
}

## post migration and before tourists

df.15<-data.frame(id.1=all.pairs[,1],
                                 id.2=all.pairs[,2],
                                 migration="Post",
                                 period="Before",
                                 behaviour="Affiliation",
                                 nr.focals=NA,
                                 total.AR=NA)
for(j in 1:nrow(df.15)){ # The loop goes through each dyad in this dataset
  
  # Sum the number of focals of each member of the dyad
  ## The total number of focals by id.1 - here we go back to the original data file, before we filtered it
  id.1<- subset(data, Migration==df.15$migration[1] & Period==df.15$period[1] & ID==df.15$id.1[j]) 
  focals.id1 = length(unique(id.1$Code))
  ## The total number of focals by id.2
  id.2<- subset(data, Migration==df.15$migration[1] & Period==df.15$period[1] & ID==df.15$id.2[j]) 
  focals.id2 = length(unique(id.2$Code))
  ## The sum of the two individuals
  df.15$nr.focals[j] = focals.id1 + focals.id2
  
  # Calculate total number of AR by the dyad
  ### Number of times id.2 was recorded as AR of focal of id.1
  AR.id.1<- subset(data.AR, Migration==df.15$migration[1] & Period==df.15$period[1] & Behaviour==df.15$behaviour[1] & ID==df.15$id.1[j] & AR.individual==df.15$id.2[j]) 
  AR.id1<- nrow(AR.id.1)
  ### Number of times id.1 was recorded as AR of focal of id.2
  AR.id.2<- subset(data.AR, Migration==df.15$migration[1] & Period==df.15$period[1] & Behaviour==df.15$behaviour[1] & ID==df.15$id.1[j] & AR.individual==df.15$id.1[j]) 
  AR.id2<- nrow(AR.id.2)
  ### Sum the number of AR by the dyad
  df.15$total.AR[j]=AR.id1+AR.id2
}

df.16<-data.frame(id.1=all.pairs[,1],
                                id.2=all.pairs[,2],
                                migration="Post",
                                period="Before",
                                behaviour="Feeding",
                                nr.focals=NA,
                                total.AR=NA)
for(j in 1:nrow(df.16)){ # The loop goes through each dyad in this dataset
  
  # Sum the number of focals of each member of the dyad
  ## The total number of focals by id.1 - here we go back to the original data file, before we filtered it
  id.1<- subset(data, Migration==df.16$migration[1] & Period==df.16$period[1] & ID==df.16$id.1[j]) 
  focals.id1 = length(unique(id.1$Code))
  ## The total number of focals by id.2
  id.2<- subset(data, Migration==df.16$migration[1] & Period==df.16$period[1] & ID==df.16$id.2[j]) 
  focals.id2 = length(unique(id.2$Code))
  ## The sum of the two individuals
  df.16$nr.focals[j] = focals.id1 + focals.id2
  
  # Calculate total number of AR by the dyad
  ### Number of times id.2 was recorded as AR of focal of id.1
  AR.id.1<- subset(data.AR, Migration==df.16$migration[1] & Period==df.16$period[1] & Behaviour==df.16$behaviour[1] & ID==df.16$id.1[j] & AR.individual==df.16$id.2[j]) 
  AR.id1<- nrow(AR.id.1)
  ### Number of times id.1 was recorded as AR of focal of id.2
  AR.id.2<- subset(data.AR, Migration==df.16$migration[1] & Period==df.16$period[1] & Behaviour==df.16$behaviour[1] & ID==df.16$id.1[j] & AR.individual==df.16$id.1[j]) 
  AR.id2<- nrow(AR.id.2)
  ### Sum the number of AR by the dyad
  df.16$total.AR[j]=AR.id1+AR.id2
}

df.17<-data.frame(id.1=all.pairs[,1],
                                id.2=all.pairs[,2],
                                migration="Post",
                                period="Before",
                                behaviour="Inactive",
                                nr.focals=NA,
                                total.AR=NA)
for(j in 1:nrow(df.17)){ # The loop goes through each dyad in this dataset
  
  # Sum the number of focals of each member of the dyad
  ## The total number of focals by id.1 - here we go back to the original data file, before we filtered it
  id.1<- subset(data, Migration==df.17$migration[1] & Period==df.17$period[1] & ID==df.17$id.1[j]) 
  focals.id1 = length(unique(id.1$Code))
  ## The total number of focals by id.2
  id.2<- subset(data, Migration==df.17$migration[1] & Period==df.17$period[1] & ID==df.17$id.2[j]) 
  focals.id2 = length(unique(id.2$Code))
  ## The sum of the two individuals
  df.17$nr.focals[j] = focals.id1 + focals.id2
  
  # Calculate total number of AR by the dyad
  ### Number of times id.2 was recorded as AR of focal of id.1
  AR.id.1<- subset(data.AR, Migration==df.17$migration[1] & Period==df.17$period[1] & Behaviour==df.17$behaviour[1] & ID==df.17$id.1[j] & AR.individual==df.17$id.2[j]) 
  AR.id1<- nrow(AR.id.1)
  ### Number of times id.1 was recorded as AR of focal of id.2
  AR.id.2<- subset(data.AR, Migration==df.17$migration[1] & Period==df.17$period[1] & Behaviour==df.17$behaviour[1] & ID==df.17$id.1[j] & AR.individual==df.17$id.1[j]) 
  AR.id2<- nrow(AR.id.2)
  ### Sum the number of AR by the dyad
  df.17$total.AR[j]=AR.id1+AR.id2
}

df.18<-data.frame(id.1=all.pairs[,1],
                                 id.2=all.pairs[,2],
                                 migration="Post",
                                 period="Before",
                                 behaviour="Scratch",
                                 nr.focals=NA,
                                 total.AR=NA)
for(j in 1:nrow(df.18)){ # The loop goes through each dyad in this dataset
  
  # Sum the number of focals of each member of the dyad
  ## The total number of focals by id.1 - here we go back to the original data file, before we filtered it
  id.1<- subset(data, Migration==df.18$migration[1] & Period==df.18$period[1] & ID==df.18$id.1[j]) 
  focals.id1 = length(unique(id.1$Code))
  ## The total number of focals by id.2
  id.2<- subset(data, Migration==df.18$migration[1] & Period==df.18$period[1] & ID==df.18$id.2[j]) 
  focals.id2 = length(unique(id.2$Code))
  ## The sum of the two individuals
  df.18$nr.focals[j] = focals.id1 + focals.id2
  
  # Calculate total number of AR by the dyad
  ### Number of times id.2 was recorded as AR of focal of id.1
  AR.id.1<- subset(data.AR, Migration==df.18$migration[1] & Period==df.18$period[1] & Behaviour==df.18$behaviour[1] & ID==df.18$id.1[j] & AR.individual==df.18$id.2[j]) 
  AR.id1<- nrow(AR.id.1)
  ### Number of times id.1 was recorded as AR of focal of id.2
  AR.id.2<- subset(data.AR, Migration==df.18$migration[1] & Period==df.18$period[1] & Behaviour==df.18$behaviour[1] & ID==df.18$id.1[j] & AR.individual==df.18$id.1[j]) 
  AR.id2<- nrow(AR.id.2)
  ### Sum the number of AR by the dyad
  df.18$total.AR[j]=AR.id1+AR.id2
}

df.19<-data.frame(id.1=all.pairs[,1],
                                id.2=all.pairs[,2],
                                migration="Post",
                                period="Before",
                                behaviour="Self-directed",
                                nr.focals=NA,
                                total.AR=NA)
for(j in 1:nrow(df.19)){ # The loop goes through each dyad in this dataset
  
  # Sum the number of focals of each member of the dyad
  ## The total number of focals by id.1 - here we go back to the original data file, before we filtered it
  id.1<- subset(data, Migration==df.19$migration[1] & Period==df.19$period[1] & ID==df.19$id.1[j]) 
  focals.id1 = length(unique(id.1$Code))
  ## The total number of focals by id.2
  id.2<- subset(data, Migration==df.19$migration[1] & Period==df.19$period[1] & ID==df.19$id.2[j]) 
  focals.id2 = length(unique(id.2$Code))
  ## The sum of the two individuals
  df.19$nr.focals[j] = focals.id1 + focals.id2
  
  # Calculate total number of AR by the dyad
  ### Number of times id.2 was recorded as AR of focal of id.1
  AR.id.1<- subset(data.AR, Migration==df.19$migration[1] & Period==df.19$period[1] & Behaviour==df.19$behaviour[1] & ID==df.19$id.1[j] & AR.individual==df.19$id.2[j]) 
  AR.id1<- nrow(AR.id.1)
  ### Number of times id.1 was recorded as AR of focal of id.2
  AR.id.2<- subset(data.AR, Migration==df.19$migration[1] & Period==df.19$period[1] & Behaviour==df.19$behaviour[1] & ID==df.19$id.1[j] & AR.individual==df.19$id.1[j]) 
  AR.id2<- nrow(AR.id.2)
  ### Sum the number of AR by the dyad
  df.19$total.AR[j]=AR.id1+AR.id2
}

df.20<-data.frame(id.1=all.pairs[,1],
                               id.2=all.pairs[,2],
                               migration="Post",
                               period="Before",
                               behaviour="Vigilant",
                               nr.focals=NA,
                               total.AR=NA)
for(j in 1:nrow(df.20)){ # The loop goes through each dyad in this dataset
  
  # Sum the number of focals of each member of the dyad
  ## The total number of focals by id.1 - here we go back to the original data file, before we filtered it
  id.1<- subset(data, Migration==df.20$migration[1] & Period==df.20$period[1] & ID==df.20$id.1[j]) 
  focals.id1 = length(unique(id.1$Code))
  ## The total number of focals by id.2
  id.2<- subset(data, Migration==df.20$migration[1] & Period==df.20$period[1] & ID==df.20$id.2[j]) 
  focals.id2 = length(unique(id.2$Code))
  ## The sum of the two individuals
  df.20$nr.focals[j] = focals.id1 + focals.id2
  
  # Calculate total number of AR by the dyad
  ### Number of times id.2 was recorded as AR of focal of id.1
  AR.id.1<- subset(data.AR, Migration==df.20$migration[1] & Period==df.20$period[1] & Behaviour==df.20$behaviour[1] & ID==df.20$id.1[j] & AR.individual==df.20$id.2[j]) 
  AR.id1<- nrow(AR.id.1)
  ### Number of times id.1 was recorded as AR of focal of id.2
  AR.id.2<- subset(data.AR, Migration==df.20$migration[1] & Period==df.20$period[1] & Behaviour==df.20$behaviour[1] & ID==df.20$id.1[j] & AR.individual==df.20$id.1[j]) 
  AR.id2<- nrow(AR.id.2)
  ### Sum the number of AR by the dyad
  df.20$total.AR[j]=AR.id1+AR.id2
}

df.21<-data.frame(id.1=all.pairs[,1],
                                id.2=all.pairs[,2],
                                migration="Post",
                                period="Before",
                                behaviour="Play",
                                nr.focals=NA,
                                total.AR=NA)
for(j in 1:nrow(df.21)){ # The loop goes through each dyad in this dataset
  
  # Sum the number of focals of each member of the dyad
  ## The total number of focals by id.1 - here we go back to the original data file, before we filtered it
  id.1<- subset(data, Migration==df.21$migration[1] & Period==df.21$period[1] & ID==df.21$id.1[j]) 
  focals.id1 = length(unique(id.1$Code))
  ## The total number of focals by id.2
  id.2<- subset(data, Migration==df.21$migration[1] & Period==df.21$period[1] & ID==df.21$id.2[j]) 
  focals.id2 = length(unique(id.2$Code))
  ## The sum of the two individuals
  df.21$nr.focals[j] = focals.id1 + focals.id2
  
  # Calculate total number of AR by the dyad
  ### Number of times id.2 was recorded as AR of focal of id.1
  AR.id.1<- subset(data.AR, Migration==df.21$migration[1] & Period==df.21$period[1] & Behaviour==df.21$behaviour[1] & ID==df.21$id.1[j] & AR.individual==df.21$id.2[j]) 
  AR.id1<- nrow(AR.id.1)
  ### Number of times id.1 was recorded as AR of focal of id.2
  AR.id.2<- subset(data.AR, Migration==df.21$migration[1] & Period==df.21$period[1] & Behaviour==df.21$behaviour[1] & ID==df.21$id.1[j] & AR.individual==df.21$id.1[j]) 
  AR.id2<- nrow(AR.id.2)
  ### Sum the number of AR by the dyad
  df.21$total.AR[j]=AR.id1+AR.id2
}

## post migration and after tourists

df.22<-data.frame(id.1=all.pairs[,1],
                                id.2=all.pairs[,2],
                                migration="Post",
                                period="After",
                                behaviour="Affiliation",
                                nr.focals=NA,
                                total.AR=NA)
for(j in 1:nrow(df.22)){ # The loop goes through each dyad in this dataset
  
  # Sum the number of focals of each member of the dyad
  ## The total number of focals by id.1 - here we go back to the original data file, before we filtered it
  id.1<- subset(data, Migration==df.22$migration[1] & Period==df.22$period[1] & ID==df.22$id.1[j]) 
  focals.id1 = length(unique(id.1$Code))
  ## The total number of focals by id.2
  id.2<- subset(data, Migration==df.22$migration[1] & Period==df.22$period[1] & ID==df.22$id.2[j]) 
  focals.id2 = length(unique(id.2$Code))
  ## The sum of the two individuals
  df.22$nr.focals[j] = focals.id1 + focals.id2
  
  # Calculate total number of AR by the dyad
  ### Number of times id.2 was recorded as AR of focal of id.1
  AR.id.1<- subset(data.AR, Migration==df.22$migration[1] & Period==df.22$period[1] & Behaviour==df.22$behaviour[1] & ID==df.22$id.1[j] & AR.individual==df.22$id.2[j]) 
  AR.id1<- nrow(AR.id.1)
  ### Number of times id.1 was recorded as AR of focal of id.2
  AR.id.2<- subset(data.AR, Migration==df.22$migration[1] & Period==df.22$period[1] & Behaviour==df.22$behaviour[1] & ID==df.22$id.1[j] & AR.individual==df.22$id.1[j]) 
  AR.id2<- nrow(AR.id.2)
  ### Sum the number of AR by the dyad
  df.22$total.AR[j]=AR.id1+AR.id2
}

df.23<-data.frame(id.1=all.pairs[,1],
                               id.2=all.pairs[,2],
                               migration="Post",
                               period="After",
                               behaviour="Feeding",
                               nr.focals=NA,
                               total.AR=NA)
for(j in 1:nrow(df.23)){ # The loop goes through each dyad in this dataset
  
  # Sum the number of focals of each member of the dyad
  ## The total number of focals by id.1 - here we go back to the original data file, before we filtered it
  id.1<- subset(data, Migration==df.23$migration[1] & Period==df.23$period[1] & ID==df.23$id.1[j]) 
  focals.id1 = length(unique(id.1$Code))
  ## The total number of focals by id.2
  id.2<- subset(data, Migration==df.23$migration[1] & Period==df.23$period[1] & ID==df.23$id.2[j]) 
  focals.id2 = length(unique(id.2$Code))
  ## The sum of the two individuals
  df.23$nr.focals[j] = focals.id1 + focals.id2
  
  # Calculate total number of AR by the dyad
  ### Number of times id.2 was recorded as AR of focal of id.1
  AR.id.1<- subset(data.AR, Migration==df.23$migration[1] & Period==df.23$period[1] & Behaviour==df.23$behaviour[1] & ID==df.23$id.1[j] & AR.individual==df.23$id.2[j]) 
  AR.id1<- nrow(AR.id.1)
  ### Number of times id.1 was recorded as AR of focal of id.2
  AR.id.2<- subset(data.AR, Migration==df.23$migration[1] & Period==df.23$period[1] & Behaviour==df.23$behaviour[1] & ID==df.23$id.1[j] & AR.individual==df.23$id.1[j]) 
  AR.id2<- nrow(AR.id.2)
  ### Sum the number of AR by the dyad
  df.23$total.AR[j]=AR.id1+AR.id2
}

df.24<-data.frame(id.1=all.pairs[,1],
                               id.2=all.pairs[,2],
                               migration="Post",
                               period="After",
                               behaviour="Inactive",
                               nr.focals=NA,
                               total.AR=NA)
for(j in 1:nrow(df.24)){ # The loop goes through each dyad in this dataset
  
  # Sum the number of focals of each member of the dyad
  ## The total number of focals by id.1 - here we go back to the original data file, before we filtered it
  id.1<- subset(data, Migration==df.24$migration[1] & Period==df.24$period[1] & ID==df.24$id.1[j]) 
  focals.id1 = length(unique(id.1$Code))
  ## The total number of focals by id.2
  id.2<- subset(data, Migration==df.24$migration[1] & Period==df.24$period[1] & ID==df.24$id.2[j]) 
  focals.id2 = length(unique(id.2$Code))
  ## The sum of the two individuals
  df.24$nr.focals[j] = focals.id1 + focals.id2
  
  # Calculate total number of AR by the dyad
  ### Number of times id.2 was recorded as AR of focal of id.1
  AR.id.1<- subset(data.AR, Migration==df.24$migration[1] & Period==df.24$period[1] & Behaviour==df.24$behaviour[1] & ID==df.24$id.1[j] & AR.individual==df.24$id.2[j]) 
  AR.id1<- nrow(AR.id.1)
  ### Number of times id.1 was recorded as AR of focal of id.2
  AR.id.2<- subset(data.AR, Migration==df.24$migration[1] & Period==df.24$period[1] & Behaviour==df.24$behaviour[1] & ID==df.24$id.1[j] & AR.individual==df.24$id.1[j]) 
  AR.id2<- nrow(AR.id.2)
  ### Sum the number of AR by the dyad
  df.24$total.AR[j]=AR.id1+AR.id2
}

df.25<-data.frame(id.1=all.pairs[,1],
                                id.2=all.pairs[,2],
                                migration="Post",
                                period="After",
                                behaviour="Scratch",
                                nr.focals=NA,
                                total.AR=NA)
for(j in 1:nrow(df.25)){ # The loop goes through each dyad in this dataset
  
  # Sum the number of focals of each member of the dyad
  ## The total number of focals by id.1 - here we go back to the original data file, before we filtered it
  id.1<- subset(data, Migration==df.25$migration[1] & Period==df.25$period[1] & ID==df.25$id.1[j]) 
  focals.id1 = length(unique(id.1$Code))
  ## The total number of focals by id.2
  id.2<- subset(data, Migration==df.25$migration[1] & Period==df.25$period[1] & ID==df.25$id.2[j]) 
  focals.id2 = length(unique(id.2$Code))
  ## The sum of the two individuals
  df.25$nr.focals[j] = focals.id1 + focals.id2
  
  # Calculate total number of AR by the dyad
  ### Number of times id.2 was recorded as AR of focal of id.1
  AR.id.1<- subset(data.AR, Migration==df.25$migration[1] & Period==df.25$period[1] & Behaviour==df.25$behaviour[1] & ID==df.25$id.1[j] & AR.individual==df.25$id.2[j]) 
  AR.id1<- nrow(AR.id.1)
  ### Number of times id.1 was recorded as AR of focal of id.2
  AR.id.2<- subset(data.AR, Migration==df.25$migration[1] & Period==df.25$period[1] & Behaviour==df.25$behaviour[1] & ID==df.25$id.1[j] & AR.individual==df.25$id.1[j]) 
  AR.id2<- nrow(AR.id.2)
  ### Sum the number of AR by the dyad
  df.25$total.AR[j]=AR.id1+AR.id2
}

df.26<-data.frame(id.1=all.pairs[,1],
                               id.2=all.pairs[,2],
                               migration="Post",
                               period="After",
                               behaviour="Self-directed",
                               nr.focals=NA,
                               total.AR=NA)
for(j in 1:nrow(df.26)){ # The loop goes through each dyad in this dataset
  
  # Sum the number of focals of each member of the dyad
  ## The total number of focals by id.1 - here we go back to the original data file, before we filtered it
  id.1<- subset(data, Migration==df.26$migration[1] & Period==df.26$period[1] & ID==df.26$id.1[j]) 
  focals.id1 = length(unique(id.1$Code))
  ## The total number of focals by id.2
  id.2<- subset(data, Migration==df.26$migration[1] & Period==df.26$period[1] & ID==df.26$id.2[j]) 
  focals.id2 = length(unique(id.2$Code))
  ## The sum of the two individuals
  df.26$nr.focals[j] = focals.id1 + focals.id2
  
  # Calculate total number of AR by the dyad
  ### Number of times id.2 was recorded as AR of focal of id.1
  AR.id.1<- subset(data.AR, Migration==df.26$migration[1] & Period==df.26$period[1] & Behaviour==df.26$behaviour[1] & ID==df.26$id.1[j] & AR.individual==df.26$id.2[j]) 
  AR.id1<- nrow(AR.id.1)
  ### Number of times id.1 was recorded as AR of focal of id.2
  AR.id.2<- subset(data.AR, Migration==df.26$migration[1] & Period==df.26$period[1] & Behaviour==df.26$behaviour[1] & ID==df.26$id.1[j] & AR.individual==df.26$id.1[j]) 
  AR.id2<- nrow(AR.id.2)
  ### Sum the number of AR by the dyad
  df.26$total.AR[j]=AR.id1+AR.id2
}

df.27<-data.frame(id.1=all.pairs[,1],
                              id.2=all.pairs[,2],
                              migration="Post",
                              period="After",
                              behaviour="Vigilant",
                              nr.focals=NA,
                              total.AR=NA)
for(j in 1:nrow(df.27)){ # The loop goes through each dyad in this dataset
  
  # Sum the number of focals of each member of the dyad
  ## The total number of focals by id.1 - here we go back to the original data file, before we filtered it
  id.1<- subset(data, Migration==df.27$migration[1] & Period==df.27$period[1] & ID==df.27$id.1[j]) 
  focals.id1 = length(unique(id.1$Code))
  ## The total number of focals by id.2
  id.2<- subset(data, Migration==df.27$migration[1] & Period==df.27$period[1] & ID==df.27$id.2[j]) 
  focals.id2 = length(unique(id.2$Code))
  ## The sum of the two individuals
  df.27$nr.focals[j] = focals.id1 + focals.id2
  
  # Calculate total number of AR by the dyad
  ### Number of times id.2 was recorded as AR of focal of id.1
  AR.id.1<- subset(data.AR, Migration==df.27$migration[1] & Period==df.27$period[1] & Behaviour==df.27$behaviour[1] & ID==df.27$id.1[j] & AR.individual==df.27$id.2[j]) 
  AR.id1<- nrow(AR.id.1)
  ### Number of times id.1 was recorded as AR of focal of id.2
  AR.id.2<- subset(data.AR, Migration==df.27$migration[1] & Period==df.27$period[1] & Behaviour==df.27$behaviour[1] & ID==df.27$id.1[j] & AR.individual==df.27$id.1[j]) 
  AR.id2<- nrow(AR.id.2)
  ### Sum the number of AR by the dyad
  df.27$total.AR[j]=AR.id1+AR.id2
}

df.28<-data.frame(id.1=all.pairs[,1],
                               id.2=all.pairs[,2],
                               migration="Post",
                               period="After",
                               behaviour="Play",
                               nr.focals=NA,
                               total.AR=NA)
for(j in 1:nrow(df.28)){ # The loop goes through each dyad in this dataset
  
  # Sum the number of focals of each member of the dyad
  ## The total number of focals by id.1 - here we go back to the original data file, before we filtered it
  id.1<- subset(data, Migration==df.28$migration[1] & Period==df.28$period[1] & ID==df.28$id.1[j]) 
  focals.id1 = length(unique(id.1$Code))
  ## The total number of focals by id.2
  id.2<- subset(data, Migration==df.28$migration[1] & Period==df.28$period[1] & ID==df.28$id.2[j]) 
  focals.id2 = length(unique(id.2$Code))
  ## The sum of the two individuals
  df.28$nr.focals[j] = focals.id1 + focals.id2
  
  # Calculate total number of AR by the dyad
  ### Number of times id.2 was recorded as AR of focal of id.1
  AR.id.1<- subset(data.AR, Migration==df.28$migration[1] & Period==df.28$period[1] & Behaviour==df.28$behaviour[1] & ID==df.28$id.1[j] & AR.individual==df.28$id.2[j]) 
  AR.id1<- nrow(AR.id.1)
  ### Number of times id.1 was recorded as AR of focal of id.2
  AR.id.2<- subset(data.AR, Migration==df.28$migration[1] & Period==df.28$period[1] & Behaviour==df.28$behaviour[1] & ID==df.28$id.1[j] & AR.individual==df.28$id.1[j]) 
  AR.id2<- nrow(AR.id.2)
  ### Sum the number of AR by the dyad
  df.28$total.AR[j]=AR.id1+AR.id2
}

## Add all of them into the final dataset

dataset.final<- bind_rows(df.1,
                          df.2,
                          df.3,
                          df.4,
                          df.5,
                          df.6,
                          df.7,
                          df.8,
                          df.9,
                          df.10,
                          df.11,
                          df.12,
                          df.13,
                          df.14,
                          df.15,
                          df.16,
                          df.17,
                          df.18,
                          df.19,
                          df.20,
                          df.21,
                          df.22,
                          df.23,
                          df.24,
                          df.25,
                          df.26,
                          df.27,
                          df.28)

write.csv(dataset.final, "processed_data.csv")
