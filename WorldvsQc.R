#Project: Comparing Quebec to other regions 


#####
library(tidyverse)
library(lubridate)
library(broom)
library(ggplot2)
library(ggthemes)
library(hrbrthemes)
library(rvest)
library(gt)
library(deSolve)
library(EpiEstim)
library(incidence)
library(distcrete)
library(epitrix)
library(projections)
library(earlyR)
library(gridExtra)
library(anytime)

#Data Collection
##### 
rm_commas <- function(x) gsub(",","",x) #Function to remove commas from numbers

#Italy Data
italy_wikipedia_data_url <- "https://en.wikipedia.org/wiki/Template:COVID-19_pandemic_data/Italy_medical_cases"
italy_outbreak_table <- read_html(italy_wikipedia_data_url)

italy_regions <- italy_outbreak_table %>% html_nodes("table")%>%.[[2]]%>%html_table(header=FALSE, fill=TRUE)%>%
  slice(-(1))%>%slice(1:(nrow(.)-7))%>%select(X1,X5)%>%
  rename(Date = X1,Lombardy = X5) %>% #Rename the Columns
  slice (-1) %>% #Removing the top row 
  mutate(Lombardy = str_replace_all(Lombardy,"\\(.*\\)",""))%>% #Removing numbers with parenthesis in front using regular expressions
  mutate_if(is.character, rm_commas)%>%
  mutate(Lombardy = str_trim(Lombardy)) %>%  #Remove the trailing space at the front of the number
  mutate_at('Lombardy',as.numeric) %>% #Properly Convert to Numbers
  drop_na()%>%
  mutate(Date=anytime(Date)) %>% 
  mutate(Date=ymd(format(Date,"2020-%m-%d")))

#Germany Data

ger_wikipedia_data_url <-  "https://en.wikipedia.org/wiki/COVID-19_pandemic_in_Germany"
ger_outbreak_table <- read_html(ger_wikipedia_data_url)

ger_regions <- ger_outbreak_table %>% html_nodes("table")%>%.[[3]]%>%html_table(header=FALSE, fill=TRUE)%>%
  slice(-(1)) %>% slice(1:(nrow(.)-5)) %>% select(X1,X4) %>%
  dplyr::rename(Date = X1,Berlin = X4)%>%
  slice(-(1:2)) %>%
  mutate(Berlin = str_replace_all(Berlin,"\\(.*\\)","")) %>%
  mutate_at('Berlin',as.numeric) %>%
  drop_na() %>%
  mutate(Berlin = Berlin - lag(Berlin,default = first(Berlin)))%>%
  mutate(Date=anytime(Date)) %>%
  mutate(Date=ymd(format(Date,"2020-%m-%d")))

#Australia Data

aus_wikipedia_data_url <- "https://en.wikipedia.org/wiki/COVID-19_pandemic_in_Australia"
aus_outbreak_table <- read_html(aus_wikipedia_data_url)

#aus_regions <- aus_outbreak_table %>% html_nodes('table') %>% [[4]] %>% html_table()

#New York State data

ny_wikipedia_data_url <- "https://en.wikipedia.org/wiki/COVID-19_pandemic_in_New_York_(state)"

ny_outbreak_table <- read_html(ny_wikipedia_data_url)

ny_regions <- ny_outbreak_table %>% html_nodes('table') %>% .[[7]]%>%html_table(header=FALSE, fill=TRUE)%>%
  slice(-(1)) %>% slice(1:(nrow(.)-1))%>% select(X1,X3)%>%
  rename(Date = X1,NewYork = X3)%>%
  slice(-(1))%>%
  mutate_if(is.character, rm_commas)%>%
  mutate_at('NewYork',as.numeric)%>%
  mutate(NewYork = NewYork - lag(NewYork ,default = first(NewYork)))%>%
  mutate(Date=anytime(Date)) %>%
  mutate(Date=ymd(format(Date,"2020-%m-%d")))

#Quebec Province Data

quebec_wikipedia_data_url <- "https://en.wikipedia.org/wiki/Template:COVID-19_pandemic_data/Canada/Quebec_medical_cases"
quebec_outbreak_table <- read_html(quebec_wikipedia_data_url)

qc_cases <- quebec_outbreak_table %>% html_nodes("table") %>%
  .[[1]]%>%html_table(header=FALSE, fill=TRUE) %>%
  slice(-(1:2)) %>%  slice(1:(nrow(.)-4)) %>% #getting rid of extra rows, keeping one for the region names
  select(X1,X22) %>% #selecting only Region columns
  filter(!str_detect(X1, "Notes"))%>%
  rename(Date = X1,Quebec = X22)%>%
  slice(-(1))%>%
  mutate_if(is.character, rm_commas)%>%
  mutate(Quebec = str_replace_all(Quebec,"\\[.*\\]",""))%>%
  mutate_at('Quebec',as.numeric)%>%
  mutate(Date=anytime(Date)) %>%
  mutate(Date=ymd(format(Date,"2020-%m-%d")))

  
#Merging all Tables
data <- list(italy_regions, ny_regions, qc_cases, ger_regions) %>% 
  reduce(full_join, by="Date") %>% arrange(Date) %>%
  replace(is.na(.),0)





