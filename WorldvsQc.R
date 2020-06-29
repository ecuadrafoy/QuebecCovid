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
#library(deSolve)
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
  mutate(Date=ymd(format(Date,"2020-%m-%d", origin="1970-01-01")))

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
  mutate(Date=ymd(format(Date,"2020-%m-%d", origin="1970-01-01")))


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
  mutate(Date=ymd(format(Date,"2020-%m-%d", origin="1970-01-01")))

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
  mutate(Date = str_replace_all(Date,"\\[.*\\]",""))%>%
  mutate_at('Quebec',as.numeric)%>%
  mutate(Date=anytime(Date)) %>%
  mutate(Date=ymd(format(Date,"2020-%m-%d", origin="1970-01-01")))

  
#Merging all Tables
data <- list(italy_regions, ny_regions, qc_cases, ger_regions) %>% 
  reduce(full_join, by="Date", origin = "1970-01-01") %>% arrange(Date) %>%
  replace(is.na(.),0)

#Final Table

df <- data %>% 
  pivot_longer(-Date,
               names_to="Regions",
               values_to="incident_cases")%>%
  mutate(incident_cases=as.integer(gsub(",","",incident_cases))) %>%
  mutate(incident_cases=ifelse(incident_cases<0, 0 ,incident_cases))%>%
  mutate(incident_cases=ifelse(is.na(incident_cases),0,incident_cases)) %>%
  arrange(Regions, Date) %>%
  group_by(Regions) %>%
  mutate(cum_cases=cumsum(incident_cases)) %>%
  mutate(incident_cases=ifelse(incident_cases == 0 & cumsum(incident_cases) == 0, NA, incident_cases)) %>%
  ungroup() %>%
  select(-cum_cases) %>%
  arrange(Regions, desc(Date)) %>%
  group_by(Regions) %>%
  mutate(cum_cases=cumsum(incident_cases)) %>%
  mutate(incident_cases=ifelse(incident_cases == 0 & cumsum(incident_cases) == 0, NA, incident_cases)) %>%
  ungroup() %>%
  select(-cum_cases) %>%
  arrange(Regions, Date)

#End of the Data Step


#Visualizations
#####
df %>% group_by(Date,Regions)%>%
  summarise(incident_cases=sum(incident_cases, na.rm=TRUE))%>%
  ggplot(aes(x=Date,y=incident_cases))+
  geom_bar(stat="identity")+
  labs(y="Incident Cases", title="COVID-19 Incident Cases")+
  theme(legend.position = "top")


##Plot By Region

df %>%
  group_by(Regions)%>%
  summarise(incident_cases=sum(incident_cases, na.rm = TRUE)) %>%
  ggplot(aes(x=reorder(Regions, incident_cases),y=incident_cases))+
  geom_bar(stat="identity")+
  coord_flip()+
  labs(y="Total cases to date",
       x = "",
       title="COVID-19 Total Cases")+
  theme(legend.position="top")+
  theme_economist()



#Plot cases by Regions

df %>%
  group_by(Regions)%>%
  summarise(total=sum(incident_cases, na.rm=TRUE)) %>%
  arrange(desc(total)) %>%
  pull(Regions) -> world_regions_order


q <- df %>%
  filter(Regions %in% world_regions_order[1:6]) %>%
  mutate(Regions = ordered(Regions, levels = world_regions_order[1:4])) %>%
  ggplot(aes(x=Date, y=incident_cases)) + 
  geom_bar(stat="identity", fill="green") + 
  facet_wrap(Regions ~., scales = "free_y", ncol=2) + labs(y="Daily incremental incidence",
                                                           title="Confirmed cases of COVID-19 in other Regions",
                                                           subtitle="Note: differing y-axis scales") +
  theme(legend.position = "none", 
        strip.text.y = element_text(size=11))
print(q)

#####
#Epidemiological Model Framework (Run Only Once)


plot_Ri <- function(estimate_R_obj) {
  p_I <- plot(estimate_R_obj, "incid", add_imported_cases=FALSE) # plots the incidence
  p_SI <- plot(estimate_R_obj, "SI") # plots the serial interval distribution
  p_Ri <- plot(estimate_R_obj, "R") + labs(title="Instantaneous effective R")
  return(gridExtra::grid.arrange(p_I, p_SI, p_Ri, ncol = 1))
}

plot_R_w <- function(reg) {
  
  confirmed_cases_q <- df %>%
    filter(Regions == reg) %>%
    filter(Date >= ymd("2020-02-28"))%>%
    filter(!is.na(incident_cases))%>%
    filter(incident_cases > 0)%>%
    select(Date, incident_cases)%>%
    rename(dates=Date,
           I=incident_cases)
  estimate_R_obj <- estimate_R(confirmed_cases_q$I,
                               method="uncertain_si",
                               config = make_config(list(
                                 mean_si = 7.5, std_mean_si = 2.0,
                                 min_mean_si = 1, max_mean_si = 8.4,
                                 std_si = 3.4, std_std_si = 1.0,
                                 min_std_si = 0.5, max_std_si = 4.0,
                                 n1 = 1000, n2 = 1000)))
  
  plot(estimate_R_obj, "R") + labs(title=paste("Instantaneous effective R0 for region - ", reg, ":", 
                                               round(tail(estimate_R_obj[["R"]][["Mean(R)"]],n=1),digits=2),
                                               "\nDate: ",today()))+
    theme_clean()
  
}

epicurve_w <- function(reg){
  df%>%
    filter(Regions == reg)%>%
    group_by(Date)%>%
    summarise(incident_cases=sum(incident_cases, na.rm=TRUE))%>%
    ggplot(aes(x=Date,y=incident_cases))+
    geom_bar(stat="identity")+
    labs(y="Incident Cases", title=paste("Covid-19 incidence cases in", reg))+
    theme(legend.position="top")
}

region_earlyR <- function(df,region,si_mean,si_sd){
  df %>%
    filter(!is.na(incident_cases),
           Regions == region)%>%
    select(Date,incident_cases)%>%
    uncount(incident_cases)%>%
    pull(Date) ->local_case_dates
  
  local_case_dates %>%
    incidence(.) -> local_cases
  
  res <- get_R(local_cases, si_mean=si_mean, si_sd=si_sd)
  res$local_case_dates <- local_case_dates
  res$region <- region
  res$si_mean <- si_mean
  res$si_sd <- si_sd
  return(res)
}

region_plot_R <- function(res) {
  plot(res, "R", main=paste("COVID-19 estimated R for", 
                            res$region),
       sub=paste("(assuming serial interval mean =",
                 res$si_mean, 
                 ", sd =", 
                 res$si_sd,")"),
       bty="n")
}

region_plot_lambda <- function(res) {
  plot(res, "lambdas", scale = length(res$local_case_dates) + 1,
       bty="n")
  title(sub=paste("\nEstimated", expression(lambda), "for", 
                  res$region, 
                  "(assuming serial interval mean =",
                  res$si_mean, 
                  ", sd =", 
                  res$si_sd, ")"))
  points(res$local_case_dates, seq_along(res$local_case_dates), pch = 20, cex = 2)
}


lambda_plot_w<- function(reg){
  res_obj <- region_earlyR(df,
                           reg,
                           si_mean,
                           si_sd)
  
  region_plot_lambda(res_obj)
}


si_mean <- 5.0
si_sd <- 3.4
alt_si_mean <- 7.5
alt_si_sd <- 3.4
##End of the models


#####
#Combined metrics

table_R <- function(df, col) {
  
  cases_regions <- df %>%
    filter(Regions == col)%>%
    #filter(Date >= ymd("2020-02-28"))%>%
    filter(!is.na(incident_cases))%>%
    filter(incident_cases > 0)%>%
    select(Date, incident_cases)%>%
    rename(dates=Date,
           I=incident_cases)
  estimate_R_obj <- estimate_R(cases_regions$I,
                               method="uncertain_si",
                               config = make_config(list(
                                 mean_si = 7.5, std_mean_si = 2.0,
                                 min_mean_si = 1, max_mean_si = 8.4,
                                 std_si = 3.4, std_std_si = 1.0,
                                 min_std_si = 0.5, max_std_si = 4.0,
                                 n1 = 1000, n2 = 1000)))
  
  return(tibble(time = estimate_R_obj[["R"]][["t_start"]], R = estimate_R_obj[["R"]][["Mean(R)"]]))
  
}





Quebec <- table_R(df, "Quebec")
Berlin <- table_R(df, "Berlin")
NewYork <- table_R(df, "NewYork")
Milan <- table_R(df, "Lombardy")




#All Tables

Rdata <- list(Quebec, Berlin, NewYork, Milan) %>% 
  reduce(full_join, by="time") %>% arrange(time) %>%
  rename(Quebec = R.x, Berlin = R.y, NewYork = R.x.x, Lombardy = R.y.y)
Rdata_long <- gather(Rdata, region, R_value, Quebec:Lombardy, factor_key = TRUE) %>% arrange(time,region)

Rdata_long %>% ggplot(aes(x=time, y = R_value, group=region))+
  xlab("Days Since the First Case Recorded") + ylab("Mean Instant R0 value (Standard deviation not shown)")+
  ggtitle("Comparing Instantaneous R0 between Regions Around the World") +
  theme_light()+
  geom_line(aes(colour=region), size = 1, linetype = 1)+
  geom_hline(yintercept=1, linetype="dashed")+
  scale_color_hue()+
  scale_colour_discrete(name = "Regions",
                      breaks = c("Quebec", "Berlin", "NewYork", "Lombardy"),
                      labels = c("Province of Quebec", "Berlin", "New York State", "Region of Lombardy"))


