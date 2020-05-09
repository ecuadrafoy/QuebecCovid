#installing packages (Only run once if you close the program)
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

#Updating the data (Run This section at the start of the day)
##### 

quebec_wikipedia_data_url <- "https://en.wikipedia.org/wiki/Template:COVID-19_pandemic_data/Canada/Quebec_medical_cases"
quebec_outbreak_table <- read_html(quebec_wikipedia_data_url)


quebec_regions_confirmed <- quebec_outbreak_table %>% html_nodes("table") %>%
                            .[[1]]%>%html_table(header=FALSE, fill=TRUE) %>%
                            slice(-(1:2)) %>%  slice(1:(nrow(.)-3)) %>% #getting rid of extra rows, keeping one for the region names
                            select(X1:X19) %>% #selecting only Region columns
                            filter(!str_detect(X1, "Notes"))

colnames(quebec_regions_confirmed) <- quebec_regions_confirmed %>%
  filter(X1 == "Date") %>%
  unlist(use.names = FALSE)

quebec_regions_confirmed <-quebec_regions_confirmed %>% slice(-1)


quebec_regions_confirmed <- quebec_regions_confirmed %>% 
  mutate(Date=anytime(Date)) %>%
  mutate(Date=ymd(format(Date,"2020-%m-%d")))%>%
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

quebec_regions_confirmed_wide <- quebec_regions_confirmed %>%
  pivot_wider(names_from = Regions,
              values_from = incident_cases)

#End of Cleanup stage

#Visualizations

#####
#Visualizations

#Look at Incidence in the whole province
quebec_regions_confirmed %>% group_by(Date,Regions)%>%
  summarise(incident_cases=sum(incident_cases, na.rm=TRUE))%>%
  ggplot(aes(x=Date,y=incident_cases))+
  geom_bar(stat="identity")+
  labs(y="Incident Cases", title="COVID-19 Incident Cases in Quebec")+
  theme(legend.position = "top")


##Plot By Region

quebec_regions_confirmed %>%
  group_by(Regions)%>%
  summarise(incident_cases=sum(incident_cases, na.rm = TRUE)) %>%
  ggplot(aes(x=reorder(Regions, incident_cases),y=incident_cases))+
  geom_bar(stat="identity")+
  coord_flip()+
  labs(y="Total cases to date",
       x = "",
       title="COVID-19 total cases in Quebec, by administrative region")+
  theme(legend.position="top")+
  theme_economist()


#Isolate top 6 Regions

quebec_regions_confirmed %>%
  group_by(Regions)%>%
  summarise(total=sum(incident_cases, na.rm=TRUE)) %>%
  arrange(desc(total)) %>%
  pull(Regions) -> quebec_regions_order


  
#Plot top6 regions
q <- quebec_regions_confirmed %>%
  filter(Regions %in% quebec_regions_order[1:6]) %>%
  mutate(Regions = ordered(Regions, levels = quebec_regions_order[1:6])) %>%
  ggplot(aes(x=Date, y=incident_cases)) + 
  geom_bar(stat="identity", fill="green") + 
  facet_wrap(Regions ~., scales = "free_y", ncol=2) + labs(y="Daily incremental incidence",
                                                              title="Confirmed cases of COVID-19 in top six Quebec regions",
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

plot_R_q <- function(reg) {
  
  confirmed_cases_q <- quebec_regions_confirmed %>%
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
                                               "\nDate: ",today()))
  
}

epicurve_q <- function(reg){
  quebec_regions_confirmed%>%
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


lambda_plot_q <- function(reg){
  res_obj <- region_earlyR(quebec_regions_confirmed,
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
#Predictions, only modify variable reg
reg <- "Mtl"

#Plot Incidence of Region (Cases over time)
epicurve_q(reg)

#Plot infectiousness
#If the outbreak is brought under control, then the orange bars need to be falling prior to or at the date of the last observation
lambda_plot_q(reg)

#Plot R0 values
plot_R_q(reg)
