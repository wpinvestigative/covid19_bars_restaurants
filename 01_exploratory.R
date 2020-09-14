
if (!require("devtools")) {
  install.packages("devtools")
}

packages <- c("tidyverse", "MMWRweek", "lubridate", "geofacet",
              "lubridate", "censusapi", "janitor")

if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(packages, rownames(installed.packages())), repos = "http://cran.us.r-project.org")  
}


library(tidyverse)
library(MMWRweek)
library(lubridate)
library(geofacet)
library(censusapi)
library(janitor)

# get your census api key: https://api.census.gov/data/key_signup.html
census_key <- "yourcensuskeygoeshere"
Sys.setenv(CENSUS_KEY=census_key)

#### covid cases prep ----

# get population for each state
state2019 <- getCensus(name="2019/pep/population",
                       vars=c("NAME", "POP"),
                       region="state:*")

# build up crosswalk file for state names and abbreviations
# don't forget DC
state_names <- c(state.name, "District of Columbia")
state_abbs <- c(state.abb, "DC")
states_all <- data.frame(state_names, state_abbs)
colnames(states_all) <- c("STATE", "region")

# import daily case count data as compiled by The Washington Post
covid <- read_csv("data/wapo_state_daily.csv")

# aggregate case count by week and state
covid_week <- covid %>% 
  left_join(states_all) %>% 
  group_by(region, MMWRyear, MMWRweek) %>% 
  summarize(cases=sum(cases, na.rm=T),
            tot_death=sum(sum(deaths, na.rm=T)))

# joining the covid summarized data with state population data
covid_week <- left_join(covid_week, states)

# calculating per capita
# cases per 10,000 people
# deaths per 100,000 people
covid_week <- covid_week %>% 
  mutate(POP=as.numeric(POP)) %>% 
  mutate(cases_pc=round(cases/POP*10000,2),
         death_pc=round(tot_death/POP*100000,2))

#### bars ----

# load SafeGraph data
barsdf <- read_csv("data/daily_bars_state.csv") %>% 
  filter(MMWRyear==2020)

# load normalization stats
dfn <- read_csv("data/normalized_us.csv") %>% 
  filter(year==2020)

# group by week
dfn <- dfn %>% 
  group_by(MMWRyear, MMWRweek) %>% 
  summarize(total_devices=sum(total_devices_seen))

# have to normalize the SafeGraph data with nummber of devices
# anything beyond week 31 gets a little wonky with the numbers
barsdf_week <- barsdf %>% 
  group_by(MMWRyear, MMWRweek, region) %>% 
  summarize(visits=sum(visits)) %>% 
  left_join(dfn) %>% 
  mutate(per=visits/total_devices*1000) %>% 
  filter(MMWRweek<31)

## sanity check - mapping the standardized foot traffic to bars
barsdf_week %>% 
  ggplot(aes(x=MMWRweek, y=visits)) +
  geom_line() +
  facet_geo(~region, grid="us_state_grid2", scales="free_y") 


# joining bar data to covid cases data
barsdf_all <- barsdf_week %>% left_join(covid_week)   

# structuring the data for easy charting of foot traffic, cases, and deaths
barsdf_narrow <- barsdf_all %>% 
  select(MMWRweek, region, per, cases_pc, death_pc) %>% 
  mutate(per=per*100) %>% 
  pivot_longer(cols=4:6, names_to="type", values_to="values")


# chart of foot traffic, cases, and deaths
barsdf_narrow %>% 
  mutate(type=case_when(
    type=="cases_pc" ~ "covid positives per capita",
    type=="death_pc" ~ "deaths per capita",
    TRUE ~ "foot traffic to bars"
  )) %>% 
  ggplot(aes(x=MMWRweek, y=values, group=type, color=type)) +
  geom_line() +
  facet_geo(~region, grid="us_state_grid2") +
  labs(title="Visitors to bars and covid cases/deaths")

# bringing in data on when restaurants and bars closed and reopened
open <- read_csv("data/state_dates.csv")

# cleaning up the data
open <- open %>% 
  mutate(date=mdy(date)) %>% 
  rename(region=state)

# isolating the rows to only look at bar opening dates
bars_open <- open %>% 
  filter(bars=="T") %>% 
  group_by(region) %>% 
  arrange(region, date) %>% 
  slice(1)

# figuring out the MMWR weeks per date
bars_mmwr <-  MMWRweek(bars_open$date)
bars_open <- cbind(bars_open, bars_mmwr)

# setting aside a dataframe of when bars reclosed
bars_closed <- bars_open %>% 
  filter(bars=="C")

bars_opened <- bars_open %>% 
  filter(bars!="C")

# chart of foot traffic, cases, and deaths like before but with lines 
# indicating opening and closings

barsdf_narrow %>% 
  mutate(type=case_when(
    type=="cases_pc" ~ "covid positives per capita",
    type=="death_pc" ~ "deaths per capita",
    TRUE ~ "foot traffic to bars"
  )) %>% 
  ggplot(aes(x=MMWRweek, y=values, group=type, color=type)) +
  geom_line() +
  geom_vline(data=bars_opened, aes(xintercept=MMWRweek), alpha=.5) +
  geom_vline(data=bars_closed, aes(xintercept=MMWRweek), alpha=.5, color="purple") +
  facet_geo(~region, grid="us_state_grid2", scales="free_y") +
  labs(title="Visitors to bars and covid cases/deaths")

# figuring out what weeks were before and after the week 
# a state reopened bars
bars_open <- bars_open %>% 
  mutate(week4a=MMWRweek-4,
         week3a=MMWRweek-3,
         week2a=MMWRweek-2,
         week1a=MMWRweek-1,
         week0=MMWRweek,
         week1b=MMWRweek+1,
         week2b=MMWRweek+2,
         week3b=MMWRweek+3,
         week4b=MMWRweek+4)




for (i in 1:nrow(bars_open)) {
  if (bars_open$bars[i]=="T") {
    state_of <- bars_open$region[i]
    
    # only care about week the bar opened and three weeks later
    # can adjust this loop later if interested in groups of other weeks  
    pull_array2_before <- c(bars_open$week0[i])
    pull_array2_after <- c(bars_open$week3b[i])
    
    # grabbing a state's data and the weeks we're interested in    
    pulled_df_before2 <- barsdf_all %>% 
      filter(region==state_of) %>% 
      filter(MMWRweek %in% pull_array2_before) %>% 
      mutate(type="opened")
    
    # grabbing a state's data and the weeks we're interested in    
    pulled_df_after2 <- barsdf_all %>% 
      filter(region==state_of) %>% 
      filter(MMWRweek %in% pull_array2_after) %>% 
      mutate(type="after")
    
    # joining the two groups: opened and after
    pulled_df_ba2 <- rbind(pulled_df_before2, pulled_df_after2)
    
    # isolating the weeks and type for later
    pulled_weeks2 <- pulled_df_ba2 %>% 
      select(MMWRweek, type)
    
    
    pulled_df_summary2 <- pulled_df_ba2 %>% 
      group_by(region, type) %>% 
      mutate(avg_per=per/total_devices,
             cases_pc=round(cases/POP*10000,2),
             deaths_pc=round(tot_death/POP*1000000,2))
    
    
    if (i==1) {
      pulled_df_all2 <- pulled_df_ba2
      state_agg2 <- pulled_df_summary2
    } else {
      state_agg2 <- rbind(state_agg2, pulled_df_summary2)
      pulled_df_all2 <- rbind(pulled_df_all2, pulled_df_ba2)
    }
    print(i)
  }
  
}

#### restaurants ----


# load safegraph data
restdf <- read_csv("data/daily_restaurants_state.csv")

# load normalization stats
dfn <- read_csv("data/normalized_us.csv")

# group by week
dfn <- dfn %>% 
  group_by(MMWRyear, MMWRweek) %>% 
  summarize(total_devices=sum(total_devices_seen))

# have to normalize the safegraph data with nummber of devices
# anything beyond week 31 gets a little wonky with the numbers
restdf_week <- restdf %>% 
  group_by(MMWRyear, MMWRweek, region) %>% 
  summarize(rest_visits=sum(visits)) %>% 
  left_join(dfn) %>% 
  mutate(per_rest=rest_visits/total_devices*1000) %>% 
  filter(MMWRweek<31)

#### for export ----

event_dates <- read_csv("data/event_dates.csv")

ed_dates <- MMWRweek(event_dates$date)
event_dates <- cbind(event_dates, ed_dates)
event_dates <- event_dates %>% 
  select(region=state, detail, detail_date=date, MMWRyear, MMWRweek)

barsdf_all <- barsdf_all %>% 
  rename(bar_visits=visits, bar_per=per)

barsdf_all <- left_join(barsdf_all, restdf_week)

for_export <- barsdf_all %>% left_join(event_dates)

weeks <- read_csv("data/weeks.csv") %>% 
  filter(SEASON=="2019-20") %>% 
  select(MMWRweek=WEEK, start_date) %>% 
  mutate(start_date=dmy(start_date)-1)

for_export <- left_join(for_export, weeks)

bars_baseline <- for_export %>% 
  filter(detail=="bars reopen") %>% 
  select(region, bars_baseline=MMWRweek)%>% 
  unique()

for_export <- left_join(for_export, bars_baseline) 

for_export <- for_export %>% 
  mutate(bars_baseline=MMWRweek-bars_baseline)


rest_baseline <- for_export %>% 
  filter(detail=="restaurants reopen") %>% 
  select(region, rest_baseline=MMWRweek)%>% 
  unique()

for_export <- left_join(for_export, rest_baseline) 

for_export <- for_export %>% 
  mutate(rest_baseline=MMWRweek-rest_baseline)


shutdown_baseline <- for_export %>% 
  filter(detail=="shut down") %>% 
  select(region, shutdown_baseline=MMWRweek)%>% 
  unique()

for_export <- left_join(for_export, shutdown_baseline) 

for_export <- for_export %>% 
  mutate(shutdown_baseline=MMWRweek-shutdown_baseline)


reclosed_baseline <- for_export %>% 
  filter(detail=="bars reclosed" | detail=="restaurants reclosed") %>% 
  select(region, reclosed_baseline=MMWRweek)%>% 
  unique()

for_export <- left_join(for_export, reclosed_baseline) 

for_export <- for_export %>% 
  mutate(reclosed_baseline=MMWRweek-reclosed_baseline)


reopened_again_baseline <- for_export %>% 
  filter(detail=="bars reopen again" | detail=="restaurants reopen again") %>% 
  select(region, reopened_again_baseline=MMWRweek) %>% 
  unique()

for_export <- left_join(for_export, reopened_again_baseline) 

for_export <- for_export %>% 
  mutate(reopened_again_baseline=MMWRweek-reopened_again_baseline)


for_export <- for_export %>% 
  select(region, NAME, MMWRyear, MMWRweek, start_date, detail, detail_date, cases, cases_pc, deaths=tot_death,
         deaths_pc=death_pc, bar_visits, bar_per, rest_visits, rest_per=per_rest,
         total_devices, population=POP, shutdown_baseline, bars_reopen_baseline=bars_baseline, 
         rest_reopen_baseline=rest_baseline, reclosed_baseline, reopened_again_baseline
  ) 

for_export <- for_export %>% 
  arrange(NAME, MMWRweek)

write_csv(for_export, "data/for_export.csv", na="")

### correlation tester

# just looking at bar traffic and cases 2 weeks later (not change in cases week to week)
for_correlation <- for_export %>% 
  group_by(NAME) %>% 
  arrange(NAME, MMWRweek) %>% 
  filter(shutdown_baseline>=0) %>% 
  select(-detail, -detail_date) %>%
  unique() %>% 
  mutate(weeks_later=lead(cases, 2)) %>% 
  filter(!is.na(weeks_later)) %>% 
  summarize(COR=cor(bar_per, weeks_later))

# looking at correlation between weekly bar traffic change and weekly covid case change (3 week lag)
for_correlation2 <- for_export %>% 
  group_by(NAME) %>% 
  arrange(NAME, MMWRweek) %>% 
  filter(shutdown_baseline>=0) %>% 
  select(-detail, -detail_date) %>%
  unique() %>% 
  mutate(week_lead_pc_a=lead(cases_pc, 2),
         week_lead_pc_b=lead(cases_pc, 3),
         week_prior_bar_traffic=lag(bar_per, 1),
         week_prior_rest_traffic=lag(rest_per, 1),
         week_lead_case_change=(week_lead_pc_b-week_lead_pc_a)/week_lead_pc_a*100,
         week_prior_bar_change=(bar_per-week_prior_bar_traffic)/week_prior_bar_traffic*100,
         week_prior_rest_change=(rest_per-week_prior_rest_traffic)/week_prior_rest_traffic*100) %>% 
  filter(!is.na(week_lead_case_change)) %>% 
  filter(!is.na(week_prior_bar_change)) %>% 
  summarize(COR_bar=cor(week_lead_case_change, week_prior_bar_change),
            COR_rest=cor(week_lead_case_change, week_prior_rest_change))

#### visualize ----

shutdown_baseline <- for_export %>% 
  filter(detail=="shut down") %>% 
  unique()

bars_reopen_baseline<- for_export %>% 
  filter(detail=="bars reopen") %>% 
  unique()
rest_reopen_baseline<- for_export %>% 
  filter(detail=="restaurants reopen") %>% 
  unique()

reclosed_baseline<- for_export %>% 
  filter(detail=="restaurants reclosed" | detail=="bars reclosed") %>% 
  unique()

reopened_again_baseline<- for_export %>% 
  filter(detail=="restaurants reopen again" | detail=="bars reopen again") %>% 
  unique()

# visualizing cases per capita and when bars/restaurants opened/closed
for_export %>% 
  select(-detail, -detail_date, -shutdown_baseline, -bars_reopen_baseline,-rest_reopen_baseline,
         -reclosed_baseline,-reopened_again_baseline) %>%
  unique() %>% 
  ggplot(aes(x=start_date, y=cases_pc)) +
  geom_col() +
  geom_vline(data=shutdown_baseline, aes(xintercept=start_date), alpha=.5, color="tomato") +
  geom_vline(data=bars_reopen_baseline, aes(xintercept=start_date), alpha=.5, color="midnightblue") +
  geom_vline(data=rest_reopen_baseline, aes(xintercept=start_date), alpha=.5, color="blue2") +
  geom_vline(data=reclosed_baseline, aes(xintercept=start_date), alpha=.5, color="tomato3") +
  geom_vline(data=reopened_again_baseline, aes(xintercept=start_date), alpha=.5, color="dodgerblue4") +
  facet_geo(~region)



# visualizing bar foot traffic and cases per capita
for_export %>% 
  select(-detail, -detail_date, -shutdown_baseline, -bars_reopen_baseline,-rest_reopen_baseline,
         -reclosed_baseline,-reopened_again_baseline) %>%
  unique() %>% 
  mutate(MMWRweek_lead=lead(MMWRweek, 3)) %>% 
  ggplot() +
  geom_line(aes(x=start_date, y=cases_pc), color="tomato") +
  geom_line(aes(x=start_date, y=bar_per*100), color="dodgerblue") +
  facet_geo(~region, scale="free_y")

#### visualizing with the weeks reopening as a constant ----

three <- c(-3, 0, 3)
four<- c(-4, -3, -2, -1, 0, 1,2,3, 4)

for_export %>% 
  #  filter(bars_baseline %in% three) %>% 
  filter(bars_reopen_baseline %in% four) %>% 
  select(-detail, -detail_date) %>% 
  mutate(bar_per=bar_per*100) %>% 
  unique() %>% 
  filter(bar_per<200) %>% 
  ggplot(aes(x=bars_reopen_baseline, y=cases_pc)) +
  geom_line() +
  geom_line(aes(x=bars_reopen_baseline, y=bar_per), color="blue") +
  geom_vline(aes(xintercept=0), alpha=.5, color="red")+
  facet_geo(~region) +
  labs(title="cases per capita four weeks before, when bars reopened, and four weeks after")


#### national bars ---
nat_bars <- for_export %>% 
  filter(bars_reopen_baseline %in% c(0, 4)) %>% 
  select(-detail, -detail_date) %>% 
  mutate(bar_per=bar_per*100) %>% 
  unique() %>% 
  select(region, NAME, MMWRyear, cases, population) %>% 
  arrange(region, NAME, MMWRyear, MMWRweek) %>% 
  group_by(region, NAME, MMWRyear) %>% 
  mutate(id=row_number()) %>% 
  mutate(id=case_when(
    id == 1 ~ "open",
    TRUE ~ "after"
  )) %>% 
  group_by(id) %>% 
  summarize(cases=sum(cases),
            population=sum(population)) %>% 
  mutate(cases_per_10k=cases/population*10000)

nat_bar_traffic <- for_export %>% 
  filter(bars_reopen_baseline %in% c(0, 4)) %>% 
  select(-detail, -detail_date) %>% 
  unique() %>% 
  select(region, NAME, MMWRyear, bar_visits, total_devices) %>% 
  arrange(region, NAME, MMWRyear, MMWRweek) %>% 
  group_by(region, NAME, MMWRyear) %>% 
  mutate(id=row_number()) %>% 
  mutate(id=case_when(
    id == 1 ~ "open",
    TRUE ~ "after"
  )) %>% 
  group_by(id) %>% 
  summarize(bar_visits=sum(bar_visits),
            total_devices=mean(total_devices)) %>% 
  mutate(bar_traffic=bar_visits/total_devices*1000)

nat_bars <- left_join(nat_bars, nat_bar_traffic) %>% 
  select(id, cases, cases_per_10k, bar_traffic)

bars_table_cases <- for_export %>% 
  filter(bars_reopen_baseline %in% c(0, 4)) %>% 
  select(-detail, -detail_date) %>% 
  mutate(bar_per=bar_per*100) %>% 
  unique() %>% 
  select(region, NAME, MMWRyear, cases_pc) %>% 
  arrange(region, NAME, MMWRyear, MMWRweek) %>% 
  group_by(region, NAME, MMWRyear) %>% 
  mutate(id=row_number()) %>% 
  mutate(id=case_when(
    id == 1 ~ "open",
    TRUE ~ "after"
  )) %>% 
  ungroup() %>% 
  select(-MMWRweek) %>% 
  pivot_wider(names_from="id",values_from="cases_pc") %>% 
  mutate(more=case_when(
    after>open ~ "More", 
    TRUE ~ "Less"))

table(bars_table_cases$more)

#### national restaurants ---

nat_rest <- for_export %>% 
  filter(rest_reopen_baseline %in% c(0, 4)) %>% 
  select(-detail, -detail_date) %>% 
  mutate(rest_per=rest_per*100) %>% 
  unique() %>% 
  select(region, NAME, MMWRyear, cases, population) %>% 
  arrange(region, NAME, MMWRyear, MMWRweek) %>% 
  group_by(region, NAME, MMWRyear) %>% 
  mutate(id=row_number()) %>% 
  mutate(id=case_when(
    id == 1 ~ "open",
    TRUE ~ "after"
  )) %>% 
  group_by(id) %>% 
  summarize(cases=sum(cases),
            population=sum(population)) %>% 
  mutate(cases_per_10k=cases/population*10000)

nat_rest_traffic <- for_export %>% 
  filter(rest_reopen_baseline %in% c(0, 4)) %>% 
  select(-detail, -detail_date) %>% 
  unique() %>% 
  select(region, NAME, MMWRyear, rest_visits, total_devices) %>% 
  arrange(region, NAME, MMWRyear, MMWRweek) %>% 
  group_by(region, NAME, MMWRyear) %>% 
  mutate(id=row_number()) %>% 
  mutate(id=case_when(
    id == 1 ~ "open",
    TRUE ~ "after"
  )) %>% 
  group_by(id) %>% 
  summarize(rest_visits=sum(rest_visits),
            total_devices=mean(total_devices)) %>% 
  mutate(rest_traffic=rest_visits/total_devices*1000)
nat_rest <- left_join(nat_rest, nat_rest_traffic) %>% 
  select(id, cases, cases_per_10k, rest_traffic)

rest_table_cases <- for_export %>% 
  filter(rest_reopen_baseline %in% c(0, 4)) %>% 
  select(-detail, -detail_date) %>% 
  mutate(rest_per=rest_per*100) %>% 
  unique() %>% 
  select(region, NAME, MMWRyear, cases_pc) %>% 
  arrange(region, NAME, MMWRyear, MMWRweek) %>% 
  group_by(region, NAME, MMWRyear) %>% 
  mutate(id=row_number()) %>% 
  mutate(id=case_when(
    id == 1 ~ "open",
    TRUE ~ "after"
  )) %>% 
  ungroup() %>% 
  select(-MMWRweek) %>% 
  pivot_wider(names_from="id",values_from="cases_pc") %>% 
  mutate(more=case_when(
    after>open ~ "More", 
    TRUE ~ "Less"))

table(rest_table_cases$more)

### shut down ----

shutdown <- for_export %>% 
  filter(shutdown_baseline %in% c(0, 4)) %>% 
  select(-detail, -detail_date) %>% 
  mutate(rest_per=rest_per*100, bar_per=bar_per*100) %>% 
  unique() %>% 
  select(region, NAME, MMWRyear, cases, population) %>% 
  arrange(region, NAME, MMWRyear, MMWRweek) %>% 
  group_by(region, NAME, MMWRyear) %>% 
  mutate(id=row_number()) %>% 
  mutate(id=case_when(
    id == 1 ~ "shutdown",
    TRUE ~ "after"
  )) %>% 
  group_by(id) %>% 
  summarize(cases=sum(cases),
            population=sum(population)) %>% 
  mutate(cases_per_10k=cases/population*10000)

shutdown_traffic <- for_export %>% 
  filter(shutdown_baseline %in% c(0, 4)) %>% 
  select(-detail, -detail_date) %>% 
  unique() %>% 
  select(region, NAME, MMWRyear, rest_visits, bar_visits, total_devices) %>% 
  arrange(region, NAME, MMWRyear, MMWRweek) %>% 
  group_by(region, NAME, MMWRyear) %>% 
  mutate(id=row_number()) %>% 
  mutate(id=case_when(
    id == 1 ~ "shutdown",
    TRUE ~ "after"
  )) %>% 
  group_by(id) %>% 
  summarize(rest_visits=sum(rest_visits),
            bar_visits=sum(bar_visits),
            total_devices=mean(total_devices)) %>% 
  mutate(rest_traffic=rest_visits/total_devices*1000,
         bar_traffic=bar_visits/total_devices*1000)

shutdown <- left_join(shutdown, shutdown_traffic) %>% 
  select(id, cases, cases_per_10k, rest_traffic, bar_traffic)

shutdown_table_cases <- for_export %>% 
  filter(shutdown_baseline %in% c(0,4)) %>% 
  select(-detail, -detail_date) %>% 
  mutate(rest_per=rest_per*100,
         bar_per=bar_per*100) %>% 
  unique() %>% 
  select(region, NAME, MMWRyear, cases_pc) %>% 
  arrange(region, NAME, MMWRyear, MMWRweek) %>% 
  group_by(region, NAME, MMWRyear) %>% 
  mutate(id=row_number()) %>% 
  mutate(id=case_when(
    id == 1 ~ "shutdown",
    TRUE ~ "after"
  )) %>% 
  ungroup() %>% 
  select(-MMWRweek) %>% 
  pivot_wider(names_from="id",values_from="cases_pc") %>% 
  mutate(more=case_when(
    after>shutdown ~ "More", 
    TRUE ~ "Less"))

table(shutdown_table_cases$more)

