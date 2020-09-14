[This data is published under an [Attribution-NonCommercial-ShareAlike 4.0 International (CC BY-NC-SA 4.0) license](https://creativecommons.org/licenses/by-nc-sa/4.0/)]

# About this story

**[Story title pending](https://www.washingtonpost.com/)**

# Methodology

Foot traffic data for this story was provided by SafeGraph, a company that aggregates location data from tens of millions of devices and compares it with building footprints. The Post analyzed its social distancing metrics data set to estimate the share of time spent at home for each county.
 
This data doesn’t account for all devices — only those with one of the apps SafeGraph uses to track GPS location. Since the data relies on GPS pings and official building footprints, it doesn’t account for people who don’t have phones or those who are visiting outdoor parking lots. Bar traffic in particular varies widely state to state, since SafeGraph's data contains a relatively small sampling of bars, but also because the definition of a bar is somewhat loose.
 
The data is not perfect, but it is a good indicator of where people are going.
 
The number of mobile devices recorded fluctuates, so the visits data is normalized according to how many devices were reported each day.
 
Per capita calculations were based on covid-19 case counts tracked by The Washington Post and data from the U.S. Census.

# Scripts

* 01_exploratory.R
* 02_methodology.R
* [02_methodology.html](http://wpinvestigative.github.io/covid19_bars_restaurants/02_methodology.html)

# The data

* daily_bars_state.csv
  - Aggregated daily bar traffic data by state from SafeGraph

* daily_restaurants_state.csv
  - Aggregated daily restaurant traffic data by state from SafeGraph

* event_dates.csv
  - Long file of events (reopenings) by state as tracked by The Washington Post

* for_export.csv
  - Wrangled data used to construct graphics used in story

* normalized_us.csv
  - Safegraph normalization data (like number of devices) to normalize traffic numbers
  
* state_dates.csv
  - (outdated) Long file of events (reopenings) by state as tracked by The Washington Post

* wapo_state_daily.csv
  - Daily case counts tracked by The Washington Post
  
* weeks.csv
  - Data frame of week of the year with corresponding start and end dates

