[This data is published under an [Attribution-NonCommercial-ShareAlike 4.0 International (CC BY-NC-SA 4.0) license](https://creativecommons.org/licenses/by-nc-sa/4.0/)]

# About this story

**[More cities and states are opening bars and restaurants despite mounting evidence of potential danger](https://www.washingtonpost.com/health/2020/09/14/covid-spread-restaurants-bars/)**

# Methodology

Foot traffic data for this article was provided by SafeGraph, a company that aggregates location data from tens of millions of devices and compares it with building footprints. This doesn’t account for all devices — only those with one of the apps SafeGraph uses to track GPS location. Because the data relies on GPS pings and official building footprints, it doesn’t account for people who don’t have phones or those who are visiting outdoor parking lots.

Bar traffic in particular varies widely state to state, because until recently, SafeGraph’s data contained a relatively small sampling of bars, but also because the definition of a bar is somewhat loose. Visits data is normalized according to how many devices were reported each day.

Per capita calculations were based on coronavirus case counts tracked by The Post and data from the census. On average, states were seeing 3.7 cases per 10,000 residents the week they reopened. Three weeks later, that had increased to 6.9 cases per capita.

The correlation coefficient from comparing the increase in bar foot traffic and the increase in cases per capita three weeks later is .489 for 43 point. There is, of course, possible confounding by increases in other types of contact also coinciding with bar reopenings.

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

