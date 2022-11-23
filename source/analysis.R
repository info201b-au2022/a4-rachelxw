library(tidyverse)
library("dplyr")
library("ggplot2")
library("ggmap")

# The functions might be useful for A4
source("../source/a4-helpers.R")

## Load data frame ---- 
data <- get_data(100000)
state_name <- read.csv("../source/state_names_and_codes.csv", stringsAsFactors = FALSE)
## Test queries ----
#----------------------------------------------------------------------------#
# Simple queries for basic testing
#----------------------------------------------------------------------------#
# Return a simple string
test_query1 <- function() {
  return ("Hello world")
}

# Return a vector of numbers
test_query2 <- function(num=6) {
  v <- seq(1:num)
  return(v)
}

## Section 2  ---- 
#----------------------------------------------------------------------------#
#average value of total black jail population across all counties in 2018
avg_prop_jail_black_2018 <- data %>%
  group_by(year) %>%
  filter(year == 2018) %>%
  summarize(prop_black_jail_2018 = sum(black_jail_pop, na.rm = TRUE) / sum(total_jail_pop, na.rm = TRUE)) %>%
  pull(prop_black_jail_2018)
#proportion of black people in 2018
prop_black_2018 <- data %>%
  group_by(year) %>%
  filter(year == 2018) %>%
  summarize(prop_black_2018 = sum(black_pop_15to64, na.rm = TRUE) / sum(total_pop, na.rm = TRUE)) %>%
  pull(prop_black_2018)
#return the county name which has the highest proportion of black people in jail in 2018
highest_prop_black_jail_county_2018 <- data %>%
  filter(year == 2018) %>%
  mutate(prop_black = black_jail_pop / total_jail_pop) %>%
  filter(!is.na(prop_black)) %>%
  filter(prop_black == max(prop_black)) %>%
  pull(county_name)


#----------------------------------------------------------------------------#

## Section 3  ---- 
#----------------------------------------------------------------------------#
# Growth of the U.S. Prison Population
# 
#----------------------------------------------------------------------------#
# This function return the total jail population each year
get_year_jail_pop <- function() {
  # TODO: Implement this function
  total_jail_pop_year <- data %>%
  group_by(year) %>%
  summarise(total_jail_pop = sum(total_jail_pop, na.rm = TRUE))
  return(total_jail_pop_year)
}
# This function produce a bar chart shows the growth of the U.S. prison population from 1970 to 2018
plot_jail_pop_for_us <- function() {
  # TODO: Implement this function
  total_jail_pop_chart <- get_year_jail_pop() %>%
    ggplot(total_jail_pop_year, mapping = aes(x = year, y = total_jail_pop)) +
      geom_col() +
    labs(
      title = "Increase of Jail Population in U.S. (1970-2018)",
      x = "Year",
      y = "Total Jail Population"
      )
  return(total_jail_pop_chart)   
} 
plot_jail_pop_for_us()
## Section 4  ---- 
#----------------------------------------------------------------------------#
# Growth of Prison Population by State 

get_jail_pop_by_states <- function(states) {
  jail_pop_by_states_df <- data %>%
  filter(state %in% states) %>%
  group_by(state, year) %>%
  summarize(total_jail_pop = sum(total_jail_pop, na.rm = TRUE))
  return(jail_pop_by_states_df)
}
states_vector <- c("AK","CA","NC")

#produce a line graph of counties' total jail population changes from 1970 to 2018
plot_jail_pop_by_states <- function(states) {
  jail_pop_by_states_chart <- ggplot(
    get_jail_pop_by_states(states))+
    geom_line(mapping = aes(x = year, y = total_jail_pop, group = state, color = state)) +
    labs(
      title = "Increase of Jail Population in U.S. (1970-2018)",
      x = "Year",
      y = "Total Jail Population"
    )
  return(jail_pop_by_states_chart)
}
plot_jail_pop_by_states(states_vector)

# See Canvas
#----------------------------------------------------------------------------#
## Section 5  ---- 
#----------------------------------------------------------------------------#
# <variable comparison that reveals potential patterns of inequality>
get_black_prop <- function() {
  black_prop_df <- data %>%
    group_by(year) %>%
    summarize(black_prop_jail = (sum(black_jail_pop, na.rm=TRUE) / sum(black_pop_15to64, na.rm=TRUE)) / (sum(white_jail_pop, na.rm=TRUE) / sum(white_pop_15to64, na.rm=TRUE))) %>%
    drop_na()
  return(black_prop_df)
}
plot_black_jail_prop <- function() {
  chart_black_jail_prop <- ggplot(
    get_black_prop()) +
    geom_line(mapping = aes(x = year, y = black_prop_jail)) +
    labs(
      title = "Relative proportion of black's jail proportion and white's jail proportion (1970-2018)",
      x = "Year",
      y = "Multiplicity ratio (black vs. white)"
    )
  return(chart_black_jail_prop)
}
# See Canvas
#----------------------------------------------------------------------------#

## Section 6  ---- 
#----------------------------------------------------------------------------#
# <a map shows potential patterns of inequality that vary geographically>
map_df <- function() {
  black_prop_df <- data %>%
    group_by(state) %>%
    summarize(black_vs_white = (sum(black_jail_pop, na.rm=TRUE) / sum(black_pop_15to64, na.rm=TRUE)) / (sum(white_jail_pop, na.rm=TRUE) / sum(white_pop_15to64, na.rm=TRUE))) %>%
    drop_na() %>%
    rename(Code = state)
  return(black_prop_df)
}
map_df <- map_df()

state_name <- state_name %>%
  mutate(state = tolower(State))

state_shape <- map_data("state") %>%
  rename(state = region) %>%
  left_join(state_name, by = "state") %>%
  left_join(map_df, by = "Code")

map_chart <- function() {
    p <- ggplot(state_shape) +
      geom_polygon(
        mapping = aes(x = long, y = lat, group = group, fill = black_vs_white),
        color = "black",
        size = 0.3
      ) +
      
      scale_fill_continuous(low = "Pink", high = "Red") +
      labs(
        fill = "ratio",
        title = "United States (1970-2018)"
        ) 
    return(p)
}  
# See Canvas
#----------------------------------------------------------------------------#


