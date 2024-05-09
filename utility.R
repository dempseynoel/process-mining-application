
# Library ---------------------------------------------------------------------

# Data analysis
library(tidyverse)
library(scales)
library(DT)
library(tools)
library(pilot)
library(lubridate)

# Process map
library(bupaR)
library(edeaR)
library(processmapR)
library(eventdataR)
library(svgPanZoom)
library(DiagrammeRsvg)
library(DiagrammeR)

# Shiny
library(shiny)
library(shinycssloaders)
library(fresh)
library(waiter)
library(bs4Dash)

library(cli)

# Data ------------------------------------------------------------------------

default_event_log <- readRDS("utility/patients.rds")
default_event_log <- default_event_log %>%
  as_tibble() %>% 
  mutate(
    case_id = as.character(patient),
    activity_id = as.character(handling),
    timestamp = time) %>% 
  filter(!registration_type == "complete") %>% 
  select(c("case_id", "activity_id", "timestamp")) %>% 
  simple_eventlog(
    case_id = "case_id",
    activity_id = "activity_id",
    timestamp = "timestamp")

# UI Constants ----------------------------------------------------------------

PROFILES <- list(
  "Absolute" = "absolute",
  "Absolute case" = "absolute_case",
  "Relative case" = "relative_case",
  "Relative" = "relative")
REQUIRED_COL_NAMES <- c("case_id", "activity_id", "timestamp")

pilot::set_pilot_family("Source Sans Pro")

