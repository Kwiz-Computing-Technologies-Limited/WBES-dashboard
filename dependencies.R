# dependencies.R
# Rhino Framework Dependency Management
# All packages used in the application. The modules load packages with
# box::use(), which deployment tools do not scan, so every package must also be
# listed here or a shinyapps.io / Posit Connect deploy will not install it.

# Core Shiny
library(shiny)
library(bslib)
library(htmltools)

# Rhino Framework
library(box)
library(rhino)

# Visualization
library(plotly)
library(leaflet)
library(DT)
library(htmlwidgets)
library(ggplot2)
library(scales)
library(RColorBrewer)

# Data Manipulation
library(dplyr)
library(tidyr)
library(haven)
library(readr)
library(purrr)
library(stringr)
library(tibble)
library(labelled)
library(zoo)
library(arrow)        # reads the precomputed parquet data artifacts
library(countrycode)
library(MASS)         # distribution fitting

# API Access
library(httr)
library(jsonlite)
library(wbstats)

# UI Enhancements
library(waiter)
library(shinyjs)

# Logging
library(logger)

# Styling
library(sass)

# paths
library(here)

# Mobile UI
library(shinyMobile)

# Utilities
library(rlang)
library(cachem)
