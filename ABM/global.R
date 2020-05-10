
# pkg <- c("shiny", "tidyverse", "R6", "gt")
# lapply(pkg, library, character.only = TRUE)

library(shiny)
library(tidyverse)
library(R6)
library(gt)

source("class.R")

# for ui.R
type_choices <- c("Hedge", "Inversive", "Herd")
plot_choices <- c("stock", "cash", "asset", "decision")

# for server.R
