########################## Create Global Variables ########################## 

########## Attach Packages ########## 

library(shiny)
library(shinythemes)
library(shinycssloaders)
library(tidyverse)
library(tidyselect)
library(lubridate)
library(here)
library(patchwork)
library(kableExtra)
library(psych)
library(DT)
library(plotly)
library(randomForest)


######### Source functions ########## 

source(here::here("R", "plot_imp.R"))
source(here::here("R", "plotly_partial_dependence.R"))
source(here::here("R", "full_name_units.R"))


########## Set working directory ########## 

setwd(here::here())


########## Get datasets from the feature importance workflow ########## 

# Original and Aggregate Datasets -----------------------------------------

# Raw dataset
df <- readRDS(here::here("data", "sageres.RDS")) %>% 
  as.data.frame()

# Main aggregated dataset
df_wy <- readRDS(here::here("shiny", "aggregated_datasets", "df_wy.RDS")) %>%
  as.data.frame()

# Aggregated dataset for climate scenario 0
df_wy0 <- readRDS(here::here("shiny", "aggregated_datasets", "df_wy0.RDS")) %>%
  as.data.frame()

# Aggregated dataset for climate scenario 2
df_wy2 <- readRDS(here::here("shiny", "aggregated_datasets", "df_wy2.RDS")) %>%
  as.data.frame()

all_datasets <- c("df", "df_wy", "df_wy0", "df_wy2")

# Variable Importance Datasets --------------------------------------------

imp_wy0 <- readRDS(here::here("shiny", "aggregated_datasets", "imp_wy0.RDS")) %>%
  arrange(Rank) %>%
  as.data.frame()

imp_wy2 <- readRDS(here::here("shiny", "aggregated_datasets", "imp_wy2.RDS")) %>%
  arrange(Rank) %>%
  as.data.frame()

# Metadata Table ----------------------------------------------------------

metadata <- readRDS(here::here("shiny", "aggregated_datasets", "metadata.RDS")) %>%
  select("variable", "full_name", "units", "description") %>%
  as.data.frame()

# Partial Dependence Plot Data --------------------------------------------

# Get random forest models
rf_wy0 <- readRDS(here::here("data", "rf_wy0.RDS"))
rf_wy2 <- readRDS(here::here("data", "rf_wy2.RDS"))

# Create reduced data frames from the random forest models
df_wy0_reduced <- df_wy0 %>%
  select(c(rownames(rf_wy0$finalModel$importance)))

df_wy2_reduced <- df_wy2 %>%
  select(c(rownames(rf_wy2$finalModel$importance)))


########## User Inputs ########## 

factor_vars <- c("stratumID", "scen", "topo")
response_var <- colnames(df_wy[1])

######### Text for the welcome page ########## 

welcome <- "Welcome to the RHESSys Interpretation App. Use this app to explore your RHESSys output data."

intro_1 <- "- Use the \"Metadata\" tab to learn more about each variable within your dataset. To add or remove variables specific to your dataset, you can do so using the metadata.Rmd file included within the github repo."

intro_2 <- "- The \"Dataset Viewer\" tab allows you to view your datasets, both raw and aggregated. Datasets are imported from the random forest or gradient boosting workflow. If you have not already run the workflow with your own RHESSys dataset, the default data will be from Sagehen Creek."

intro_3 <- "- The \"Variable Importance\" tab will display variable importance from the random forest or gradient boosting workflow. If you have not gone through this workflow, we recommend doing that first. They can be found on the RHESSys Github Wiki."

intro_4 <- "- The \"Visualizations\" tab will allow you to explore your data's variables and their relationships."

importance_caption <- paste0("The above graphs uses the random forest workflow to rank how important each varible is in predicting your response variable, in this case", 
                             full_name_units(response_var, metadata, units = FALSE), 
                             ". The graph on the left ranks the variables in a normal climate scenario, and the graph on the right ranks the variables in a +2 degree C climate warming scenario.")


########## Inputs ########## 

# Dataset Viewer Inputs ---------------------------------------------------

dataset_sel <- selectInput(
  inputId = "dataset_sel",
  label = tags$h4("Select your dataset to view:"),
  choices = c(
    "Raw Data",
    "Aggregated Data",
    "Aggregated Data (Normal Climate)",
    "Aggregated Data (+2 Degree C Climate)"
  ),
  selected = "Aggregated Data"
)

# Visualizations Inputs ---------------------------------------------------

stratum_sel <- checkboxGroupInput("stratum_sel",
  label = tags$h4("Select desired strata to look at:"),
  choices = unique(df_wy$stratumID),
  selected = unique(df_wy$stratumID)
)

topo_sel <- checkboxGroupInput("topo_sel",
  label = tags$h4("Select topography types to look at:"),
  choices = c(
    "Upslope" = "U",
    "Mid-slope" = "M",
    "Riparian" = "R"
  ),
  selected = c(
    "Upslope" = "U",
    "Mid-slope" = "M",
    "Riparian" = "R"
  )
)

clim_sel <- checkboxGroupInput("clim_sel",
  label = tags$h4("Select your climate scenario(s):"),
  choices = c(
    "Normal Scenario" = 0,
    "+2 Degree C Scenario" = 2
  ),
  selected = c(
    "Normal Scenario" = 0,
    "+2 Degree C Scenario" = 2
  )
)

wy_sel <- sliderInput("wy_sel",
  label = tags$h4("Select water year range:"),
  min = min(df_wy$wy),
  max = max(df_wy$wy),
  value = c(min(df_wy$wy), max(df_wy$wy)),
  sep = "",
  step = 1
)

dependent_variable <- varSelectInput(
  inputId = "dependent_variable",
  label = tags$h4("Select your dependent variable:"),
  data = df_wy,
  selected = "npp"
)

independent_variable <- varSelectInput(
  inputId = "independent_variable",
  label = tags$h4("Select your independent variable:"),
  data = df_wy,
  selected = "precip"
)

facet_variable <- varSelectInput(
  inputId = "facet_variable",
  label = tags$h6("Here you can pick a variable to facet the graph by. This allows you to see how the relationships between your independent and dependent variables change at different levels of your facet variable. This takes the range of your facet variable and splits the data into even quantiles. Select variable to facet by here:"),
  data = df_wy,
  selected = "rz_storage"
)

quantile_slider <- sliderInput("quantile_sel",
  label = tags$h4("How many quantiles to facet?"),
  min = 1,
  max = 9,
  value = 1
)

# Partial Dependence Inputs -----------------------------------------------

partial_dep_model <- selectInput("partial_dep_model",
  label = tags$h4("Select your model"),
  choices = c("Normal Scenario", "+2 Degree C Scenario"),
  selected = "Normal Scenario",
  multiple = FALSE
)

partial_dep_var1 <- selectInput("partial_dep_var1",
  label = tags$h4("Select Variable 1"),
  choices = colnames(df_wy0_reduced),
  multiple = FALSE
)

partial_dep_var2 <- selectInput("partial_dep_var2",
  label = tags$h4("Select Variable 2"),
  choices = colnames(df_wy0_reduced),
  multiple = FALSE
)
