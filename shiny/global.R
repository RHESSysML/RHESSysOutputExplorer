########################## Create Global Variables ########################## 

########## Attach Packages ########## 

library(shiny)
library(shinythemes)
library(shinydashboard)
library(shinycssloaders)
library(ggbiplot) # Must be loaded before tidyverse due to plyr conflicts
library(tidyverse)
library(tidyselect)
library(lubridate)
library(broom)
library(here)
library(patchwork)
library(kableExtra)
library(psych)
library(DT)
library(plotly)
library(randomForest)
library(scales)
library(broom)
library(purrr)


######### Source functions ########## 

source(here::here("R", "plot_imp.R"))
source(here::here("R", "plotly_partial_dependence.R"))
source(here::here("R", "full_name_units.R"))
source(here::here("R", "df_imp_table.R"))


########## Set working directory ########## 

setwd(here::here())


########## Get datasets from the feature importance workflow ########## 

# Datasets and Variable Importance Results --------------------------------

load(here::here("data", "input", "prepared_data.RData"))
load(here::here("data", "output", "model_output.RData"))

# Rename response columns
df_raw <- df_raw %>% 
  rename(!!response_var := response) %>% 
  as.data.frame()

df <- df %>% 
  rename(!!response_var := response) %>% 
  as.data.frame()

df_clim0 <- df_clim0 %>% 
  rename(!!response_var := response) %>% 
  as.data.frame()

df_clim2 <- df_clim2 %>% 
  rename(!!response_var := response) %>% 
  as.data.frame()

# Create list of datasets for certain inputs
all_datasets <- c("df_raw", "df", "df_clim0", "df_clim2")

# Metadata Table ----------------------------------------------------------

metadata <- readRDS(here::here("shiny", "metadata.RDS")) %>%
  select("variable", "full_name", "units", "description") %>%
  as.data.frame()

# Partial Dependence Plot Data --------------------------------------------

# Create reduced data frames from the random forest models
df_clim0_reduced <- df_clim0 %>%
  select(c(rownames(rf_clim0$finalModel$importance)))

df_clim2_reduced <- df_clim2 %>%
  select(c(rownames(rf_clim2$finalModel$importance)))


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
  choices = unique(df$stratumID),
  selected = unique(df$stratumID)
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
  min = min(df$wy),
  max = max(df$wy),
  value = c(min(df$wy), max(df$wy)),
  sep = "",
  step = 1
)

dependent_variable <- varSelectInput(
  inputId = "dependent_variable",
  label = tags$h4("Select your dependent variable:"),
  data = df,
  selected = "npp"
)

independent_variable <- varSelectInput(
  inputId = "independent_variable",
  label = tags$h4("Select your independent variable:"),
  data = df,
  selected = "precip"
)

facet_variable <- varSelectInput(
  inputId = "facet_variable",
  label = tags$h6("Here you can pick a variable to facet the graph by. This allows you to see how the relationships between your independent and dependent variables change at different levels of your facet variable. This takes the range of your facet variable and splits the data into even quantiles. Select variable to facet by here:"),
  data = df %>% select(where(is.numeric)),
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
  choices = colnames(df_clim0_reduced),
  multiple = FALSE
)

partial_dep_var2 <- selectInput("partial_dep_var2",
  label = tags$h4("Select Variable 2"),
  choices = colnames(df_clim0_reduced),
  multiple = FALSE
)

# Principal Component Analysis --------------------------------------------

pca_data_select <- selectInput("pca_data_select",
  label = tags$h4("Select your dataset"),
  choices = all_datasets,
  selected = all_datasets[2],
  multiple = FALSE)

pca_group_select <- selectInput("pca_group_select",
  label = tags$h4("Select your groups"),
  choices = c("None", colnames(df)[sapply(df, is.factor)]),
  multiple = FALSE)

pca_alpha <- sliderInput("pca_alpha",
  label = tags$h4("Observation opacity"),
  min = 0,
  max = 1,
  value = 0.25
)

pca_ellipse <- checkboxInput("pca_ellipse",
  label = NULL,
  value = FALSE
)

# Distribution Plots ------------------------------------------------------

dist_data_select <- selectInput("dist_data_select",
  label = tags$h4("Select your dataset"),
  choices = all_datasets,
  selected = all_datasets[2],
  multiple = FALSE
)

dist_group_select <- selectInput("dist_group_select",
  label = tags$h4("Select your groups"),
  choices = c("None", colnames(df)[sapply(df, is.factor)]),
  multiple = FALSE
)

dist_num_select <- selectInput("dist_num_select",
  label = tags$h4("Select numeric variable"),
  choices = c(colnames(df)[sapply(df, is.numeric)]),
  multiple = FALSE
)

# Time Series -------------------------------------------------------------

ts_data_select <- selectInput("ts_data_select",
  label = tags$h4("Select your dataset"),
  choices = all_datasets[1:2],
  selected = all_datasets[2],
  multiple = FALSE
)

ts_group_select <- selectInput("ts_group_select",
  label = tags$h4("Select your groups"),
  choices = c("None", colnames(df)[sapply(df, is.factor)]),
  multiple = FALSE
)

ts_num_select <- selectInput("ts_num_select",
  label = tags$h4("Select numeric variable"),
  choices = c(colnames(df)[sapply(df, is.numeric)]),
  multiple = FALSE
)

ts_wy_sel <- sliderInput("ts_wy_sel",
  label = tags$h4("Select water year range:"),
  min = min(df$wy),
  max = max(df$wy),
  value = c(min(df$wy), max(df$wy)),
  sep = "",
  step = 1
)

