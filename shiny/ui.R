########################## Create UI ########################## 

ui <- fluidPage(
  theme = shinytheme("readable"),
  tags$h1("RHESSys Output Exploration"),
  navbarPage(
    "Explore your dataset!",
    tabPanel(
      "Welcome!",
      tags$h3(welcome),
      br(),
      tags$p(intro_1),
      tags$p(intro_2),
      tags$p(intro_3),
      tags$p(intro_4),
      img(src = "RHESSys_logo_V2.png", height = 450, width = 450)
    ),
    tabPanel(
      "Metadata",
      tags$h3("Explore the metadata:"),
      DT::dataTableOutput("metadata_DT")
    ),
    tabPanel(
      "Dataset Viewer",
      tags$h3("View your datasets:"),
      dataset_sel,
      DT::dataTableOutput("datatable_viewer")
    ),
    tabPanel(
      "Variable Importance",
      tags$h3("Random Forest Variable Importance Output:"),
      tags$p(paste0("Your Response Variable: ", full_name_units(response_var, metadata, units = FALSE))),
      plotOutput(outputId = "imp_plot", height = 550),
      tags$p(importance_caption)
    ),
    tabPanel(
      "Visualizations",
      sidebarPanel(
        if ("stratumID" %in% colnames(df_wy)) {
          stratum_sel
        },
        if ("topo" %in% colnames(df_wy)) {  
          topo_sel
        },
        if ("clim" %in% colnames(df_wy)) {
          clim_sel
        },
        if ("wy" %in% colnames(df_wy)) {
          wy_sel
        },
        "Variables to Explore:",
        independent_variable,
        facet_variable,
        quantile_slider
      ),
      mainPanel(
        "Visual Graph of your variable relationships:",
        plotlyOutput(outputId = "variable_plot", height = 700) %>% 
          withSpinner(type = 6),
        DT::dataTableOutput("visualization_statistics")
      )
    ),
    tabPanel(
      "Partial Dependence",
      sidebarPanel(
        partial_dep_model,
        partial_dep_var1,
        partial_dep_var2
      ),
      mainPanel(
        "Bivariate Partial Depedence",
        plotlyOutput(outputId = "partial_dep_plot") %>% 
          withSpinner(type = 6)
      )
    )
  )
)
