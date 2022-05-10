########################## Create UI ########################## 

ui <- fluidPage(
  theme = shinytheme("superhero"),
  tags$h1("RHESSys Output Exploration"),
  navbarPage(
    "Explore your dataset!",
    tabPanel(
      "Welcome!",
      tags$h2(welcome),
      tags$h2(intro_3),
      tags$h2(intro_4),
      tags$h2(intro_1),
      tags$h2(intro_2),
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
      tags$h4(paste0("Your Response Variable: ", response_var)),
      plotOutput(outputId = "imp_plot", height = 550),
      tags$h6(importance_caption)
    ),
    tabPanel(
      "Visualizations",
      sidebarPanel(
        stratum_sel,
        topo_sel,
        clim_sel,
        wy_sel,
        "Variables to Explore:",
        independent_variable,
        facet_variable,
        quantile_slider
      ),
      mainPanel(
        "Visual Graph of your variable relationships:",
        plotlyOutput(outputId = "variable_plot", height = 700)
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
        plotlyOutput(outputId = "partial_dep_plot") %>% withSpinner(type = 6)
      )
    )
  )
)
