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
      tags$p(importance_caption),
      br(),
      tags$h3("Table Comparison"),
      tableOutput("imp_table")
    ),
    tabPanel(
      "Variable Relationships",
      tabBox(width = "100%",
        tabPanel(
          "Scatter Plot",
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
            br(),
            "Scatter Plot",
            plotlyOutput(outputId = "variable_plot", height = 700) %>%
              withSpinner(type = 6),
            br(),
            fluidRow(
              column(width = 7, 
                     tags$h4("Linear Regression Slope and Fit"),
                     DT::dataTableOutput("visualization_statistics")),
              column(width = 5, 
                     tags$h4("Facet Variable Range"),
                     DT::dataTableOutput("facet_stats"))
            )
          )
        ),
        tabPanel(
          "Principal Component Analysis",
          sidebarPanel(
            pca_data_select,
            pca_group_select,
            tags$h4("Draw a normal data ellipse for each group?"),
            pca_ellipse,
            pca_alpha
          ),
          mainPanel(
            br(),
            "Principal Components Plot",
            plotlyOutput(outputId = "pca_plot") %>%
              withSpinner(type = 6)
          )
        ),
        tabPanel(
          "Distribution Plots",
          sidebarPanel(
            dist_data_select,
            dist_num_select,
            dist_group_select
          ),
          mainPanel(
            br(),
            "Boxplots",
            plotOutput(outputId = "dist_plot") %>%
              withSpinner(type = 6),
            br(),
            "Histograms",
            plotOutput(outputId = "dist_hist") %>%
              withSpinner(type = 6),
            br(),
            "Summary",
            DT::dataTableOutput("dist_table"),
            br()
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
            br(),
            "Bivariate Partial Dependence",
            plotlyOutput(outputId = "partial_dep_plot") %>%
              withSpinner(type = 6)
          )
        ),
        tabPanel(
          "Time Series",
          sidebarPanel(
            ts_data_select,
            ts_num_select,
            ts_group_select,
            ts_wy_sel
          ),
          mainPanel(
            br(),
            "Time Series Plot",
            plotlyOutput(outputId = "ts_plot") %>%
              withSpinner(type = 6)
          )
        )
      )
    )
  )
)
