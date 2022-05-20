########################## RHESSysML Shiny App for Sagehen Creek ##########################

########## Create the server ##########

server <- function(input, output) {

  # Metadata ----------------------------------------------------------------

  output$metadata_DT <- DT::renderDataTable({
    DT::datatable(metadata,
      options = list(
        pageLength = 30
      )
    )
  })

  # Dataset Viewer ----------------------------------------------------------

  data_display <- reactive({
    switch(input$dataset_sel,
      "Raw Data" = df,
      "Aggregated Data" = df_wy,
      "Aggregated Data (Normal Climate)" = df_wy0,
      "Aggregated Data (+2 Degree C Climate)" = df_wy2
    )
  })

  output$datatable_viewer <- DT::renderDataTable({
    data_display() %>%
      mutate(across(where(is.numeric), round, 6)) %>%
      DT::datatable(
        options = list(
          pageLength = 15
        )
      )
  })

  # Variable Importance -----------------------------------------------------

  output$imp_plot <- renderPlot({
    plot_imp(imp_wy0) + plot_imp(imp_wy2)
  })

  # Visualizations ----------------------------------------------------------

  # Create reactive data frame for visualizations tab plot
  df_wy_reactive <- reactive({
    validate(
      if ("stratumID" %in% colnames(df_wy)) {
        need(
          length(input$stratum_sel) > 0,
          "No data contained in selected strata. Please select more strata."
        )
      },
      if ("topo" %in% colnames(df_wy)) {
        need(
          length(input$topo_sel) > 0,
          "No data contained in selected topographies. Please select more topography types."
        )
      },
      if ("clim" %in% colnames(df_wy)) {
        need(
          length(input$clim_sel) > 0,
          "Must have atleast one climate scenario selected."
        )
      }
    )

    reactive_df <- df_wy %>%
      mutate(quantile = paste0("Quantile ", ntile(!!input$facet_variable, input$quantile_sel)))
    
    if ("stratumID" %in% colnames(df_wy)) {
      reactive_df <- reactive_df %>% filter(stratumID %in% input$stratum_sel)
    }
    
    if ("topo" %in% colnames(df_wy)) {
      reactive_df <- reactive_df %>% filter(topo %in% input$topo_sel)
    }
    
    if ("clim" %in% colnames(df_wy)) {
      reactive_df <- reactive_df %>% filter(clim %in% input$clim_sel)
    }
    
    return(reactive_df)
    
  })

  # Create visualizations plot
  output$variable_plot <- renderPlotly({
    p <- ggplot(data = df_wy_reactive(), aes(
      x = !!input$independent_variable,
      y = df_wy_reactive()[, response_var]
    )) +
      geom_point(aes(color = clim), size = 0.75) +
      geom_smooth(se = FALSE, method = lm, color = "#B251F1", size = 0.75) +
      scale_color_manual(values = c(
        "0" = "#FEA346",
        "2" = "#4BA4A4"
      )) +
      labs(
        color = "Climate Scenario",
        title = paste(
          "Relationship between",
          full_name_units(response_var, metadata, units = FALSE),
          "and",
          full_name_units(input$independent_variable, metadata, units = FALSE)
        ),
        subtitle = paste("Faceting by", full_name_units(input$facet_variable, metadata, units = FALSE)),
        y = full_name_units(response_var, metadata),
        x = full_name_units(input$independent_variable, metadata)
      ) +
      facet_wrap(~quantile) +
      theme_light() +
      theme(text = element_text(size = 10))

    plotly_p <- ggplotly(p)
    
    text_x <- number(
      plotly_p$x$data[[1]]$x,
      prefix = paste0(full_name_units(input$independent_variable, metadata, units = FALSE), ": ")
    )
    
    text_y <- number(
      plotly_p$x$data[[1]]$y,
      prefix = paste0(full_name_units(response_var, metadata, units = FALSE), ": "),
      accuracy = 0.000000001
    )
    
    plotly_p %>% 
      style(text = paste0(text_y, "</br></br>", text_x))
  })

  output$visualization_statistics <- DT::renderDataTable({
    df_wy_reactive() %>%
      group_by(clim, quantile) %>%
      summarize(
        "mean.y" = mean(df_wy_reactive()[, response_var]),
        "min.y" = min(df_wy_reactive()[, response_var]),
        "max.y" = max(df_wy_reactive()[, response_var]),
        "mean.x" = mean(!!input$independent_variable),
        "min.x" = min(!!input$independent_variable),
        "max.x" = max(!!input$independent_variable)
      ) %>%
      ungroup() %>%
      mutate(across(where(is.numeric), round, 6)) %>%
      DT::datatable(options = list(dom = "t"))
  })

  # Partial Dependence ------------------------------------------------------

  # Update select inputs based on the model choice
  observe({
    x <- input$partial_dep_model

    if (x == "Normal Scenario") {
      cols <- colnames(df_wy0_reduced %>% select(where(is.numeric)))
    } else if (x == "+2 Degree C Scenario") {
      cols <- colnames(df_wy2_reduced %>% select(where(is.numeric)))
    }

    updateSelectInput(
      session = getDefaultReactiveDomain(),
      "partial_dep_var1",
      choices = cols,
      selected = cols[1]
    )

    updateSelectInput(
      session = getDefaultReactiveDomain(),
      "partial_dep_var2",
      choices = cols,
      selected = cols[2]
    )
  })

  # Get correct RF model based on input
  partial_dep_model_obj <- reactive({
    if (input$partial_dep_model == "Normal Scenario") {
      rf_wy0$finalModel
    } else if (input$partial_dep_model == "+2 Degree C Scenario") {
      rf_wy2$finalModel
    }
  })

  # Get correct predictor data frame based on input
  partial_dep_data <- reactive({
    if (input$partial_dep_model == "Normal Scenario") {
      df_wy0_reduced
    } else if (input$partial_dep_model == "+2 Degree C Scenario") {
      df_wy2_reduced
    }
  })

  # Create 3D partial dependence plot
  output$partial_dep_plot <- renderPlotly({
    plotly_partial_dependence(
      x = partial_dep_model_obj(),
      pred.data = partial_dep_data(),
      v1 = input$partial_dep_var1,
      v2 = input$partial_dep_var2,
      grid.size = 15
    )
  })
}
