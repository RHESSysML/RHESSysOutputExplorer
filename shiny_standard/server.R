########################## RHESSysML Shiny App for Standard Output ##########################
# 
# The following file is used to create the tables and plots seen within the Shiny app.
# Edits to the appearance or reactivity of these objects can be made within this file.
# 
# The section headers correspond to the respective tab within the Shiny app.
# 
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
    data_display <- get(input$dataset_sel)
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
    plot_imp(imp)
  })
  
  output$imp_table <- function() {
    table_imp(imp)
  }

  # Visualizations ----------------------------------------------------------

  # Create reactive data frame for visualizations tab plot
  df_wy_reactive <- reactive({
    validate(
      if ("stratumID" %in% colnames(df)) {
        need(
          length(input$stratum_sel) > 0,
          "No data contained in selected strata. Please select more strata."
        )
      },
      if ("topo" %in% colnames(df)) {
        need(
          length(input$topo_sel) > 0,
          "No data contained in selected topographies. Please select more topography types."
        )
      },
      if ("clim" %in% colnames(df)) {
        need(
          length(input$clim_sel) > 0,
          "Must have at least one climate scenario selected."
        )
      },
      if ("wy" %in% colnames(df)) {
        need(
          length(input$wy_sel) > 0,
          "No data for selected years."
        )
      }
    )

    reactive_df <- df %>%
      dplyr::mutate(quantile = paste0("Quantile ", dplyr::ntile(!!input$facet_variable, input$quantile_sel)))

    if ("stratumID" %in% colnames(df)) {
      reactive_df <- reactive_df %>% dplyr::filter(stratumID %in% input$stratum_sel)
    }

    if ("topo" %in% colnames(df)) {
      reactive_df <- reactive_df %>% dplyr::filter(topo %in% input$topo_sel)
    }

    if ("clim" %in% colnames(df)) {
      reactive_df <- reactive_df %>% dplyr::filter(clim %in% input$clim_sel)
    }

    if ("wy" %in% colnames(df)) {
      reactive_df <- reactive_df %>% dplyr::filter(wy %in% input$wy_sel[1]:input$wy_sel[2])
    }

    return(reactive_df)
  })

  # Create visualizations plot
  output$variable_plot <- renderPlotly({
    p <- ggplot(data = df_wy_reactive(), aes(
      x = !!input$independent_variable,
      y = df_wy_reactive()[, response_var]
    )) +
      geom_point(size = 0.75) +
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

  ### Helper function for linear regression line in reactive table below ###
  mdl <- function(df) {
    formula <- as.formula(paste(response_var, input$independent_variable, sep = "~"))
    lm(formula = formula, data = df)
  }

  output$visualization_statistics <- DT::renderDataTable({
    df_wy_reactive() %>%
      select(quantile, response_var, input$independent_variable) %>%
      dplyr::group_by(quantile) %>%
      nest() %>%
      mutate(m = map(data, mdl)) %>%
      mutate(stats = map(m, tidy)) %>%
      mutate(r2 = map(m, glance)) %>%
      ungroup() %>%
      unnest(stats) %>%
      filter(term != "(Intercept)") %>%
      select(-c(statistic, p.value)) %>%
      unnest(r2) %>%
      select(quantile, estimate, r.squared, adj.r.squared) %>%
      dplyr::mutate(dplyr::across(where(is.numeric), round, 6)) %>%
      dplyr::arrange(quantile) %>%
      dplyr::rename("Quantile" = quantile, "Slope" = estimate, "R.squared" = r.squared, "R.squared (adj)" = adj.r.squared) %>%
      DT::datatable(options = list(dom = "t"))
  })

  output$facet_stats <- DT::renderDataTable({
    facet_stats <- df_wy_reactive() %>%
      group_by(quantile) %>%
      summarize(
        "Facet Range" = paste0("(", round(min(get(input$facet_variable)), 4), ", ", 
                               round(max(get(input$facet_variable)), 4), ")")
      ) %>%
      select(-quantile)
    
    names(facet_stats) <- c(paste0(# "Facet Range - ", 
                                   full_name_units(input$facet_variable, metadata)))
    
    facet_stats %>% 
      DT::datatable(rownames = FALSE, options = list(dom = "t"))
  })


  # Partial Dependence ------------------------------------------------------

  # Create 3D partial dependence plot
  output$partial_dep_plot <- renderPlotly({
    plotly_partial_dependence(
      x = rf$finalModel,
      pred.data = df_reduced,
      v1 = input$partial_dep_var1,
      v2 = input$partial_dep_var2,
      grid.size = 15
    ) %>% layout(height = "600px")
  })

  # Principal Component Analysis --------------------------------------------

  pca_data <- reactive({
    pca_data <- get(input$pca_data_select)
  })

  output$pca_plot <- renderPlotly({
    plot_data <- pca_data() %>%
      select(where(is.numeric))

    pca <- stats::prcomp(plot_data, center = TRUE, scale. = TRUE)
  
    pca.plot <- ggbiplot(pca,
      groups = (if (input$pca_group_select != "None") {pca_data()[, input$pca_group_select]} else {NULL}),
      alpha = input$pca_alpha,
      ellipse = input$pca_ellipse
    ) +
      theme_light()

    ggplotly(pca.plot)
  })

  # Distribution Plots ------------------------------------------------------

  dist_data <- reactive({
    dist_data <- get(input$dist_data_select)
  })

  output$dist_plot <- renderPlot({
    dist_plot <- ggplot(dist_data(), aes(y = dist_data()[, input$dist_num_select])) +
      labs(y = full_name_units(input$dist_num_select, metadata)) +
      theme_light()
    
    if(input$dist_group_select!="None") { 
      dist_plot <- dist_plot + 
        geom_boxplot(aes(fill = dist_data()[, input$dist_group_select])) +
        scale_fill_discrete(name = full_name_units(input$dist_group_select, metadata, units = FALSE))
    }
    else {
      dist_plot <- dist_plot + 
        geom_boxplot()
    }
    return(dist_plot)
    
  })

  output$dist_hist <- renderPlot({
    dist_hist <- ggplot(dist_data(), aes(x = dist_data()[, input$dist_num_select])) +
      geom_histogram() +
      theme_light() +
      labs(
        x = full_name_units(input$dist_num_select, metadata),
        y = "Count"
      ) 
    
    if(input$dist_group_select!="None") { 
      dist_hist <- dist_hist + 
        facet_wrap(~ dist_data()[, input$dist_group_select])
    }
    return(dist_hist)
  })

  output$dist_table <- DT::renderDataTable({
    if(input$dist_group_select!="None") {
      dist_table <- dist_data() %>%
        group_by(across(input$dist_group_select)) %>%
        summarise(
          N. = n(),
          Min = min(get(input$dist_num_select)),
          Q1 = quantile(get(input$dist_num_select), 0.25),
          Median = median(get(input$dist_num_select)),
          Mean = mean(get(input$dist_num_select)),
          Q3 = quantile(get(input$dist_num_select), 0.75),
          Max = max(get(input$dist_num_select))
        )
    } 
    else {
      dist_table <- dist_data() %>%
        summarise(
          N. = n(),
          Min = min(get(input$dist_num_select)),
          Q1 = quantile(get(input$dist_num_select), 0.25),
          Median = median(get(input$dist_num_select)),
          Mean = mean(get(input$dist_num_select)),
          Q3 = quantile(get(input$dist_num_select), 0.75),
          Max = max(get(input$dist_num_select))
        )
    }
    
    dist_table <- dist_table %>% 
      mutate(across(where(is.numeric), round, 6)) %>%
      DT::datatable(options = list(dom = "t"))
  })

  # Time Series Plots -------------------------------------------------------

  ts_plot_data <- reactive({
    if (input$ts_group_select=="None") {
      df_raw %>%
        filter(wy %in% input$ts_wy_sel[1]:input$ts_wy_sel[2]) %>%
        group_by(wy) %>%
        summarize_if(is.numeric, mean) %>%
        ungroup()
    }
    else {
      df_raw %>%
        filter(wy %in% input$ts_wy_sel[1]:input$ts_wy_sel[2]) %>%
        group_by(wy, across(input$ts_group_select)) %>%
        summarize_if(is.numeric, mean) %>%
        ungroup()
    }
  })

  output$ts_plot <- renderPlotly({
    ts_plot_data <- as.data.frame(ts_plot_data())

    ts_plot <- ggplot(ts_plot_data, aes(
      x = wy,
      y = ts_plot_data[, input$ts_num_select])
    ) +
      labs(x = "Water Year",
           y = full_name_units(input$ts_num_select, metadata),
           color = full_name_units(input$ts_group_select, metadata, units = FALSE)) +
      theme_light()
    
    if(input$ts_group_select!="None") { 
      ts_plot <- ts_plot + geom_line(aes(color = ts_plot_data[, input$ts_group_select]))
    }
    else {
      ts_plot <- ts_plot + geom_line()
    }
    

    ts_plotly <- ggplotly(ts_plot)

    text_x <- number(
      ts_plotly$x$data[[1]]$x,
      prefix = paste0(full_name_units("wy", metadata, units = FALSE), ": ")
    )

    text_y <- number(
      ts_plotly$x$data[[1]]$y,
      prefix = paste0(full_name_units(input$ts_num_select, metadata, units = FALSE), ": "),
      accuracy = 0.000000001
    )

    ts_plotly %>%
      style(text = paste0(text_y, "</br></br>", text_x))
  })
}
