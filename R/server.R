server <- function(input, output, session) {
  # Observer for flip button
  observeEvent(input$flip, {
    if (input$view_mode == "single") {
      num <- input$single_numerator
      den <- input$denominator
      updateSelectInput(session, "single_numerator", selected = den)
      updateSelectInput(session, "denominator", selected = num)
    } else if (input$view_mode == "double") {
      num1 <- input$num1
      num2 <- input$num2
      den <- input$denominator
      updateSelectInput(session, "num1", selected = den)
      updateSelectInput(session, "num2", selected = num1)
      updateSelectInput(session, "denominator", selected = num2)
    } else {
      num <- input$dual_numerator
      den1 <- input$denom1
      den2 <- input$denom2
      updateSelectInput(session, "dual_numerator", selected = den1)
      updateSelectInput(session, "denom1", selected = den2)
      updateSelectInput(session, "denom2", selected = num)
    }
  })
  
  relative_prices <- reactive({
    if (input$view_mode == "single") {
      num_data <- all_data[[input$single_numerator]]
      den_data <- all_data[[input$denominator]]
      full_join(num_data, den_data, by = "date") %>%
        rename(num_price = price.x,
               den_price = price.y) %>%
        mutate(rel_price = num_price / den_price) %>%
        filter(!is.na(rel_price)) %>%
        filter(date >= input$date_range[1] & date <= input$date_range[2])
    } else if (input$view_mode == "double") {
      num1_data <- all_data[[input$num1]]
      num2_data <- all_data[[input$num2]]
      den_data <- all_data[[input$denominator]]
      full_join(num1_data, num2_data, by = "date") %>%
        full_join(den_data, by = "date") %>%
        rename(num1_price = price.x,
               num2_price = price.y,
               den_price = price) %>%
        mutate(
          rel_price1 = num1_price / den_price,
          rel_price2 = num2_price / den_price
        ) %>%
        filter(!is.na(rel_price1) & !is.na(rel_price2)) %>%
        filter(date >= input$date_range[1] & date <= input$date_range[2]) %>%
        mutate(
          norm_price1 = rel_price1 / first(rel_price1),
          norm_price2 = rel_price2 / first(rel_price2)
        )
    } else {
      num_data <- all_data[[input$dual_numerator]]
      den1_data <- all_data[[input$denom1]]
      den2_data <- all_data[[input$denom2]]
      full_join(num_data, den1_data, by = "date") %>%
        full_join(den2_data, by = "date") %>%
        rename(num_price = price.x,
               den1_price = price.y,
               den2_price = price) %>%
        mutate(
          price_in_den1 = num_price / den1_price,
          price_in_den2 = num_price / den2_price
        ) %>%
        filter(!is.na(price_in_den1) & !is.na(price_in_den2)) %>%
        filter(date >= input$date_range[1] & date <= input$date_range[2]) %>%
        mutate(
          norm_price_den1 = price_in_den1 / first(price_in_den1),
          norm_price_den2 = price_in_den2 / first(price_in_den2)
        )
    }
  })
  
  output$price_plot <- renderPlotly({
    data <- relative_prices()
    if (input$view_mode == "single") {
      plot_ly() %>%
        add_trace(data = data, x = ~date, y = ~rel_price, type = 'scatter', 
                  mode = 'none', name = input$single_numerator,
                  fill = 'tozeroy',
                  fillcolor = 'rgba(10, 191, 255, 0.2)',
                  showlegend = FALSE,
                  hoverinfo = 'none') %>% 
        add_trace(data = data, x = ~date, y = ~rel_price, type = 'scatter', 
                  mode = 'lines', name = input$single_numerator,
                  line = list(color = 'rgba(10, 191, 255, 1)', width = 2)) %>%
        layout(
          title = list(
            text = paste(input$single_numerator, "priced in", input$denominator),
            x = 0.03,
            y = 0.99,
            xanchor = "left",
            font = list(size = 30)
          ),
          margin = list(
            t = 30
          ),
          xaxis = list(title = ""),
          yaxis = list(title = "Price Ratio")
        ) %>% 
        layout(hovermode = 'x')
    } else if (input$view_mode == "double") {
      plot_ly() %>%
        add_trace(data = data, x = ~date, y = ~norm_price1, type = 'scatter', 
                  mode = 'none', name = input$num1,
                  fill = 'tozeroy',
                  fillcolor = 'rgba(255, 120, 140, 0.2)',
                  showlegend = FALSE,
                  hoverinfo = 'none') %>% 
        add_trace(data = data, x = ~date, y = ~norm_price2, type = 'scatter', 
                  mode = 'none', name = input$num2,
                  fill = 'tozeroy',
                  fillcolor = 'rgba(10, 191, 255, 0.2)',
                  showlegend = FALSE,
                  hoverinfo = 'none') %>% 
        add_trace(data = data, x = ~date, y = ~norm_price1, type = 'scatter', 
                  mode = 'lines', name = input$num1,
                  line = list(color = 'rgba(255, 120, 140, 1)', width = 2)) %>%
        add_trace(data = data, x = ~date, y = ~norm_price2, type = 'scatter', 
                  mode = 'lines', name = input$num2,
                  line = list(color = 'rgba(10, 191, 255, 1)', width = 2)) %>%
        layout(
          title = list(
            text = paste("Relative Performance:", input$num1, "vs", input$num2,
                         "(in", input$denominator, ")"),
            x = 0.03,
            y = 0.99,
            xanchor = "left",
            font = list(size = 30)
          ),
          legend = list(
            orientation = "h",    
            y = 0.88,
            x = 0.5,     
            xanchor = "center",
            yanchor = "bottom",
            font = list(size = 20)
          ),
          margin = list(
            t = 30
          ),
          xaxis = list(title = ""),
          yaxis = list(title = "Normalized Price (Start = 1)")
        ) %>% 
        layout(hovermode = 'x')
    } else {
      plot_ly() %>%
        add_trace(data = data, x = ~date, y = ~norm_price_den1, type = 'scatter', 
                  mode = 'none', name = paste("In", input$denom1),
                  fill = 'tozeroy',
                  fillcolor = 'rgba(255, 120, 140, 0.2)',
                  showlegend = FALSE,
                  hoverinfo = 'none') %>% 
        add_trace(data = data, x = ~date, y = ~norm_price_den2, type = 'scatter', 
                  mode = 'none', name = paste("In", input$denom2),
                  fill = 'tozeroy',
                  fillcolor = 'rgba(10, 191, 255, 0.2)',
                  showlegend = FALSE,
                  hoverinfo = 'none') %>% 
        add_trace(data = data, x = ~date, y = ~norm_price_den1, type = 'scatter', 
                  mode = 'lines', name = paste("In", input$denom1),
                  line = list(color = 'rgba(255, 120, 140, 1)', width = 2)) %>%
        add_trace(data = data, x = ~date, y = ~norm_price_den2, type = 'scatter', 
                  mode = 'lines', name = paste("In", input$denom2),
                  line = list(color = 'rgba(10, 191, 255, 1)', width = 2)) %>%
        layout(
          title = list(
            text = paste(input$dual_numerator, "relative strength:", 
                         input$denom1, "vs", input$denom2),
            x = 0.03,
            y = 0.99,
            xanchor = "left",
            font = list(size = 30)
          ),
          legend = list(
            orientation = "h",
            y = 0.88,
            x = 0.5,
            xanchor = "center",
            yanchor = "bottom",
            font = list(size = 20)
          ),
          margin = list(
            t = 30
          ),
          xaxis = list(title = ""),
          yaxis = list(title = "Normalized Price (Start = 1)")
        ) %>% 
        layout(hovermode = 'x')
    }
  })
}