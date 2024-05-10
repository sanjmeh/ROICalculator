library(shiny)
library(DT)
library(ggplot2)
library(plotly)

server <- function(input, output) {

  # Values relevant across all calculations put in a reactive for easier access

  values <- reactive({
    # these variables are out since they are being used for calculation in data frame
    # data frame scope prevents creation and usage in the same scope hence outside creation
    entries_per_year = input$fuel_entry_count * 365
    working_days = 365 - 104 - 20 - 12
    # annual_consump_vol = input$hemm_count * input$hemm_daily_consump * 365
    annual_consump_vol = if( input$hemm_count > 0 && input$hemm_daily_consump > 0 )
      input$hemm_count * input$hemm_daily_consump * 365 else
        input$truck_count * 65 * 70 * 365 # each bowser fuelling 65 hemm, each hemm daily consumption of 70lts
    count_of_loggers = input$shift_count * input$truck_count * input$logger_count_per_bowser
    count_of_dataEntry = round((entries_per_year * 5) / 60 / 5 / working_days, digits = 0)

    logger_total_cost = input$fuel_logger_cost * count_of_loggers
    dto_total_cost = input$data_entry_emp * count_of_dataEntry
    dipatcher_total_cost = input$coordinator_count * input$fuel_dispatcher_cost
    accountant_total_cost = input$accountant_cost * input$accountant_count

    data.frame(
      entries_per_year = entries_per_year,
      count_of_loggers = count_of_loggers,
      count_of_dataEntry = count_of_dataEntry,
      working_days = working_days,

      hours = (entries_per_year * 3) / 60,
      days = round((entries_per_year * 3) / 60 / 5, digits = 0),
      annual_consump_vol = annual_consump_vol,

      logger_total_cost = logger_total_cost,
      dto_total_cost = dto_total_cost,
      dipatcher_total_cost = dipatcher_total_cost,
      accountant_total_cost = accountant_total_cost
    )
  })

  pilferage_values <- reactive({
    ur_daily_vol <- input$ur_day_count * input$ur_day_vol
    vol_saved_yearly = (ur_daily_vol * 365) + (input$bowser_fuel_sold_monthly * 12)

    data.frame(
      ur_daily_vol = ur_daily_vol,
      under_reporting_yearly =  ur_daily_vol * 365,
      bowser_fuel_sold_yearly = input$bowser_fuel_sold_monthly * 12,
      annual_consump_vol = values()$annual_consump_vol,
      vol_saved_yearly = vol_saved_yearly
    )
  })

  cost.df <- reactive({
    saving_logger =  (1 - input$manpower_save_fdl/100) * values()$logger_total_cost
    saving_dto = (1 - input$manpower_save_dto/100) * values()$dto_total_cost
    saving_accountant = (1 - input$manpower_save_accounts/100) * values()$accountant_total_cost
    saving_dispatcher = (1 - input$manpower_save_fdc/100) * values()$dipatcher_total_cost

    data.frame(
      Titles = c(
        "Fuel logger",
        "Data entry operator",
        "Accountant",
        "Fuel Dispatch Coordinator"
      ),
      Cost = c(
        values()$logger_total_cost,
        values()$dto_total_cost,
        values()$accountant_total_cost,
        values()$dipatcher_total_cost
      ),
      Saved = c(
        saving_logger,
        saving_dto,
        saving_accountant,
        saving_dispatcher
      )
    )
  })

  field.data.df <- reactive({
    data_entry_emps = round( (values()$hours/5) /values()$working_days,digits = 0)
    cost_of_dataEmps = data_entry_emps * input$data_entry_emp

    intermediate_result <- values()$entries_per_year * input$error_margin
    err_entries <- intermediate_result / 100.0
    err_hours_int = err_entries / 60.0
    err_hours = round(err_hours_int/25.0,digits=0)


    err_data_emp = round(err_hours /values()$working_days + 1,digits = 0)


    field_data <- data.frame(
      FTE = c("Count of Field Loggers",
              "Count of Data Entry Operators",
              "Erroneous Entries",
              "Employees for Correction"
      ),
      Value = c(
        values()$count_of_loggers,
        values()$count_of_dataEntry,
        err_entries,
        err_data_emp
      )
    )

    return (field_data)
  })

  travelling_data <- reactive({
    annual_refuels = if(input$movable_refuels_day != 0){
      input$movable_refuels_day * 365
    } else{
      input$movable_refuels_month * 12
    }

    movable_refuels = (annual_refuels * input$movable_percent_get) / 100
    movable_time_spent = round(movable_refuels * input$movable_get_time,digits = 0)
    movable_time_money = movable_time_spent * input$movable_hemm_price

    data.frame(
      annual_refuels = annual_refuels,
      movable_refuels = movable_refuels,
      movable_time_spent = movable_time_spent,
      movable_time_money = movable_time_money
    )
  })

  # ********************************************


# MANPOWER MENU BAR

  output$logger_count <- renderText({
    values()$count_of_loggers
  })

  output$entries_per_year <- renderText({
    values()$entries_per_year
  })

  output$data_entry_count <- renderText({
    values()$count_of_dataEntry
  })

  output$manpower_data <- renderTable({
    # working days
    data_entry_emps = round( (values()$hours/5) /values()$working_days,digits = 0)
    cost_of_dataEmps = data_entry_emps * input$data_entry_emp

    intermediate_result <- values()$entries_per_year * input$error_margin
    err_entries <- intermediate_result / 100.0
    err_hours_int = err_entries / 60.0
    err_hours = round(err_hours_int/25.0,digits=0)


    err_data_emp = round(err_hours /values()$working_days + 1,digits = 0)


    field_data <- data.frame(
      FTE = c("Count of Field Loggers",
              "Count of Data Entry Operators",
              "Count of Accountants",
              "Count of Fuel Dispatchers"
                ),
      Value = c(
        values()$count_of_loggers,
        values()$count_of_dataEntry,
        input$accountant_count,
        input$coordinator_count
        )
    )

    return (field_data)
  })

  output$manpower_data_2 <- renderTable({
    coord_cost = input$coordinator_count * input$shift_count * input$fuel_dispatcher_cost

    intermediate_result <- values()$entries_per_year * input$error_margin
    err_entries <- intermediate_result / 100.0
    err_hours_int = err_entries / 60.0
    err_hours = round(err_hours_int/25.0,digits=0)
    err_data_emp = round(err_hours /values()$working_days + 1,digits = 0)
    cost_of_err = err_data_emp * input$data_entry_emp

    field_data <- data.frame(
      FTE = c(
        "Fuel Logger",
        "Data Entry Operator",
        "Accountant/Compiler",
        "Fuel Dispatch Coordinator"
        ),
      Cost = c(
        values()$logger_total_cost,
        values()$dto_total_cost,
        values()$accountant_total_cost,
        values()$dipatcher_total_cost
        )
    )

    return (field_data)
  })

  output$histogram <- renderPlotly({

    data <- data.frame(cost.df()$Titles, cost.df()$Cost, cost.df()$Saved)
    colnames(data) <- c("Category","base_value","saved_value")

    gg <- ggplot(data) +
      geom_bar(aes(x = Category, y = base_value, fill="original"), stat = "identity", position="dodge") +
      geom_bar(aes(x = Category, y = saved_value, fill="saved"), stat = "identity", position="dodge") +
      scale_fill_manual(values = c("original" = "grey", "saved" = "blue")) +
      labs(fill = "Saving Comparisions")

    # Convert ggplot object to plotly for interactive plots
    p_plotly <- ggplotly(gg, tooltip = c("x", "y"))

    return(p_plotly)
  })

  output$pieChart <- renderPlot({
    ggplot(cost.df(), aes(x = "", y = Cost, fill = Titles)) +
      geom_bar(width = 1, stat = "identity") +
      coord_polar(theta = "y") +
      scale_fill_manual(values = c('#FF9999', '#66B3FF', '#99FF99', '#FFCC99')) +
      theme_void()
  })




  # PILFERAGE OLD

  # output$fuel_write_off_yearly <- renderText({
  #   values()$fuel_write_off_yearly
  # })
  #
  # output$fuel_per_year <- renderText({
  #   values()$fuel_per_year
  # })
  #
  # output$pilferage_data <- renderPlot({
  #   cost_fuel = values()$fuel_per_year * input$fuel_price
  #   fuel_saved = 0.5 * values()$fuel_per_year
  #   cost_fuel_saved = values()$fuel_write_off_yearly * input$fuel_price
  #
  #   value_names <- c("Litres of Fuel Saved/year", "Estimated Fuel Savings/Year")
  #   values <- c(fuel_saved,cost_fuel_saved)
  #
  #   # Create a data frame from the values
  #   data <- data.frame(
  #     name = value_names,
  #     value = values
  #   )
  #
  #   # ggplot(data, aes(x = name, y = value)) +
  #   #   geom_bar(stat = "identity", fill = "skyblue") +
  #   #   labs(title = "Pilferage Comparision", x = "Mindshift Offerings", y = "Cost")
  #   barplot(data$value,
  #           main="Pilferage Savings",
  #           names.arg=value_names,
  #           xlab="Mindshift Offerings",
  #           ylab="Cost Difference",
  #           col="steelblue",
  #           axes=FALSE)
  # })

  # PILFERAGE

  output$annual_fuel_consump <- renderText({
    pilferage_values()$annual_consump_vol
  })

  output$refuels_per_month <- renderText({
    ref_per_month <- if(input$hemm_count != 0) {
      (input$fuel_entry_count * 30) / input$hemm_count
    } else {
      (input$fuel_entry_count * 30) / 100
    }
    return(ref_per_month)
  })

  output$refuels_per_year <- renderText({
    ref_per_year <- if(input$hemm_count != 0) {
      (input$fuel_entry_count * 365) / input$hemm_count
    } else {
      (input$fuel_entry_count * 365) / 100
    }
    return(ref_per_year)
  })

  output$underreported_calculations <- renderTable({
    pilferage_percent = (pilferage_values()$under_reporting_yearly / pilferage_values()$annual_consump_vol) * 100

    data.frame(
      Field = c("Daily over reported volume across fleet","Yearly figure","Percentage of Yearly consumption"),
      Values = c(pilferage_values()$ur_daily_vol, pilferage_values()$under_reporting_yearly, pilferage_percent)
    )
  })

  output$stolen_assumption <- renderTable({
    pilferage_percent = (pilferage_values()$bowser_fuel_sold_yearly / pilferage_values()$annual_consump_vol) * 100

    data.frame(
      Field = c("Yearly figure","Percentage of Yearly consumption"),
      Values = c(pilferage_values()$bowser_fuel_sold_yearly, pilferage_percent)
    )
  })

  output$pilferage_hist <- renderPlotly({
    pilferage_percent = (pilferage_values()$vol_saved_yearly / pilferage_values()$annual_consump_vol) * 100

    value_names <- c("Under Reporting", "Fuel Sold Illegally", "Total Saved")
    values <- c(pilferage_values()$under_reporting_yearly, pilferage_values()$bowser_fuel_sold_yearly, pilferage_values()$vol_saved_yearly)

    # Create a data frame from the values
    data <- data.frame(
      Category = value_names,
      Value = values
    )

    # Create bar plot using ggplot2
    p <- ggplot(data, aes(x = Category, y = Value)) +
      geom_bar(stat = "identity", fill = "steelblue") +
      labs(title = "", x = "Category", y = "Volume Saved")

    # Convert ggplot object to plotly for interactive plots
    p_plotly <- ggplotly(p, tooltip = c("x", "y"))

    return(p_plotly)
  })

  output$pilferage_explanation <- renderText({
    pilferage_values()$vol_saved_yearly
  })

  output$pilferage_cost <- renderText({
    pilferage_values()$vol_saved_yearly * 86
  })

  # MOVABLE VEHICLE COST CALCULATION

  output$movable_refuel_sumannual <- renderText({
    travelling_data()$annual_refuels
  })
  output$movable_time_spent <- renderText({
    travelling_data()$movable_time_spent
  })
  output$movable_visualisation <- renderPlotly({
    data <- data.frame(
      category = c("Movement Statistics"),
      value1 = c(travelling_data()$annual_refuels),
      value2 = c(travelling_data()$movable_time_spent),
      value3 = c(travelling_data()$movable_time_spent * input$movable_hemm_price)
    )

    colnames(data)[2:4] <- c("Annual Refuel Count", "Overall Time Spent","Cost of Time")

    # Reshape data for ggplot
    data_long <- tidyr::pivot_longer(data, cols = c("Annual Refuel Count", "Overall Time Spent","Cost of Time"), names_to = "variable", values_to = "value")

    # Plot using ggplot
    # gg <- ggplot(data_long, aes(x = category, y = value, fill = variable)) +
    #   geom_bar(stat = "identity", position = "dodge") +
    #   scale_fill_manual(values = c("Annual Refuel Count" = "lightblue", "Overall Time Spent" = "orange","Cost of Time" = "green")) + # Assign colors
    #   labs(title = "",
    #        x = "Category", y = "Value") +
    #   theme_minimal()

    gg <- ggplot(data_long, aes(x = category, y = value, fill = variable)) +
      geom_bar(stat = "identity", position = "dodge") +
      scale_fill_manual(values = c("Annual Refuel Count" = "lightblue", "Overall Time Spent" = "orange","Cost of Time" = "green")) + # Assign colors
      labs(title = "",
           x = "Movement Statistics", y = "Value") +
      theme_minimal() +
      scale_y_continuous(trans = "log10")

    # ggplotly(gg, originalData = TRUE) %>%
    #   style(hoverinfo = "text",
    #         text = paste("Variable: ", data_long$Category, "<br>Value: ", 10^data_long$Value))


    # Convert ggplot to plotly
    ggplotly(gg)
  })
  output$movale_money_loss_hours <- renderText({
    travelling_data()$movable_time_spent * input$movable_hemm_price
  })
  output$movale_annual_money_loss_hours <- renderText({
    travelling_data()$movable_time_spent * input$movable_hemm_price * input$hemm_count
  })
}
