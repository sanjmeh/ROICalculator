library(shiny)
library(DT)
library(ggplot2)
library(plotly)
library(stringr)
library(scales)
library(gt)
options(scipen = 999)

server <- function(input, output, session) {

  acc <- reactiveValues(
    count_of_accountant = 0
  )

  observeEvent(input$fuel_entry_count,{
    count_of_accountant <- round(((input$fuel_entry_count * 0.3 * 10) / 60 + (10 * 6 * 12)) / 6 / 281, 2)
    acc$count_of_accountant <- count_of_accountant
    print(count_of_accountant)
    print(input$fuel_entry_count*0.3*10)
  })

  # values-reactive Values relevant across all calculations put in a reactive for easier access
  values <- reactive({
    # these variables are out since they are being used for calculation in data frame
    # data frame scope prevents creation and usage in the same scope hence outside creation
    entries_per_year = req(input$fuel_entry_count) * 365
    error_entries = entries_per_year * req(input$error_margin) / 100
    correct_entries = entries_per_year - error_entries
    working_days = 365 - 104 - 20 - 12

    # annual_consump_vol = input$hemm_count * input$hemm_daily_consump * 365
    annual_consump_vol = if( input$hemm_count > 0 && input$hemm_daily_consump > 0 )
      input$hemm_count * input$hemm_daily_consump * 365 else
        input$truck_count * 65 * 70 * 365 # each bowser fuelling 65 hemm, each hemm daily consumption of 70lts

    count_of_loggers = input$shift_count * input$truck_count * req(input$logger_count_per_bowser)


    count_of_dataEntry = round((correct_entries * 3) / 60 / 5 / working_days, digits = 0) +
      round((error_entries * input$correction_time) / 60 / 5 / working_days, digits = 0)

    # correction and compilation hours are being considered for accountant calculation
    correction_hours = (req(input$fuel_entry_count) * 0.3 * 10)/60 #time spent in correction
    compilation_hours = ((10*6*12) )/6 #time spent in compilation

    if(correction_hours == 0){
      compilation_hours = 0
    }

    count_of_accountant = round( (  correction_hours + compilation_hours )/281, 2)

    logger_total_cost = input$fuel_logger_cost * count_of_loggers

    dipatcher_total_cost = input$coordinator_count * input$fuel_dispatcher_cost
    dto_total_cost = input$data_entry_cost * count_of_dataEntry
    if(input$accountant_cost == 0 && input$manpower_reduction_accountant == 0){
      count_of_accountant = 0

    }
    accountant_total_cost = (count_of_accountant * input$accountant_cost)

    data.frame(
      entries_per_year = entries_per_year,
      count_of_loggers = count_of_loggers,
      count_of_accountant = count_of_accountant,
      count_of_dataEntry = count_of_dataEntry,
      working_days = working_days,

      hours = (entries_per_year * 5) / 60,
      days = round((entries_per_year * 5) / 60 / 5, digits = 0),
      annual_consump_vol = annual_consump_vol,

      logger_total_cost = logger_total_cost,
      dto_total_cost = dto_total_cost,
      dipatcher_total_cost = dipatcher_total_cost,
      accountant_total_cost = accountant_total_cost
    )
  })

  pilferage_values <- reactive({
    ur_daily_vol <- input$ur_day_count * input$ur_day_vol
    under_reporting_yearly =  ur_daily_vol * 365

    bowser_fuel_sold_yearly = input$tank_steals_monthly * input$bowser_theft_vol * 12

    vol_saved_yearly = under_reporting_yearly + bowser_fuel_sold_yearly


    saving_ur = under_reporting_yearly
    saving_ftheft = bowser_fuel_sold_yearly

    ref_per_month = if(input$hemm_count != 0) {
      (req(input$fuel_entry_count) * 30) / input$hemm_count
    } else {
      (req(input$fuel_entry_count) * 30) / 100
    }

    ref_per_month = round(ref_per_month,0)
    ref_per_year = round(ref_per_month * 12,digits=0)

    data.frame(
      ur_daily_vol = ur_daily_vol,
      under_reporting_yearly =  under_reporting_yearly,
      bowser_fuel_sold_yearly = bowser_fuel_sold_yearly,
      annual_consump_vol = values()$annual_consump_vol,

      ref_per_month = ref_per_month,
      ref_per_year = ref_per_year,

      vol_saved_yearly = vol_saved_yearly,
      saving_ftheft = saving_ftheft,
      saving_ur = saving_ur
    )
  })

  cost.df <- reactive({
    dispatcher_reduced_cost = input$manpower_reduction_dispatcher * input$fuel_dispatcher_cost
    logger_reduced_cost = input$manpower_reduction_logger * input$fuel_logger_cost
    dto_reduced_cost = input$manpower_reduction_dte * input$data_entry_cost
    accountant_reduced_cost = input$manpower_reduction_accountant * input$accountant_cost

    saving_dispatcher <- values()$dipatcher_total_cost - dispatcher_reduced_cost
    saving_logger <- values()$logger_total_cost - logger_reduced_cost
    saving_dto <- values()$dto_total_cost - dto_reduced_cost
    saving_accountant <- (values()$accountant_total_cost - input$manpower_reduction_accountant*input$accountant_cost)

    data.frame(
      Titles = c(
        "Fuel Dispatcher",
        "Fuel Logger",
        "Data Entry Operator",
        "Accountant"
      ),
      Cost = c(
        (values()$dipatcher_total_cost),
        (values()$logger_total_cost),
        (values()$dto_total_cost),
        (values()$accountant_total_cost)
      ),
      Saved = c(
        saving_dispatcher,
        saving_logger,
        saving_dto,
        saving_accountant
      )
    )
  })

  field.data.df <- reactive({

    intermediate_result <- values()$entries_per_year * req(input$error_margin)
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

    annual_refuels = if(input$hemm_count != 0) {
      (req(input$fuel_entry_count) * 365) / input$hemm_count
    } else {
      (req(input$fuel_entry_count) * 365) / 100
    }

    movable_refuels = (annual_refuels * input$movable_percent_get) / 100
    movable_time_spent = round(movable_refuels * input$movable_get_time,digits = 0)
    movable_time_money = movable_time_spent * input$movable_hemm_price

    annual_movable_sum = movable_time_spent * input$movable_hemm_price * input$movable_hemm_count

    data.frame(
      annual_refuels = annual_refuels,
      movable_refuels = movable_refuels,
      movable_time_spent = movable_time_spent,
      movable_time_money = movable_time_money,
      annual_movable_sum = annual_movable_sum
    )
  })

  format_indian <- function(x) {
    format_single <- function(y) {
      y <- as.character(y)
      if (grepl("\\.", y)) {
        parts <- unlist(strsplit(y, "\\."))
        int_part <- parts[1]
        dec_part <- parts[2]
      } else {
        int_part <- y
        dec_part <- NULL
      }
      n <- nchar(int_part)
      if (n > 3) {
        last3 <- substr(int_part, n-2, n)
        other <- substr(int_part, 1, n-3)
        formatted_int <- paste0(gsub("(\\d)(?=(\\d{2})+$)", "\\1,", other, perl=TRUE), ",", last3)
      } else {
        formatted_int <- int_part
      }
      if (!is.null(dec_part)) {
        result <- paste0(formatted_int, ".", dec_part)
      } else {
        result <- formatted_int
      }
      return(result)
    }

    if (is.vector(x)) {
      sapply(x, format_single)
    } else {
      format_single(x)
    }
  }


  updateTextOutput <- function(outputId, newText) {
    output[[outputId]] <- renderText({
      newText
    })
  }

  # ********************************************
  # ********************************************
  # ********************************************
  # ********************************************


  # MANPOWER MENU BAR

  # setting pre calculated reduction values
  observeEvent(c(input$error_margin,input$fuel_entry_count,input$truck_count,input$shift_count),{
    predicted_red_dispatcher =  1
    predicted_red_logger = values()$count_of_loggers - 1
    predicted_red_dte = round(0.66 * values()$count_of_dataEntry ,0)
    predicted_red_accountant = 0.4 * values()$count_of_accountant

    if(input$manpower_reduction_dispatcher == 0 && input$manpower_dispatch_q == TRUE){
      updateNumericInput(session, "manpower_reduction_dispatcher", value = predicted_red_dispatcher)
    }
    if(input$manpower_reduction_logger == 0 && input$manpower_logger_q == TRUE){
      print("going inside if")
      updateNumericInput(session, "manpower_reduction_logger", value = predicted_red_logger)
    }
    if(input$manpower_reduction_dte == 0 && input$manpower_dte_q == TRUE){
      updateNumericInput(session, "manpower_reduction_dte", value = predicted_red_dte)
      updateNumericInput(session, "manpower_reduction_accountant", value = predicted_red_accountant)
    }
    # if(input$manpower_reduction_accountant != 0 && input$manpower_acc_q == TRUE){
    # }
  })

  # this is for updating the values of reduced logger once changes are made to logger count
  observeEvent(input$logger_count_per_bowser,{
    predicted_red_logger = values()$count_of_loggers - 1
    if(input$manpower_logger_q != FALSE){
      updateNumericInput(session, "manpower_reduction_logger", value = predicted_red_logger)
    }
  })

  # DISPATCHER CHECKING
  observeEvent(input$manpower_dispatch_q,{
    if(input$manpower_dispatch_q == FALSE){
      updateNumericInput(session,"coordinator_count",value = 0)
      updateNumericInput(session,"manpower_reduction_dispatcher",value=0)
    } else {
      updateNumericInput(session,"coordinator_count",value = 2)
      updateNumericInput(session,"manpower_reduction_dispatcher",value=1)
    }
  })

  # FUEL LOGGER CHECKING
  observeEvent(input$manpower_logger_q, {
    if(input$manpower_logger_q){
      output$manpower_logger_check <- renderUI({
        numericInput("logger_count_per_bowser","Fuel Logger/Bowser",value=1)
      })
    } else {
      output$manpower_logger_check <- renderUI({
        numericInput("logger_count_per_bowser","Fuel Logger/Bowser",value=0)
      })

      updateNumericInput(session,"manpower_reduction_logger",value=0)
    }
  })

  # DATA ENTRY OPERATOR CHECKING
  observeEvent(input$manpower_dte_q,{

    if(input$manpower_dte_q){
      output$fuel_entry_count_check <- renderUI({
        numericInput("fuel_entry_count", "Number of Entries per Day:",value = 200)
      })

      output$error_margin_check <- renderUI({
        numericInput("error_margin", "% Erroneous Entries",value = 5)
      })

      # output$manpower_acc_check <- renderUI({
      #   radioButtons("manpower_acc_q","Do you have Accountants for operation hour and fuel data compilation?",
      #                choices = c("Yes" = TRUE, "No" = FALSE),
      #                inline = TRUE,
      #                selected = TRUE)
      # })
      output$manpower_acc_check <- renderUI({
        NULL
      })

      updateSliderInput(session,"accountant_cost",value=500000)
      predicted_red_accountant = 0.4 * values()$count_of_accountant
      predicted_red_dte = round(0.66 * values()$count_of_dataEntry ,0)
      updateNumericInput(session,"manpower_reduction_accountant",value = predicted_red_accountant)
      updateNumericInput(session,"manpower_reduction_dte",value = predicted_red_dte)

    } else {
      output$fuel_entry_count_check <- renderUI({
        numericInput("fuel_entry_count", "No Entries:",value = 0)
      })

      output$error_margin_check <- renderUI({
        NULL
      })

      output$manpower_acc_check <- renderUI({
        NULL
      })

      updateNumericInput(session,"manpower_reduction_dte",value = 0)

      # output$manpower_acc_check <- renderUI({
      #   radioButtons("manpower_acc_q","Do you have Accountants for operation hour and fuel data compilation?",
      #                choices = c("Yes" = TRUE, "No" = FALSE),
      #                inline = TRUE,
      #                selected = FALSE)
      # })

      output$manpower_acc_check <- renderUI({
        NULL
      })

      updateNumericInput(session,"manpower_reduction_accountant",value = 0)
      updateSliderInput(session,"accountant_cost",value=0)
    }
  })

  # observeEvent(input$manpower_acc_q, {
  #   # For the case when dte is required but acc not required
  #   if(input$manpower_acc_q == FALSE && input$manpower_dte_q == TRUE){
  #     updateSliderInput(session,"accountant_cost",value=0)
  #     updateNumericInput(session,"manpower_reduction_accountant",value = 0)
  #   }else{
  #     updateSliderInput(session,"accountant_cost",value=500000)
  #     predicted_red_accountant = 0.4 * values()$count_of_accountant
  #
  #     updateNumericInput(session,"manpower_reduction_accountant",value = predicted_red_accountant)
  #
  #   }
  # })

  # observe({
  #   predicted_red_dispatcher =  1
  #   predicted_red_logger = values()$count_of_loggers - 1
  #   predicted_red_dte = round(0.66 * values()$count_of_dataEntry ,0)
  #   predicted_red_accountant = 0.4 * values()$count_of_accountant
  #
  #
  #   if(input$manpower_dispatch_q){
  #     # updateNumericInput(session, "coordinator_count", value = 1)
  #
  #     updateNumericInput(session, "manpower_reduction_dispatcher", value = 1)
  #   }else{
  #     updateNumericInput(session, "coordinator_count", value = 0)
  #     updateNumericInput(session, "manpower_reduction_dispatcher", value = 0)
  #   }
  #
  #   if(input$manpower_logger_q == FALSE){
  #     updateNumericInput(session, "logger_count_per_bowser", value = 0)
  #     updateNumericInput(session, "manpower_reduction_logger", value = 0)
  #
  #     print(values()$logger_total_cost)
  #     print(values()$count_of_loggers)
  #   }
  #
  #   if(input$manpower_acc_q == FALSE){
  #     updateNumericInput(session, "manpower_reduction_accountant", value = 0)
  #   }
  # })


  output$logger_count <- renderText({
    values()$count_of_loggers
  })

  output$entries_per_year <- renderText({
    format_indian(values()$entries_per_year)
  })

  output$data_entry_count <- renderText({
    values()$count_of_dataEntry
  })

  output$manpower_data <- render_gt({
    field_data <- data.frame(
      FTE = c(
        "Fuel Dispatchers",
        "Fuel Loggers",
        "Data Entry Operators",
        "Accountants",
        "Total"
      ),
      current_Count = c(
        format_indian(input$coordinator_count),
        format_indian(values()$count_of_loggers),
        format_indian(values()$count_of_dataEntry),
        values()$count_of_accountant,
        round(sum(
          input$coordinator_count,
          values()$count_of_loggers,
          values()$count_of_dataEntry,
          values()$count_of_accountant
        ),2)
      ),
      current_Cost = c(
        format_indian(values()$dipatcher_total_cost),
        format_indian(values()$logger_total_cost),
        format_indian(values()$dto_total_cost),
        format_indian(values()$accountant_total_cost),
        format_indian(sum(
          values()$dipatcher_total_cost,
          values()$logger_total_cost,
          values()$dto_total_cost,
          values()$accountant_total_cost
        ))
      ),
      reduced_Count = c(
        paste("(-)",format_indian(input$manpower_reduction_dispatcher)),
        paste("(-)",format_indian(input$manpower_reduction_logger)),
        paste("(-)",format_indian(input$manpower_reduction_dte)),
        paste("(-)",round(input$manpower_reduction_accountant,2)),
        paste("(-)",round(sum(input$manpower_reduction_dispatcher,input$manpower_reduction_logger,input$manpower_reduction_dte,input$accountant_count - 1),2))
      ),
      future_Count = c(
        format_indian(input$coordinator_count - input$manpower_reduction_dispatcher),
        format_indian(values()$count_of_loggers - input$manpower_reduction_logger),
        format_indian(values()$count_of_dataEntry - input$manpower_reduction_dte),
        round(values()$count_of_accountant - input$manpower_reduction_accountant,2),
        sum(input$coordinator_count - input$manpower_reduction_dispatcher,
            values()$count_of_loggers - input$manpower_reduction_logger,
            values()$count_of_dataEntry - input$manpower_reduction_dte,
            round(values()$count_of_accountant - input$manpower_reduction_accountant),2)
      ),
      future_Cost = c(
        format_indian(cost.df()$Saved[1]),
        format_indian(cost.df()$Saved[2]),
        format_indian(cost.df()$Saved[3]),
        format_indian(cost.df()$Saved[4]),
        format_indian(round(sum(cost.df()$Saved[1],
                          cost.df()$Saved[2],
                          cost.df()$Saved[3],
                          cost.df()$Saved[4]),2))
      )
    )

    # Create the gt table with customization
    gt(field_data) %>%
      tab_header(
        title = md("Manpower Expense Comparisions"),
        subtitle = md("A tabulation of current expenses and `MindShift` impact")
      ) %>%
      cols_label(
        FTE = "Roles",
        current_Count = "Head Count(FTE)",
        current_Cost = "Annual CTC (₹)",
        reduced_Count = "Reduction in Head Count (FTE)",
        future_Count = "Impacted Head Count (FTE)",
        future_Cost = "Impacted Annual CTC (₹)"
      ) %>%

      # adding column spanner and styling
      tab_spanner(
        label="Current State",
        columns = vars(current_Count,current_Cost)
      ) %>%
      tab_style(
        style = list(
          cell_fill(color = "blue"),
          cell_text(color = "white", weight = "bold")
        ),
        locations = cells_column_spanners(spanners = "Current State")
      ) %>%
      tab_spanner(
        label="MindShift's Solution Impact",
        columns = vars(reduced_Count,future_Count,future_Cost)
      ) %>%
      tab_style(
        style = list(
          cell_fill(color = "orange"),
          cell_text(weight = "bold")
        ),
        locations = cells_column_spanners(spanners = "MindShift's Solution Impact")
      )  %>%

      # adding styling to cells and borders
      tab_style(
        style = list(
          cell_text(weight = "bold")
        ),
        locations = cells_column_labels(everything())
      ) %>%
      tab_style(
        style = cell_borders(
          sides = "right",
          color = "gray",
          weight = px(1)
        ),
        locations = cells_body(columns = everything())
      ) %>%
      tab_style(
        style = cell_borders(
          sides = "left",
          color = "gray",
          weight = px(1)
        ),
        locations = cells_body(columns = everything())
      ) %>%
      tab_style(
        style = cell_borders(
          sides = "right",
          color = "gray",
          weight = px(1)
        ),
        locations = cells_column_labels(everything())
      ) %>%
      tab_style(
        style = cell_borders(
          sides = "left",
          color = "gray",
          weight = px(1)
        ),
        locations = cells_column_labels(everything())
      ) %>%
      tab_style(
        style = list(
          cell_fill(color = "pink")
        ),
        locations = cells_body(
          columns = vars(current_Cost),
          rows = c(5)
        )
      ) %>%
      tab_style(
        style = list(
          cell_fill(color = "lightgreen")
        ),
        locations = cells_body(
          columns = vars(future_Cost),
          rows = c(5)
          )
        ) %>%
      tab_style(
          style = list(
            cell_text(weight = "bold")
          ),
          locations = cells_body(everything())
          )
  })

  output$histogram <- renderPlotly({

    orig_explanation <- c(
      paste("Current Cost: <b>₹",format_indian(values()$dipatcher_total_cost),"/-</b> of <b>Fuel Dispatcher</b>"), #Dispatcher
      paste("Current Cost: <b>₹",format_indian(values()$logger_total_cost),"/-</b> of <b>Fuel Data loggers</b>"), #Loggers
      paste("Current Cost: <b>₹",format_indian(values()$dto_total_cost),"/-</b> of <b>Data Entry operators</b>"), #DTE
      paste("Current Cost: <b>₹",format_indian(values()$accountant_total_cost),"/-</b> of <b>Accountants</b>") #Accountant
    )

    saved_explanation <- c(

      paste("With a predicted reduction of",input$manpower_reduction_dispatcher," in <b>Fuel Dispatchers<b>, Updated Cost is: <b>₹",format_indian(cost.df()$Saved[1]),"/-</b>"), #Dispatcher
      paste("With a predicted reduction of",input$manpower_reduction_logger," in <b>Fuel Data Loggers<b>, Updated Cost is: <b>₹",format_indian(cost.df()$Saved[2]),"/-</b>"), #Logger
      paste("With a predicted reduction of",input$manpower_reduction_dte," in <b>Data Entry Operators<b>, Updated Cost is: <b>₹",format_indian(cost.df()$Saved[3]),"/-</b>"), #Data Entry Operator
      paste("With a predicted reduction of",input$manpower_reduction_accountant," in <b>Accountants<b>, Updated Cost is: <b>₹",format_indian(cost.df()$Saved[4]),"/-</b>") #Accountant
    )

    data <- data.frame(cost.df()$Titles, cost.df()$Cost, cost.df()$Saved)
    colnames(data) <- c("Category","Metrics","saved_value")

    middle_pos = cost.df()$Saved/2

    gg <- ggplot(data) +
      geom_bar(aes(x = Category, y = Metrics, fill="original",text=orig_explanation), stat = "identity", position="dodge") +
      geom_bar(aes(x = Category, y = saved_value, fill="saved",text=saved_explanation), stat = "identity", position="dodge") +
      geom_text(aes(x = Category, y = middle_pos, label = paste("₹",format_indian(saved_value))), vjust = 0, size = 4,color="white") +
      geom_text(aes(x= Category, y = 0.8*cost.df()$Cost, label = paste("₹",format_indian(Metrics))), vjust=0, size = 3.5,color="white") +
      scale_fill_manual(values = c("original" = "blue", "saved" = "orange")) +
      labs(fill = "Saving Comparisions") +
      theme(legend.position = "none")

    # Convert ggplot object to plotly for interactive plots
    p_plotly <- ggplotly(gg, tooltip = "text")

    return(p_plotly)
  })

  output$manpower_summation_current <- renderText({
    paste("₹",format_indian(values()$logger_total_cost + values()$dto_total_cost + values()$dipatcher_total_cost + values()$accountant_total_cost))
  })
  output$manpower_summation <- renderText({
    paste("₹",format_indian(cost.df()$Saved[1] + cost.df()$Saved[2] + cost.df()$Saved[3] + cost.df()$Saved[4]))
  })
  output$manpower_fte_total <- renderText({
    format_indian(values()$count_of_dataEntry + values()$count_of_loggers + input$coordinator_count )
  })
  output$manpower_pte_total <- renderText({
    values()$count_of_accountant
  })

  # output$pieChart <- renderPlot({
  #   ggplot(cost.df(), aes(x = "", y = Cost, fill = Titles)) +
  #     geom_bar(width = 1, stat = "identity") +
  #     coord_polar(theta = "y") +
  #     scale_fill_manual(values = c('#FF9999', '#66B3FF', '#99FF99', '#FFCC99')) +
  #     theme_void()
  # })
  #





  # PILFERAGE

  observeEvent(input$annualf_consump_info,{
    shinyalert("Annual Fuel Consumption Calculation","Case1: If Hemm count and Hemm consumption is specified:\n
                           HEMM daily consumption * count of HEMM * 365 days\n
                           Case2: If HEMM info not specified:\n
                           Each bowser assumed to fuel 65 HEMMs having 70lt daily consumption * 365 days",type="info")
  })

  output$annual_fuel_consump <- renderText({
    format_indian(pilferage_values()$annual_consump_vol)
  })

  output$refuels_per_month <- renderText({
    pilferage_values()$ref_per_month
  })

  # info modals using shinyalert
  observeEvent(input$ur_info, {
    shinyalert("Under Refueling", "Alleged collusion between a diesel bowser driver and fuel supplier results in discrepancies between fuel input records and actual amounts.\n
               Falsified records and the misappropriation of 20 liters of fuel for offsite sale.", type = "info")
  })
  observeEvent(input$theft_info, {
    shinyalert("HEMM Fuel Tank Theft", "Collusion between a heavy machinery operator and off-site accomplices leads to the theft of fuel from the machine's tank for resale.\n
               Extended idling periods are employed to obscure the fuel loss", type = "info")
  })


  output$refuels_per_year <- renderText({
    pilferage_values()$ref_per_year
  })

  output$underreported_calculations <- renderTable({
    pilferage_percent = (pilferage_values()$under_reporting_yearly / pilferage_values()$annual_consump_vol) * 100

    data.frame(
      Field = c("Daily over reported volume across fleet","Yearly figure","Percentage of Yearly consumption"),
      Values = c(format_indian(pilferage_values()$ur_daily_vol), format_indian(pilferage_values()$under_reporting_yearly), round(pilferage_percent,2))
    )
  })

  output$stolen_assumption <- renderTable({
    pilferage_percent = (pilferage_values()$bowser_fuel_sold_yearly / pilferage_values()$annual_consump_vol) * 100

    data.frame(
      Field = c("Yearly figure","Percentage of Yearly consumption"),
      Values = c(format_indian(pilferage_values()$bowser_fuel_sold_yearly), round(pilferage_percent,2))
    )
  })

  output$pilferage_hist <- renderPlotly({
    pilferage_percent = (pilferage_values()$vol_saved_yearly / pilferage_values()$annual_consump_vol) * 100

    value_names <- c("Under Reporting", "Fuel Sold Illegally")
    original_value <- c(
      pilferage_values()$under_reporting_yearly,
      pilferage_values()$bowser_fuel_sold_yearly
    )
    saved_value <- c(
      pilferage_values()$saving_ur,
      pilferage_values()$saving_ftheft
    )

    # Create a data frame from the values
    data <- data.frame(
      Category = value_names,
      original = original_value,
      saved = saved_value
    )
    orig_explanation <- c(
      paste("<b>",format_indian(pilferage_values()$bowser_fuel_sold_yearly),"Litres</b> of Fuel currently Sold Illegally"),
      paste("<b>",format_indian(pilferage_values()$under_reporting_yearly),"Litres</b> of Fuel currently Under-reported")
    )
    saved_explanation <- c(
      paste("Savings of <b>",format_indian(pilferage_values()$saving_ftheft),"Litres</b> of Fuel after MindShift Under-reported"),
      paste("Savings of <b>",format_indian(pilferage_values()$saving_ur),"Litres</b> of Fuel after MindShift Under-reported")
    )

    # Create bar plot using ggplot2
    p <- ggplot(data) +
      geom_bar(aes(x=Category, y=original, fill="saved_col", text=orig_explanation),stat = "identity",position = "dodge") +
      # geom_bar(aes(x=Category, y=saved, fill="saved_col", text=saved_explanation),stat = "identity",position = "dodge",width=0.8) +
      geom_text(aes(x=Category, y=saved/2, label=format_indian(saved)), vjust=0,size=5,color="white") +
      scale_fill_manual(values = c("original_col" = "blue", "saved_col" = "orange")) +
      labs(fill = "Saving Comparisions") +
      theme(legend.position = "none")


    # Convert ggplot object to plotly for interactive plots
    p_plotly <- ggplotly(p, tooltip = c("x", "text"))

    return(p_plotly)
  })

  output$pilferage_explanation <- renderText({
    format_indian(pilferage_values()$vol_saved_yearly)
  })

  output$pilferage_cost <- renderText({
    paste("₹",format_indian(pilferage_values()$vol_saved_yearly * 86))
  })



  # IDLING Tab

  idle_total <- reactive({
    #checking input to prevent crashes
    req(!is.null(input$idle_load_perc), input$idle_load_perc != 0)
    req(!is.null(input$idle_mod_on_val), input$idle_mod_on_val != 0)



    off_perc = 100 - input$idle_usage_per


    total_hours = input$shift_count * 8
    working_hours = round(total_hours * input$idle_usage_per/100,1)

    idle_on_perc = 100 - input$idle_load_perc


    idle_idling_working_hours = round(working_hours * idle_on_perc/100,1)
    idle_loading_working_hours = round(working_hours * input$idle_load_perc/100,1)
    idle_off_working_hours = total_hours - working_hours


    # Overall consumption of fuel of single HEMM
    idling_ldp = idle_idling_working_hours * input$idle_on_lph + idle_loading_working_hours * input$idle_loaded_lph
    # Overall consumption of fuel of all HEMM
    idling_all_ldp = idling_ldp * input$hemm_count

    mod_idle_hours_consump = input$idle_mod_on_val * input$idle_on_lph

    # Overall modified consumption of fuel of single HEMM
    idle_mod_consump_lpd = idling_ldp - idle_idling_working_hours * input$idle_on_lph + mod_idle_hours_consump
    # Overall modified consumption of fuel of all HEMM
    idle_mod_all_consump_lpd = idle_mod_consump_lpd * input$hemm_count

    diff_value = (idling_ldp - idle_mod_consump_lpd)*input$hemm_count * 365 * 86

    # Difference in overall fuel consumption of a single HEMM
    idle_single_diff = idling_ldp - idle_mod_consump_lpd

    data.frame(
      # on_load_sum = on_load_sum,
      off_perc = off_perc,
      idle_on_perc = idle_on_perc,

      total_hours = total_hours,

      working_hours = working_hours,
      idle_idling_working_hours = idle_idling_working_hours,
      idle_loading_working_hours = idle_loading_working_hours,
      idle_off_working_hours = idle_off_working_hours,

      idling_ldp = idling_ldp,
      idle_mod_consump_lpd = idle_mod_consump_lpd,
      idling_all_ldp = idling_all_ldp,
      idle_mod_all_consump_lpd = idle_mod_all_consump_lpd,

      diff_value = diff_value,
      idle_single_diff = idle_single_diff
    )
  })

  output$idle_total_time <- renderText({
    paste(idle_total()$total_hours,"hours")
  })
  output$idle_on_perc <- renderText({
    idle_total()$idle_on_perc
  })
  output$idle_util_hours <- renderText({
    idle_total()$working_hours
  })
  output$idle_off_hours <- renderText({
    idle_total()$idle_off_working_hours
  })

  observeEvent(input$idle_mod_on_val,{
    if(input$idle_mod_on_val >= idle_total()$idle_idling_working_hours || input$idle_mod_on_val <= 0){
      rechcked_idle_val = idle_total()$idle_idling_working_hours - 0.5
      updateNumericInput(session,"idle_mod_on_val",value=rechcked_idle_val)
    }
  })

  output$idle_comparision_table <- render_gt({
    perc_val = (idle_total()$idling_ldp - idle_total()$idle_mod_consump_lpd)/idle_total()$idling_ldp
    perc_val = perc_val * 100

    idle_lpd_diff_perc = paste(round(perc_val,2),"%")

    idle_consump_single_diff = round(idle_total()$idling_ldp - idle_total()$idle_mod_consump_lpd,2)

    field_data <- data.frame(
      Metrics = c(
        "Litres Consumed/Day/HEMM (Litres)",
        "Litres Consumed/Day/All HEMM (Litres)",
        "Difference"
      ),
      current_Consumption = c(
        format_indian(idle_total()$idling_ldp),
        format_indian(idle_total()$idling_all_ldp),
        idle_consump_single_diff
      ),
      new_Consumption = c(
        format_indian(idle_total()$idle_mod_consump_lpd),
        format_indian(idle_total()$idle_mod_all_consump_lpd),
        idle_lpd_diff_perc
      )
    )

    # Create the gt table with customization
    gt(field_data) %>%
      tab_header(
        title = md("Consumption Comparisions"),
        subtitle = md("A tabulation of consumption change by `MindShift` impact")
      ) %>%
      cols_label(
        Metrics = "Metrics",
        current_Consumption = "Current LPH",
        new_Consumption = "Reduced LPH"
      ) %>%

      # adding styling to cells and borders
      tab_style(
        style = list(
          cell_text(weight = "bold")
        ),
        locations = cells_column_labels(everything())
      ) %>%
      tab_style(
        style = cell_borders(
          sides = "right",
          color = "gray",
          weight = px(1)
        ),
        locations = cells_body(columns = everything())
      ) %>%
      tab_style(
        style = cell_borders(
          sides = "left",
          color = "gray",
          weight = px(1)
        ),
        locations = cells_body(columns = everything())
      ) %>%
      tab_style(
        style = cell_borders(
          sides = "right",
          color = "gray",
          weight = px(1)
        ),
        locations = cells_column_labels(everything())
      ) %>%
      tab_style(
        style = cell_borders(
          sides = "left",
          color = "gray",
          weight = px(1)
        ),
        locations = cells_column_labels(everything())
      ) %>%
      tab_style(
        style = list(
          cell_fill(color = "pink")
        ),
        locations = cells_body(
          columns = vars(current_Consumption),
          rows = c(3)
        )
      ) %>%
      tab_style(
        style = list(
          cell_fill(color = "pink")
        ),
        locations = cells_body(
          columns = vars(new_Consumption),
          rows = c(3)
        )
      ) %>%
      tab_style(
        style = list(
          cell_text(weight = "bold")
        ),
        locations = cells_body(everything())
      )
  })

  output$idle_off_perc <- renderText({
    idle_total()$off_perc
  })

  output$idle_idling_working_hours <- renderText({
    idle_total()$idle_idling_working_hours
  })
  output$idle_loading_working_hours <- renderText({
    idle_total()$idle_loading_working_hours
  })
  output$idle_off_working_hours <- renderText({
    idle_total()$idle_off_working_hours
  })

  output$idle_yearly_value <- renderText({
    paste("₹",format_indian(round(idle_total()$diff_value,2)))
  })
  output$idle_single_diff <- renderText({
    idle_total()$idle_single_diff
  })

  # output$idling_plot <- renderPlotly({
  #   data = data.frame(
  #     title = c("Daily Consumption/HEMM"),
  #     original = c(idle_total()$idling_ldp),
  #     saved = c(idle_total()$idle_mod_consump_lpd)
  #   )
  #
  #   gg <- ggplot(data)+
  #     geom_bar(aes(x=title,y=original,fill="original_col"),stat="identity",position = position_dodge(width = 0.7))+
  #     geom_bar(aes(x=title,y=saved, fill="saved_col"),stat="identity",position=position_dodge(width = 0.7))+
  #     geom_text(aes(x=title, y=saved/2, label=format_indian(saved)), vjust=0,size=3.5)+
  #     scale_fill_manual(values = c("original_col" = "blue", "saved_col" = "orange")) +
  #     labs(fill = "Saving Comparisions") +
  #     theme(legend.position = "none")
  #
  #   return(gg)
  # })
  output$idling_plot <- renderPlotly({
    data <- data.frame(
      title = rep("Daily Consumption/HEMM", 2),
      type = c("Original", "Saved"),
      value = c(idle_total()$idling_ldp, idle_total()$idle_mod_consump_lpd),
      explanation = c(
        paste("Originally <b>",idle_total()$idling_ldp," litres</b> of fuel is consumed per day"),
        paste("After MindShift <b>",idle_total()$idle_mod_consump_lpd," litres</b> of fuel is consumed per day"))
    )

    gg <- ggplot(data, aes(y = title, x = value, fill = type, text=explanation)) +
      geom_bar(stat = "identity", position = position_dodge(width = 1)) +
      geom_text(aes(x=value/2,label = format_indian(value)),
                position = position_dodge(width = 1),
                vjust = 0.5, hjust = -0.3, size = 5,color="white") +
      scale_fill_manual(values = c("Original" = "blue", "Saved" = "orange")) +
      labs(fill = "Saving Comparisons",x="Litres Consumed /HEMM/Day",y="Comparision Before After") +
      theme(legend.position = "none") +
      coord_flip()

    ggplotly(gg, tooltip = "text")
  })





  # MOVABLE VEHICLE COST CALCULATION

  # output$movable_refuel_sumannual <- renderText({
  #   travelling_data()$annual_refuels
  # })
  # output$movable_time_spent <- renderText({
  #   travelling_data()$movable_time_spent
  # })
  # output$ref_per_month <- renderText({
  #   pilferage_values()$ref_per_month
  # })
  # output$movable_visualisation <- renderPlotly({
  #   data <- data.frame(
  #     category = c("Movement Statistics"),
  #     value1 = c(travelling_data()$annual_refuels),
  #     value2 = c(travelling_data()$movable_time_spent),
  #     value3 = c(travelling_data()$movable_time_spent * input$movable_hemm_price)
  #   )
  #
  #   colnames(data)[2:4] <- c("Annual Refuel Count", "Overall Time Spent", "Cost of Time")
  #
  #   # Reshape data for ggplot
  #   data_long <- tidyr::pivot_longer(data, cols = c("Annual Refuel Count", "Overall Time Spent", "Cost of Time"), names_to = "variable", values_to = "value")
  #
  #   # Format the values with the format_indian function
  #   data_long$formatted_value <- format_indian(data_long$value)
  #
  #   # Plot using ggplot
  #   gg <- ggplot(data_long, aes(x = category, y = value, fill = variable)) +
  #     geom_bar(stat = "identity", position = "dodge") +
  #     geom_text(aes(label = formatted_value), position = position_dodge(width = 1), vjust = 0) +
  #     scale_fill_manual(values = c("Annual Refuel Count" = "lightblue", "Overall Time Spent" = "orange", "Cost of Time" = "green")) + # Assign colors
  #     labs(title = "",
  #          x = "Movement Statistics", y = "Value") +
  #     theme_minimal() +
  #     scale_y_continuous(trans = "log10")
  #
  #   # Convert ggplot to plotly
  #   ggplotly(gg, tooltip="fill")
  # })

  output$movale_money_loss_hours <- renderText({
    travelling_data()$movable_time_spent * input$movable_hemm_price
  })
  output$movale_annual_money_loss_hours <- renderText({
    travelling_data()$annual_movable_sum
  })



  # SUMMARY

  output$summary_waterfall <- renderPlotly({
    # manpower sum
    mp_sum <- cost.df()$Saved[1] + cost.df()$Saved[2] + cost.df()$Saved[3] + cost.df()$Saved[4] + 0

    # pilferage sum
    pl_sum <- pilferage_values()$vol_saved_yearly * 86

    # movement sum
    mv_sum <- 1000000

    #idling sum
    idle_sum <- (idle_total()$idling_all_ldp - idle_total()$idle_mod_all_consump_lpd)*365*86

    x <- list("Manpower", "Pilferage", "Idling", "Annual Sum")
    measure <- c("relative", "relative", "relative", "total")
    text <- c("Manpower Savings", "Pilferage Savings", "Idling Savings","Total Sum (₹)")
    y <- c(mp_sum, pl_sum, idle_sum, mp_sum + pl_sum + idle_sum)
    labels <- c(format_indian(mp_sum), format_indian(pl_sum), format_indian(idle_sum), format_indian(mp_sum + pl_sum + idle_sum))
    explanation <- c(
      paste("Value saved from ManPower: <b>₹",format_indian(mp_sum),"/-</b>"),
      paste("Value saved from Pilferage: <b>₹",format_indian(pl_sum),"/-</b>"),
      paste("Value saved from Idling: <b>₹",format_indian(idle_sum),"/-</b>"),
      paste("Yearly Savings by using <b>MindShift:  ₹",format_indian(mp_sum + pl_sum + idle_sum),"/-</b>")
    )

    data <- data.frame(
      x = factor(x, levels = x),
      measure = measure,
      text = text,
      y = y,
      labels = labels,
      explanation = explanation
    )

    fig <- plot_ly(
      data, name = "Savings", type = "waterfall", measure = ~measure,
      x = ~x, y = ~y, text = ~labels, textposition = "outside",
      hoverinfo = "text", texttemplate = ~labels,
      hovertext = ~explanation,
      connector = list(line = list(color = "rgb(63, 63, 63)"))
    )

    fig <- fig %>%
      layout(
        title = "Overall Annual Savings across 4 Domains Yearly(₹)",
        xaxis = list(title = "Domains"),
        yaxis = list(title = "Metrics"),
        autosize = TRUE,
        showlegend = TRUE
      )

    fig
  })

}
