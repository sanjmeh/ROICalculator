library(shiny)
library(DT)
library(ggplot2)
library(plotly)
library(stringr)
library(scales)
options(scipen = 999)

server <- function(input, output, session) {

  # Values relevant across all calculations put in a reactive for easier access

  values <- reactive({
    # these variables are out since they are being used for calculation in data frame
    # data frame scope prevents creation and usage in the same scope hence outside creation
    entries_per_year = input$fuel_entry_count * 365
    error_entries = entries_per_year * input$error_margin / 100
    corrent_entries = entries_per_year - error_entries
    working_days = 365 - 104 - 20 - 12

    # annual_consump_vol = input$hemm_count * input$hemm_daily_consump * 365
    annual_consump_vol = if( input$hemm_count > 0 && input$hemm_daily_consump > 0 )
      input$hemm_count * input$hemm_daily_consump * 365 else
        input$truck_count * 65 * 70 * 365 # each bowser fuelling 65 hemm, each hemm daily consumption of 70lts

    count_of_loggers = input$shift_count * input$truck_count * input$logger_count_per_bowser
    # count_of_dataEntry = round((entries_per_year * 5) / 60 / 5 / working_days, digits = 0)


    count_of_dataEntry = round((corrent_entries * 3) / 60 / 5 / working_days, digits = 0) +
      round((error_entries * input$correction_time) / 60 / 5 / working_days, digits = 0)

    logger_total_cost = input$fuel_logger_cost * count_of_loggers
    dto_total_cost = input$data_entry_emp * count_of_dataEntry
    dipatcher_total_cost = input$coordinator_count * input$fuel_dispatcher_cost
    accountant_total_cost = input$accountant_cost * input$accountant_count

    data.frame(
      entries_per_year = entries_per_year,
      count_of_loggers = count_of_loggers,
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
      (input$fuel_entry_count * 30) / input$hemm_count
    } else {
      (input$fuel_entry_count * 30) / 100
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
        (values()$logger_total_cost),
        (values()$dto_total_cost),
        (values()$accountant_total_cost),
        (values()$dipatcher_total_cost)
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

    annual_refuels = if(input$hemm_count != 0) {
      (input$fuel_entry_count * 365) / input$hemm_count
    } else {
      (input$fuel_entry_count * 365) / 100
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
      n <- nchar(y)
      if (n > 3) {
        last3 <- substr(y, n-2, n)
        other <- substr(y, 1, n-3)
        result <- paste0(gsub("(\\d)(?=(\\d{2})+$)", "\\1,", other, perl=TRUE), ",", last3)
      } else {
        result <- y
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

  observeEvent(input$dto_count_info, {
    shinyalert("Count of Data Entry Operators:", "Assuming that 5% of the entries made by the operator will be erroneous and hence will require correction.\n
               3 mins for entry and extra mins for erroneous entry\n
               Assuming working hours productivity to be 5hrs/8hrs\n
               (entries_per_year * 5) / 60 / 5 : number of working days\n
               number of working days / actual working days/year = number of Data Entry Operators", type = "info")
  })

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
      FTE = c("Count of Fuel Loggers",
              "Count of Data Entry Operators",
              "Count of Accountants",
              "Count of Fuel Dispatchers"
      ),
      Value = c(
        format_indian(values()$count_of_loggers),
        format_indian(values()$count_of_dataEntry),
        format_indian(input$accountant_count),
        format_indian(input$coordinator_count)
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
        format_indian(values()$logger_total_cost),
        format_indian(values()$dto_total_cost),
        format_indian(values()$accountant_total_cost),
        format_indian(values()$dipatcher_total_cost)
      )
    )

    colnames(field_data) <- c("FTE","Cost (₹)")

    return (field_data)
  })

  output$histogram <- renderPlotly({

    orig_explanation <- c(paste("Current Cost: <b>₹",format_indian(values()$logger_total_cost),"/-</b> of <b>Field loggers</b>"),
                          paste("Current Cost: <b>₹",format_indian(values()$dto_total_cost),"/-</b> of <b>Data Entry operators</b>"),
                          paste("Current Cost: <b>₹",format_indian(values()$accountant_total_cost),"/-</b> of <b>Accountants</b>"),
                          paste("Current Cost: <b>₹",format_indian(values()$dipatcher_total_cost),"/-</b> of <b>Fuel Dispatcher</b>"))

    saved_explanation <- c(paste("By saving ",input$manpower_save_fdl,"%, future cost of <b>Fuel Loggers<b>: <b>₹",format_indian(cost.df()$Saved[1]),"/-</b>"), #Logger
                           paste("By saving ",input$manpower_save_dto,"%, future cost of <b>Data Entry Operators<b>: <b>₹",format_indian(cost.df()$Saved[2]),"/-</b>"), #Data Entry Operator
                           paste("By saving ",input$manpower_save_accounts,"%, future cost of <b>Accountant<b>: <b>₹",format_indian(cost.df()$Saved[3]),"/-</b>"), #Accountant
                           paste("By saving ",input$manpower_save_fdc,"%, future cost of <b>Dispatcher<b>: <b>₹",format_indian(cost.df()$Saved[4]),"/-</b>")) #Dispatcher

    data <- data.frame(cost.df()$Titles, cost.df()$Cost, cost.df()$Saved)
    colnames(data) <- c("Category","Metrics","saved_value")

    middle_pos = cost.df()$Saved/2

    gg <- ggplot(data) +
      geom_bar(aes(x = Category, y = Metrics, fill="original",text=orig_explanation), stat = "identity", position="dodge") +
      geom_bar(aes(x = Category, y = saved_value, fill="saved",text=saved_explanation), stat = "identity", position="dodge",width=0.8) +
      geom_text(aes(x = Category, y = middle_pos, label = format_indian(saved_value)), vjust = 0, size = 5,color="white") +
      scale_fill_manual(values = c("original" = "blue", "saved" = "orange")) +
      labs(fill = "Saving Comparisions") +
      theme_void() + theme(legend.position = "bottom")

    # Convert ggplot object to plotly for interactive plots
    p_plotly <- ggplotly(gg, tooltip = "text")

    return(p_plotly)
  })

  output$pieChart <- renderPlot({
    ggplot(cost.df(), aes(x = "", y = Cost, fill = Titles)) +
      geom_bar(width = 1, stat = "identity") +
      coord_polar(theta = "y") +
      scale_fill_manual(values = c('#FF9999', '#66B3FF', '#99FF99', '#FFCC99')) +
      theme_void()
  })

  output$manpower_summation_current <- renderText({
    format_indian(values()$logger_total_cost + values()$dto_total_cost + values()$dipatcher_total_cost + values()$accountant_total_cost)
  })
  output$manpower_summation <- renderText({
    format_indian(cost.df()$Saved[1] + cost.df()$Saved[2] + cost.df()$Saved[3] + cost.df()$Saved[4])
  })
  output$manpower_fte_total <- renderText({
    format_indian(values()$count_of_dataEntry + values()$count_of_loggers + input$coordinator_count + input$accountant_count)
  })




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
    shinyalert("Over and Under reporting", "Alleged collusion between a diesel bowser driver and fuel supplier results in discrepancies between fuel input records and actual amounts.\n
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
    format_indian(pilferage_values()$vol_saved_yearly * 86)
  })



  # IDLING Tab

  idle_total <- reactive({
    #checking input to prevent crashes
    req(!is.null(input$idle_load_perc), input$idle_load_perc != 0)
    req(!is.null(input$idle_on_perc), input$idle_on_perc != 0)
    # req(!is.null(input$idle_mod_off_val), input$idle_mod_off_val != 0)
    req(!is.null(input$idle_mod_on_val), input$idle_mod_on_val != 0)


    on_load_sum = sum(c(input$idle_load_perc, input$idle_on_perc))
    off_perc = 100 - input$idle_usage_per
    total_perc = on_load_sum + off_perc

    total_hours = input$shift_count * 8
    working_hours = round(total_hours * input$idle_usage_per/100,0)

    idle_idling_working_hours = round(working_hours * input$idle_on_perc/100,0)
    idle_loading_working_hours = round(working_hours * input$idle_load_perc/100,0)
    idle_off_working_hours = total_hours - working_hours

    idling_ldp = idle_idling_working_hours * input$idle_on_lph + idle_loading_working_hours * input$idle_loaded_lph
    idling_all_ldp = idling_ldp * input$hemm_count

    mod_idle_hours_consump = input$idle_mod_on_val * input$idle_on_lph

    idle_mod_consump_lpd = idling_ldp - idle_idling_working_hours * input$idle_on_lph + mod_idle_hours_consump
    idle_mod_all_consump_lpd = idle_mod_consump_lpd * input$hemm_count

    data.frame(
      on_load_sum = on_load_sum,
      off_perc = off_perc,

      total_hours = total_hours,

      working_hours = working_hours,
      idle_idling_working_hours = idle_idling_working_hours,
      idle_loading_working_hours = idle_loading_working_hours,
      idle_off_working_hours = idle_off_working_hours,

      idling_ldp = idling_ldp,
      idling_all_ldp = idling_all_ldp,
      idle_mod_consump_lpd = idle_mod_consump_lpd,
      idle_mod_all_consump_lpd = idle_mod_all_consump_lpd
    )
  })


  # New Hours input check and dynamic update
  observeEvent(input$idle_mod_on_val, {

    # # check for: 1.sum of old and new value stays same 2.neither value gets to 0
    # if(original_sum != new_sum || updated_val == 0 || input$idle_mod_on_val == 0){
    #   updated_val = idle_total()$idle_off_working_hours
    #   updateNumericInput(session, "idle_mod_on_val", value = idle_total()$idle_idling_working_hours)
    # } else {
    #   updateTextOutput("idle_mod_off_val", updated_val)
    # }

    updated_off_val = idle_total()$total_hours - input$idle_mod_on_val - idle_total()$idle_loading_working_hours

    original_on_off_sum = idle_total()$idle_idling_working_hours + idle_total()$idle_off_working_hours
    updated_on_off_sum = input$idle_mod_on_val + updated_off_val

    if(original_on_off_sum != updated_on_off_sum || updated_off_val == 0 || input$idle_mod_on_val == 0){
      updated_off_val = idle_total()$idle_off_working_hours
      updateNumericInput(session, "idle_mod_on_val", value = idle_total()$idle_idling_working_hours)
    }else{
      updateTextOutput("idle_mod_off_val",updated_off_val)
    }
  })

  # Current State Percentage input check to prevent unexpected
  observe({
    # Idling and Loading Percentage Check:

    on_load_sum <- idle_total()$on_load_sum

    # idle+load+offtime = total_hours
    if (100 - on_load_sum > 0) {
      updateTextInput(session, "idle_load_perc", label = "% Time Loaded State")
      updateTextInput(session, "idle_on_perc", label = "% Time Idling State")
    } else {
      updateTextInput(session, "idle_load_perc", value = 70)
      updateTextInput(session, "idle_on_perc", value = 30)
    }

  })


  output$idle_off_perc <- renderText({
    idle_total()$off_perc
  })

  output$idle_consump_lpd <- renderText({
    format_indian(idle_total()$idling_ldp)
  })
  output$idle_all_consump_lpd <- renderText({
    format_indian(idle_total()$idling_all_ldp)
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

  output$idle_mod_consump_lpd <- renderText({
    format_indian(idle_total()$idle_mod_consump_lpd)
  })
  output$idle_mod_all_consump_lpd <- renderText({
    format_indian(idle_total()$idle_mod_all_consump_lpd)
  })

  output$idle_lpd_diff <- renderText({
    idle_total()$idling_ldp - idle_total()$idle_mod_consump_lpd
  })
  output$idle_lpd_diff_perc <- renderText({
    perc_val = (idle_total()$idling_ldp - idle_total()$idle_mod_consump_lpd)/idle_total()$idling_ldp
    perc_val = perc_val * 100
    return(perc_val)
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
