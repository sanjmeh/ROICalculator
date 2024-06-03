library(shiny)
library(shinyBS)
library(bslib)
library(shinydashboard)
library(DT)
library(plotly)
library(ggplot2)
library(shinyalert)



ui <- shinyUI(fluidPage(

  tags$head(
    tags$style(HTML("
      pre {
        border: 2px solid #007BFF;
        background: linear-gradient(to bottom, rgba(255, 255, 255, 0.8), rgba(200, 200, 200, 0.8));
        cursor: not-allowed;
        color: #333;
        padding: 10px;
        margin-bottom: 10px;
      }
    "))
  ),

  titlePanel("Return Of Investment Calculator"),


  h2("Universal Values"),
  fluidRow(column(width=2,
                  numericInput("shift_count","Number of shifts",value = 2)),
           column(width=2,
                  numericInput("hemm_count","Number of HEMM",value=100)),
           column(width=2,
                  numericInput("truck_count","Number of diesel bowsers/site",value = 3,min=0,max=10)),
           column(width=2,
                  numericInput("hemm_daily_consump","HEMM Fuel Consumption/Day",value=100)),
           column(width=2,
                  numericInput("sfs_count","Number of Fuel Stations",value=2)),
           column(width=2,
                  uiOutput("fuel_entry_count_check"))
  ),


  navbarPage("Menu",

             tabPanel("Manpower",
                      sidebarLayout(
                        sidebarPanel(
                          fluidRow(
                            column(12,
                                   div(
                                     fluidRow(column(9,h3("Step 1: Enter FTE Details")),column(3,br(),actionButton("manpower_info_button", "Info", icon = icon("info-circle")))),
                                     radioButtons("manpower_dispatch_q","Do you have Fuel Dispatchers for scheduling Fuel Bowser Trips?",
                                                  choices = c("Yes" = TRUE, "No" = FALSE),
                                                  inline = TRUE),
                                     numericInput("coordinator_count"," Fuel Dispatchers/Shift",value=2),
                                     radioButtons("manpower_logger_q","Do you have Fuel Data Loggers for logging on-site fuel transactions?",
                                                  choices = c("Yes" = TRUE, "No" = FALSE),
                                                  inline = TRUE),
                                     uiOutput("manpower_logger_check"),
                                     radioButtons("manpower_dte_q","Do you have Data Entry Operators for manual backend updation?",
                                                  choices = c("Yes" = TRUE, "No" = FALSE),
                                                  inline = TRUE,
                                                  selected = TRUE),
                                     uiOutput("error_margin_check"),
                                     uiOutput("manpower_acc_check")
                                     )
                                   )
                            ),
                          br(),
                          fluidRow(
                            column(12,
                                   div(
                                     fluidRow(column(9,h3("CTC Input:")),column(3,br(),actionButton("manpower_ctc_info_button", "Info", icon = icon("info-circle")))),

                                     sliderInput("fuel_dispatcher_cost","Avg Annual CTC of Fuel Dispatcher: ",value=500000,min=100000,max=1000000),
                                     sliderInput("fuel_logger_cost","Avg Annual CTC of Fuel Logger: ",,value=150000,min=100000,max=300000),
                                     sliderInput("data_entry_cost","Avg Annual CTC of Data Entry Operator FTE: ",value=300000,min=100000,max=500000),
                                     sliderInput("accountant_cost","Avg Annual CTC of Accountant FTE: ",value=500000,min=100000,max=800000),
                                     )
                                   )
                            )
                          ),
                        mainPanel(fluidPage(
                          column(8,
                                 fluidRow(
                                   tableOutput("manpower_data")
                                 ),
                                 fluidRow(
                                   plotlyOutput("histogram"),
                                 )
                          ),
                          column(4,
                                 h5("Current Cost of Manpower (₹):"),
                                 verbatimTextOutput("manpower_summation_current"),
                                 h5("Revised Cost of Manpower (₹):"),
                                 verbatimTextOutput("manpower_summation"),
                                 h5("Employed FTE count:"),
                                 verbatimTextOutput("manpower_fte_total"),
                                 h3("How you can acheive savings with MindShift:"),
                                 p("MindShift offers the capability to automate manual data entry, updates, and analysis processes, facilitating a transition to a time-efficient fuel management method.\n
                                   This transition leads to cost savings and increased productivity for your organization.")),
                          )
                          )
                        ),
                      bsModal("manpower_info_modal", "Manpower Roles Information:", "manpower_info_button", size = "large",
                              p("This section provides a concise overview of the duties associated with various personnel positions."),
                              h4("More Details:"),
                              tags$ul(
                                tags$li(
                                  h3("Fuel Dispatchers:"),
                                  p("A fuel dispatcher oversees and coordinates the scheduling of diesel bowser trips to Heavy Earth Moving Machinery (HEMM) in each operational shift, ensuring timely refueling services are provided as required."),
                                ),
                                tags$li(
                                  h3("Fuel Loggers:"),
                                  p("These individuals manually log HMR and fuel transaction from a bowser to an HEMM on paper"),
                                ),
                                tags$li(
                                  h3("Data Entry Operators:"),
                                  p("A data entry operator is tasked with the responsibility of transcribing manually recorded logs and inputting the data into the backend system."),
                                  p("The allocation of data entry operators is determined by the volume of entries recorded per day, facilitating efficient management of yearly entries and the corresponding upload timeframe.")
                                ),
                                tags$li(
                                  h3("Accountants:"),
                                  p("Accountants are tasked with the responsibility of verifying the accuracy of data entered into the backend system, rectifying any discrepancies as needed."),
                                  p("They monitor the operational hours of Heavy Earth Moving Machinery (HEMM) and reconcile fuel consumption data with fuel dispensing records.")
                                ),
                                )
                              ),
                      bsModal("manpower_ctc_info_modal", "Manpower Roles Information:", "manpower_ctc_info_button", size = "large",
                              p("In an attempt to estimate Return of Investment, we are trying to acquire cost incurred for a single FTE across different roles.")
                              )
                      ),



             # MANPOWER TAB ORIGINAL

             # tabPanel("Manpower Calculation",
             #          h1("Enter Work Parameters"),
             #          sidebarLayout(
             #            sidebarPanel(
             #              fluidRow(
             #                column(6,numericInput("logger_count_per_bowser","Fuel Logger/Bowser",value=1)),
             #                column(6,numericInput("manpower_save_fdl","% Saving in Fuel Logger",value=86))),
             #              div(
             #                h5("Total Fuel Loggers:"),
             #                verbatimTextOutput("logger_count",TRUE)),
             #
             #              fluidRow(column(6,h5("Entries per year: (entries/day * 365)"),verbatimTextOutput("entries_per_year")),
             #                       column(6,h5("Data Entry Operators employed annually."),verbatimTextOutput("data_entry_count"))),
             #
             #              # h6("Assuming that 5% of the entries made by the operator will be erroneous and hence will require correction."),
             #              # h6("5 mins per entry ~ 3 mins for entry and 2 mins for correction"),
             #              actionButton("dto_count_info", "Info",
             #                           icon("lightbulb"),
             #                           style="color: #fff; background-color: #008000; border-color: #2e6da4"),
             #              fluidRow(column(6,numericInput("error_margin","Enter % of erroneous entries",value=5)),
             #                       column(6,
             #                              numericInput("manpower_save_dto","% Saving in Data Entry Operators",value=75))),
             #              br(),
             #              fluidRow(column(6,
             #                              numericInput("coordinator_count"," Fuel Dispatchers/Shift",value=2)),
             #                       column(6,
             #                              numericInput("manpower_save_fdc","% Saving in Fuel Dispatcher",value=50))),
             #              fluidRow(column(6,
             #                              numericInput("accountant_count","Accountants required: ",value=3)),
             #                       column(6,
             #                              numericInput("manpower_save_accounts","% Saving in Accountants",value=66))),
             #
             #
             #              br(),
             # sliderInput("fuel_logger_cost","Avg cost of fuel logger: ",value=150000,min=100000,max=300000),
             # sliderInput("fuel_dispatcher_cost","Avg cost of fuel dispatch coordinator: ",value=500000,min=100000,max=1000000),
             # sliderInput("data_entry_cost","Avg cost of data entry FTE: ",value=300000,min=100000,max=500000),
             # sliderInput("accountant_cost","Avg cost of accountant FTE: ",value=300000,min=100000,max=800000),
             #            ),
             #            mainPanel(
             #              fluidPage(
             #                column(8,
             #                       fluidRow(
             #                         splitLayout(
             #                           tableOutput("manpower_data"),
             #                           tableOutput("manpower_data_2")
             #                         )
             #                       ),
             #                       fluidRow(
             #                         # plotlyOutput("histogram"),
             #                       ),
             #                       fluidRow(
             #                         plotOutput("pieChart")
             #                       ),
             #                ),
             #                column(4,
             #                       h5("Current Cost of Manpower (₹):"),
             #                       verbatimTextOutput("manpower_summation_current"),
             #                       h5("Revised Cost of Manpower (₹):"),
             #                       verbatimTextOutput("manpower_summation"),
             #                       h5("Employed FTE count:"),
             #                       verbatimTextOutput("manpower_fte_total"),
             #                       h3("Assumptions"),
             #                       p("For a data entry full time employee(FTE): woking 8 hour shift, 5 hours of productivity is considered in calculation of data entry operators."),
             #                       br(),
             #                       br(),
             #                       p("Data Aggregator/Compiler FTE: is estimated at 5 LPA * number of compilers required to meet requirements in a year."),
             #                       br(),
             #                       br(),
             #                       p("Cost of Correction: Calculating from the provided margin of errors, number of additional working hours are estimated and number of data entry operators required are calculated")
             #                ),
             #              )
             #            )
             #          )
             # ),

             # PILFERAGE

             tabPanel("Pilferage",
                      fluidPage(
                        fluidRow(
                          column(12,
                                 fluidRow(
                                   column(width=6,
                                          fluidRow(column(10,h4("Average Fuel Consumption/Year: (litres)")),
                                                   column(2,
                                                          actionButton("annualf_consump_info", "Info",
                                                                       icon("lightbulb"),
                                                                       style="color: #fff; background-color: #008000; border-color: #2e6da4")
                                                   )
                                          ),
                                          fluidRow(verbatimTextOutput("annual_fuel_consump")
                                          )
                                   ),
                                   column(width=3,
                                          h4("Refuellings/HEMM/month"),
                                          verbatimTextOutput("refuels_per_month")),
                                   column(width=3,
                                          h4("Refuellings/HEMM/year"),
                                          verbatimTextOutput("refuels_per_year"))
                                 ),
                          )
                        ),
                        fluidRow(
                          column(width = 6,
                                 column(width = 5,
                                        fluidRow(
                                          h3('Under Refueling and Over Reporting'),
                                          # useShinyalert(),  # Set up shinyalert
                                          actionButton("ur_info", "Info",
                                                       icon("lightbulb"),
                                                       style="color: #fff; background-color: #008000; border-color: #2e6da4"),
                                          numericInput("ur_day_count", "How many over-reportings across all fleet do you think happen per day?", value = 40),
                                          numericInput("ur_day_vol", "How many litres is over reported each instance?", value = 10),
                                        ),
                                        fluidRow(
                                          tableOutput("underreported_calculations")
                                        ),
                                 ),
                                 column(2,""),
                                 column(width = 5,
                                        fluidRow(
                                          h3('HEMM Fuel Tank Theft'),
                                          actionButton("theft_info","Info",
                                                       icon("lightbulb"),
                                                       style="color: #fff; background-color: #008000; border-color: #2e6da4"),
                                          numericInput("tank_steals_monthly","How many thefts do you think happen from HEMM fuel tank/monthly?",value=40),
                                          numericInput("bowser_theft_vol", "How many litres of fuel do you think is stolen each instance?", value = 1000),
                                        ),
                                        fluidRow(
                                          tableOutput("stolen_assumption")
                                        ),
                                 )
                          ),
                          column(width = 4,
                                 fluidRow(plotlyOutput("pilferage_hist"))),
                          column(2,fluidRow(
                            h1("Fuel Savings:"),
                            h3("Fuel Savings (Litres)"),
                            verbatimTextOutput("pilferage_explanation"),
                            h3("Fuel Savings (₹)"),
                            verbatimTextOutput("pilferage_cost")))
                        ),
                      )
             ),



             # IDLING Tab

             tabPanel("Idling",
                      h1("Idling Metrics:"),
                      sidebarLayout(
                        sidebarPanel(width=6,fluidRow(column(4,
                                                             numericInput("idle_usage_per","% Machinery Utilisation",min=-10,max=100,value=60)),
                                                      column(4,
                                                             numericInput("idle_load_perc","% Time Loaded State",min=-10,max=100,value=70)),
                                                      column(4,
                                                             numericInput("idle_on_perc","% Time Idling State",min=-10,max=100,value=30))),
                                     fluidRow(column(4,
                                                     numericInput("idle_on_lph","Idling Lires/Hr",value=8)),
                                              column(4,
                                                     numericInput("idle_loaded_lph","Loaded Lires/Hr",value=16)),
                                              column(4,numericInput("idle_mod_on_val","New Idling Hours",value=1))),
                                     br(),
                                     fluidRow(column(8,plotlyOutput("idling_plot")),
                                              column(4,
                                                     h4("Graphical Understanding:"),
                                                     p("In the realm of heavy equipment and machinery management,
                                                                     the initial bar denotes the daily fuel consumption per Heavy Earth Moving Machinery (HEMM).
                                                                     However, after investing in MindShift Analytics,
                                                                     one gains the capability to meticulously track and mitigate idle durations,
                                                                     consequently reducing consumption metrics and enhancing operational efficiency.")))),
                        mainPanel(width=6,fluidPage(fluidRow(column(4,
                                                                    h5("Idling Hours"),
                                                                    verbatimTextOutput("idle_idling_working_hours")),
                                                             column(4,
                                                                    h5("Loaded Hours"),
                                                                    verbatimTextOutput("idle_loading_working_hours")),
                                                             column(4,
                                                                    h5("Off Hours"),
                                                                    verbatimTextOutput("idle_off_working_hours"))),
                                                    fluidRow(column(12,
                                                                    h4("Current Consumption:"))),
                                                    h5("Litres Consumed/Day/HEMM (Litres)"),
                                                    verbatimTextOutput("idle_consump_lpd"),
                                                    fluidRow(column(12,
                                                                    h5("Litres Consumed/Day/All HEMM (Litres)"),
                                                                    verbatimTextOutput("idle_all_consump_lpd"))),
                                                    fluidRow(column(12,
                                                                    h4("A possible scenario to increase savings:"),
                                                                    p("Prolonged periods of idling during operational shifts result in escalated fuel consumption rates and diminished productivity levels.
                                          This application facilitates clients in monitoring idle, loading, and off periods,
                                          thereby enabling the optimization of productivity and the maximization of output yields.")
                                                                    )
                                                             ),
                                                    fluidRow(column(12,
                                                                    h4("New Consumption:"))),
                                                    h5("New Litres Consumed/Day/HEMM (Litres)"),
                                                    verbatimTextOutput("idle_mod_consump_lpd"),
                                                    fluidRow(column(12,
                                                                    h5("Litres Consumed/Day/All HEMM (Litres)"),
                                                                    verbatimTextOutput("idle_mod_all_consump_lpd"))),
                                                    fluidRow(column(6,
                                                                    h5("Consumption Difference (Litres/Day/HEMM)"),
                                                                    verbatimTextOutput("idle_lpd_diff")),
                                                             column(6,
                                                                    h5("% Difference"),
                                                                    verbatimTextOutput("idle_lpd_diff_perc"))),

                                                    )
                                  )
                        )
                      ),




             # MOVEMENT TAB

             # tabPanel("Movement Statistics",
             #          fluidPage(
             #            fluidRow(
             #              column(6,h1("Movable Vehicle Summary/HEMM")),
             #              column(6,fluidRow(
             #                h4("Refuels/HEMM//month"),
             #                verbatimTextOutput("ref_per_month")))
             #            ),
             #            fluidRow(
             #              column(3,numericInput("movable_hemm_count","Number of movable Hemm",value=50)),
             #              column(width=3,numericInput("movable_percent_get","% of refuellings from SFS",value=20)),
             #              column(width=3,numericInput("movable_get_time","Time Spent in each trip",value=1)),
             #              column(width=3,numericInput("movable_hemm_price","Enter price of HEMM/hour",value=1500)),
             #
             #            ),
             #            fluidRow(column(width=6,
             #                            plotlyOutput("movable_visualisation")),
             #                     fluidRow(column(width=3,
             #                                     h3("Number of refuels/annually"),
             #                                     verbatimTextOutput("movable_refuel_sumannual")),
             #                              column(width=3,
             #                                     h3("Total self refeulling time"),
             #                                     verbatimTextOutput("movable_time_spent")),
             #                              fluidRow(
             #                                column(width=5,
             #                                       h3("Annual opportunity cost of 'lost hours' ₹1,500 / hr for single heavy machinery"),
             #                                       verbatimTextOutput("movale_money_loss_hours")),
             #                                column(width=5,
             #                                       h3("Annual opportunity cost of 'lost hours' ₹1,500 / hr for all heavy machinery"),
             #                                       verbatimTextOutput("movale_annual_money_loss_hours"))
             #                              )
             #                     )
             #            ),
             #          )
             # ),

             tabPanel("Summary",
                      fluidPage(
                        h1("Overall Summary"),
                        plotlyOutput("summary_waterfall"),
                        br(),
                        h4("Description"),
                        h5("This waterfall chart illustrates the total savings achieved across three key domains: manpower, pilferage, and movement. Each segment of the chart represents the individual contributions of these domains to the overall savings. The cumulative effect is shown by sequentially adding the savings from each domain, culminating in the final bar that presents the total savings amount. This visual representation allows for a clear and concise understanding of the impact each domain has on our cost-saving initiatives, highlighting the areas with the most significant contributions.")
                      )
             ),
             tabPanel("Corner-Stone Values",
                      fluidPage(
                        fluidRow(column(4,
                                        fluidRow(
                                          h3("Manpower Section"),
                                          numericInput("correction_time","Time taken for erroneous entry correction",value=10),
                                          numericInput("manpower_reduction_dispatcher","Predicted Reduction in Fuel Dispatchers:",value=1),
                                          numericInput("manpower_reduction_logger","Predicted Reduction in Fuel Data Loggers:",value=4),
                                          numericInput("manpower_reduction_dte","Predicted Reduction in Data Entry Operators:",value=2),
                                          numericInput("manpower_reduction_accountant","Predicted Reduction in Accountants:",value=1)
                                        )
                        ),
                        column(4,
                               fluidRow(
                                 h3("Pilferage Section"),
                                 # numericInput("idle_off","Off duty cycle",value=20)
                               )
                        ),
                        column(4,
                               fluidRow(
                                 h3("Movement Section"),
                                 # p("add assumptions here")
                               ))
                        )
                      )
             ),
  )
)
)
