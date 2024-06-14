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
      .right-align {
      float: right;
      }
      .title_container {
          display: flex;
          align-items: center;
          justify-content: space-between;
          margin-top: 20px;
          background: linear-gradient(to right, #f58220, #2a52be);
          border-radius: 15px;
          box-shadow: 0 0 30px 15px rgba(255, 255, 255, 0.8);
          position: relative;
          z-index: 1;
      }
      .title_container::before{
          content: '';
          position: absolute;
          top: -20px;
          left: -20px;
          right: -20px;
          bottom: -20px;
          background: inherit;
          z-index: -1;
          filter: blur(8px);
          border-radius: inherit;
      }
      .logo {
          max-width: 250px;
          height: auto;
      }

      .heading {
          font-size: 24px;
          margin: 0;
      }
    "))
  ),

  div(class="title_container",
      div(class="heading",titlePanel("Return Of Investment Calculator")),
      div(
        img(src = "new_logo.png", class = "logo")
      )),


  h2("Universal Values"),
  fluidRow(column(width=2,
                  numericInput("shift_count","Number of shifts",value = 2,min = 1)),
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
                                     fluidRow(column(9,h3("Step 1: Enter FTE Details")),column(3,div(class="right-align",br(),actionButton("manpower_info_button", "Info", icon = icon("info-circle"))))),
                                     radioButtons("manpower_dispatch_q","Do you have Fuel Dispatchers for scheduling Fuel Bowser Trips?",
                                                  choices = c("Yes" = TRUE, "No" = FALSE),
                                                  inline = TRUE),
                                     numericInput("coordinator_count"," Fuel Dispatchers/Shift",value=2),
                                     radioButtons("manpower_logger_q","Do you have Fuel Loggers for logging on-site fuel transactions?",
                                                  choices = c("Yes" = TRUE, "No" = FALSE),
                                                  inline = TRUE),
                                     uiOutput("manpower_logger_check"),
                                     radioButtons("manpower_dte_q","Do you have Data Entry Operators and Accountants for manual backend updation?",
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
                                     fluidRow(column(9,h3(" Step 2: CTC Input")),column(3,div(class="right-align",br(),actionButton("manpower_ctc_info_button", "Info", icon = icon("info-circle"))))),

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
                                   h3("Step 3: Employee Count and Salary Comparison"),
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
                                 splitLayout(h5("Full Time count:"),h5("Part Time count:")),

                                 splitLayout(verbatimTextOutput("manpower_fte_total"),
                                             verbatimTextOutput("manpower_pte_total")),
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


             # PILFERAGE

             tabPanel("Pilferage",
                      fluidPage(
                        fluidRow(
                          column(9,
                                 fluidRow(column(6,h3("Monitoring Pilferage Fuel Loss")),
                                          column(6, br(), div(class = "right-align", actionButton("pilferage_info_button", "Info", icon = icon("info-circle"))))
                                 ),
                                 fluidRow(
                                   column(width=6,
                                          fluidRow(column(10,h4("Average Fuel Consumption/Year: (litres)")),
                                                   column(2,div(class="right-align",
                                                          actionButton("annualf_consump_info", "Info",
                                                                       icon("lightbulb"),
                                                                       style="color: #fff; background-color: #008000; border-color: #2e6da4"))
                                                   )
                                          ),
                                          fluidRow(verbatimTextOutput("annual_fuel_consump")
                                          )
                                   ),
                                   column(width=6,
                                          h4("Refuellings/HEMM/month"),
                                          verbatimTextOutput("refuels_per_month"))
                                 ),
                                 fluidRow(column(width = 4,
                                                 fluidRow(
                                                   fluidRow(column(6,h3('Under Refueling')),
                                                            # useShinyalert(),  # Set up shinyalert
                                                            column(6,div(br(),actionButton("ur_info", "Info",
                                                                                                               icon("lightbulb"),
                                                                                                               style="color: #fff; background-color: #008000; border-color: #2e6da4")))),
                                                   numericInput("ur_day_count", "How many over-reportings across all fleet do you think happen per day?", value = 70),
                                                   numericInput("ur_day_vol", "How many litres is over reported each instance?", value = 1.5),
                                                   tableOutput("underreported_calculations")
                                                 )),
                                          column(width = 4,
                                                 fluidRow(
                                                   fluidRow(column(6,h3('HEMM Fuel Tank Theft')),
                                                            # useShinyalert(),  # Set up shinyalert
                                                            column(6,div(br(),actionButton("theft_info","Info",
                                                                                                               icon("lightbulb"),
                                                                                                               style="color: #fff; background-color: #008000; border-color: #2e6da4")))),
                                                   numericInput("tank_steals_monthly","How many thefts do you think happen from HEMM fuel tank/monthly?",value=40),
                                                   numericInput("bowser_theft_vol", "How many litres of fuel do you think is stolen each instance?", value = 30),
                                                   tableOutput("stolen_assumption")
                                                 )),
                                          column(width=4,plotlyOutput("pilferage_hist")))
                          ),
                          column(3,
                                 h4("Fuel Savings:"),
                                 h5("Fuel Savings (Litres)"),
                                 verbatimTextOutput("pilferage_explanation"),
                                 h5("Fuel Savings (₹)"),
                                 verbatimTextOutput("pilferage_cost"),
                                 h4("How you can acheive savings with MindShift:"),
                                 tags$ul(
                                   tags$li(
                                     h5("Real-Time Monitoring:"),
                                     p("Continuous tracking of fuel levels and usage through IoT-enabled sensors and analytics platforms allows for real-time monitoring. Any irregularities can be flagged immediately, enabling prompt action.")
                                   ),
                                   tags$li(
                                     h5("Predictive Analytics:"),
                                     p("Mindshift Analytics can forecast potential pilferage scenarios based on historical data and usage trends. This proactive approach helps in identifying and mitigating risks before they result in significant losses.")
                                   ),
                                   tags$li(
                                     h5("Detailed Reporting:"),
                                     p("Comprehensive reporting tools provide detailed insights into fuel consumption patterns, helping in identifying specific areas or operations prone to pilferage.")
                                   ),
                                 )
                                 )
                        )
                      ),
                      bsModal("pilferage_info",title="Pilferage Information","pilferage_info_button",size="large",
                              h4("Understanding Pilferage in Fuel Intensive Industries"),
                              p("Pilferage refers to the theft or misappropriation of small quantities of goods or materials, which, in the context of industries utilizing large amounts of fuel daily, can aggregate into significant losses. This unauthorized siphoning off of fuel, though seemingly minor in isolated instances, can cumulatively impact operational efficiency and financial health."),
                              tags$ul(
                                tags$li(
                                  h3("Financial Losses:"),
                                  p("Unmonitored pilferage can lead to substantial financial losses over time. Even a small percentage of daily fuel usage, if stolen consistently, can result in a significant annual deficit.\n"),
                                  p("For instance, in an industry consuming thousands of liters of fuel per day, a mere 1% pilferage can translate into thousands of dollars in losses annually.")
                                ),
                                tags$li(
                                  h3("Operational Inefficiencies:"),
                                  p("Fuel pilferage can disrupt the smooth operation of machinery and vehicles, leading to unanticipated downtime.\n"),
                                  p("This can hamper productivity and increase maintenance costs due to inconsistent fuel availability.")
                                ),
                                tags$li(
                                  h3("Inventory Discrepancies:"),
                                  p("Regular pilferage creates discrepancies in fuel inventory records, complicating audits and inventory management processes."),
                                  p("This can lead to further inefficiencies and potential inaccuracies in financial reporting.")
                                ),
                              )
                              )
             ),



             # IDLING Tab

             tabPanel("Idling",
                      fluidRow(column(9,h3("Monitoring Excessive Idling")),column(3,div(class="right-align",br(),actionButton("idling_info_button", "Info", icon = icon("info-circle"))))),
                      sidebarLayout(
                        sidebarPanel(width=6,fluidRow(column(6,
                                                       numericInput("idle_usage_per","Effective utilization percentage?",min=-10,max=100,value=60)),
                                                column(6,numericInput("idle_load_perc","Loaded state percentage during utilisation",min=-10,max=99,value=70))),
                                     fluidRow(column(6,
                                                     h5("Total Time in Consideration"),
                                                     verbatimTextOutput("idle_total_time")),
                                              column(6,
                                                     h5("Idle time percentage during utilization"),
                                                     verbatimTextOutput("idle_on_perc"))),
                                     fluidRow(column(6,
                                                     numericInput("idle_loaded_lph","Loaded State LPH",value=16)),
                                              column(6,
                                                     numericInput("idle_on_lph","Idling State LPH",value=8))),
                                     br(),
                                     fluidRow(column(8,plotlyOutput("idling_plot")),
                                              column(4,
                                                     h4("Graphical Understanding:"),
                                                     p("In the realm of heavy equipment and machinery management,
                                                                     the initial bar denotes the daily fuel consumption per Heavy Earth Moving Machinery (HEMM).
                                                                     However, after investing in MindShift Analytics,
                                                                     one gains the capability to meticulously track and mitigate idle durations,
                                                                     consequently reducing consumption metrics and enhancing operational efficiency.")))),
                        mainPanel(width=6,fluidPage(column(8,fluidRow(column(6,h5("Total Utilisation Hours"),
                                                                             verbatimTextOutput("idle_util_hours")),
                                                                      column(6,h5("Total Off Hours"),
                                                                             verbatimTextOutput("idle_off_hours"))),
                                                           fluidRow(column(6,
                                                                           h5("Current Idling Hours"),
                                                                           verbatimTextOutput("idle_idling_working_hours")),
                                                                    column(6,
                                                                           h5("Current Loaded Hours"),
                                                                           verbatimTextOutput("idle_loading_working_hours"))),
                                                           fluidRow(column(8,h5("Meticulous tracking and mitigation of idle durations,
                                                                                can consequently reduce consumption.")),
                                                                    column(4,numericInput("idle_mod_on_val","Alter Idling Hours",value=1,min=0.1))),
                                                           fluidRow(tableOutput("idle_comparision_table")
                                                                    ),
                                                           fluidRow(h5("Consumption DIfference per HEMM"),verbatimTextOutput("idle_single_diff"))
                                                           ),
                                                    column(4,fluidRow(column(12,h5("Yearly Savings:"),
                                                                             verbatimTextOutput("idle_yearly_value"))),
                                                           fluidRow(column(12,
                                                                             h3("How you can achieve savings with MindShift:"),
                                                                             p("A reduction of even 1 liter per HEMM in fuel consumption, when applied to all HEMM over 365 days, results in a substantial savings.")
                                                                             )
                                                                      )
                                                           )
                                                    )
                                  )
                        ),
                      bsModal("idle_info_modal", "Idling Information:", "idling_info_button", size = "large",
                              tags$ul(
                                tags$li(
                                  h3("Shift Utilization Analysis:"),
                                  p("During shifts, the machine operates for a certain percentage of the total time, while the remaining time is categorized as off time."),
                                ),
                                tags$li(
                                  h3("Utilization Breakdown:"),
                                  p("The utilization time is further divided into idle time and loaded time."),
                                ),
                                tags$li(
                                  h3("Idle State:"),
                                  p("In this state, the machine is not actively working but remains ready for operation, consuming fuel while idling.")
                                ),
                                tags$li(
                                  h3("Loaded State:"),
                                  p("In this state, the machine is actively working, consuming the maximum amount of fuel.")
                                ),
                                tags$li(
                                  h3("Fuel Consumption Reduction"),
                                  p("Reducing idle time by increasing off time can significantly decrease overall fuel consumption.")
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
                        h1("Overall Savings"),
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
                                          numericInput("manpower_reduction_dispatcher","Predicted Reduction in Fuel Dispatchers:",value=0),
                                          numericInput("manpower_reduction_logger","Predicted Reduction in Fuel Loggers:",value=0),
                                          numericInput("manpower_reduction_dte","Predicted Reduction in Data Entry Operators:",value=0),
                                          numericInput("manpower_reduction_accountant","Predicted Reduction in Accountants:",value=1)
                                        )
                        ),
                        # column(4,
                        #        fluidRow(
                        #          h3("Pilferage Section"),
                        #          # numericInput("idle_off","Off duty cycle",value=20)
                        #        )
                        # ),
                        # column(4,
                        #        fluidRow(
                        #          h3("Movement Section"),
                        #          # p("add assumptions here")
                        #        ))
                        )
                      )
             ),
  )
)
)
