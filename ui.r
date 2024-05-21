library(shiny)
library(bslib)
library(shinydashboard)
library(DT)
library(plotly)
library(ggplot2)
library(shinyalert)

ui <- shinyUI(fluidPage(

  titlePanel("Return Of Investment Calculator"),


  h2("Universal Values"),
  fluidRow(column(width=2,
                  numericInput("shift_count","Number of shifts",value = 2)
  ),
  column(width=2,
         numericInput("hemm_count","Number of HEMM",value=100)
  ),
  column(width=2,
         numericInput("truck_count","Number of diesel bowsers/site",value = 7,min=0,max=10),
  ),
  column(width=2,
         numericInput("hemm_daily_consump","HEMM Fuel Consumption/Day",value=100)
  ),
  column(width=2,
         numericInput("sfs_count","Number of Fuel Stations",value=2),
  ),
  column(width=2,
         numericInput("fuel_entry_count", "Number of refuels-entries/all Hemm/day:",value = 200),
  )
  ),


  navbarPage("Menu",

             # MANPOWER TAB

             tabPanel("Manpower Calculation",
                      h1("Enter Work Parameters"),
                      sidebarLayout(
                        sidebarPanel(
                          numericInput("logger_count_per_bowser","Number of fuel data recorders/bowser",value=1),
                          h5("Total numbers of fuel dispatch loggers: bowser count * logger per bowser"),
                          verbatimTextOutput("logger_count",TRUE),
                          br(),
                          h5("Entries per year: (entries/day * 365)"),
                          verbatimTextOutput("entries_per_year",TRUE),
                          numericInput("error_margin","Enter % of erroneous entries",value=5),
                          br(),
                          # h6("Assuming that 5% of the entries made by the operator will be erroneous and hence will require correction."),
                          # h6("5 mins per entry ~ 3 mins for entry and 2 mins for correction"),
                          actionButton("dto_count_info", "Info",
                                       icon("lightbulb"),
                                       style="color: #fff; background-color: #008000; border-color: #2e6da4"),
                          h4("Number of Data Entry Operators required per year."),
                          verbatimTextOutput("data_entry_count",TRUE),
                          br(),
                          numericInput("coordinator_count","Number of fuel dispatch coordinators per shift",value=1),
                          numericInput("accountant_count","Number of FTE for data aggregation(at month end): ",value=2),
                          br(),
                          sliderInput("fuel_logger_cost","Avg cost of fuel logger: ",value=150000,min=100000,max=300000),
                          sliderInput("fuel_dispatcher_cost","Avg cost of fuel dispatch coordinator: ",value=500000,min=100000,max=1000000),
                          sliderInput("data_entry_emp","Avg cost of data entry FTE: ",value=300000,min=100000,max=500000),
                          sliderInput("accountant_cost","Avg cost of accountant FTE: ",value=300000,min=100000,max=800000),
                        ),
                        mainPanel(
                          fluidPage(
                            column(8,
                                   fluidRow(
                                     splitLayout(
                                       tableOutput("manpower_data"),
                                       tableOutput("manpower_data_2")
                                     )
                                   ),
                                   fluidRow(
                                     plotlyOutput("histogram"),
                                   ),
                                   fluidRow(
                                     column(3,numericInput("manpower_save_accounts","% Saving in Accountants",value=5)),
                                     column(3,numericInput("manpower_save_dto","% Saving in Data Entry Operators",value=5)),
                                     column(3,numericInput("manpower_save_fdc","% Saving in Fuel Dispatcher",value=5)),
                                     column(3,numericInput("manpower_save_fdl","% Saving in Fuel Data Logger",value=5))
                                   ),
                                   fluidRow(
                                     plotOutput("pieChart")
                                   ),
                            ),
                            column(4,
                                   h3("Assumptions"),
                                   p("For a data entry full time employee(FTE): woking 8 hour shift, 5 hours of productivity is considered in calculation of data entry operators."),
                                   br(),
                                   br(),
                                   p("Data Aggregator/Compiler FTE: is estimated at 5 LPA * number of compilers required to meet requirements in a year."),
                                   br(),
                                   br(),
                                   p("Cost of Correction: Calculating from the provided margin of errors, number of additional working hours are estimated and number of data entry operators required are calculated")
                            ),
                          )
                        )
                      )
             ),

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
                          column(width = 9,
                                 column(width = 4,
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
                                        fluidRow(
                                          numericInput("pilferage_save_ur","% Savings from Over and Under reporting:",value=10)
                                        )
                                 ),
                                 column(width = 4,
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
                                        fluidRow(numericInput("pilferage_save_theft","% Savings from HEMM Tank Theft:",value=10)
                                        )
                                 ),
                                 column(width = 4,
                                        # fluidRow(
                                        #   h3('IDLE Time Loss'),
                                        #   actionButton("idle_info","Info",
                                        #                icon("lightbulb"),
                                        #                style="color: #fff; background-color: #008000; border-color: #2e6da4"),
                                        #   numericInput("idle_on_lph","Idle ON Lires/Hr",value=8),
                                        #   fluidRow(
                                        #     column(6,
                                        #            numericInput("idle_assumed_on","% Assumed Data Cycle",value=30)
                                        #     ),
                                        #     column(6,
                                        #            numericInput("idle_disc_on","% Discovered Data Cycle",value=40)
                                        #     )
                                        #   ),
                                        #   numericInput("idle_loaded_lph","Idle Loaded Lires/Hr",value=8),
                                        #   fluidRow(
                                        #     column(6,
                                        #            numericInput("idle_assumed_loaded", "% Assumed Data Cycle", value = 50)
                                        #     ),
                                        #     column(6,
                                        #            numericInput("idle_disc_loaded", "% Discovered Data Cycle", value = 40)
                                        #     )
                                        #   )
                                        # )

                                        # fluidRow(
                                        #   tableOutput("idle_calculations")
                                        # ),
                                        # fluidRow(numericInput("idle_save_theft","% Savings IDLE monitoring:",value=10))
                                 )
                          ),
                          column(width = 3,
                                 fluidRow(plotlyOutput("pilferage_hist")),
                                 br(),
                                 fluidRow(tableOutput("idle_table"))
                          )
                          # column(width=1,
                          #        h1("video space"))
                          # )
                        ),
                        fluidRow(h1("Overall Savings"),
                                 column(width=6,h3("Litres of savings in fuel"),
                                        verbatimTextOutput("pilferage_explanation")),
                                 column(width=6,h3("Accounts for:"),
                                        verbatimTextOutput("pilferage_cost"))
                        )
                      )
             ),



             # IDLING Tab

             tabPanel("Idling",fluidPage(column(6,
                                                fluidPage(
                                                  fluidRow(h1("Current State")),
                                                  fluidRow(column(4,
                                                                  numericInput("idle_usage_per","% Machinery Utilisation",min=-10,max=100,value=60)),
                                                           column(4,
                                                                  numericInput("idle_load_perc","% Time Loaded State",min=-10,max=100,value=50)),
                                                           column(4,
                                                                  numericInput("idle_on_perc","% Time Idling State",min=-10,max=100,value=30))),

                                                  fluidRow(column(4,
                                                                  h5("Idling Hours"),
                                                                  verbatimTextOutput("idle_idling_working_hours")),
                                                           column(4,
                                                                  h5("Loading Hours"),
                                                                  verbatimTextOutput("idle_loading_working_hours")),
                                                           column(4,
                                                                  h5("Off Hours"),
                                                                  verbatimTextOutput("idle_off_working_hours"))),

                                                  fluidRow(column(6,
                                                                  numericInput("idle_on_lph","Idling Lires/Hr",value=8)),
                                                           column(6,
                                                                  numericInput("idle_loaded_lph","Loaded Lires/Hr",value=16))),
                                                  fluidRow(column(12,
                                                                  h4("Current Consumption:"))),
                                                                  h5("Litres Consumed/Day/HEMM (Litres)"),
                                                                  verbatimTextOutput("idle_consump_lpd"),
                                                  fluidRow(column(12,
                                                                  h5("Litres Consumed/Day/All HEMM (Litres)"),
                                                                  verbatimTextOutput("idle_all_consump_lpd"))),
                                                  fluidRow(column(12,h4("A possible scenario to increase savings:"),p("Monitoring of off and idling hours can help instruct the reponsible to reduce idling by increasing off time of the machinery resulting in fuel savings.")))
                                                  )
                                                ),
                                         column(6,
                                                fluidPage(
                                                  fluidRow(h1("Future State")),
                                                  fluidRow(column(6,numericInput("idle_mod_on_val","New Idling Hours",value=1)),
                                                           column(6,
                                                                  div(h5("New Off Hours"),verbatimTextOutput("idle_mod_off_val")))),
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
                                                  fluidRow(column(12,plotlyOutput("idling_plot")))
                                                  )
                                         ))
                      ),




             # MOVEMENT TAB

             tabPanel("Movement Statistics",
                      fluidPage(
                        fluidRow(
                          column(6,h1("Movable Vehicle Summary/HEMM")),
                          column(6,fluidRow(
                            h4("Refuels/HEMM//month"),
                            verbatimTextOutput("ref_per_month")))
                        ),
                        fluidRow(
                          column(3,numericInput("movable_hemm_count","Number of movable Hemm",value=50)),
                          column(width=3,numericInput("movable_percent_get","% of refuellings from SFS",value=20)),
                          column(width=3,numericInput("movable_get_time","Time Spent in each trip",value=1)),
                          column(width=3,numericInput("movable_hemm_price","Enter price of HEMM/hour",value=1500)),

                        ),
                        fluidRow(column(width=6,
                                        plotlyOutput("movable_visualisation")),
                                 fluidRow(column(width=3,
                                                 h3("Number of refuels/annually"),
                                                 verbatimTextOutput("movable_refuel_sumannual")),
                                          column(width=3,
                                                 h3("Total self refeulling time"),
                                                 verbatimTextOutput("movable_time_spent")),
                                          fluidRow(
                                            column(width=5,
                                                   h3("Annual opportunity cost of 'lost hours' ₹1,500 / hr for single heavy machinery"),
                                                   verbatimTextOutput("movale_money_loss_hours")),
                                            column(width=5,
                                                   h3("Annual opportunity cost of 'lost hours' ₹1,500 / hr for all heavy machinery"),
                                                   verbatimTextOutput("movale_annual_money_loss_hours"))
                                          )
                                 )
                        ),
                      )
             ),

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
                                          numericInput("correction_time","Time taken for erroneous entry correction",value=10)
                                        )
                        ),
                        column(4,
                               fluidRow(
                                 h3("Pilferage Section"),
                                 numericInput("idle_off","Off duty cycle",value=20)
                               )
                        ),
                        column(4,
                               fluidRow(
                                 h3("Movement Section"),
                                 p("add assumptions here")
                               ))
                        )
                      )
             ),
  )
)
)
