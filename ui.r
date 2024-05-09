library(shiny)
library(bslib)
library(shinydashboard)
library(DT)
library(plotly)
library(ggplot2)

ui <- shinyUI(fluidPage(

  titlePanel("Return Of Investment Calculator"),


  h2("Universal Values"),
  fluidRow(column(width=2,
                  numericInput("shift_count","Number of shifts",value = 2)
                  ),
           column(width=2,
                  numericInput("hemm_count","Number of HEMM",value=0)
           ),
           column(width=2,
                  numericInput("truck_count","Number of diesel bowsers/site",value = 7,min=0,max=10),
           ),
           column(width=2,
                  numericInput("hemm_daily_consump","HEMM Fuel Consumption/Day",value=0)
           ),
           column(width=2,
                  numericInput("sfs_count","Number of Fuel Stations",value=2),
           ),
           column(width=2,
                  numericInput("fuel_entry_count", "Number of refuels-entries/day:",value = 200),
                  )
  ),


  navbarPage("Menu",

             tabPanel("Manpower Calculation",
                          h1("Enter Work Parameters"),
                      sidebarLayout(
                        sidebarPanel(
                          numericInput("logger_count_per_bowser","Number of fuel data recorders/bowser",value=1),
                          "Total numbers of fuel dispatch loggers: bowser count * logger per bowser",
                          textOutput("logger_count"),
                          br(),
                          "Entries per year: (entries/day * 365)",
                          textOutput("entries_per_year"),
                          br(),
                          "Number of Data Entry Operators required per year can be calculated accordingly.",
                          numericInput("coordinator_count","Number of fuel dispatch coordinators per shift",value=1),
                          numericInput("error_margin","Error correction for manual data entry: ",value=5),
                          "Assuming that 5% of the entries made by the operator will be erroneous and hence will require correction.",
                          sliderInput("compilation_emps","Number of FTE for data aggregation(at month end): ",value=2,min=1,max=5),
                          # numericInput("hemm_fuel_trucks","Enter how many fuel trucks required per HEMM:",value=3),
                          # "HEMM stands for Heavy Earth Moving Machinery",
                          "Considering 3 minutes/entry, required data entry operators required per year:",
                          textOutput("data_entry_count"),
                          br(),
                          sliderInput("fuel_logger_cost","Avg cost of fuel logger: ",value=150000,min=100000,max=300000),
                          sliderInput("fuel_dispatcher_cost","Avg cost of fuel dispatch coordinator: ",value=500000,min=100000,max=1000000),
                          sliderInput("data_entry_emp","Avg cost of data entry FTE: ",value=300000,min=100000,max=500000),
                        ),
                        mainPanel(
                          fluidRow(
                            column(width = 6,
                                   DT::dataTableOutput("myTable")
                            ),
                            column(width=6,
                                   DT::dataTableOutput("myTable2"))
                          ),
                          fluidRow(
                            splitLayout(
                              tableOutput("manpower_data"),
                              tableOutput("manpower_data_2")
                            )
                          ),
                          fluidRow(
                            box(
                              title = "Calculation Briefing",    # Title of the card
                              status = "primary",                 # Color of the box
                              solidHeader = TRUE,                 # Solid color header
                              # collapsible = TRUE,                 # Allows the box to be collapsed
                              "For a data entry full time employee(FTE): woking 8 hour shift, 5 hours of productivity is considered in calculation of data entry operators",  # Content
                              br(),
                              br(),
                              "Data Aggregator/Compiler FTE: is estimated at 5 LPA * number of compilers required to meet requirements in a year",
                              br(),
                              br(),
                              "Cost of Correction: Calculating from the provided margin of errors, number of additional working hours are estimated and number of data entry operators required are calculated"
                            )
                          ),
                          fluidRow(
                            plotlyOutput("histogram"),
                          ),
                          fluidRow(
                            plotOutput("pieChart")
                          )



                          # tableOutput("manpower_data"),
                          # tableOutput("manpower_data_2"),
                          # br(),
                          # card(
                          #   card_header("Info:"),
                          #   "Salary of data entry operator: 3,00,000",
                          #   br(),
                          #   "For a full time employee(FTE), woking 8 hour shift, 5 hours of productivity is considered in calculation of data entry operators",
                          #   br(),
                          #   "Fuel coordinator is accounted by 'x' number of shifts * 'y' coordinators required, with salary estimate of 8,00,000"
                        )
                      )
             ),

             # tabPanel("Pilferage",
             #          sidebarLayout(
             #            sidebarPanel(
             #              numericInput("fuel_per_day","Litres of fuel consumed per day",value = 8000),
             #              br(),
             #              "Litres of fuel consumed/year",
             #              textOutput("fuel_per_year"),
             #              br(),
             #              # sliderInput("fuel_loss_margin","Percetage loss of fuel lost to pilferage: ",value=5,min=0,max=10),
             #              numericInput("fuel_write_off_monthly","Litres of fuel written off per month",value=1000),
             #              br(),
             #              "Litres of fuel written off/year",
             #              textOutput("fuel_write_off_yearly"),
             #              br(),
             #              numericInput("fuel_price","Enter the current price of fuel/ litre: ",value=86),
             #              "Assuming the current price of fuel to be 86"
             #            ),
             #            mainPanel(
             #              # Add output for part 2
             #              plotOutput("pilferage_data")
             #            )
             #          )
             # ),

             tabPanel("Pilferage",
                      fluidPage(

                        tabPanel("Consumption summary",
                                 # Add content for the Menu tab here
                                 fluidRow(
                                   column(width=12,
                                          h1("Scenarios"),
                                   ),
                                 ),
                                 fluidRow(
                                   column(width=6,
                                          h4("Average Fuel Consumption/Year:"),
                                          verbatimTextOutput("annual_fuel_consump")
                                   ),
                                   column(width=3,
                                          h4("Refuellings/HEMM/month"),
                                          verbatimTextOutput("refuels_per_month")),
                                   column(width=3,
                                          h4("Refuellings/HEMM/year"),
                                          verbatimTextOutput("refuels_per_year"))
                                 ),
                        ),
                        tabPanel("Consumption bifercated",
                                 # Add content for the second tab here
                                 fluidRow(column(width = 6,
                                                 column(width = 6,
                                                        fluidRow(
                                                          h3('Under Refueling and Over Reporting'),
                                                          numericInput("ur_day_count", "How many over-reportings across all fleet do you think happen per day?", value = 5),
                                                          numericInput("ur_day_vol", "How many litres is over reported each instance?", value = 100),
                                                        ),
                                                        fluidRow(
                                                          tableOutput("underreported_calculations")
                                                        ),
                                                 ),

                                                 column(width = 6,
                                                        fluidRow(
                                                          h3('HEMM Fuel Tank Theft'),
                                                          numericInput("tank_steals_monthly","How many thefts do you think happen from HEMM fuel tank/monthly?",value=10),
                                                          numericInput("bowser_fuel_sold_monthly", "How many litres of fuel do you think is stolen each instance?", value = 1000),
                                                          br()
                                                        ),
                                                        fluidRow(
                                                          tableOutput("stolen_assumption")
                                                        )
                                                 )
                                                 ),
                                          column(width = 6,
                                                 plotlyOutput("pilferage_hist")
                                                 )
                        ),
                        mainPanel(fluidRow(h1("Overall Savings"),
                                           column(width=6,h3("Litres of savings in fuel"),
                                                  verbatimTextOutput("pilferage_explanation")),
                                           column(width=6,h3("Accounts for:"),
                                                  verbatimTextOutput("pilferage_cost"))
                                           )
                        )
                      )
              )
      ),

      tabPanel("Movement Statistics",
               fluidPage(
                 fluidRow(
                   h1("Movable Vehicle Summary"),
                 ),
                 fluidRow(
                   column(width=2,numericInput("movable_refuels_day","Number of refuels/day",value=0)),
                   column(width=2,numericInput("movable_refuels_month","Number of refuels/month",value=0)),
                   column(width=2,numericInput("movable_percent_get","% of refuellings from SFS",value=0)),
                   column(width=2,numericInput("movable_get_time","Time Spent in each trip",value=1)),
                   column(width=2,numericInput("movable_hemm_price","Enter price of HEMM/hour",value=1500)),
                   # column(width=6,
                   #        fluidRow(numericInput("movable_refuels_day","Number of refuels/day",value=0),
                   #                 numericInput("movable_refuels_month","Number of refuels/month",value=0)),
                   #        fluidRow(h3("Number of refuels/annually"),
                   #                 verbatimTextOutput("movable_refuel_sumannual"))
                   #        ),
                   # column(width=6,
                   #        fluidRow(numericInput("movable_percent_get","Percentage of refuellings/total going to SFS",value=0),
                   #                 numericInput("movable_get_time","Time Spent in each trip",value=1),
                   #                 numericInput("movable_hemm_price","Enter price of HEMM/hour",value=1500)
                   #                 ),
                   #        fluidRow(h3("Overall time spent in refeulling"),
                   #                 verbatimTextOutput("movable_time_spent"))
                   #        )
                 ),
                 fluidRow(column(width=6,
                                 plotlyOutput("movable_visualisation")),
                          fluidRow(column(width=3,
                                          h3("Number of refuels/annually"),
                                          verbatimTextOutput("movable_refuel_sumannual")),
                                   column(width=3,
                                          h3("Total refeulling time"),
                                          verbatimTextOutput("movable_time_spent")),
                                   fluidRow(
                                     h3("Annual opportunity cost of 'lost hours' ₹1,500 / hr for heavy machinery"),
                                     verbatimTextOutput("movale_money_loss_hours"))),
                 )
               )

               )
  )
             # tabPanel("Tab 4",
             #          sidebarLayout(
             #            sidebarPanel(
             #              # Add sliders and numeric inputs for part 4
             #            ),
             #            mainPanel(
             #              # Add output for part 4
             #            )
             #          )
             # )
)
)
