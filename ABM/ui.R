source("global.R")

# Define UI for application that draws a histogram
shinyUI(fluidPage(

    # Application title
    titlePanel("Agent Based Modeling of EBG"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            h3("Simulation setting"),
            selectInput("p1_type", "Player1 type:", 
                        choices = type_choices, selected = "Herd"),
            selectInput("p2_type", "Player2 type:", 
                        choices = type_choices, selected = "Hedge"),
            sliderInput("sim_times", "Simulation times",
                        min = 1, max = 200, value = 50),
            actionButton("run", "RUN"),
            
            hr(),
            h3("Result"),
            sliderInput("trial_range", "Trial range", 
                        min = 1, max = 101, value = c(1, 101)),
            selectInput("plot_choice", "The alternatives of player's plot:",
                        choices = plot_choices),
            
            actionButton("update", "UPDATE")
        ),

        # Show a plot of the generated distribution
        mainPanel(
            plotOutput("price_plot"),
            plotOutput("player_plot"),
            gt_output("winRate_table")
        )
    )
))
