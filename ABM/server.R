
# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    ## Simulation 
    simulation <- reactiveValues(data = NULL)
    
    observeEvent(input$run, {
        showNotification("Simulating ...", duration = NULL, id = "sim")
        game <- Game$new(input$p1_type, input$p2_type)
        game$simulate(input$sim_times)
        removeNotification(id = "sim")
        showNotification("DONE!", duration = 3)
        simulation$data <- game$simulation_data
    })
    
    ## Result
    ### Price plot
    price_data <- eventReactive(input$update,{
        .data <- lapply(simulation$data, function(x){
            x <- as_tibble(x) %>% 
                select(times, trials, price) %>% 
                filter(trials >= input$trial_range[1], trials <= input$trial_range[2])
        })
        return(.data)
    })
    
    output$price_plot <- renderPlot({
        g <- do.call(rbind, price_data()) %>% 
            ggplot(aes(x = trials, y = price, group = times)) +
            geom_line(alpha = I(0.75)) +
            theme_bw() 
        g
    })
    
    ### Player plot
    player_data <- eventReactive(input$update, {
        .data <- lapply(simulation$data, function(x){
            x <- as_tibble(x) %>% 
                select(times, trials, ends_with(input$plot_choice)) %>% 
                filter(trials >= input$trial_range[1], trials <= input$trial_range[2])
        })
        return(.data)
    })
    
    output$player_plot <- renderPlot({
        g <- do.call(rbind, player_data()) %>% 
            pivot_longer(cols = contains("_"), names_to = "player", values_to = "value") %>% 
            ggplot(aes(x = trials, y = value, group = times, color = player)) +
            geom_line(alpha = I(0.75)) +
            facet_grid(as.factor(player) ~ .) +
            theme_bw() +
            theme(legend.position = "bottom")
        g
    })
    
    ### win rate table
    winRate_data <- eventReactive(input$update, {
        win_list <- lapply(simulation$data, function(x){
            .x <- as_tibble(x) %>% 
                select(times, trials, ends_with("cash")) %>% 
                filter(trials == 101) %>% 
                mutate(delta_cash = p1_cash - p2_cash)
        })
    })
    
    output$winRate_table <- render_gt({
        do.call(rbind, winRate_data()) %>% 
            summarise(n = n(),
                      p1_win = sum(delta_cash > 0),
                      p1_tie = sum(delta_cash == 0), 
                      p1_loss = sum(delta_cash < 0),
                      p1_winRate = p1_win/n,
                      mean = mean(delta_cash), 
                      sd = sd(delta_cash)) %>% 
            gt() %>% 
            tab_header(title = "delta_cash = p1_cash - p2_cash")
    })
})
