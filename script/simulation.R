setwd("./GitHub/ABM_of_EBG/script")
source('class1.R', echo=TRUE)     # source

Playgame <- function(times, P1type, P2type){
  market <- Market$new(total=100)
  P1 <- Player$new(10000,10,P1type,100)   #P1
  P2 <- Player$new(10000,10,P2type,100)   #P2
  for (i in 1:market$total) {
    P1$decide(market)
    P2$decide(market)
    if(i <= 20){
      market$condition("Balance")
    } else if (i <= 60){
      market$condition("Bubble")
    } else {
      market$condition("Burst")
    }
    market$game(P1$decision[i],P2$decision[i])
    P1$ending(market)
    P2$ending(market)
  }
  data <- list(
    market$price,
    market$dprice,
    P1$cash,
    P1$value,
    P1$asset,
    P1$decision,
    P2$cash,
    P2$value,
    P2$asset,
    P2$decision
  )
  return(data)
}

Hedge_Hedge <- lapply(1:100, FUN = Playgame, P1type = "Hedge", P2type = "Hedge")

Price <- sapply(Hedge_Hedge, "[[", 1) %>% 
  data.frame() %>% 
  add_column(trial = 1:101) %>% 
  pivot_longer(-trial, names_to = "sim_times", values_to = "value")
  
g <- ggplot(Price, aes(x = trial, y = value, group = sim_times)) +
  geom_line(color = "grey") +
  theme_classic()



library(plotly)
ggplotly(g)


# 
cash <- data.frame(rep(1:101),P1$cash, P2$cash)
asset <- data.frame(rep(1:101),P1$asset, P2$asset)
ggplot(cash) +
  geom_line(aes(rep.1.101.,P1$cash), color = "red") +
  geom_line(aes(rep.1.101.,P2$cash), color = "blue")
ggplot(asset) +
  geom_line(aes(rep.1.101.,P1$asset), color = "red") +
  geom_line(aes(rep.1.101.,P2$asset), color = "blue")

P1$decision
P1$cash
P1$stock
P2$decision
P2$cash
P2$stock
Market$price
