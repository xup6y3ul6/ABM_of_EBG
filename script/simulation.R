source('class1.R', echo=TRUE)

Market <- Market$new(total = 100)
P1 <- Player$new(10000,10,"Herd",100)
P2 <- Player$new(10000,10,"Hedge",100)
for (i in 1:Market$total) {
  P1$decide()
  P2$decide()
  if(i <= 20){
    Market$condition("Balance")
  } else if (i <= 60){
    Market$condition("Bubble")
  } else {
    Market$condition("Burst")
  }
  Market$game(P1$decision[i],P2$decision[i])
  P1$ending()
  P2$ending()
}
# 
P1$decision
P2$cash
P2$decision
P1$cash
Market$price
