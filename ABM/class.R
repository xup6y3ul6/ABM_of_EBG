# load library
library(tidyverse)
library(R6)

# Market Class
Market <- R6Class("Market",
  public = list(
    ## Property
    total = NA,     #總回合數
    price = NULL,   #價格
    dprice = NULL,  #價格波動
    trial = 1,      #當前回合
    mrow = NA,      #P1抉擇
    mcol = NA,      #P2抉擇
    change = NA,    #漲跌幅
    board = matrix(c(0.05, 0.03, 0, 0.03, 0, -0.03, 0, -0.03, -0.05), nrow = 3, ncol = 3),  #漲跌幅陣列
    
    ## Method
    # 初始化變項
    initialize = function(total){
      stopifnot(is.numeric(total), length(total) == 1)
      self$total <- total
      self$price <- vector("numeric", self$total+1)
      self$dprice <- vector("numeric", self$total+1)
      self$price[1] <- 100
      self$dprice[1] <- 0
    },
    # 市場情況
    condition = function(con){
      self$board = switch (con,
        Balance={matrix(c(0.05, 0.03, 0, 0.03, 0, -0.03, 0, -0.03, -0.05), nrow = 3, ncol = 3)},
        Bubble={matrix(c(0.10, 0.06, 0, 0.06, 0, -0.03, 0, -0.03, -0.05), nrow = 3, ncol = 3)},
        Burst={matrix(c(0.05, 0.03, 0, 0.03, 0, -0.06, 0, -0.06, -0.10), nrow = 3, ncol = 3)})
    },
    # 遊戲進行
    game = function(p1act, p2act){
      self$mrow = switch(p1act, B={1}, N={2}, S={3})
      self$mcol = switch(p2act, B={1}, N={2}, S={3})
      self$change = self$board[self$mrow, self$mcol]
      self$dprice[self$trial+1] = self$price[self$trial] * self$change
      self$price[self$trial+1] = self$price[self$trial] + self$dprice[self$trial+1]
      self$trial = self$trial + 1
    },
  
    lock_objects = F
  )
)

#Player Class
Player <- R6Class("Player",
  public = list(
    ## Property
    type = NULL,     #玩家類型
    cash = NULL,     #現金
    stock = NULL,    #股票
    value = NULL,    #股票價值
    asset = NULL,    #總資產
    decision = NULL, #決策
    total = NA,      #總回合數
    prob = NULL,     #機率
    change = NA,     #隨回合變動機率
    
    ## Method
    # 初始化變項
    initialize = function(cash,stock,type,total){
      stopifnot(is.numeric(cash), length(cash) == 1)
      stopifnot(is.numeric(stock), length(stock) == 1)
      stopifnot(is.numeric(total), length(total) == 1)
      stopifnot(is.character(type), length(type) == 1)
      self$cash = vector("numeric", total)
      self$stock = vector("numeric", total)
      self$value = vector("numeric", total)
      self$asset = vector("numeric", total)
      self$decision = vector("character", total)
      self$type = type
      self$cash[1] = cash
      self$stock[1] = stock
      self$value[1] = stock * 100
      self$asset[1] = cash + stock * 100
      self$decision[101] = "O"
      self$decision[101] = "O"
    },

    # 決策
    decide = function(Market){
      # 價格波動變化
      if(Market$trial > 60){
        self$change = 1.05^(Market$trial - 60)
      } else {self$change = 1}
      if(Market$dprice[Market$trial] > 0){
        Prob = switch(self$type,
              Herd={rmultinom(1, size = 1, prob = c(0.7,0.2,0.1*self$change))},
              Inversive={rmultinom(1, size = 1, prob = c(0.1,0.2,0.7*self$change))},
              Hedge={rmultinom(1, size = 1, prob = c(0.05,0.3,0.65*self$change))}) 
      } else if(Market$dprice[Market$trial] == 0){
        Prob = switch(self$type,
              Herd={rmultinom(1, size = 1, prob = c(1/3,1/3,1/3*self$change))},
              Inversive={rmultinom(1, size = 1, prob = c(1/3,1/3,1/3*self$change))},
              Hedge={rmultinom(1, size = 1, prob = c(0.45,0.45,0.1*self$change))}) 
      } else{
        Prob = switch(self$type,
              Herd={rmultinom(1, size = 1, prob = c(0.1,0.2,0.7*self$change))},
              Inversive={rmultinom(1, size = 1, prob = c(0.7,0.2,0.1*self$change))},
              Hedge={rmultinom(1, size = 1, prob = c(0.35,0.5,0.15*self$change))}) 
      }
      # 判斷變動
      if(Prob[1,1]){
        if(self$cash[Market$trial] > Market$price[Market$trial]){
          self$decision[Market$trial] = "B"
          self$cash[Market$trial+1] = self$cash[Market$trial] - Market$price[Market$trial]
          self$stock[Market$trial+1] = self$stock[Market$trial] + 1
        } else {
          self$decision[Market$trial] = "N"
          self$cash[Market$trial+1] = self$cash[Market$trial]
          self$stock[Market$trial+1] = self$stock[Market$trial]
        }
      }
      else if(Prob[2,1]){
        self$decision[Market$trial] = "N"
        self$cash[Market$trial+1] = self$cash[Market$trial]
        self$stock[Market$trial+1] = self$stock[Market$trial]
      }
      else {
        if(self$stock[Market$trial] > 0){
          self$decision[Market$trial] = "S"
          self$cash[Market$trial+1] = self$cash[Market$trial] + Market$price[Market$trial]
          self$stock[Market$trial+1] = self$stock[Market$trial] - 1}
        else {
          self$decision[Market$trial] = "N"
          self$cash[Market$trial+1] = self$cash[Market$trial]
          self$stock[Market$trial+1] = self$stock[Market$trial]
        }
      }
    },

    # 結算
    ending = function(Market){
      self$value[Market$trial] = self$stock[Market$trial] * Market$price[Market$trial]
      self$asset[Market$trial] = self$cash[Market$trial] + self$value[Market$trial]
    },  
    
    lock_objects = F
  )
)

Game <- R6Class("Game",
  public = list(
    ## Property
    times = NA,
    p1type = NULL,
    p2type = NULL,
    type = c("Hedge", "Inversive", "Herd"),
    game = NULL,
    P1 = NULL,
    P2 = NULL,
    win = NULL,     #加總贏的dataframe
    winlst = NULL,  #每一次的win統計
    thing = c("Price", "Dprice", "P1Cash", "P2Cash", "P1Stock", "P2Stock", "P1Value", "P2Value", "P1Asset", "P2Asset", "P1Decision", "P2Decision"),
    market = Market$new(total=100),
    simulation_data = list(),
    
    ## Method
    initialize = function(P1type, P2type){
      # stopifnot(is.numeric(Times), length(Times) == 1)
      stopifnot(is.character(P1type), length(P1type) == 1)
      stopifnot(is.character(P2type), length(P2type) == 1)
      # self$times = Times
      self$p1type = P1type
      self$p2type = P2type
    },
    playing = function(x){
      # print(x)
      self$P1 <- Player$new(10000,10,self$p1type,100)
      self$P2 <- Player$new(10000,10,self$p2type,100)  
      self$market <- Market$new(total=100)                    
      for (i in 1:self$market$total) {
        self$P1$decide(self$market)
        self$P2$decide(self$market)
        if(i <= 20){
          self$market$condition("Balance")
        } else if (i <= 60){
          self$market$condition("Bubble")
        } else {
          self$market$condition("Burst")
        }
        self$market$game(self$P1$decision[i],self$P2$decision[i])
        self$P1$ending(self$market)
        self$P2$ending(self$market)
      }
      data <- list(times = x, trials = 1:101,
                   price = self$market$price, deltaPrice = self$market$dprice, 
                   p1_cash = self$P1$cash, p2_cash = self$P2$cash,
                   p1_stock = self$P1$stock, p2_stock = self$P2$stock,
                   p1_value = self$P1$value, p2_value = self$P2$value,
                   p1_asset = self$P1$asset, p2_asset = self$P2$asset,
                   p1_decision = self$P1$decision, p2_decision = self$P2$decision)
      return(data)
    },
    
    simulate = function(sim_times){
      stopifnot(is.numeric(sim_times), length(sim_times) == 1)
      self$simulation_data <- sapply(1:sim_times, self$playing, simplify = FALSE, USE.NAMES = TRUE)
    }
    
    # csv1 = function(){
    #   for (i in 1:3) {
    #     for (j in 1:i) {
    #       self$p1type = self$type[i]
    #       self$p2type = self$type[j]
    #       self$game <- as.data.frame(self$playing(1))
    #       colnames(self$game) <- self$thing
    #       write_csv(self$game, paste0(self$p1type,"_",self$p2type,".csv"))
    #     }
    #   }
    # },
    # csvwin = function(){
    #   a = 0   #第幾個
    #   self$win <- data.frame()
    #   for (i in 1:3) {
    #     for (j in 1:i) {
    #       a = a + 1 
    #       self$winlst <- c()
    #       self$p1type = self$type[i]
    #       self$p1type = self$type[j]
    #       for (k in 1:self$times) {
    #         self$game <- self$playing(1)
    #         if(self$game[[3]][100] > self$game[[4]][100]){
    #           self$winlst <- append(self$winlst, 1)
    #         } else if(self$game[[3]][100] < self$game[[4]][100]){
    #           self$winlst <- append(self$winlst, 2)
    #         } else{
    #           self$winlst <- append(self$winlst, 0)
    #         }
    #       }
    #       self$win[1,a] <- sum(self$winlst==1)
    #       self$win[2,a] <- sum(self$winlst==2)
    #     }
    #   }
    #   colnames(self$win) <- c("Hedge_Hedge", "Inversive_Hedge", "Inversive_Inversive", "Herd_Hedge", "Herd_Inversive", "Herd_Herd")
    #   write_csv(self$win, "win.csv")
    # },
    # plotting = function(){
    #   self$game <- lapply(1:self$times, self$playing)
    #   for (i in 1:self$times) {
    #     self$game <- as.data.frame(self$playing(1))
    #     colnames(self$game) <- self$thing
    #     self$win
    #     self$pic <- sapply(self$game, "[[", i) %>% 
    #       data.frame() %>% 
    #       add_column(trial = 1:101) %>% 
    #       pivot_longer(-trial, names_to = "sim_times", values_to = "value") %>% 
    #       ggplot(aes(x = trial, y = value, group = sim_times)) +
    #       geom_line(color = "skyblue") +
    #       labs(x = paste(self$p1type,self$p2type,sep="_")) +
    #       labs(y = self$thing[i]) +
    #       theme_classic()
    #   }
    #   self$pic
    #   # grid.arrange(self$pic[1:1])
    # }
  )
)
