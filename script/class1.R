# load library
library(tidyverse)
library(R6)

# Market Class
Market <- R6Class("Market",
          public = list(
            total = NA,     #?????????
            price = NULL,   #??????
            dprice = NULL,  #????????????
            trial = 1,      #???????????????
            mrow = NA,      #P1??????
            mcol = NA,      #P2??????
            change = NA,    #?????????
            board = matrix(c(0.05, 0.03, 0, 0.03, 0, -0.03, 0, -0.03, -0.05), nrow = 3, ncol = 3),  #??????????????? 
            # ???????????????
            initialize = function(total){
              stopifnot(is.numeric(total), length(total) == 1)
              self$total <- total
              self$price <- vector("numeric", self$total+1)
              self$dprice <- vector("numeric", self$total+1)
              self$price[1] <- 100
              self$dprice[1] <- 0
            },
            # ????????????
            condition = function(con){
              self$board = switch (con,
              Balance={matrix(c(0.05, 0.03, 0, 0.03, 0, -0.03, 0, -0.03, -0.05), nrow = 3, ncol = 3)},
              Bubble={matrix(c(0.10, 0.06, 0, 0.06, 0, -0.03, 0, -0.03, -0.05), nrow = 3, ncol = 3)},
              Burst={matrix(c(0.05, 0.03, 0, 0.03, 0, -0.06, 0, -0.06, -0.10), nrow = 3, ncol = 3)})
            },
            # ????????????
            game = function(p1act, p2act){
              self$mrow = switch(p1act, B={1}, N={2}, S={3})
              self$mcol = switch(p2act, B={1}, N={2}, S={3})
              self$change = self$board[self$mrow, self$mcol]
              self$dprice[self$trial+1] = self$price[self$trial] * self$change
              self$price[self$trial+1] = self$price[self$trial] + self$dprice[self$trial+1]
              self$trial = self$trial + 1
            },
            lock_objects = F
          ))

#Player Class
Player <- R6Class("Player",
          public = list(
            type = NULL,     #????????????
            cash = NULL,     #??????
            stock = NULL,    #??????
            value = NULL,    #????????????
            asset = NULL,    #?????????
            decision = NULL, #??????
            total = NA,      #????????????
            prob = NULL,     #??????
            # ???????????????
            initialize = function(cash,stock,type,total){
              stopifnot(is.numeric(cash), length(cash) == 1)
              stopifnot(is.numeric(stock), length(stock) == 1)
              stopifnot(is.character(type), length(type) == 1)
              stopifnot(is.numeric(total), length(total) == 1)
              
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
            },
            # ????????????
            decide = function(){
              # ????????????????????????
              if(Market$dprice[Market$trial] > 0){
                Prob = switch(self$type,
                      Herd={rmultinom(1, size = 1, prob = c(0.7,0.2,0.1))},
                      Inversive={rmultinom(1, size = 1, prob = c(0.1,0.2,0.7))},
                      Hedge={rmultinom(1, size = 1, prob = c(0.05,0.3,0.65))}) 
              } else if(Market$dprice[Market$trial] == 0){
                Prob = switch(self$type,
                      Herd={rmultinom(1, size = 1, prob = c(1/3,1/3,1/3))},
                      Inversive={rmultinom(1, size = 1, prob = c(1/3,1/3,1/3))},
                      Hedge={rmultinom(1, size = 1, prob = c(0.45,0.45,0.1))}) 
              } else{
                Prob = switch(self$type,
                      Herd={rmultinom(1, size = 1, prob = c(0.1,0.2,0.7))},
                      Inversive={rmultinom(1, size = 1, prob = c(0.7,0.2,0.1))},
                      Hedge={rmultinom(1, size = 1, prob = c(0.3,0.65,0.05))}) 
              }
              # ??????????????????
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
            # ????????????
            ending = function(){
              self$value[Market$trial] = self$stock[Market$trial-1] * Market$price[Market$trial-1]
              self$asset[Market$trial] = self$cash[Market$trial-1] + self$value[Market$trial-1]
          },  
            lock_objects = F
          ))
