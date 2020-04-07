library(tidyverse)
library(R6)

Market <- R6Class("Market",
          public = list(
            initialize = function(){
              trial = 0,
              price = vector("numeric", 101),
              dprice = vector("numeric", 101),
              board = matrix(c(1.05, 1.03, 1, 1.06, 1, 0.97, 1, 0.97, 0.94), nrow = 3, ncol = 3)
            },
            game = function(p1act, p2act){
              if
            },
          ))