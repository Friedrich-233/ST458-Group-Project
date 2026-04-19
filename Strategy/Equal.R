# Equal-weight buy-and-hold:第一天平均买入,之后不动
library(putils)
num_symbols <- 100

initialise_state <- function(data){
  list(done = FALSE)
}

trading_algorithm <- function(new_data, state){
  if (!state$done){
    trades <- rep(1 / num_symbols, num_symbols)   # 首日:每只买 1/N
    state$done <- TRUE
  } else {
    trades <- rep(0, num_symbols)                 # 之后:不交易
  }
  list(trades = trades, new_state = state)
}