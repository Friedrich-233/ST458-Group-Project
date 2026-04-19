library(putils)

num_symbols <- 100

# -----------------------------
# Utility functions
# -----------------------------

safe_rank <- function(x) {
  ok <- is.finite(x)
  out <- rep(0, length(x))
  if (sum(ok) <= 1) return(out)
  r <- rank(x[ok], ties.method = "average")
  out[ok] <- (r - 1) / (sum(ok) - 1) - 0.5
  out
}

clip <- function(x, lo, hi) {
  pmax(lo, pmin(hi, x))
}

# -----------------------------
# Initialise state
# -----------------------------

initialise_state <- function(data) {
  data$date <- as.Date(data$date)
  
  dates <- sort(unique(data$date))
  symbols <- sort(unique(data$symbol))
  n_symbols <- length(symbols)
  max_lookback <- 20
  
  # store last 20 close prices
  lagged_close <- matrix(NA_real_, nrow = max_lookback, ncol = n_symbols)
  colnames(lagged_close) <- symbols
  rownames(lagged_close) <- paste0("lag_", 1:max_lookback)
  
  for (i in 1:max_lookback) {
    d <- dates[length(dates) - i + 1]
    sub_df <- data[data$date == d, ]
    idx <- match(sub_df$symbol, symbols)
    lagged_close[i, idx] <- sub_df$close
  }
  
  positions <- setNames(rep(0, n_symbols), symbols)
  cash <- 1
  
  state <- list(
    symbols = symbols,
    n_symbols = n_symbols,
    max_lookback = max_lookback,
    lagged_close = lagged_close,
    positions = positions,
    cash = cash,
    
    # final tuned parameters
    gross_frac = 1.0,
    n_long = 20,
    n_short = 20,
    max_pos_frac = 0.06,
    smooth = 0.40,
    cost_rate = 0.0005
  )
  
  return(state)
}

# -----------------------------
# Trading algorithm
# -----------------------------

trading_algorithm <- function(new_data, state) {
  # explicit unpacking: safer than bunch(...) %=% state
  symbols <- state$symbols
  n_symbols <- state$n_symbols
  max_lookback <- state$max_lookback
  lagged_close <- state$lagged_close
  positions <- state$positions
  cash <- state$cash
  
  gross_frac <- state$gross_frac
  n_long <- state$n_long
  n_short <- state$n_short
  max_pos_frac <- state$max_pos_frac
  smooth <- state$smooth
  cost_rate <- state$cost_rate
  
  # ensure order aligned with symbols
  idx <- match(symbols, new_data$symbol)
  new_data <- new_data[idx, ]
  
  close_today <- new_data$close
  
  # ---------------------------------
  # 1. Mark old positions to market
  # ---------------------------------
  prev_close <- lagged_close[1, ]
  r1d <- close_today / prev_close - 1
  r1d[!is.finite(r1d)] <- 0
  
  positions <- positions * (1 + r1d)
  wealth <- cash + sum(positions)
  
  # if wealth invalid or dead, stop trading
  if (!is.finite(wealth) || wealth <= 0) {
    trades <- setNames(rep(0, n_symbols), symbols)
    
    lagged_close[2:max_lookback, ] <- lagged_close[1:(max_lookback - 1), ]
    lagged_close[1, ] <- close_today
    
    state$lagged_close <- lagged_close
    state$positions <- positions
    state$cash <- 0
    
    return(list(trades = trades, state = state))
  }
  
  # ---------------------------------
  # 2. Update rolling close history
  # ---------------------------------
  lagged_close[2:max_lookback, ] <- lagged_close[1:(max_lookback - 1), ]
  lagged_close[1, ] <- close_today
  
  # ---------------------------------
  # 3. Core features: 5d and 10d momentum
  # ---------------------------------
  mom5 <- lagged_close[1, ] / lagged_close[6, ] - 1
  mom10 <- lagged_close[1, ] / lagged_close[11, ] - 1
  
  s_mom5 <- safe_rank(mom5)
  s_mom10 <- safe_rank(mom10)
  
  score <- 0.5 * s_mom5 + 0.5 * s_mom10
  score[!is.finite(score)] <- 0
  
  # ---------------------------------
  # 4. Construct target positions
  # ---------------------------------
  ord_desc <- order(score, decreasing = TRUE)
  long_idx <- ord_desc[1:n_long]
  short_idx <- ord_desc[(n_symbols - n_short + 1):n_symbols]
  
  target_raw <- rep(0, n_symbols)
  target_raw[long_idx] <- 1 / n_long
  target_raw[short_idx] <- -1 / n_short
  
  gross_budget <- gross_frac * wealth
  target_positions_full <- gross_budget * target_raw
  
  max_abs_pos <- max_pos_frac * wealth
  target_positions_full <- clip(target_positions_full, -max_abs_pos, max_abs_pos)
  
  target_positions <- positions + smooth * (target_positions_full - positions)
  
  # ---------------------------------
  # 5. Compute trades and update state
  # ---------------------------------
  trades <- target_positions - positions
  trades <- as.numeric(trades)
  names(trades) <- symbols
  trades[!is.finite(trades)] <- 0
  
  trade_cost <- cost_rate * sum(abs(trades), na.rm = TRUE)
  cash <- cash - sum(trades, na.rm = TRUE) - trade_cost
  positions <- target_positions
  
  state$lagged_close <- lagged_close
  state$positions <- positions
  state$cash <- cash
  
  return(list(trades = trades, state = state))
}

