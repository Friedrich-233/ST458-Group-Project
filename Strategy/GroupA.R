library(putils)
library(data.table)
library(lightgbm)

# we first define the global parameters
num_symbols <- 100
train_length <- 252
lookahead <- 21
retrain_freq <- 21
n_long <- 10
n_short <- 10
warmup_days <- 200

lgb_params <- list(
  objective = "regression",
  num_leaves = 3,
  min_data_in_leaf = 100,
  learning_rate = 0.03,
  feature_fraction = 0.3,
  bagging_fraction = 0.6,
  bagging_freq = 1
)
lgb_nrounds <- 200

covariate_names <- c(
  "rsi", "bb_high", "bb_low", "MACD", "PPO", "MACD_hist",
  "price_sma5", "price_sma10", "price_sma20", "price_sma50", "price_sma200",
  "sma5_sma20", "sma10_sma50",
  "r01", "r05", "r10", "r21", "r42", "r63",
  "rvol_5", "rvol_10", "rvol_21", "rvol_63",
  "vol_ratio", "vol_trend", "log_dollar_vol", "oc_ret",
  "roc_5", "roc_21", "zscore_20", "zscore_50",
  "month", "weekday",
  "r01dec", "r05dec", "r10dec", "r21dec", "r42dec", "r63dec",
  "dollar_vol_rank"
) # feature vector

cat_features <- c("month", "weekday")

#features computation
compute_all_features <- function(close_mat, vol_mat, open_mat) {
  n <- nrow(close_mat)
  m <- ncol(close_mat)
  n_feat <- 30  
  
  feat_3d <- array(NA_real_, dim = c(n, m, n_feat)) # baseline feature
  
  a12 <- 2/13; a26 <- 2/27; a9 <- 2/10 # compute MACD, PPO, MACD_hist
  ema12 <- matrix(NA_real_, n, m); ema26 <- matrix(NA_real_, n, m)
  ema12[1, ] <- close_mat[1, ]; ema26[1, ] <- close_mat[1, ]
  for (i in 2:n) {
    ema12[i, ] <- ema12[i-1, ] + a12 * (close_mat[i, ] - ema12[i-1, ])
    ema26[i, ] <- ema26[i-1, ] + a26 * (close_mat[i, ] - ema26[i-1, ])
  }
  macd_line <- ema12 - ema26
  sig_ema <- matrix(NA_real_, n, m); sig_ema[1, ] <- macd_line[1, ]
  for (i in 2:n) sig_ema[i, ] <- sig_ema[i-1, ] + a9 * (macd_line[i, ] - sig_ema[i-1, ])
  
  feat_3d[, , 4] <- macd_line                                  # MACD
  ppo <- macd_line / ema26 * 100; ppo[ema26 == 0] <- 0
  feat_3d[, , 5] <- ppo                                        # PPO
  feat_3d[, , 6] <- macd_line - sig_ema                          # MACD_hist
  
  delta <- close_mat[-1, , drop = FALSE] - close_mat[-n, , drop = FALSE] # RSI 14
  gain  <- pmax(delta, 0); loss <- pmax(-delta, 0)
  ag_mat <- matrix(NA_real_, n, m); al_mat <- matrix(NA_real_, n, m)
  if (n >= 15) {
    ag_mat[15, ] <- colMeans(gain[1:14, , drop = FALSE], na.rm = TRUE)
    al_mat[15, ] <- colMeans(loss[1:14, , drop = FALSE], na.rm = TRUE)
    if (n >= 16) for (i in 16:n) {
      ag_mat[i, ] <- (ag_mat[i-1, ] * 13 + gain[i-1, ]) / 14
      al_mat[i, ] <- (al_mat[i-1, ] * 13 + loss[i-1, ]) / 14
    }
    feat_3d[15:n, , 1] <- 100 - 100 / (1 + ag_mat[15:n, , drop=FALSE] /
                                         pmax(al_mat[15:n, , drop=FALSE], 1e-10))
  }

  cs_c  <- apply(close_mat, 2, cumsum) #Rolling mean via cumsum
  cs_v  <- apply(vol_mat, 2, cumsum)
  cs0_c <- rbind(rep(0, m), cs_c)
  cs0_v <- rbind(rep(0, m), cs_v)
  
  rmean_mat <- function(cs0, k) {
    out <- matrix(NA_real_, n, m)
    if (n >= k) out[k:n, ] <- (cs0[(k+1):(n+1), , drop=FALSE] - cs0[1:(n-k+1), , drop=FALSE]) / k
    out
  }
  sma5 <- rmean_mat(cs0_c, 5); sma10 <- rmean_mat(cs0_c, 10)
  sma20 <- rmean_mat(cs0_c, 20); sma50 <- rmean_mat(cs0_c, 50)
  sma200 <- rmean_mat(cs0_c, 200)
  vsma5 <- rmean_mat(cs0_v, 5); vsma20 <- rmean_mat(cs0_v, 20)

  cs_c2 <- apply(close_mat^2, 2, cumsum)   # Rolling SD via cumsum of squares
  cs0_c2 <- rbind(rep(0, m), cs_c2)
  rsd_mat <- function(cs0, cs0_sq, k) {
    out <- matrix(NA_real_, n, m)
    if (n >= k) {
      sx  <- cs0[(k+1):(n+1),,drop=FALSE] - cs0[1:(n-k+1),,drop=FALSE]
      sx2 <- cs0_sq[(k+1):(n+1),,drop=FALSE] - cs0_sq[1:(n-k+1),,drop=FALSE]
      v <- (sx2 - sx^2/k) / (k-1)
      out[k:n, ] <- sqrt(pmax(v, 0))
    }
    out
  }
  sd20 <- rsd_mat(cs0_c, cs0_c2, 20); sd50 <- rsd_mat(cs0_c, cs0_c2, 50)

  feat_3d[, , 2] <- (close_mat - (sma20 + 2*sd20)) / close_mat   # bb_high
  feat_3d[, , 3] <- (close_mat - (sma20 - 2*sd20)) / close_mat   # bb_low
  feat_3d[, , 7]  <- (close_mat - sma5)   / close_mat   # price_sma5
  feat_3d[, , 8]  <- (close_mat - sma10)  / close_mat   # price_sma10
  feat_3d[, , 9]  <- (close_mat - sma20)  / close_mat   # price_sma20
  feat_3d[, , 10] <- (close_mat - sma50)  / close_mat   # price_sma50
  feat_3d[, , 11] <- (close_mat - sma200) / close_mat   # price_sma200
  feat_3d[, , 12] <- (sma5 - sma20)  / close_mat        # sma5_sma20
  feat_3d[, , 13] <- (sma10 - sma50) / close_mat        # sma10_sma50
  
  retk <- function(k) {
    out <- matrix(NA_real_, n, m)
    if (n > k) out[(k+1):n, ] <- close_mat[(k+1):n,,drop=FALSE] / close_mat[1:(n-k),,drop=FALSE] - 1
    out
  }
  feat_3d[, , 14] <- retk(1)    # r01
  feat_3d[, , 15] <- retk(5)    # r05
  feat_3d[, , 16] <- retk(10)   # r10
  feat_3d[, , 17] <- retk(21)   # r21
  feat_3d[, , 18] <- retk(42)   # r42
  feat_3d[, , 19] <- retk(63)   # r63

  log_ret <- matrix(0, n, m) # Realised volatility (rolling SD of log returns)
  if (n >= 2) log_ret[2:n, ] <- log(close_mat[2:n,,drop=FALSE] / close_mat[1:(n-1),,drop=FALSE])
  cs_lr <- apply(log_ret, 2, cumsum)
  cs_lr2 <- apply(log_ret^2, 2, cumsum)
  cs0_lr <- rbind(rep(0, m), cs_lr)
  cs0_lr2 <- rbind(rep(0, m), cs_lr2)
  rvol_f <- function(w) {
    out <- matrix(NA_real_, n, m)
    if (n >= w+1) {
      rows <- (w+1):n
      sx  <- cs0_lr[rows+1,,drop=FALSE] - cs0_lr[rows-w+1,,drop=FALSE]
      sx2 <- cs0_lr2[rows+1,,drop=FALSE] - cs0_lr2[rows-w+1,,drop=FALSE]
      v <- (sx2 - sx^2/w) / (w-1)
      out[rows, ] <- sqrt(pmax(v, 0)) * sqrt(252)
    }
    out
  }
  feat_3d[, , 20] <- rvol_f(5)    # rvol_5
  feat_3d[, , 21] <- rvol_f(10)   # rvol_10
  feat_3d[, , 22] <- rvol_f(21)   # rvol_21
  feat_3d[, , 23] <- rvol_f(63)   # rvol_63
  
  vr <- vol_mat / vsma20; vr[is.na(vsma20) | vsma20 == 0] <- NA
  vt <- (vsma5 - vsma20) / vsma20; vt[is.na(vsma20) | vsma20 == 0] <- NA
  feat_3d[, , 24] <- vr                                    # vol_ratio
  feat_3d[, , 25] <- vt                                    # vol_trend
  feat_3d[, , 26] <- log1p(close_mat * vol_mat)            # log_dollar_vol
  
  oc <- (close_mat - open_mat) / open_mat
  oc[is.na(open_mat) | open_mat == 0] <- 0
  feat_3d[, , 27] <- oc                                    # oc_ret

  feat_3d[, , 28] <- feat_3d[, , 15] * 100                # roc_5
  feat_3d[, , 29] <- feat_3d[, , 17] * 100                # roc_21
  
  feat_3d[, , 30] <- (close_mat - sma20) / pmax(sd20, 1e-8)   # zscore_20
  
  feat_3d_new <- array(NA_real_, dim = c(n, m, 31))
  feat_3d_new[, , 1:30] <- feat_3d
  feat_3d_new[, , 31] <- (close_mat - sma50) / pmax(sd50, 1e-8)  # zscore_50
  feat_3d <- feat_3d_new
  
  ema_state <- list(
    ema12  = ema12[n, ],
    ema26  = ema26[n, ],
    signal = sig_ema[n, ],
    rsi_ag = ag_mat[n, ],
    rsi_al = al_mat[n, ]
  )
  
  list(feat_3d = feat_3d, ema_state = ema_state)
}

FI <- list(rsi=1, bb_high=2, bb_low=3, MACD=4, PPO=5, MACD_hist=6,
           price_sma5=7, price_sma10=8, price_sma20=9, price_sma50=10,
           price_sma200=11, sma5_sma20=12, sma10_sma50=13,
           r01=14, r05=15, r10=16, r21=17, r42=18, r63=19,
           rvol_5=20, rvol_10=21, rvol_21=22, rvol_63=23,
           vol_ratio=24, vol_trend=25, log_dollar_vol=26, oc_ret=27,
           roc_5=28, roc_21=29, zscore_20=30, zscore_50=31) # Feature index map 

# Build training data from feature 
build_training_data <- function(feat_3d, r21_fwd, date_strs) {
  n <- dim(feat_3d)[1]; m <- dim(feat_3d)[2]; nf <- dim(feat_3d)[3]
  
  valid_rows <- which((1:n) > warmup_days & !is.na(r21_fwd[, 1]))
  if (length(valid_rows) > train_length) valid_rows <- tail(valid_rows, train_length)
  if (length(valid_rows) < 50) return(NULL)
  
  nr <- length(valid_rows)
  n_total_feat <- length(covariate_names)
  
  feat_mat <- matrix(NA_real_, nr * m, n_total_feat)
  label_vec <- numeric(nr * m)
  valid_mask <- rep(TRUE, nr * m)
  
  for (ri in seq_along(valid_rows)) {
    i <- valid_rows[ri]
    row_start <- (ri - 1) * m + 1
    row_end   <- ri * m
    idx <- row_start:row_end

    for (fi in 1:nf) feat_mat[idx, fi] <- feat_3d[i, , fi]     # Base features
    
    dt <- as.Date(date_strs[i])
    feat_mat[idx, 32] <- as.integer(format(dt, "%m"))  # month
    feat_mat[idx, 33] <- as.integer(format(dt, "%u"))  # weekday

    ret_cols <- c(14, 15, 16, 17, 18, 19)     # Cross-sectional deciles
    dec_cols <- 34:39  # r01dec, r63dec
    for (k in seq_along(ret_cols)) {
      vals <- feat_mat[idx, ret_cols[k]]
      rk <- rank(vals, ties.method = "first", na.last = "keep")
      feat_mat[idx, dec_cols[k]] <- as.integer(cut(rk, breaks = 10, labels = FALSE))
    }
    
    vals <- feat_mat[idx, FI$log_dollar_vol] # dollar_vol_rank
    feat_mat[idx, 40] <- rank(vals, na.last = "keep") / m
    
    label_vec[idx] <- r21_fwd[i, ] # Labels
    
    valid_mask[idx] <- !is.na(r21_fwd[i, ]) & !is.na(feat_3d[i, , 1] + 0)  # crude check
  }
  
  ok <- valid_mask & !is.na(label_vec) & rowSums(is.na(feat_mat)) < n_total_feat / 2
  feat_mat <- feat_mat[ok, , drop = FALSE]
  label_vec <- label_vec[ok]
  
  colnames(feat_mat) <- covariate_names
  feat_mat[is.na(feat_mat)] <- 0
  
  list(feat_mat = feat_mat, label_vec = label_vec)
}

# Train LightGBM model
train_lgb_fast <- function(feat_mat, label_vec) {
  if (is.null(feat_mat) || nrow(feat_mat) < 50) return(NULL)
  dtrain <- lgb.Dataset(data = feat_mat, label = label_vec,
                        categorical_feature = cat_features)
  lgb.train(params = lgb_params, data = dtrain,
            nrounds = lgb_nrounds, verbose = -1)
}

extract_today_features <- function(close_mat, vol_mat, open_mat, ema_st,
                                   date_str, n_sym) {
  n <- nrow(close_mat)
  m <- n_sym
  cl <- close_mat[n, ]
  
  feat_row <- matrix(NA_real_, m, length(covariate_names))
  colnames(feat_row) <- covariate_names
  
  feat_row[, "rsi"] <- 100 - 100 / (1 + ema_st$rsi_ag / pmax(ema_st$rsi_al, 1e-10))
  
  macd_val <- ema_st$ema12 - ema_st$ema26
  feat_row[, "MACD"] <- macd_val
  ppo_val <- macd_val / ema_st$ema26 * 100; ppo_val[ema_st$ema26 == 0] <- 0
  feat_row[, "PPO"] <- ppo_val
  feat_row[, "MACD_hist"] <- macd_val - ema_st$signal
  
  sma_k <- function(k) if (n >= k) colMeans(close_mat[(n-k+1):n, , drop=FALSE]) else rep(NA, m)
  s5 <- sma_k(5); s10 <- sma_k(10); s20 <- sma_k(20); s50 <- sma_k(50); s200 <- sma_k(200)
  
  if (n >= 20) {
    sd20_v <- sqrt(pmax(colMeans(close_mat[(n-19):n, , drop=FALSE]^2) - s20^2, 0) * 20/19)
  } else sd20_v <- rep(NA, m)
  
  feat_row[, "bb_high"] <- (cl - (s20 + 2*sd20_v)) / cl
  feat_row[, "bb_low"]  <- (cl - (s20 - 2*sd20_v)) / cl
  feat_row[, "price_sma5"]   <- (cl - s5)   / cl
  feat_row[, "price_sma10"]  <- (cl - s10)  / cl
  feat_row[, "price_sma20"]  <- (cl - s20)  / cl
  feat_row[, "price_sma50"]  <- (cl - s50)  / cl
  feat_row[, "price_sma200"] <- (cl - s200) / cl
  feat_row[, "sma5_sma20"]   <- (s5 - s20)  / cl
  feat_row[, "sma10_sma50"]  <- (s10 - s50) / cl
  retk <- function(k) if (n > k) cl / close_mat[n-k, ] - 1 else rep(NA, m)
  feat_row[, "r01"] <- retk(1);  feat_row[, "r05"] <- retk(5)
  feat_row[, "r10"] <- retk(10); feat_row[, "r21"] <- retk(21)
  feat_row[, "r42"] <- retk(42); feat_row[, "r63"] <- retk(63)
  
  if (n >= 2) {
    lr <- log(close_mat[2:n, , drop=FALSE] / close_mat[1:(n-1), , drop=FALSE])
    rvol_k <- function(w) {
      if (nrow(lr) >= w) {
        tail_lr <- lr[(nrow(lr)-w+1):nrow(lr), , drop=FALSE]
        apply(tail_lr, 2, sd) * sqrt(252)
      } else rep(NA, m)
    }
  } else {
    rvol_k <- function(w) rep(NA, m)
  }
  
  feat_row[, "rvol_5"]  <- rvol_k(5);  feat_row[, "rvol_10"] <- rvol_k(10)
  feat_row[, "rvol_21"] <- rvol_k(21); feat_row[, "rvol_63"] <- rvol_k(63)
  
  if (n >= 20) {
    vsma20 <- colMeans(vol_mat[(n-19):n, , drop=FALSE])
    vsma5  <- colMeans(vol_mat[(n-4):n, , drop=FALSE])
    vr <- vol_mat[n, ] / vsma20; vr[vsma20 == 0] <- NA
    vt <- (vsma5 - vsma20) / vsma20; vt[vsma20 == 0] <- NA
    feat_row[, "vol_ratio"] <- vr
    feat_row[, "vol_trend"] <- vt
  }
  feat_row[, "log_dollar_vol"] <- log1p(cl * vol_mat[n, ])
  
  oc <- (cl - open_mat[n, ]) / open_mat[n, ]
  oc[is.na(open_mat[n, ]) | open_mat[n, ] == 0] <- 0
  feat_row[, "oc_ret"] <- oc
  feat_row[, "roc_5"]  <- feat_row[, "r05"] * 100
  feat_row[, "roc_21"] <- feat_row[, "r21"] * 100
  
  if (n >= 20) feat_row[, "zscore_20"] <- (cl - s20) / pmax(sd20_v, 1e-8)
  if (n >= 50) {
    sd50_v <- sqrt(pmax(colMeans(close_mat[(n-49):n, , drop=FALSE]^2) - s50^2, 0) * 50/49)
    feat_row[, "zscore_50"] <- (cl - s50) / pmax(sd50_v, 1e-8)
  }
  
  dt <- as.Date(date_str)
  feat_row[, "month"]   <- as.integer(format(dt, "%m"))
  feat_row[, "weekday"] <- as.integer(format(dt, "%u"))
  
  for (col in c("r01", "r05", "r10", "r21", "r42", "r63")) {
    dec_col <- paste0(col, "dec")
    vals <- feat_row[, col]
    rk <- rank(vals, ties.method = "first", na.last = "keep")
    feat_row[, dec_col] <- as.integer(cut(rk, breaks = 10, labels = FALSE))
  }
  feat_row[, "dollar_vol_rank"] <- rank(feat_row[, "log_dollar_vol"], na.last = "keep") / m
  
  feat_row
}

# initialise_state
initialise_state <- function(data) {
  data <- as.data.table(data)
  data[, date := as.Date(date)]
  setorder(data, date, symbol)
  
  symbols <- sort(unique(data$symbol))
  dates   <- sort(unique(data$date))
  n_dates <- length(dates)
  
  keep_n <- min(n_dates, train_length + lookahead + warmup_days)
  recent_dates <- tail(dates, keep_n)
  data_recent <- data[date %in% recent_dates]
  
  close_mat <- vol_mat <- open_mat <- matrix(NA, keep_n, num_symbols,
                                             dimnames = list(as.character(recent_dates), symbols))
  for (i in 1:keep_n) {
    d <- recent_dates[i]
    sub <- data_recent[date == d]
    close_mat[i, sub$symbol] <- sub$close
    vol_mat[i, sub$symbol]   <- sub$volume
    open_mat[i, sub$symbol]  <- sub$open
  }
  
  r21_fwd <- matrix(NA, keep_n, num_symbols)
  for (i in 1:(keep_n - lookahead)) {
    r21_fwd[i, ] <- close_mat[i + lookahead, ] / close_mat[i, ] - 1
  }
  res <- compute_all_features(close_mat, vol_mat, open_mat)
  td <- build_training_data(res$feat_3d, r21_fwd, rownames(close_mat))
  
  model <- if (!is.null(td)) train_lgb_fast(td$feat_mat, td$label_vec) else NULL
  positions <- matrix(0, lookahead, num_symbols, dimnames = list(NULL, symbols))
  
  state <- list(
    symbols   = symbols,
    close_mat = close_mat,
    vol_mat   = vol_mat,
    open_mat  = open_mat,
    r21_fwd   = r21_fwd,
    model     = model,
    positions = positions,
    day_idx   = 0,
    wealth    = 1,
    ema_state = res$ema_state
  )
  return(state)
}

# trading_algorithm
trading_algorithm <- function(new_data, state) {
  bunch(symbols, close_mat, vol_mat, open_mat, r21_fwd,
        model, positions, day_idx, wealth, ema_state) %=% state
  
  new_data <- as.data.table(new_data)
  day_idx <- day_idx + 1
  n_rows <- nrow(close_mat)

  close_mat <- close_mat[-1, , drop = FALSE] # Drop oldest, append new
  vol_mat   <- vol_mat[-1, , drop = FALSE]
  open_mat  <- open_mat[-1, , drop = FALSE]
  r21_fwd   <- r21_fwd[-1, , drop = FALSE]
  
  new_date <- as.character(new_data$date[1])
  new_close <- rep(NA, num_symbols); names(new_close) <- symbols
  new_vol   <- rep(NA, num_symbols); names(new_vol)   <- symbols
  new_open  <- rep(NA, num_symbols); names(new_open)  <- symbols
  new_close[new_data$symbol] <- new_data$close
  new_vol[new_data$symbol]   <- new_data$volume
  new_open[new_data$symbol]  <- new_data$open
  
  close_mat <- rbind(close_mat, new_close)
  vol_mat   <- rbind(vol_mat, new_vol)
  open_mat  <- rbind(open_mat, new_open)
  rownames(close_mat)[nrow(close_mat)] <- new_date
  rownames(vol_mat)[nrow(vol_mat)]     <- new_date
  rownames(open_mat)[nrow(open_mat)]   <- new_date
  
  n_cur <- nrow(close_mat)
  r21_new <- rep(NA, num_symbols)
  r21_fwd <- rbind(r21_fwd, r21_new)
  if (n_cur > lookahead) {
    idx <- n_cur - lookahead
    r21_fwd[idx, ] <- close_mat[n_cur, ] / close_mat[idx, ] - 1
  }
  
  if (day_idx > 1) {
    prev_close <- close_mat[n_cur - 1, ]
    cur_close  <- close_mat[n_cur, ]
    r1d <- cur_close / prev_close - 1
    r1d[is.na(r1d)] <- 0
    combined_pos <- colSums(positions)
    wealth <- wealth + sum(combined_pos * r1d)
    
    for (i in 1:lookahead) {
      positions[i, ] <- positions[i, ] * (1 + r1d)
    }
  }
  
  a12 <- 2/13; a26 <- 2/27; a9 <- 2/10
  ema_state$ema12  <- ema_state$ema12 + a12 * (new_close - ema_state$ema12)
  ema_state$ema26  <- ema_state$ema26 + a26 * (new_close - ema_state$ema26)
  macd_now <- ema_state$ema12 - ema_state$ema26
  ema_state$signal <- ema_state$signal + a9 * (macd_now - ema_state$signal)
  
  prev_close_vec <- close_mat[n_cur - 1, ]
  delta_today <- new_close - prev_close_vec
  gain_today <- pmax(delta_today, 0); loss_today <- pmax(-delta_today, 0)
  ema_state$rsi_ag <- (ema_state$rsi_ag * 13 + gain_today) / 14
  ema_state$rsi_al <- (ema_state$rsi_al * 13 + loss_today) / 14

  if (day_idx %% retrain_freq == 1 || is.null(model)) {
    res <- compute_all_features(close_mat, vol_mat, open_mat)
    td <- build_training_data(res$feat_3d, r21_fwd, rownames(close_mat))
    model <- if (!is.null(td)) train_lgb_fast(td$feat_mat, td$label_vec) else model
    ema_state <- res$ema_state
  }
  
  feat_mat <- extract_today_features(close_mat, vol_mat, open_mat,
                                     ema_state, new_date, num_symbols)
  feat_mat[is.na(feat_mat)] <- 0
  
  if (!is.null(model)) {
    preds <- predict(model, feat_mat)
  } else {
    preds <- rep(0, num_symbols)
  }
  
  rnks <- rank(preds, ties.method = "random")
  new_pos <- rep(0, num_symbols); names(new_pos) <- symbols
  
  target_exposure <- max(wealth, 0.01) * 0.99
  pos_size <- target_exposure / (n_long + n_short) / lookahead
  
  new_pos[rnks > (num_symbols - n_long)] <-  pos_size
  new_pos[rnks <= n_short]               <- -pos_size
  old_pos <- positions[lookahead, ]
  positions[2:lookahead, ] <- positions[1:(lookahead-1), ]
  positions[1, ] <- new_pos
  
  trades <- new_pos - old_pos
  names(trades) <- symbols
  
  new_state <- list(
    symbols   = symbols,
    close_mat = close_mat,
    vol_mat   = vol_mat,
    open_mat  = open_mat,
    r21_fwd   = r21_fwd,
    model     = model,
    positions = positions,
    day_idx   = day_idx,
    wealth    = wealth,
    ema_state = ema_state
  )
  
  return(list(trades = trades, new_state = new_state))
}

