#HEC-RAS Automation functions
####################################################
#####################################################
library(stringr)
library(dplyr)

#Creates geometry input file for specific river characteristics
geometry_input <- function(in_file, print_file, inputs, levees = NULL){
  
  txt_input <- readr::read_lines(in_file)
  
  header <- matrix(txt_input[1:10], ncol = 1)
  footer<- matrix(txt_input[311:321], ncol = 1)
  
  XS_generic <- txt_input[28:41]
  
  n_xs <- inputs$L / inputs$XS_spacing + 1
  
  sta <- round(c(0, 0, inputs$w_fp / 2, 
                 inputs$w_fp / 2 + inputs$z * inputs$h, 
                 inputs$w_fp / 2 + inputs$z * inputs$h + inputs$b,
                 inputs$w_fp / 2 + inputs$TW,
                 inputs$w_fp + inputs$TW,
                 inputs$w_fp + inputs$TW), 2)
  
  elev <- round(c(30 + inputs$w_fp / 2 / inputs$z_fp  + inputs$h, 
                  inputs$w_fp / 2 / inputs$z_fp + inputs$h,
                  inputs$h, 0, 0,
                  inputs$h, inputs$w_fp / 2 / inputs$z_fp  + inputs$h, 
                  30 + inputs$w_fp / 2 / inputs$z_fp  + inputs$h), 2)
  
  bank_sta <- c(sta[3], sta[6])
  
  XS_all <- list()
  for (i in 1:n_xs){
    XS <- XS_generic
    
    #change RS and downstream lengths
    RS <- inputs$L - inputs$XS_spacing * (i - 1)
    XS[1] <- paste0(c(str_split_fixed(XS[1], ",", 2)[1], RS,
                      inputs$XS_spacing, inputs$XS_spacing, inputs$XS_spacing), collapse = ",")
    
    #Input XS station-elevation
    elev_adj <- round(elev + RS * inputs$S, 2)
    sta_elev <- c(rbind(sta, elev_adj))
    XS[4] <- paste0(stringr::str_pad(sta_elev[1:10], width = 8), collapse = "")
    XS[5] <- paste0(stringr::str_pad(sta_elev[11:16], width = 8), collapse = "")
    
    #Change Manning's n
    XS[7] <- paste0(stringr::str_pad(c(0, inputs$n_fp, 0, bank_sta[1], inputs$n, 0, bank_sta[2], inputs$n_fp, 0),
                                     width = 8), collapse = "")
    
    #Change Levee
    XS[8] <- ifelse(length(levees) > 0,
                    paste0(c("Levee=-1", bank_sta[1] - levees$RB_sta[i], max(elev_adj),
                             -1, bank_sta[2] + levees$LB_sta[i], max(elev_adj), ","),
                           collapse = ","),
                    paste0(c("Levee=-1", bank_sta[1], elev_adj[3],
                             -1, bank_sta[2], elev_adj[6], ","),
                           collapse = ","))
    
    #Change Bank stations
    XS[9] <- paste0("Bank Sta=", bank_sta[1], ",", bank_sta[2])
    
    #Change HTab parameters
    XS[11] <- paste0("XS HTab Starting El and Incr=",
                     min(elev_adj), ",0.3, 100")
    
    XS_all[[i]] <- matrix(XS, ncol = 1)
  }
  
  XS_combined <- do.call("rbind", XS_all)
  XS_combined <- rbind(header, XS_combined, footer)
  
  write.table(XS_combined, print_file, row.names = FALSE, col.names = FALSE, quote = FALSE)
}


#Creates unsteady flow input data file
flow_input <- function(in_file, print_file, input_Q, S, L){
  #Unsteady flow data
  flow <- readr::read_lines(in_file)
  
  flow_new <- flow
  
  #Change US river station
  flow_new[4] <- stringr::str_replace(flow_new[4], pattern = "5000", 
                                      replacement = as.character(L))
  
  #Change number of flow indices
  flow_new[6] <- stringr::str_replace_all(flow_new[6], pattern = "[[:digit:]]+",
                                      replacement = as.character(length(Q)))
  
  
  #Add 0.1 to prevent rounding to whole number
  Q <- as.character(signif(input_Q, digits = 7))
  Q <- if_else(stringr::str_detect(Q, "\\."), Q, paste0(Q, ".")) #Add a decimal at the end of any integers
  Q <- stringr::str_pad(Q, width = 8, side = "right", pad = "0") %>%
    paste0(collapse = " ") %>%
    stringr::str_wrap(width = 89, indent = 0, exdent = 0)
  
  Q <- t(stringr::str_split(Q, pattern = "\n", simplify = TRUE))
  Q <- apply(Q, 1, stringr::str_remove_all, pattern = " ")
  
  #Delete old flows and add new ones
  end_line <- stringr::str_which(flow_new, pattern = "DSS Path") - 1
  flow_new <- flow_new[-(7:end_line)]
  
  flow_new <- append(flow_new, Q, after = 6)
  
  #Change friction slope for ds boundary condition
  slope_line <- stringr::str_which(flow_new, pattern = "Friction Slope=")
  flow_new[slope_line] <- stringr::str_replace(flow_new[slope_line], pattern = "0.0001", replacement = as.character(S))
  
  write.table(flow_new, print_file, row.names = FALSE, col.names = FALSE, quote = FALSE)
}

#Creates a hydrograph of a given duration and peak flow
hydrograph <- function(Qb, Qp, gamma, tp, t){
  alpha <- 1 / (gamma - 1)
  Qt <- Qb * (1 + (Qp / Qb - 1) * (t / tp) ^ alpha * exp(alpha * (1 - t / tp)))
  
  return(Qt)
}


#Reads in results from hdf file
get_results <- function(file_name){
  
  f <- hdf5r::H5File$new(file_name, mode = "r")
  
  XS <- f[["Geometry/Cross Sections/Attributes"]]$read()
  
  Q <- f[["Results/Unsteady/Output/Output Blocks/Base Output/Unsteady Time Series/Cross Sections"]][["Flow"]]$read()
  WS <- f[["Results/Unsteady/Output/Output Blocks/Base Output/Unsteady Time Series/Cross Sections"]][["Water Surface"]]$read()
  V_tot <- f[["Results/Unsteady/Output/Output Blocks/Base Output/Unsteady Time Series/Cross Sections"]][["Velocity Total"]]$read()
  V_channel <- f[["Results/Unsteady/Output/Output Blocks/Base Output/Unsteady Time Series/Cross Sections"]][["Velocity Channel"]]$read()
  
  n_time <- ncol(Q)
  
  combined <- data.frame(Q = as.integer(round(as.vector(matrix(t(Q), ncol = 1)))),
                         WS = round(as.vector(matrix(t(WS), ncol = 1)), 3),
                         V_tot = round(as.vector(matrix(t(V_tot), ncol = 1)), 3),
                         V_channel = round(as.vector(matrix(t(V_channel), ncol = 1)), 3),
                         RS = as.integer(rep(XS$RS, each = n_time)))
  
  f$close_all()
  
  return(combined)
  
}

#Read in warnings (mostly WSE errors)
read_warnings <- function(file_name){
  txt <- readr::read_lines(file_name)
  
  wse_err <- as.numeric(stringr::str_extract(txt[stringr::str_detect(txt, "xsec wsel")], "\\d+\\.*\\d*"))
  
  return(wse_err)
}

#Calculates length of river with the angled part of the setback (before it reaches it's full width)
dist_calc <- function(width, angle, xs_spacing, length){
  #Find if angle works, if not adjust
  dist <- width / tan(angle * pi / 180)
  dist <- round(dist / xs_spacing) * xs_spacing
  if ((dist * 2) > length){
    angle <- atan(width / (length / 2)) * 180 / pi
    dist <- width / tan(angle * pi / 180)
  }
  
  return(dist)
}

#Create levee inputs
modify_levees <- function(levees, angle = 30, start_xs, length, width, xs_spacing, both_sides = TRUE){
  
  #Find if angle works, if not adjust
  dist <- width / tan(angle * pi / 180)
  dist <- round(dist / xs_spacing) * xs_spacing
  if ((dist * 2) > length){
    angle <- atan(width / (length / 2)) * 180 / pi
    dist <- width / tan(angle * pi / 180)
  }
  
  start_station <- levees$RB_sta[1]
  end_xs <- start_xs + length / xs_spacing
  num_angled_xs <- dist / xs_spacing 
  
  #Adjust levee stations
  levees$RB_sta[start_xs:(start_xs + num_angled_xs)] <- seq(start_station, start_station + width, length.out = num_angled_xs + 1)
  levees$RB_sta[end_xs:(end_xs - num_angled_xs)] <- seq(start_station, start_station + width, length.out = num_angled_xs + 1)
  levees$RB_sta[(start_xs + num_angled_xs):(end_xs - num_angled_xs)] <- width + start_station
  
  if (both_sides){
    levees$LB_sta <- levees$RB_sta
  }
  
  
  return(levees)
  
}

#Create levee inputs - slightly different for testing different configurations
modify_levees2 <- function(levees, angle = 30, start_xs_RB, start_xs_LB, length_RB, length_LB, width_RB, width_LB, xs_spacing){
  
  #start_station <- levees$RB_sta[1]
  
  if (!is.na(length_RB[1])){
    for (i in 1:length(length_RB)){
      dist_RB <- dist_calc(width_RB[i], angle, xs_spacing, length_RB[i])
      
      end_xs <- start_xs_RB[i] + length_RB[i] / xs_spacing
      num_angled_xs <- dist_RB / xs_spacing
      
      start_station <- levees$RB_sta[start_xs_RB[i]]
      
      #Adjust levee stations
      levees$RB_sta[start_xs_RB[i]:(start_xs_RB[i] + num_angled_xs)] <- seq(start_station, start_station + width_RB[i], length.out = num_angled_xs + 1)
      levees$RB_sta[end_xs:(end_xs - num_angled_xs)] <- seq(start_station, start_station + width_RB[i], length.out = num_angled_xs + 1)
      levees$RB_sta[(start_xs_RB[i] + num_angled_xs):(end_xs - num_angled_xs)] <- width_RB[i] + start_station
    }
  }
  
  if (!is.na(length_LB[1])){
    for (i in 1:length(length_LB)){
      dist_LB <- dist_calc(width_LB[i], angle, xs_spacing, length_LB[i])
      
      end_xs <- start_xs_LB[i] + length_LB[i] / xs_spacing
      num_angled_xs <- dist_LB / xs_spacing 
      
      start_station <- levees$LB_sta[start_xs_LB[i]]
      
      #Adjust levee stations
      levees$LB_sta[start_xs_LB[i]:(start_xs_LB[i] + num_angled_xs)] <- seq(start_station, start_station + width_LB[i], length.out = num_angled_xs + 1)
      levees$LB_sta[end_xs:(end_xs - num_angled_xs)] <- seq(start_station, start_station + width_LB[i], length.out = num_angled_xs + 1)
      levees$LB_sta[(start_xs_LB[i] + num_angled_xs):(end_xs - num_angled_xs)] <- width_LB[i] + start_station
    }
  }
  
  return(levees)
  
}

#2D levee creation/adjustment
modify_levees_2D <- function(RB_cols, LB_cols, angle = 30, start_RB, start_LB, length_RB, length_LB, width_RB, width_LB, dx){
  
  if (!is.na(length_RB[1])){
    for (i in 1:length(length_RB)){
      dist_RB <- dist_calc(width_RB[i], angle, dx, length_RB[i])
      
      end_xs <- start_RB[i] + length_RB[i] / dx
      num_angled_cells <- dist_RB / dx 
      
      #Adjust levee stations
      for (j in 1:ncol(RB_cols)){
        RB_cols[start_RB[i]:(start_RB[i] + num_angled_cells), j] <- seq(RB_cols[1, j], RB_cols[1, j] - width_RB[i] / dx, length.out = num_angled_cells + 1)
        RB_cols[end_xs:(end_xs - num_angled_cells), j] <- seq(RB_cols[1, j], RB_cols[1, j] - width_RB[i] / dx, length.out = num_angled_cells + 1)
        RB_cols[(start_RB[i] + num_angled_cells):(end_xs - num_angled_cells), j] <- RB_cols[1, j] - width_RB[i] / dx
      }
      
    }
  }
  
  if (!is.na(length_LB[1])){
    for (i in 1:length(length_LB)){
      dist_LB <- dist_calc(width_LB[i], angle, dx, length_LB[i])
      
      end_xs <- start_LB[i] + length_LB[i] / dx
      num_angled_cells <- dist_LB / dx 
      
      #Adjust levee stations
      for (j in 1:ncol(LB_cols)){
        LB_cols[start_LB[i]:(start_LB[i] + num_angled_cells), j] <- seq(LB_cols[1, j], LB_cols[1, j] + width_LB[i] / dx, length.out = num_angled_cells + 1)
        LB_cols[end_xs:(end_xs - num_angled_cells), j] <- seq(LB_cols[1, j], LB_cols[1, j] + width_LB[i] / dx, length.out = num_angled_cells + 1)
        LB_cols[(start_LB[i] + num_angled_cells):(end_xs - num_angled_cells), j] <- LB_cols[1, j] + width_LB[i] / dx
      }
    }
  }
  
  return(list(RB_cols, LB_cols))
  
}


#Creates a plot of differences in WSE vs river station for all scenarios
plot_run2 <- function(results, start_RS, run, inputs, yrange, xrange){
  
  results <- filter(results, Run == run)
  inputs <- filter(inputs, Run == run)
  
  par(mar = c(3.5, 4.1, 2.5, 0.5), mgp = c(2.5, 0.8, 0))
  plot(NA, xlim = xrange, ylim = yrange, las = 1,
       ylab = "Difference in WSE [m]", xlab = "Distance Upstream [km]",
       main = paste("Qp =", inputs$Qp[1], "  Qd =", inputs$Qd[1], "\n",
                    "S =", inputs$S[1], "  TW =", inputs$TW[1], "   n_fp =", inputs$n_fp[1]))
  
  abline(v = (start_RS - c(0, 2500, 5000, 10000, 15000, 22500, 30000)) / 1000, lty = 2, col = "gray50")
  
  colors <- c(rep(viridis::viridis(6), 3))
  lty <- rep(c(1, 2, 3), each = 6)
  
  nums <- c(3, 6, 1, 2, 4, 5)
  scenarios <- unique(WSE_diff$Scenario)[c(nums, nums + 12, nums + 6)]
  for (i in 1:length(scenarios)){
    sub <- filter(results, Scenario == scenarios[i]) %>%
      arrange(RS)
    lines(WS_diff ~ I(RS / 1000), sub, col = colors[i], lty = lty[i], lwd = 2)
  }
  
  legend("bottomleft", legend = c("2.5 km", "5 km", "10 km", "15 km", "22.5", "30 km"), lty = 1, lwd = 2, col = viridis::viridis(6), bty = "n",
         title = "Setback Length")
  legend("bottomright", legend = c("L/12", "L/8", "L/4"), lty = c(1, 2, 3), lwd = 2, col = colors[2], bty = "n",
         title = "Setback Width")
}

#Plots change in velocity vs river station for all scenarios
plot_run_vel <- function(results, start_RS, run, inputs, yrange, xrange){
  
  results <- filter(results, Run == run)
  inputs <- filter(inputs, Run == run)
  
  par(mar = c(3.5, 4.1, 2.5, 0.5), mgp = c(2.5, 0.8, 0))
  plot(NA, xlim = xrange, ylim = yrange, las = 1,
       ylab = "Difference in Velocity [m/s]", xlab = "Distance Upstream [km]",
       main = paste("Qp =", inputs$Qp[1], "  Qd =", inputs$Qd[1], "\n",
                    "S =", inputs$S[1], "  b =", inputs$b[1]))
  
  abline(v = (start_RS - c(0, 1000, 5000, 15000, 30000)) / 1000, lty = 2, col = "gray50")
  
  colors <- c("black", rep(viridis::viridis(4), each = 2))
  lty <- c(1, rep(c(1, 2), 4))
  
  scenarios <- unique(V_diff$Scenario)[c(9, 2, 6, 4, 8, 1, 5, 3, 7)]
  for (i in 1:length(scenarios)){
    sub <- filter(results, Scenario == scenarios[i]) %>%
      arrange(RS)
    lines(V_diff ~ I(RS / 1000), sub, col = colors[i], lty = lty[i], lwd = 2)
  }
  
  legend("bottomleft", legend = c("1 km", "5 km", "15 km", "30 km"), lty = 1, lwd = 2, col = viridis::viridis(4), bty = "n",
         title = "Setback Length")
  legend("bottomright", legend = c("250 m", "500 m"), lty = c(1, 2), lwd = 2, col = colors[2], bty = "n",
         title = "Setback Width")
}

#Plots change in velocity vs river station for all scenarios
plot_run_vel2 <- function(results, start_RS, run, inputs, yrange, xrange){
  
  results <- filter(results, Run == run)
  inputs <- filter(inputs, Run == run)
  
  par(mar = c(3.5, 4.1, 2.5, 0.5), mgp = c(2.5, 0.8, 0))
  plot(NA, xlim = xrange, ylim = yrange, las = 1,
       ylab = "Difference in Velocity [m/s]", xlab = "Distance Upstream [km]",
       main = paste("Qp =", inputs$Qp[1], "  Qd =", inputs$Qd[1], "\n",
                    "S =", inputs$S[1], "  TW =", inputs$TW[1], "   n_fp =", inputs$n_fp[1]))
  
  abline(v = (start_RS - c(0, 2500, 5000, 10000, 15000, 22500, 30000)) / 1000, lty = 2, col = "gray50")
  
  colors <- c(rep(viridis::viridis(6), 3))
  lty <- rep(c(1, 2, 3), each = 6)
  
  nums <- c(3, 6, 1, 2, 4, 5)
  scenarios <- unique(results$Scenario)[c(nums, nums + 12, nums + 6)]
  for (i in 1:length(scenarios)){
    sub <- filter(results, Scenario == scenarios[i]) %>%
      arrange(RS)
    lines(V_diff ~ I(RS / 1000), sub, col = colors[i], lty = lty[i], lwd = 2)
  }
  
  
  legend("topleft", legend = c("2.5 km", "5 km", "10 km", "15 km", "22.5", "30 km"), lty = 1, lwd = 2, col = viridis::viridis(6), bty = "n",
         title = "Setback Length")
  legend("topright", legend = c("L/12", "L/8", "L/4"), lty = c(1, 2, 3), lwd = 2, col = colors[2], bty = "n",
         title = "Setback Width")
}

#Plots change in sediment transport capacity vs river station for all scenarios
plot_sed <- function(results, start_RS, run, inputs, yrange, xrange){
  
  results <- filter(results, Run == run)
  inputs <- filter(inputs, Run == run)
  
  par(mar = c(3.5, 4.1, 2.5, 0.5), mgp = c(2.5, 0.8, 0))
  plot(NA, xlim = xrange, ylim = yrange, las = 1,
       ylab = "Ratio of Transport Capacity", xlab = "Distance Upstream [km]",
       main = paste("Qp =", inputs$Qp[1], "  Qd =", inputs$Qd[1], "\n",
                    "S =", inputs$S[1], "  TW =", inputs$TW[1], "   n_fp =", inputs$n_fp[1]),
       log = "y")
  
  abline(v = (start_RS - c(0, 2500, 5000, 10000, 15000, 22500, 30000)) / 1000, lty = 2, col = "gray50")
  
  colors <- c(rep(viridis::viridis(6), 3))
  lty <- rep(c(1, 2, 3), each = 6)
  
  nums <- c(3, 6, 1, 2, 4, 5)
  scenarios <- unique(results$Scenario)[c(nums, nums + 12, nums + 6)]
  for (i in 1:length(scenarios)){
    sub <- filter(results, Scenario == scenarios[i]) %>%
      arrange(RS)
    lines(diff_Qt ~ I(RS / 1000), sub, col = colors[i], lty = lty[i], lwd = 2)
  }
  
  legend("topleft", legend = c("2.5 km", "5 km", "10 km", "15 km", "22.5", "30 km"), lty = 1, lwd = 2, col = viridis::viridis(6), bty = "n",
         title = "Setback Length")
  legend("topright", legend = c("L/12", "L/8", "L/4"), lty = c(1, 2, 3), lwd = 2, col = colors[2], bty = "n",
         title = "Setback Width")
}

#Calculates ratio of sediment transport capacity
sed_capacity <- function(output, inputs_condensed){
  
  output <- output %>%
    mutate(RS = as.numeric(RS),
           Run = as.numeric(Run)) %>%
    left_join(inputs_condensed, by = "Run") %>%
    mutate(z_bed = S * RS,
           y = WS - z_bed,
           A = if_else(y <= h, (b + z * y) * y, (b + z * h) * h + (y - h) * TW),
           P = if_else(y <= h, b + 2 * y * sqrt(1 + z ^ 2),  b + 2 * h * sqrt(1 + z ^ 2)),
           R = A / P,
           ts = rep(1:sum(RS == 0), length(unique(RS))),
           H = WS + V_tot ^ 2 / (2 * 9.81)) %>%
    group_by(ts) %>%
    mutate(Sf = abs(c(0, diff(H)) / 250),
           Sf_avg = zoo::rollmean(Sf, k = 3, fill = NA)) %>%
    ungroup() %>%
    mutate(theta = 9810 * R * Sf_avg / ((2650 - 1000) * 9.81 * 0.001),
           Qt = V_channel ^ 2 * theta ^ 1.5 * sqrt(0.001 / (9.81 * (2650 / 1000 - 1))) * b,
           param = V_channel ^ 2 * (R * Sf_avg) ^ 1.5) %>%

    group_by(RS) %>%
    summarize(Qt_cum = sum(Qt) * first(Qd) / 240 * 86400,
              param_cum = sum(param),
              Run = first(Run),
              Scenario = first(Scenario)) %>%
    mutate(Qt_ratio = Qt_cum / lag(Qt_cum))
  
}