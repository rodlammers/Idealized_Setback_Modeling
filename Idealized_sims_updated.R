#HEC-RAS Idealized levee setback simulations. This code sets up HEC-RAS model files for
#a range of scenarios. The models are run in batch mode using an Excel VBA script. The
#results are read in and analyzed here.
source("HEC-RAS Automation Functions.R")

#Updated "Idealized" scenarios simulation to explore various levee setbacks scenarios
#Vary channel slope (3), channel width (3), and floodplain roughness (2). 
#Also create 9 different hydrographs - 3 peak flow sizes and 3 hydrograph
#durations (10, 30, and 60 days).

base_path <- "Runs"

#Creates inputs
vars <- list(
  n = 0.03, #Channel Manning's n
  TW = c(100, 300, 1000), #Channel top width (m)
  S = c(1e-4, 3e-4, 1e-3), #Channel slope (assumed same for fp)
  h = 5, #Fixed channel depth of 4 m
  z = 4, #Channel bank side slope (H:V)
  w_fp = 60000,#Channel floodplain width (m) - total, split between right and left banks
  z_fp = 100000, #Floodplain lateral slope (H:V)
  n_fp = c(0.05, 0.15) #Floodplain Manning's n
)

inputs <- expand.grid(vars) %>%
  mutate(XS_spacing = 250, #Xs spacing (m)
         L = 150000, #reach length (m)
         b = TW - 2 * h * z, #Channel top width (m)
         A_bf = (TW + b) / 2 * h, #Cross section area (m2)
         P_bf = b + 2 * h * sqrt(1 + z ^ 2), #Wetted perimeter (m)
         Q_bf = 1 / n * (A_bf / P_bf) ^ (2/3) * S ^ 0.5 * A_bf, #Bankfull discharge (cms)
         L = ceiling(L / XS_spacing) * XS_spacing, #round L to multiple of XS_spacing)
         Geometry = as.character(formatC(1:n(), width = 3, flag ="0", format = "d")))

Qp <- c(2.5, 3.5, 4.5) #Flow peaks as multiple of bankfull Q
Qd <- c(10, 30, 60) #Hydrograph duration (days)
Geometry <- unique(inputs$Geometry)

Q_vals <- expand.grid(list(Qp = Qp, Qd = Qd, Geometry2 = Geometry), stringsAsFactors = FALSE)

#Combine all combinations of inputs
inputs_all <- inputs %>%
  slice(rep(row_number(), 9)) %>%
  arrange(Geometry) %>%
  bind_cols(Q_vals) %>%
  mutate(Run = 1:n())

write.csv(inputs_all, "Inputs_all.csv", row.names = F)

start_RS <- 80000 #start river station (m) - same for all scenarios

#Loop through every input to create the appropriate HEC-RAS files
for (i in 1:nrow(inputs_all)){

  run <- paste0("Ideal_", formatC(inputs_all$Run[i], width = 3, format = "d", flag = "0"))
  unlink(file.path(base_path, run), recursive = TRUE) #Delete folder if it exists
  dir.create(file.path(base_path, run)) #Create folder
  
  #Get file names
  flow_file_in <- file.path(base_path, case_when(inputs_all$Qd[i] == 10 ~ "BASELINE_1D_10day.u01",
                                                 inputs_all$Qd[i] == 30 ~ "BASELINE_1D_30day.u01",
                                                 inputs_all$Qd[i] == 60 ~ "BASELINE_1D_60day.u01"))
  
  flow_file_out <- file.path(base_path, run, paste0(run, "_baseline.u01"))
  
  geom_file_in <- file.path(base_path, "BASELINE_1D_1day.g01")
  geom_file_out <- file.path(base_path, run, paste0(run, "_baseline.g01"))
  
  #Create levee location data.frame for baseline scenario (levees on both side)
  levees <- data.frame(XS_id = 1:(inputs_all$L[i] / inputs_all$XS_spacing[i] + 1),
                       LB_sta = inputs_all$TW[i], #levee location, meters from bank (equal to channel top width)
                       RB_sta = inputs_all$TW[i]) #levee location, meters from bank
  
  #Create geometry file
  geometry_input(geom_file_in, geom_file_out, inputs_all[i,], levees = levees)
  
  #Get scaled hydrograph based on Q_bf
  Q <- hydrograph(max(0.1 * inputs_all$Q_bf[i], 1), inputs_all$Qp[i] * inputs_all$Q_bf[i], 1.2, 50, t = seq(1, 241, 1))
  
  #Create flow file
  flow_input(flow_file_in, flow_file_out, Q, inputs_all$S[i], inputs_all$L[i])
  
  #Copy over plan and prj files
  if (inputs_all$Qd[i] == 10){
    file.copy(file.path(base_path, "BASELINE_1D_10day.prj"), file.path(base_path, run, paste0(run, "_baseline.prj")))
    file.copy(file.path(base_path, "BASELINE_1D_10day.p01"), file.path(base_path, run, paste0(run, "_baseline.p01")))
  }else if(inputs_all$Qd[i] == 30) {
    file.copy(file.path(base_path, "BASELINE_1D_30day.prj"), file.path(base_path, run, paste0(run, "_baseline.prj")))
    file.copy(file.path(base_path, "BASELINE_1D_30day.p01"), file.path(base_path, run, paste0(run, "_baseline.p01")))
  }else{
    file.copy(file.path(base_path, "BASELINE_1D_60day.prj"), file.path(base_path, run, paste0(run, "_baseline.prj")))
    file.copy(file.path(base_path, "BASELINE_1D_60day.p01"), file.path(base_path, run, paste0(run, "_baseline.p01")))
  }
  
  #Run levee setback scenarios - look at width and length of setbacks
  #Widths 1/12, 1/8, and 1/4 of lengths (1/4 is max width to keep 2:1 expansion and contraction zones)
  TW <- inputs_all$TW[i]
  L <- c(2.5, 5, 10, 15, 22.5, 30) * 1000 #m
  w_L <- c(1/12, 1/8, 1/4) #fraction of L
  
  #Create all possible setback scenarios
  scenarios_df <- expand.grid(TW = TW, L = L, w_L = w_L) %>%
    mutate(W = L * w_L,
           W_tw = W / TW,
           Area = ((L - 2 * W) * W) / 1000^2,
           scenarios = paste0("1_", 1 / w_L, "xL_", L / 1000, "km"))
  
  start_xs <- (inputs_all$L[i] - start_RS) / inputs_all$XS_spacing[i] #numbered US to DS
  
  #Loop through all levee scenarios and create appropriate HEC-RAS files
  for (j in 1:nrow(scenarios_df)){
    levees_scenarios <- modify_levees(levees = levees, angle = 26, start_xs = start_xs, length = scenarios_df$L[j],
                                      width = scenarios_df$W[j], xs_spacing = inputs_all$XS_spacing[i], both_sides = FALSE)
    
    geom_file_out <- file.path(base_path, run, paste0(run, "_", scenarios_df$scenarios[j], ".g01"))
    geometry_input(geom_file_in, geom_file_out, inputs_all[i,], levees = levees_scenarios)
    
    flow_file_out <- file.path(base_path, run, paste0(run, "_", scenarios_df$scenarios[j], ".u01"))
    flow_input(flow_file_in, flow_file_out, Q, inputs_all$S[i], inputs_all$L[i])
    
    #Copy over plan and prj files
    if (inputs_all$Qd[i] == 10){
      file.copy(file.path(base_path, "BASELINE_1d_10day.prj"), file.path(base_path, run, paste0(run, "_", scenarios_df$scenarios[j], ".prj")))
      file.copy(file.path(base_path, "BASELINE_1d_10day.p01"), file.path(base_path, run, paste0(run, "_", scenarios_df$scenarios[j], ".p01")))
    }else if (inputs_all$Qd[i] == 30) {
      file.copy(file.path(base_path, "BASELINE_1d_30day.prj"), file.path(base_path, run, paste0(run, "_", scenarios_df$scenarios[j], ".prj")))
      file.copy(file.path(base_path, "BASELINE_1d_30day.p01"), file.path(base_path, run, paste0(run, "_", scenarios_df$scenarios[j], ".p01")))
    }else {
      file.copy(file.path(base_path, "BASELINE_1d_60day.prj"), file.path(base_path, run, paste0(run, "_", scenarios_df$scenarios[j], ".prj")))
      file.copy(file.path(base_path, "BASELINE_1d_60day.p01"), file.path(base_path, run, paste0(run, "_", scenarios_df$scenarios[j], ".p01")))
    }
  }
}

#Create text file with simulation file paths - this will be used by the Excel VBA code to actually run HEC-RAS
folders <- stringr::str_subset(list.files(base_path, pattern = "Ideal_[[:digit:]]", full.names = TRUE), "old", negate = T)
files <- lapply(folders, list.files, pattern = ".prj", full.names = TRUE)
all_paths <- matrix(file.path(getwd(), unlist(files)),
                    ncol = 1)

write.table(all_paths, file.path(base_path, "INPUT FILES IDEALIZED.txt"), col.names = FALSE, row.names = FALSE, quote = FALSE)

####################################################################################
####STOP - YOU NEED TO RUN ALL HEC-RAS SIMULATIONS BEFORE CONTINUING################
####USE THE PROVIDED EXCEL FILE WITH VBA CODE#######################################
####################################################################################

#Get Results
output_files <- unlist(lapply(folders, list.files, pattern = ".p01.hdf", full.names = TRUE))
results <- lapply(output_files, function(x){
  run <- stringr::str_match(x, "Ideal_(.*?)/")[,2]
  scen <- stringr::str_match(x, paste0("Ideal_", run, "_(.*?).p01"))[,2]
  get_results(x) %>%
    mutate(Run = run,
           Scenario = scen)
})

#Save results as a binary file
saveRDS(results, file.path(base_path, paste0("Results/Full_results", Sys.Date(), ".Rds")))

#Get maximum WSE at each XS
max_WSE <- lapply(results, function(x){
  group_by(x, Scenario, RS) %>%
  summarize(max_WS = max(WS),
            Run = first(Run),
            max_time = which(WS == max_WS)[1],
            .groups = "keep") %>%
  mutate(RS = as.numeric(as.character(RS)))
})

max_WSE <- do.call("rbind", max_WSE)

#Get the difference in max WSE between each scenario and the baseline condition
WSE_diff <- ungroup(max_WSE) %>%
  group_by(Run) %>%
  mutate(WS_baseline = rep(max_WS[Scenario == "baseline"], length(unique(Scenario))),
         time_baseline = rep(max_time[Scenario == "baseline"], length(unique(Scenario)))) %>%
  group_by(Scenario) %>%
  mutate(WS_diff = max_WS - WS_baseline,
         time_diff = max_time - time_baseline,
         Run = as.numeric(Run))

#Save WSE_diff file
saveRDS(WSE_diff, file.path(base_path, paste0("Results/WSE_diff_", Sys.Date(), ".Rds")))

#Get the maximum channel V at each XS
max_V <- lapply(results, function(x){
  group_by(x, Scenario, RS) %>%
  summarize(max_V = max(V_channel),
            Run = first(Run),
            .groups = "keep") %>%
  mutate(RS = as.numeric(as.character(RS)))
})

max_V <- do.call("rbind", max_V)

#Get the difference in max channel V between each scenario and the baseline condition
V_diff <- ungroup(max_V) %>%
  group_by(Run) %>%
  mutate(V_baseline = rep(max_V[Scenario == "baseline"], length(unique(Scenario)))) %>%
  group_by(Scenario) %>%
  mutate(V_diff = max_V - V_baseline,
         Run = as.numeric(Run))

#Save velocity results
saveRDS(V_diff, file.path(base_path, paste0("Results/Velocity_results_max", Sys.Date(), ".Rds")))

#Get peak discharge at each XS
peak_Q <- lapply(results, function(x){
  group_by(x, Scenario, RS) %>%
  summarize(Q = max(Q),
            Run = first(Run),
            .groups = "keep") %>%
  mutate(RS = as.numeric(as.character(RS)))
})

peak_Q <- do.call("rbind", peak_Q)

#Calculate peak flow attenuation
attenuation <- ungroup(peak_Q) %>%
  group_by(Run) %>%
  mutate(Q_baseline = rep(Q[Scenario == "baseline"], length(unique(Scenario)))) %>%
  #group_by(Scenario) %>%
  mutate(Q_diff = Q - Q_baseline,
         Q_diff_perc = Q_diff / Q_baseline * 100,
         Q_atten = Q - Q_baseline[RS == 150000],
         Q_atten_perc = Q_atten / Q_baseline[RS == 150000] * 100)

#Save attenuation results
saveRDS(attenuation, file.path(base_path, paste0("Results/Attenuation_results", Sys.Date(), ".Rds")))

#Summarize WSE results
summary <- WSE_diff %>%
  mutate(Length = if_else(Scenario == "baseline", 0, as.numeric(stringr::str_match(Scenario, "L_(.*?)km")[,2])) * 1000,
         Width_x = if_else(Scenario == "baseline", 0, 1 / as.numeric(stringr::str_match(Scenario, "_(.*?)x")[,2])),
         Width = Width_x * Length,
         Area = (Length - 2 * Width) * Width / 1000^2) %>% #Floodplain area in km2
  group_by(Run, Scenario) %>%
  summarize(end_RS = start_RS - first(Length),
            max_diff = min(WS_diff), #maximum WSE diff (m)
            mean_setback = mean(WS_diff[RS <= start_RS & RS >= (end_RS)]), #mean diff in setback (m)
            median_setback = median(WS_diff[RS <= start_RS & RS >= (end_RS)]), #median diff in setback (m)
            mean_total = mean(WS_diff), #Overall mean (m)
            US_diff = mean(WS_diff[RS > start_RS]), #mean difference US of setback (m)
            dist_US = min(RS[which(WS_diff > -0.1 & RS > start_RS)] - (start_RS)), #distance US with at least 0.1 m reduction in WSE (m)
            dist_US = if_else(is.infinite(dist_US), max(RS) - start_RS, dist_US),
            DS_diff = WS_diff[RS == 0], #WSE diff at downstream end (m)
            Length = first(Length),
            Width = first(Width),
            Area = first(Area),
            travel_time = max(max_time) - min(max_time),
            .groups = "keep") %>%
  left_join(inputs_all, by = "Run") %>%
  mutate(dist_US = if_else(dist_US == -1, L - start_RS, dist_US))

#Save summary file
saveRDS(summary, file.path(base_path, paste0("Results/Summary_WSE", Sys.Date(), ".Rds")))

##Create full plots for each Run - 9 plots per figure, 1 figure per geometry, 9 total geometries
geoms <- unique(inputs_all$Geometry)
for (i in 1:length(geoms)){
  runs <- inputs_all$Run[inputs_all$Geometry == geoms[i]]
  
  png(file.path(base_path, "Results", paste0("WS_diff_geom_", geoms[i], ".png")), type = "cairo",
      height = 9, width = 9, res = 500, units = "in")
  par(mfrow = c(3, 3), mar = c(3, 3, 1.5, 2))
  for (j in 1:length(runs)){
    plot_run2(WSE_diff, start_RS = 80000, run = runs[j], inputs_all, yrange = range(WSE_diff$WS_diff), xrange = range(WSE_diff$RS) / 1000)
  }
  dev.off()
}

###########################################################
#Can start here with provided results summary files
summary_file <- list.files(file.path(base_path, "Results"), pattern = "Summary", full.names = TRUE)
summary <- readRDS(summary_file)

#Design plots - performance metric vs. setback area
slopes <- sort(unique(summary$S))
widths <- unique(inputs_all$TW)
roughness <- unique(inputs_all$n_fp)
Qds <- unique(inputs_all$Qd)
Qps <- unique(inputs_all$Qp)

source("Plot Functions.R")
#Plot of mean WSE reduction in the setback, summarized by slope, Qd, and Qp. Show only plot for n_fp = 0.05
cols <- viridis::viridis(length(Qps))
png(file.path(base_path, "Results", "Figure 2.png"), type = "cairo", height = 3.5,
    width = 6.5, res = 500, units = "in")
pts <- c(16, 15, 18)
par(mfrow = c(1,3), mar = c(3, 3, 2, 0.5), oma = c(1.5, 1.5, 0, 0))
for (j in 1:length(slopes)){
  sub <- filter(summary, S == slopes[j], n_fp == 0.05, Area > 0)
  
  par(fig = c(1/3 * (j - 1), 1/3 * j, 0, 1), new = TRUE, mar = c(3, 3, 2, 0.5))
  plot(NA, ylim = range(summary$mean_setback[summary$n_fp == 0.05]), xlim = range(sub$Area / (sub$TW * 3 / 1000)), las = 1, xlab = "", ylab = "",
       main = paste0("Slope = ", slopes[j]))
  
  for (i in 1:length(Qps)){
    for(k in 1:length(Qds)){
      sub2 <- filter(sub, Qp == Qps[i], Qd == Qds[k]) %>%
        arrange(Area / TW)
      points(mean_setback ~ I(Area / (TW * 3 / 1000)), sub2, col = adjustcolor(cols[i], alpha.f = 0.5), pch = pts[k], cex = 0.8)
      
      lo <- loess(mean_setback ~ I(Area / (TW * 3 / 1000)), sub2, span = 0.4)
      pred <- predict(lo)
      lines(pred ~ I(sub2$Area / (3 * sub2$TW / 1000)), col = cols[i], lwd = 1, lty = k)
    }
  }
  
  add_label(-0.05, -0.03, paste0("(", letters[j], ")"))
  if (j == 1){
    legend("bottomleft", legend = Qds, lty = 1:3, col = cols[1], title = "Flood Duration (Days)", bty = "n", cex = 0.8,
           pch = 21:23, pt.bg = cols[1])
    
  }else if (j == 2){
    legend("bottomleft", legend = Qps, col = cols, title = expression("Flood Magnitude (x" * Q[bf]*")"), bty = "n", cex = 0.8,
           pch = 21, pt.bg = cols, lty = 1)
  }
  
  #Add subplot
  plt <- par("plt")
  fig <- par("fig")
  plt[1:2] <- plt[1:2] * (fig[2] - fig[1]) + fig[1]
  plt[3:4] <- plt[3:4] * (fig[4] - fig[3]) + fig[3]
  xl <- plt[2] - (plt[2] - plt[1]) * 0.4
  xr <- plt[2]
  yb <- plt[4] - (plt[4] - plt[3]) * 0.25
  yt <- plt[4]
  par(fig = c(xl, xr, yb, yt), new = TRUE, mar = c(0, 0, 0, 0))
  plot(NA, xlim = c(0.2, 20), ylim = c(-0.6, 0.01), las = 1, ylab= "", xlab = "", log = "x", cex.axis = 0.8,
       xaxt = "n", yaxt = "n")
  axis(side = 1, at = c(1, 10), cex.axis = 0.8, mgp = c(2, 0.5, 0), tcl = -0.3)
  axis(side = 1, at = c(seq(0.1, 1, 0.1), 2:10, 20), labels = FALSE, tcl = -0.1)
  axis(side = 2, at = seq(0, -0.6, -0.2), mgp = c(2, 0.5, 0), cex.axis = 0.8, tcl = -0.3, las = 1)
  for (i in 1:length(Qps)){
    for(k in 1:length(Qds)){
      sub2 <- filter(sub, Qp == Qps[i], Qd == Qds[k]) %>%
        arrange(Area / TW)
      points(mean_setback ~ I(Area / (TW * 3 / 1000)), sub2, col = adjustcolor(cols[i], alpha.f = 0.5), pch = pts[k], cex = 0.8)

    }
  }

}
mtext("Mean WSE Change in Setback [m]", side = 2, outer = TRUE, line = 0)
mtext(expression("Reconnected Floodplain Area ["*km^2*"] / Levee Top Width [km]"), side = 1, outer = TRUE, line = 0.5)
dev.off()

############################
#DS change in WSE area
cols <- viridis::viridis(length(Qps))
pts <- c(16, 15, 18)
png(file.path(base_path, "Results", "Figure 3.png"), type = "cairo", height = 3.5,
    width = 6.5, res = 500, units = "in")
par(mfrow = c(1,3), mar = c(3, 3, 2, 0.5), oma = c(1.5, 1.5, 0, 0))
for (i in 1:length(slopes)){
  sub <- filter(summary, S == slopes[i], n_fp == 0.05)
  par(fig = c(1/3 * (i - 1), 1/3 * i, 0, 1), new = TRUE, mar = c(3, 3, 2, 0.5))
  plot(NA, xlim = c(0, max(summary$Area / (summary$TW * 3 / 1000))), ylim = range(summary$DS_diff[summary$n_fp == 0.05]), xlab = "", ylab = "",
       las = 1, main = paste("Slope =", slopes[i]))
  if (i == 3){
    legend("topright", legend = Qds, lwd = 1, lty = 1:3, pch = pts, cex = 0.8, col = cols[1], bty = "n", title = "Flood Length\n(days)",
           inset = c(0, 0.55))
    legend("bottomright", legend = Qps, lwd = 1, col = cols, bty = "n", pch = pts[1], cex = 0.8)
    text(330, -1.05, "Flood Peak", cex = 0.8)
    text(330, -1.11, expression("(x"*Q[bf]*")"), cex = 0.8)
  }
  for (k in 1:length(Qds)){
    for(l in 1:length(Qps)){
      sub2 <- filter(sub, Qd == Qds[k], Qp == Qps[l]) %>%
        arrange(Area / TW)
      points(DS_diff ~ I(Area / (TW * 3 / 1000)), sub2, pch = pts[k], col = adjustcolor(cols[l], 0.5), cex = 0.8)
      
      fit <- loess(DS_diff ~ I(Area / I(TW * 3 / 1000)), sub2)
      pred <- predict(fit)
      lines(sub2$Area / (sub2$TW * 3 / 1000), pred, lwd = 1, col = cols[l], lty = k)
    }
  }
  
  add_label(-0.05, -0.03, paste0("(", letters[i], ")"))

  #Subplot
  plt <- par("plt")
  fig <- par("fig")
  plt[1:2] <- plt[1:2] * (fig[2] - fig[1]) + fig[1]
  plt[3:4] <- plt[3:4] * (fig[4] - fig[3]) + fig[3]
  xl <- plt[1]
  xr <- plt[1] + (plt[2] - plt[1]) * 0.4
  yb <- plt[3]
  yt <- plt[3] + (plt[4] - plt[3]) * 0.25
  par(fig = c(xl, xr, yb, yt), new = TRUE, mar = c(0, 0, 0, 0))
  plot(NA, xlim = c(0.2, 20), ylim = c(-0.11, 0.01), las = 1, ylab= "", xlab = "", log = "x", cex.axis = 0.8,
       mgp = c(2, 0.5, 0), tcl = -0.3, xaxt = "n", yaxt = "n")
  axis(side = 3, at = c(1, 10), cex.axis = 0.8,
       mgp = c(2, 0.5, 0), tcl = -0.3)
  axis(side = 3, at = c(seq(0.2, 1, 0.1), 2:10, 20), labels = FALSE, cex.axis = 0.8,
       mgp = c(2, 0.5, 0), tcl = -0.1)
  axis(side = 4, at = seq(0, -0.1, -0.05), cex.axis = 0.8, mgp = c(2, 0.5, 0), tcl = -0.3, las = 1,
       labels = c("0", "-0.05", "-0.1"))
  for (i in 1:length(Qps)){
    for(k in 1:length(Qds)){
      sub2 <- filter(sub, Qp == Qps[i], Qd == Qds[k]) %>%
        arrange(Area / TW)
      points(DS_diff ~ I(Area / (TW * 3 / 1000)), sub2, col = adjustcolor(cols[i], alpha.f = 0.5), pch = pts[k], cex = 0.8)
      
    }
  }
  
}
mtext("WSE Change Downstream [m]", side = 2, outer = TRUE, line = 0)
mtext(expression("Reconnected Floodplain Area ["*km^2*"] / Levee Top Width [km]"), side = 1, outer = TRUE, line = 0.5)
dev.off()

###############################
#Upstream distance with lowered WSE
cols <- viridis::viridis(length(Qps))
pts <- c(16, 15, 18)
png(file.path(base_path, "Results", "Figure 4.png"), type = "cairo", height = 3.5,
    width = 6.5, res = 500, units = "in")
par(mfrow = c(1,3), mar = c(3, 3, 2, 0.5), oma = c(1.5, 1.5, 0, 0))
for (i in 1:length(slopes)){
  sub <- filter(summary, S == slopes[i], n_fp == 0.05)
  par(fig = c(1/3 * (i - 1), 1/3 * i, 0, 1), new = TRUE, mar = c(3, 3, 2, 0.5))
  plot(NA, xlim = c(0, max(summary$Area / (summary$TW * 3 / 1000))), ylim = range(summary$dist_US[summary$n_fp == 0.05]), xlab = "", ylab = "",
       las = 1, main = paste("Slope =", slopes[i]), yaxt = "n")
  axis(side = 2, at = seq(0, 50000, 10000), labels = seq(0, 50, 10), las = 1)
  if (i == 3){
    legend("left", legend = Qds, lty = 1:3, pch = pts, col = cols[1], bty = "n", title = "Flood Length\n(days)", cex = 0.8)
    legend("right", legend = Qps, col = cols, lty = 1, pch = pts[1], bty = "n", cex = 0.8, title = "\n")
    text(330, 32000, "Flood Peak", cex = 0.8)
    text(330, 30000, expression("(x"*Q[bf]*")"), cex = 0.8)
  }
  for (k in 1:length(Qds)){
    for(l in 1:length(Qps)){
      sub2 <- filter(sub, Qd == Qds[k], Qp == Qps[l]) %>%
        arrange(Area / TW)
      points(dist_US ~ I(Area / (TW * 3 / 1000)), sub2, pch = pts[k], 
             col = adjustcolor(cols[l], alpha.f = 0.5), cex = 0.8)
      fit <- loess(dist_US ~ I(Area / (TW * 3 / 1000)), sub2)
      pred <- predict(fit)
      lines(sub2$Area / (sub2$TW * 3 / 1000), pred, lwd = 1, col = cols[l], lty = k)
    }
  }
  
  if (i != 1){
    plt <- par("plt")
    fig <- par("fig")
    plt[1:2] <- plt[1:2] * (fig[2] - fig[1]) + fig[1]
    plt[3:4] <- plt[3:4] * (fig[4] - fig[3]) + fig[3]
    xl <- plt[2] - (plt[2] - plt[1]) * 0.4
    xr <- plt[2]
    yb <- plt[4] - (plt[4] - plt[3]) * 0.25
    yt <- plt[4]
    par(fig = c(xl, xr, yb, yt), new = TRUE, mar = c(0, 0, 0, 0))
    plot(NA, xlim = c(0.2, 20), ylim = c(-1000, 11000), las = 1, ylab= "", xlab = "", log = "x", cex.axis = 0.8,
         xaxt = "n", yaxt = "n")
    axis(side = 1, at = c(1, 10), cex.axis = 0.8, mgp = c(2, 0.5, 0), tcl = -0.3)
    axis(side = 1, at = c(seq(0.1, 1, 0.1), 2:10, 20), labels = FALSE, tcl = -0.1)
    axis(side = 2, at = seq(0, 10000, 2500), labels = seq(0, 10, 2.5), mgp = c(2, 0.5, 0), cex.axis = 0.8, tcl = -0.3, las = 1)
    for (i in 1:length(Qps)){
      for(k in 1:length(Qds)){
        sub2 <- filter(sub, Qp == Qps[i], Qd == Qds[k]) %>%
          arrange(Area / TW)
        points(dist_US ~ I(Area / (TW * 3 / 1000)), sub2, col = adjustcolor(cols[i], alpha.f = 0.5), pch = pts[k], cex = 0.8)
      }
    }
  } else{
    plt <- par("plt")
    fig <- par("fig")
    plt[1:2] <- plt[1:2] * (fig[2] - fig[1]) + fig[1]
    plt[3:4] <- plt[3:4] * (fig[4] - fig[3]) + fig[3]
    xl <- plt[2] - (plt[2] - plt[1]) * 0.4
    xr <- plt[2]
    yb <- plt[3]
    yt <- plt[3] + (plt[4] - plt[3]) * 0.25
    par(fig = c(xl, xr, yb, yt), new = TRUE, mar = c(0, 0, 0, 0))
    plot(NA, xlim = c(0.2, 20), ylim = c(-1000, 11000), las = 1, ylab= "", xlab = "", log = "x", cex.axis = 0.8,
         xaxt = "n", yaxt = "n")
    axis(side = 3, at = c(1, 10), cex.axis = 0.8, mgp = c(2, 0.5, 0), tcl = -0.3)
    axis(side = 3, at = c(seq(0.1, 1, 0.1), 2:10, 20), labels = FALSE, tcl = -0.1)
    axis(side = 2, at = seq(0, 10000, 2500), labels = seq(0, 10, 2.5), mgp = c(2, 0.5, 0), cex.axis = 0.8, tcl = -0.3, las = 1)
    for (i in 1:length(Qps)){
      for(k in 1:length(Qds)){
        sub2 <- filter(sub, Qp == Qps[i], Qd == Qds[k]) %>%
          arrange(Area / TW)
        points(dist_US ~ I(Area / (TW * 3 / 1000)), sub2, col = adjustcolor(cols[i], alpha.f = 0.5), pch = pts[k], cex = 0.8)
      }
    }
  }
  
}
mtext("Distance US with Lower WSE [km]", side = 2, outer = TRUE, line = 0)
mtext(expression("Reconnected Floodplain Area ["*km^2*"] / Levee Top Width [km]"), side = 1, outer = TRUE, line = 0.5)
dev.off()

#############################
#Compare effects of fp n
library(vioplot)
n_fp05 <- filter(summary, n_fp == 0.05)
n_fp15 <- filter(summary, n_fp == 0.15)

manning <- left_join(n_fp05, n_fp15, by = c("TW", "S", "Qp", "Qd", "Scenario")) %>%
  mutate(setback_diff = mean_setback.x - mean_setback.y,
         DS_diff = DS_diff.x - DS_diff.y,
         dist_US_diff =  dist_US.y - dist_US.x) #this last one is flipped because both values
                                                #are positive (unlike the others where both values are negative)

png(file.path(base_path, "Results", "Figure 5.png"), type = "cairo", height = 3,
    width = 6.5, res = 500, units = "in")
par(mfrow = c(1, 3), mar = c(3, 4.5, 2, 0.5), oma = c(1, 0, 0, 5), mgp = c(2, 0.8, 0))
cols <- RColorBrewer::brewer.pal(length(slopes) + 1, "GnBu")[2:4]

#Downstream
vioplot(DS_diff ~ S, manning, col = cols, ylim = c(-0.9, 0.9), las = 1, main = "Downstream",
        xlab = "", ylab = "", yaxt = "n")
abline(h = 0, lwd = 2)
vioplot(DS_diff ~ S, manning, col = cols, add = TRUE)

mtext(expression(paste("High FP n ", Delta, "WSE -")), side = 2, line = 3.2, cex = 0.8)
mtext(expression(paste("Low FP n ", Delta, "WSE [m]")), side = 2, line = 2.1, cex = 0.8)
add_label(-0.05, -0.04, "(a)")
axis(side = 2, at = seq(-0.8, 0.8, 0.2), las = 1)
axis(side = 1, at = 1:3, labels = slopes)

#In setback
vioplot(setback_diff ~ S, manning, col = cols, ylim = c(-0.9, 0.9), las = 1,
        main = "Within Setback", xlab = "", ylab = "", yaxt = "n")
abline(h = 0, lwd = 2)
vioplot(setback_diff ~ S, manning, col = cols, add = TRUE)
mtext(expression(paste("High FP n ", Delta, "WSE -")), side = 2, line = 3.2, cex = 0.8)
mtext(expression(paste("Low FP n ", Delta, "WSE [m]")), side = 2, line = 2.1, cex = 0.8)
mtext(side = 1, "Slope", line = 2.5, cex = 0.9)
add_label(-0.05, -0.04, "(b)")
axis(side = 2, at = seq(-0.8, 0.8, 0.2), las = 1)
axis(side = 1, at = 1:3, labels = slopes)

#Upstream
vioplot(I(dist_US_diff / 1000) ~ S, manning, col = cols, ylim = c(-20, 20), las = 1,
        main = "Upstream", xlab = "", ylab = "")
abline(h = 0, lwd = 2)
vioplot(I(dist_US_diff / 1000) ~ S, manning, col = cols, add = TRUE)
mtext("High FP n US Dist. -", side = 2, line = 3.2, cex = 0.8)
mtext("Low FP n US Dist. [km]", side = 2, line = 2.1, cex = 0.8)
add_label(-0.05, -0.04, "(c)")

lines(c(4, 5), c(0, 0), lwd = 2, xpd = NA)
text(x = 4.5, y = 7.5, paste("Higher FP n =\nLarger Diff"), xpd = NA, adj = 0.5, cex = 0.8)
text(x = 4.5, y = -7.5, paste("Lower FP n =\nLarger Diff"), xpd = NA, adj = 0.5, cex = 0.8)

dev.off()

########################
#Compare two scenarios with similar area - long and narrow vs short and wide?
short <- filter(summary, Scenario == "1_4xL_22.5km")
long <- filter(summary, Scenario == "1_12xL_30km")

combined <- left_join(short, long, by = "Run") %>%
  mutate(setback_diff = abs(mean_setback.x) - abs(mean_setback.y),
         DS_diff = abs(DS_diff.x) - abs(DS_diff.y),
         dist_US_diff = dist_US.x - dist_US.y)

png(file.path(base_path, "Results", "Figure S17.png"), type = "cairo", height = 3,
    width = 6.5, res = 500, units = "in")
par(mfrow = c(1, 3), mar = c(3, 4.5, 2, 0.5), oma = c(1, 0, 0, 5), mgp = c(2, 0.8, 0))
cols <- RColorBrewer::brewer.pal(length(slopes) + 1, "GnBu")[2:4]

#Downstream
vioplot(DS_diff ~ S.x, combined, col = cols, ylim = c(-0.2, 0.2), las = 1, main = "Downstream",
        ylab = "", xlab = "")
abline(h = 0, lwd = 2)
vioplot(DS_diff ~ S.x, combined, col = cols, add = TRUE)
mtext(expression(paste("Short & Wide ", Delta, "WSE -")), side = 2, line = 3.4, cex = 0.8)
mtext(expression(paste("Long & Narrow", Delta, "WSE [m]")), side = 2, line = 2.1, cex = 0.8)
add_label(-0.05, -0.04, "(a)")

#In setback
vioplot(setback_diff ~ S.x, combined, col = cols, ylim = c(-0.2, 0.2), las = 1,
        main = "Within Setback", ylab = "", xlab = "")
abline(h = 0, lwd = 2)
vioplot(setback_diff ~ S.x, combined, col = cols, add = TRUE)
mtext(expression(paste("Short & Wide ", Delta, "WSE -")), side = 2, line = 3.4, cex = 0.8)
mtext(expression(paste("Long & Narrow", Delta, "WSE [m]")), side = 2, line = 2.1, cex = 0.8)
mtext(side = 1, "Slope", line = 2.5, cex = 0.9)
add_label(-0.05, -0.04, "(b)")

#Upstream
#par(mar = c(3, 4, 2, 0.5))
vioplot(I(dist_US_diff / 1000) ~ S.x, combined, col = cols, ylim = c(-3, 3), las = 1,
        main = "Upstream", ylab = "", xlab = "")
abline(h = 0, lwd = 2)
vioplot(I(dist_US_diff / 1000) ~ S.x, combined, col = cols, add = TRUE)
mtext("Short & Wide US Dist. -", side = 2, line = 3.2, cex = 0.8)
mtext("Long & Narrow US Dist. [km]", side = 2, line = 2.1, cex = 0.8)
add_label(-0.05, -0.04, "(c)")

lines(c(4, 5), c(0, 0), lwd = 2, xpd = NA)
text(x = 4.5, y = 2, paste("Short & Wide =\nLarger Diff"), xpd = NA, adj = 0.5, cex = 0.8)
text(x = 4.5, y = -2, paste("Long & Narrow =\nLarger Diff"), xpd = NA, adj = 0.5, cex = 0.8)

dev.off()

######################
#Create plot summarizing peak flow attenuation
DS <- filter(attenuation, RS == 0) %>%
  mutate(Run = as.numeric(Run)) %>%
  left_join(inputs_all, by = "Run") %>%
  mutate(Length = if_else(Scenario == "baseline", 0, as.numeric(stringr::str_match(Scenario, "L_(.*?)km")[,2])) * 1000,
         Width_x = if_else(Scenario == "baseline", 0, 1 / as.numeric(stringr::str_match(Scenario, "_(.*?)x")[,2])),
         Width = Width_x * Length,
         Area = (Length - 2 * Width) * Width / 1000^2)


png(file.path(base_path, "Results/Figure S6.png"), type = "cairo", units = "in",
    height = 4, width = 6.5, res = 500)
par(mfrow = c(1, 2), mar = c(4, 3, 1, 0.5), oma = c(0, 1, 0, 0))
cols <- viridis::viridis(3)
vioplot(Q_atten_perc ~ Qp + S, DS, col = cols, las = 1, xlab = "", xaxt = "n",
        ylab = "", main = "Flood Magnitude")
axis(side = 1, at = c(2, 5, 8), labels = c("1e-04", "3e-04", "0.001"))
legend("bottomright", legend = c(2.5, 3.5, 4.5), fill = cols, title = expression("Flood Magnitude (x" * Q[bf]*")"), bty = "n", cex = 0.8)
mtext(side = 2, "Peak Attenuation [%]", line = 2.5)

add_label(-0.05, -0.03, "(a)")

cols <- RColorBrewer::brewer.pal(4, "PuRd")[2:4]
vioplot(Q_atten_perc ~ Qd + S, DS, col = cols, las = 1, ylab = "", xaxt = "n", xlab = "", main = "Flood Duration")
axis(side = 1, at = c(2, 5, 8), labels = c("1e-04", "3e-04", "0.001"))
legend("bottomright", legend = c(10, 30, 60), fill = cols, title = "Flood Duration (Days)", bty = "n", cex = 0.8)

add_label(-0.05, -0.03, "(b)")

mtext("Slope", side = 1, line = -1, outer = TRUE)
dev.off()



