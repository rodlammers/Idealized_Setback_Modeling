#Test out different spatial configurations of levee setbacks - all with same geometry and total setback area
#S = 1e-4, b = 100, Area = 15 km^2, Qp = 3.5, Qd = 10 days
library(dplyr)
source("HEC-RAS Automation Functions.R")
source("Plot Functions.R")

base_path <- "Runs"

vars <- data.frame(
  n = 0.03, #Channel Manning's n
  TW = 100, #Channel top width (m)
  S = 1e-4, #Channel slope (assumed same for fp)
  h = 5, #Fixed channel depth of 4 m
  z = 4, #Channel bank side slope (H:V)
  w_fp = 60000,#Channel floodplain width (m) - total, split between right and left banks
  z_fp = 100000, #Floodplain lateral slope (H:V)
  n_fp = 0.05 #Floodplain Manning's n
)

inputs_all <- vars %>%
  mutate(XS_spacing = 250,
         L = 125000,
         b = TW - 2 * h * z, #Channel top width (m)
         A_bf = (TW + b) / 2 * h, #Cross section area (m2)
         P_bf = b + 2 * h * sqrt(1 + z ^ 2), #Wetted perimeter (m)
         Q_bf = 1 / n * (A_bf / P_bf) ^ (2/3) * S ^ 0.5 * A_bf, #Bankfull discharge (cms)
         L = ceiling(L / XS_spacing) * XS_spacing, #round L to multiple of XS_spacing)
         Geometry = as.character(1:n()),
         Qp = 3.5,
         Qd = 10)


run <- "Spatial_configs"
dir.create(file.path(base_path, run))
start_RS <- 80000 #start river station - same for all scenarios

i <- 1
#Get file names
flow_file_in <- file.path(base_path, case_when(inputs_all$Qd[i] == 10 ~ "BASELINE_1D_10day.u01",
                                               inputs_all$Qd[i] == 30 ~ "BASELINE_1D_30day.u01",
                                               inputs_all$Qd[i] == 60 ~ "BASELINE_1D_60day.u01"))

flow_file_out <- file.path(base_path, run, paste0(run, "_baseline.u01"))

geom_file_in <- file.path(base_path, "BASELINE_1D_1day.g01")
geom_file_out <- file.path(base_path, run, paste0(run, "_baseline.g01"))

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

#All setbacks should have total area of 15 km2
A <- 15 * 1e6 #sq m
w <- c(1000, 2000)
L <- A / w + 2 * w
L2 <- (A / 2) / w[1] + 2 * w[1]

scenarios_df <- list(data.frame(start_RS_RB = NA, #1 Short and Fat
                                start_RS_LB = 80000,
                                length_RB = NA,
                                length_LB = L[2],
                                width_RB = NA,
                                width_LB = w[2]),
                     data.frame(start_RS_RB = 80000, #2 2-sided
                                start_RS_LB = 80000,
                                length_RB = L2,
                                length_LB =L2,
                                width_RB = w[1],
                                width_LB = w[1]),
                     data.frame(start_RS_RB = NA, #3 Long and Skinny
                                start_RS_LB = 80000,
                                length_RB = NA,
                                length_LB = L[1],
                                width_RB = NA,
                                width_LB = w[1]),
                     data.frame(start_RS_RB = NA, #4 Stepped
                                start_RS_LB = 80000,
                                length_RB = NA,
                                length_LB = 16000,
                                width_RB = NA,
                                width_LB = 1/4 * (16000 - sqrt(16000 ^ 2 - 8 * A/2))),
                     data.frame(start_RS_RB = NA, #5 Alternating gap
                                start_RS_LB = c(80000, 80000 - 2 * L2),
                                length_RB = NA,
                                length_LB = rep(L2, 2),
                                width_RB = NA,
                                width_LB = rep(w[1], 2)),
                     data.frame(start_RS_RB = 80000 - L2, #5 Alternating 2-sided
                                start_RS_LB = 80000,
                                length_RB = L2,
                                length_LB = L2,
                                width_RB = w[1],
                                width_LB = w[1]))

scenarios_df <- lapply(scenarios_df, function(x, L, xs_spacing){
  x <- x %>%
    mutate(start_xs_RB = (L - start_RS_RB) / xs_spacing,
           start_xs_LB = (L - start_RS_LB) / xs_spacing)
}, inputs_all$L[1], inputs_all$XS_spacing[1])
                     

scenarios <- 1:length(scenarios_df)

levees_scenarios_list <- list()

for (j in 1:length(scenarios)){
  levees_scenarios <- modify_levees2(levees = levees, angle = 26, 
                                    start_xs_RB = scenarios_df[[j]]$start_xs_RB,
                                    start_xs_LB = scenarios_df[[j]]$start_xs_LB,
                                    length_RB = scenarios_df[[j]]$length_RB,
                                    length_LB = scenarios_df[[j]]$length_LB,
                                    width_RB = scenarios_df[[j]]$width_RB,
                                    width_LB = scenarios_df[[j]]$width_LB,
                                    xs_spacing = inputs_all$XS_spacing[1])
  
  #Send modified levees to function again to add stepped setback
  if (j == 4){
    levees_scenarios <- modify_levees2(levees = levees_scenarios, angle = 26,
                                       start_xs_RB = NA,
                                       start_xs_LB = (inputs_all$L[1] - (80000 - scenarios_df[[j]]$length_LB / 4)) / inputs_all$XS_spacing[1],
                                       length_RB = NA,
                                       length_LB = scenarios_df[[j]]$length_LB / 2,
                                       width_RB = NA,
                                       width_LB = 1/4 * (scenarios_df[[j]]$length_LB / 2 - sqrt((scenarios_df[[j]]$length_LB / 2) ^ 2 - 8 * A/2)),
                                       #width_LB = 1000,
                                       xs_spacing = inputs_all$XS_spacing[1])
                                      
  }
  
  #Save levee stations for later
  levees_scenarios_list[[j]] <- levees_scenarios
  
  geom_file_out <- file.path(base_path, run, paste0(run, "_", scenarios[j], ".g01"))
  geometry_input(geom_file_in, geom_file_out, inputs_all[i,], levees = levees_scenarios)

  flow_file_out <- file.path(base_path, run, paste0(run, "_", scenarios[j], ".u01"))
  flow_input(flow_file_in, flow_file_out, Q, inputs_all$S[i], inputs_all$L[i])

  #Copy over plan and prj files
  if (inputs_all$Qd[i] == 10){
    file.copy(file.path(base_path, "BASELINE_1D_10day.prj"), file.path(base_path, run, paste0(run, "_", scenarios[j], ".prj")))
    file.copy(file.path(base_path, "BASELINE_1D_10day.p01"), file.path(base_path, run, paste0(run, "_", scenarios[j], ".p01")))
  }else if(inputs_all$Qd[i] == 30) {
    file.copy(file.path(base_path, "BASELINE_1D_30day.prj"), file.path(base_path, run, paste0(run, "_", scenarios[j], ".prj")))
    file.copy(file.path(base_path, "BASELINE_1D_30day.p01"), file.path(base_path, run, paste0(run, "_", scenarios[j], ".p01")))
  }else{
    file.copy(file.path(base_path, "BASELINE_1D_60day.prj"), file.path(base_path, run, paste0(run, "_", scenarios[j], ".prj")))
    file.copy(file.path(base_path, "BASELINE_1D_60day.p01"), file.path(base_path, run, paste0(run, "_", scenarios[j], ".p01")))
  }
  
}

#Create text file with simulation file paths - this will be used by the Excel VBA code to actually run HEC-RAS
folders <- path.expand(list.files(base_path, pattern = "Spatial_configs", full.names = TRUE))
files <- lapply(folders, list.files, pattern = ".prj", full.names = TRUE)
all_paths <- matrix(file.path(getwd(), unlist(files)),
                    ncol = 1)

write.table(all_paths, file.path(base_path, "INPUT FILES SPATIAL_CONFIGS.txt"), col.names = FALSE, row.names = FALSE, quote = FALSE)

####################################################################################
####STOP - YOU NEED TO RUN ALL HEC-RAS SIMULATIONS BEFORE CONTINUING################
####USE THE PROVIDED EXCEL FILE WITH VBA CODE#######################################
####################################################################################

#Get Results
output_files <- unlist(lapply(folders, list.files, pattern = ".p01.hdf", full.names = TRUE))
results_configs_list <- lapply(output_files, function(x){
  config <- stringr::str_match(x, "configs_(.*?).p")[,2]
  get_results(x) %>%
    mutate(config = config,
           Run = 1,
           Scenario = config)
})

results_configs <- do.call("rbind", results_configs_list)

max_WSE <- group_by(results_configs, config, RS) %>%
  summarize(max_WS = max(WS),
            .groups = "keep") %>%
  mutate(RS = as.numeric(as.character(RS)))

WSE_diff <- ungroup(max_WSE) %>%
  mutate(WS_baseline = rep(max_WS[config == "baseline"], length(unique(config)))) %>%
  group_by(config) %>%
  mutate(WS_diff = max_WS - WS_baseline)

max_V <- group_by(results_configs, config, RS) %>%
  summarize(max_V = max(V_channel),
            .groups = "keep") %>%
  mutate(RS = as.numeric(as.character(RS)))

V_diff <- ungroup(max_V) %>%
  mutate(V_baseline = rep(max_V[config == "baseline"], length(unique(config)))) %>%
  group_by(config) %>%
  mutate(V_diff = max_V - V_baseline,
         V_p_diff = V_diff / V_baseline)

sed_results <- lapply(results_configs_list, sed_capacity, mutate(inputs_all, Run = 1))

sed_results2 <- do.call("rbind", sed_results) %>%
  group_by(Run) %>%
  mutate(Qt_baseline = rep(Qt_cum[Scenario == "baseline"], length(unique(Scenario))),
         diff_Qt = Qt_cum / Qt_baseline,
         param_baseline = rep(param_cum[Scenario == "baseline"], length(unique(Scenario))),
         diff_param = param_cum / param_baseline)

#Plots - show levee setback configurations next to/on top of results plots
configs <- unique(WSE_diff$config)[1:6] #reorder from most to least delta WSE

TW <- 100
wfp <- 5000
l <- inputs_all$L[1] / 1000
n_xs <- inputs_all$XS_spacing[1] / 1000

bank_lines <- data.frame(RB_x = seq(0, l, n_xs),
                         RB_y = TW / 2,
                         LB_x = seq(0, l, n_xs),
                         LB_y = -TW / 2)

summary <- WSE_diff %>%
  group_by(config) %>%
  summarize(mean_diff = mean(WS_diff),
          DS_diff = WS_diff[RS == 0],
          .groups = "keep")

#Large plot showing spatial changes in WSE, velocity, and sed transport for all
#configurations
colors <- RColorBrewer::brewer.pal(length(configs) + 1, "YlOrRd")[2:(length(configs) + 1)]

png(file.path(base_path, "Results/Figure S16.png"), type = "cairo", units = "in",
    height = 6.5, width = 12, res = 500)
layout(mat = matrix(1:24, ncol = 4, byrow = TRUE), widths = c(0.9, 1, 1, 1), heights = rep(1, 6))
par(oma = c(1.5, 2, 1.5, 1), mgp = c(2, 0.6, 0))
for (i in 1:length(configs)){
  
  #Plot river schematic
  par(mar = c(2, 1, 0.5, 0.5))
  plot(NA, xlim = c(40, l), ylim = c(-(wfp + TW) / 2, (wfp + TW) / 2 * 0.5), las = 1, ylab = "", 
       xlab = "", yaxt = "n")
  axis(side = 2, at = seq(-2500, 1000, 500), labels = FALSE, las = 1, tcl = -0.2)
  axis(side = 2, at = seq(-2000, 1000, 1000), labels = seq(-2, 1, 1), tick = TRUE, las = 1)
  
  arrows(x0 = 100, y0 = 750, x1 = 85, y1 = 750, length = 0.1)
  text(x = 100, y = 750, "Flow", pos = 4, cex = 0.9)
  
  index <- as.numeric(configs[i])
  levees_scenarios <- levees_scenarios_list[[index]]
  
  lty <- 1
  lines(bank_lines$RB_x, rev(levees_scenarios_list[[i]]$RB_sta) + bank_lines$RB_y, col = colors[i], lwd = 2, lty = lty)
  lines(bank_lines$LB_x, -rev(levees_scenarios_list[[i]]$LB_sta) + bank_lines$LB_y, col = colors[i], lwd = 2, lty = lty)
  
  lines(RB_y ~ RB_x, bank_lines, lwd = 1)
  lines(LB_y ~ LB_x, bank_lines, lwd = 1)
  
  text(35, -2000, paste0("#", i), pos = 4)
  add_label(xfrac = -0.02, yfrac = -0.09, label = paste0("(", letters[i], ")"))
  
  if (i == 1){
    title("Levee Setback", line = 1, xpd = NA, cex.main = 1.2)
  }
  
  #Plot WSE curves
  yrange <- range(WSE_diff$WS_diff)
  xrange <- range(WSE_diff$RS) / 1000
  
  par(mar = c(2, 0.5, 0.5, 2.5))
  plot(NA, xlim = c(40, max(xrange)), ylim = yrange, las = 1,
       ylab = "", xlab = "", yaxt = "n")
  axis(side = 4, las = 1, at = seq(0, -1, -0.25), labels = F, tcl = -0.2)
  axis(side = 4, las = 1, at = seq(0, -1, -0.5), las = 1)
  abline(h = 0, lwd = 2, col = "gray60", lty = 3)
  
  add_label(xfrac = -0.02, yfrac = -0.09, label = paste0("(", letters[i + 6], ")"))
  add_label(xfrac = 1, yfrac = 1.1, label = "[m]")
  
  sub <- filter(WSE_diff, config == configs[i]) %>%
    arrange(RS)
  lines(WS_diff ~ I(RS / 1000), sub, col = colors[i], lwd = 2)
  legend("bottomright", legend = bquote("Max" ~ Delta * "WSE" * "=" * .(a), list(a = round(min(sub$WS_diff), 2))), pch = NA, bty = "n")
  legend("bottomleft", legend = bquote("DS"~ Delta * "WSE" * "=" * .(a), list(a = round(sub$WS_diff[sub$RS == 0], 2))), pch = NA, bty = "n",
         inset = c(-0.05, 0))
  
  text(-40, -0.1, paste0("#", i), pos = 4)
  if (i == 1){
    title("Change in Max WSE", line = 1, xpd = NA, cex.main = 1.2)
  }
  
  #Plot vel curves
  yrange <- range(V_diff$V_diff)
  xrange <- range(V_diff$RS) / 1000
  
  plot(NA, xlim = c(40, max(xrange)), ylim = yrange, las = 1, yaxt = "n",
       ylab = "", xlab = "")
  axis(side = 4, las = 1, at = seq(-0.3, 0.6, 0.1), labels = F, tcl = -0.2)
  axis(side = 4, las= 1, at = seq(-0.3, 0.6, 0.3))
  abline(h = 0, lwd = 2, col = "gray60", lty = 3)
  
  add_label(xfrac = -0.02, yfrac = -0.09, label = paste0("(", letters[i + 12], ")"))
  add_label(xfrac = 1, yfrac = 1.1, label = "[m/s]")
  
  sub <- filter(V_diff, config == configs[i]) %>%
    arrange(RS)
  lines(V_diff ~ I(RS / 1000), sub, col = colors[i], lwd = 2)
  
  legend("topright", legend = sapply(c(bquote("Max" ~ Delta * "V=" *.(a), list(a = round(max(sub$V_diff), 2))),
                                   bquote("Min" ~ Delta * "V=" *.(a), list(a = round(min(sub$V_diff), 2)))), as.expression),
         pch = NA, bty = "n")
  
  if (i == 1){
    title("Change in Max Vel.", line = 1, xpd = NA, cex.main = 1.2)
  }
  
  #Plot sed transport curves
  yrange <- range(sed_results2$diff_param, na.rm = TRUE)
  xrange <- range(sed_results2$RS) / 1000
  
  plot(NA, xlim = c(40, max(xrange)), ylim = yrange, las = 1,
       ylab = "", xlab = "", yaxt = "n", log = "y")
  axis(side = 4, las = 1, at = c(1/ 4:2, 2:4), labels = F, tcl = -0.2)
  axis(side = 4, las = 1, at = c(1/3, 1, 3), labels = c("1/3", "1", "3"))
  abline(h = 1, lwd = 2, col = "gray60", lty = 3)
  
  add_label(xfrac = -0.02, yfrac = -3.5, label = paste0("(", letters[i + 18], ")"))
  add_label(xfrac = 1, yfrac = 0.35, label = "[-]")
  
  sub <- filter(sed_results2, Scenario == configs[i]) %>%
    arrange(RS)
  lines(diff_param ~ I(RS / 1000), sub, col = colors[i], lwd = 2)
  
  legend("topright", legend = as.expression(bquote("Max" ~ Q[ss]/Q[sb] * "=" *.(a), list(a = round(max(sub$diff_param, na.rm = TRUE), 1)))), 
                                            pch = NA, bty = "n")
  legend("bottomright", legend = as.expression(bquote("Min" ~ Q[ss]/Q[sb] * "=" *.(a), list(a = paste0("1/", round(1 / min(sub$diff_param, na.rm = TRUE), 1))))),
         pch = NA, bty = "n")
  
  if (i == 1){
    title("Qs Ratio", line = 1, xpd = NA, cex.main = 1.2)
  }
}

mtext(side = 2, "Cross Section Station [km]", outer = TRUE, line = 0.8)
mtext(side = 4, "Change in Max WSE [m], Max Vel. [m/s], Qs Ratio [-]", outer = TRUE, line = -0.3)

mtext(side = 1, "River Station [km]", outer = TRUE, line = 0)
dev.off()
