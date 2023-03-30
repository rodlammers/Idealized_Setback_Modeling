
#calculate cumulative sediment transport capacity at each RS for the flood event
#using Engelund and Hanson total load stream power equation. Only look at in-channel
#transport capacity. 
library(dplyr)
library(vioplot)
source("HEC-RAS Automation Functions.R")
source("~/WORK/R Functions/Plot Functions.R")

base_path <- "Runs"
inputs_all <- read.csv("Inputs_all.csv")
inputs_condensed <- select(inputs_all, Run, TW, b, z, h, S, Qd)

######################
#Only run this section if you have the full results file saved

results_file <- list.files(file.path(base_path, "Results"), pattern = "Full_results", full.names = TRUE)
results <- readRDS(results_file)
sed_results <- lapply(results, sed_capacity, inputs_condensed)

sed_results2 <- do.call("rbind", sed_results) %>%
  group_by(Run) %>%
  mutate(Qt_baseline = rep(Qt_cum[Scenario == "baseline"], length(unique(Scenario))),
         diff_Qt = Qt_cum / Qt_baseline) %>%
  select(-param_cum)

         #param_baseline = rep(param_cum[Scenario == "baseline"], length(unique(Scenario))),
         #diff_param = param_cum / param_baseline,
         #ratio_baseline = rep(Qt_ratio[Scenario == "baseline"], length(unique(Scenario))),
         #diff_ratio = Qt_ratio - ratio_baseline)

#Save sediment results as Rds file
saveRDS(sed_results2, file.path(base_path, paste0("Results/Sed_results_v4", Sys.Date(), ".Rds")))
########################################

#Read in sediment results (instead of having to re-run the above code)
sed_file <- list.files(file.path(base_path, "Results"), pattern = "Sed_results_v", full.names = TRUE)
sed_results2 <- readRDS(sed_file)

##Create full plots for each Run - 9 plots per figure, 1 figure per geometry, 9 total geometries
geoms <- unique(inputs_all$Geometry)
for (i in 1:length(geoms)){
  runs <- inputs_all$Run[inputs_all$Geometry == geoms[i]]
  
  png(file.path(base_path, "Results", paste0("Sed_diff_geom_", geoms[i], "v4.png")), type = "cairo",
      height = 9, width = 9, res = 500, units = "in")
  par(mfrow = c(3, 3), mar = c(3, 3, 1.5, 2))
  for (j in 1:length(runs)){
    plot_sed(sed_results2, start_RS = 80000, run = runs[j], inputs_all, yrange = range(sed_results2$diff_Qt, na.rm = TRUE), xrange = range(sed_results2$RS) / 1000)
  }
  dev.off()
}

#Summary stats
start_RS <- 80000 #RS of start of setbacks
summary_sed <- sed_results2 %>%
  mutate(Length = if_else(Scenario == "baseline", 0, as.numeric(stringr::str_match(Scenario, "L_(.*?)km")[,2])) * 1000,
         Width_x = if_else(Scenario == "baseline", 0, 1 / as.numeric(stringr::str_match(Scenario, "_(.*?)x")[,2])),
         Width = Width_x * Length,
         Area = (Length - 2 * Width) * Width / 1000^2) %>% #Floodplain area in km2
  group_by(Run, Scenario) %>%
  summarize(end_RS = start_RS - first(Length),
            max_diff = max(diff_Qt, na.rm = TRUE),
            min_diff = min(diff_Qt, na.rm = TRUE),
            mean_setback = mean(diff_Qt[RS <= start_RS & RS >= (end_RS)], na.rm = TRUE),
            prod_Qt = prod(diff_Qt, na.rm = TRUE),
            reach_ratio = sum(Qt_baseline, na.rm = T) / sum(Qt_cum, na.rm = T), #Reach total setback capacity over baseline 
                                                                                #(>1 means setback is net deposition, <1 net erosion)
            Length = first(Length),
            Width = first(Width),
            Area = first(Area),
            River_area = Length * inputs_all$TW[1] / 1000 ^ 2,
            .groups = "keep") %>%
  mutate(range_diff = max_diff / min_diff) %>%
  left_join(inputs_all, by = "Run")# %>%
  #mutate(dist_US = if_else(dist_US == -1, L - start_RS, dist_US))

slopes <- sort(unique(summary_sed$S))
widths <- unique(inputs_all$TW)
roughness <- unique(inputs_all$n_fp)
Qds <- unique(inputs_all$Qd)
Qps <- unique(inputs_all$Qp)

####################################################
#Summary plot
###################################
cols <- colorspace::lighten(viridis::viridis(3), amount = 0.2)
cols2 <- colorspace::lighten(viridis::viridis(3), amount = 0.5)
png(file.path(base_path, "Results", "Figure 8.png"), type = "cairo", height = 3.5,
    width = 6.5, res = 500, units = "in")
par(mfrow = c(1,2), mar = c(4, 4, 2, 2), oma = c(0, 0, 0, 0), cex = 0.8)

vioplot(min_diff ~ Qp + S, summary_sed[summary_sed$Area > 0, ], col = cols2,
        las = 1, yaxt = "n", xlab = "", xaxt = "n", ylab = "",
        at = c(1:3, 4.5:6.5, 8:10), main = "Qs Ratios", ylog = TRUE, ylim = c(0.08, 12.5),
        side = "left")
abline(h = c(0.1, 0.2, 0.5, 1, 2, 5, 10), col = "lightgray", lty = 2)
abline(h = 1, lwd = 2)
vioplot(min_diff ~ Qp + S, summary_sed[summary_sed$Area > 0, ], col = cols2,
        las = 1,  at = c(1:3, 4.5:6.5, 8:10), add = TRUE, side = "left")
axis(side = 1, at = c(2, 5.5, 9), labels = c(1e-4, 3e-4, 0.001), las = 1)

axis(side = 2, at = c(0.1, 0.2, 0.5, 1, 2, 5, 10), labels = c("1/10", "1/5", "1/2", "1", "2", "5", "10"), las = 1)

vioplot(max_diff ~ Qp + S, summary_sed[summary_sed$Area > 0, ], col = cols,
        las = 1, xaxt = "n",
        at = c(1:3, 4.5:6.5, 8:10), ylog = TRUE, add = T,
        side = "right")

mtext(side = 2, "Min and Max Qs Ratio [-]", outer = TRUE, line = -1, cex = 0.8)
text(11.5, 4, "Upstream", font = 2, adj = 0.5, srt = 90, xpd = NA)
text(11.5, 0.25, "In Setback", font = 2, adj = 0.5, srt = 90, xpd = NA)
lines(c(11, 12), c(1, 1), lwd = 2, xpd = NA)
text(0.5, 20, "(a)", xpd = NA)

#Add reach ratio plot
vioplot(reach_ratio ~ Qp + S, summary_sed[summary_sed$Area > 0, ], col = cols,
        las = 1, xaxt = "n", xlab = "", ylab = "Reach Total Qs Ratio [-]",
        at = c(1:3, 4.5:6.5, 8:10), main = "Reach Wide", ylim = c(0.85, 1.15))
axis(side = 2, at = c(2, 5.5, 9), labels = slopes)

abline(h=1, lwd = 2)
vioplot(reach_ratio ~ Qp + S, summary_sed[summary_sed$Area > 0, ], col = cols,
        las = 1, xaxt = "n", xlab = "",
        at = c(1:3, 4.5:6.5, 8:10), add = T)
axis(side = 1, at = c(2, 5.5, 9), labels = c(1e-4, 3e-4, 0.001), las = 1)
legend("bottomright", legend = rev(Qps), fill = rev(cols),
       title = expression("Flood Peak (x" * Q[bf] *")"), bty = "n")
mtext("Slope", side = 1, line = -1, outer = TRUE)
text(11.5, 1.07, "Net Deposition", font = 2, adj = 0.5, srt = 90, xpd = NA)
text(11.5, 0.93, "Net Erosion", font = 2, adj = 0.5, srt = 90, xpd = NA)
lines(c(11, 12), c(1, 1), lwd = 2, xpd = NA)
text(0.5, 1.18, "(b)", xpd = NA)

dev.off()

########################################################
#Max Qs ratio, by slope
TWs <- unique(summary_sed$TW)
cols <- viridis::viridis(length(Qps))
png(file.path(base_path, "Results", "Figure S9.png"), type = "cairo", height = 3.5,
    width = 6.5, res = 500, units = "in")
pts <- c(16, 15, 18)
par(mfrow = c(1,3), mar = c(3, 3, 2, 0.5), oma = c(1.5, 1.5, 0, 0))
for (j in 1:length(slopes)){
  sub <- filter(summary_sed, S == slopes[j], n_fp == 0.05, Area > 0)
  
  par(fig = c(1/3 * (j - 1), 1/3 * j, 0, 1), new = TRUE, mar = c(3, 3, 2, 0.5))
  plot(NA, ylim = c(1,13), xlim = range(sub$Area / (sub$TW * 3 / 1000)), las = 1, xlab = "", ylab = "",
       main = paste0("Slope = ", slopes[j]), log = "y")
  axis(side = 2, at = 1:13, tcl = -0.1, labels = F)
  
  for (i in 1:length(Qps)){
    for(k in 1:length(TW)){
      sub2 <- filter(sub, Qp == Qps[i], TW == TWs[k]) %>%
        arrange(Area / TW)
      points(max_diff ~ I(Area / (TW * 3 / 1000)), sub2, col = adjustcolor(cols[i], alpha.f = 0.5), pch = pts[k], cex = 0.8)
      
      lo <- loess(max_diff ~ I(Area / (TW * 3 / 1000)), sub2, span = 0.4)
      pred <- predict(lo)
      lines(pred ~ I(sub2$Area / (3 * sub2$TW / 1000)), col = cols[i], lwd = 1, lty = k)
    }
  }
  
  add_label(-0.05, -12, paste0("(", letters[j], ")"))
  if (j == 1){
    legend("bottomright", legend = TW, lty = 1:3, col = cols[1], title = "Top Width (m)", bty = "n", cex = 0.8,
           pch = 21:23, pt.bg = cols[1])
    
  }else if (j == 2){
    legend("bottomright", legend = Qps, col = cols, title = expression("Flood Magnitude (x" * Q[bf]*")"), bty = "n", cex = 0.8,
           pch = 21, pt.bg = cols, lty = 1)
  }
  
  #Add subplot
  if (j != 1){
    plt <- par("plt")
    fig <- par("fig")
    plt[1:2] <- plt[1:2] * (fig[2] - fig[1]) + fig[1]
    plt[3:4] <- plt[3:4] * (fig[4] - fig[3]) + fig[3]
    xl <- plt[2] - (plt[2] - plt[1]) * 0.4
    xr <- plt[2]
    yb <- plt[4] - (plt[4] - plt[3]) * 0.25
    yt <- plt[4]
    par(fig = c(xl, xr, yb, yt), new = TRUE, mar = c(0, 0, 0, 0))
    plot(NA, xlim = c(0.2, 20), ylim = c(1, 3.6), las = 1, ylab= "", xlab = "", log = "xy", cex.axis = 0.8,
         xaxt = "n", yaxt = "n")
    axis(side = 1, at = c(1, 10), cex.axis = 0.8, mgp = c(2, 0.5, 0), tcl = -0.3)
    axis(side = 1, at = c(seq(0.1, 1, 0.1), 2:10, 20), labels = FALSE, tcl = -0.1)
    axis(side = 2, at = seq(1, 4, 1), mgp = c(2, 0.5, 0), cex.axis = 0.8, tcl = -0.3, las = 1)
    for (i in 1:length(Qps)){
      for(k in 1:length(TWs)){
        sub2 <- filter(sub, Qp == Qps[i], TW == TWs[k]) %>%
          arrange(Area / TW)
        points(max_diff ~ I(Area / (TW * 3 / 1000)), sub2, col = adjustcolor(cols[i], alpha.f = 0.5), pch = pts[k], cex = 0.8)
      }
    }
  } else{
    plt <- par("plt")
    fig <- par("fig")
    plt[1:2] <- plt[1:2] * (fig[2] - fig[1]) + fig[1]
    plt[3:4] <- plt[3:4] * (fig[4] - fig[3]) + fig[3]
    xl <- plt[1]
    xr <- plt[1] + (plt[2] - plt[1]) * 0.4
    yb <- plt[4] - (plt[4] - plt[3]) * 0.25
    yt <- plt[4]
    par(fig = c(xl, xr, yb, yt), new = TRUE, mar = c(0, 0, 0, 0))
    plot(NA, xlim = c(0.2, 20), ylim = c(1, 3.6), las = 1, ylab= "", xlab = "", log = "xy", cex.axis = 0.8,
         xaxt = "n", yaxt = "n")
    axis(side = 1, at = c(1, 10), cex.axis = 0.8, mgp = c(2, 0.5, 0), tcl = -0.3)
    axis(side = 1, at = c(seq(0.1, 1, 0.1), 2:10, 20), labels = FALSE, tcl = -0.1)
    axis(side = 4, at = seq(1, 4, 1), mgp = c(2, 0.5, 0), cex.axis = 0.8, tcl = -0.3, las = 1)
    for (i in 1:length(Qps)){
      for(k in 1:length(TWs)){
        sub2 <- filter(sub, Qp == Qps[i], TW == TWs[k]) %>%
          arrange(Area / TW)
        points(max_diff ~ I(Area / (TW * 3 / 1000)), sub2, col = adjustcolor(cols[i], alpha.f = 0.5), pch = pts[k], cex = 0.8)
      }
    }
  }
  
}
mtext("Max Qs Ratio", side = 2, outer = TRUE, line = 0)
mtext(expression("Reconnected Floodplain Area ["*km^2*"] / Levee Top Width [km]"), side = 1, outer = TRUE, line = 0.5)
dev.off()

##############################
#Min Q ratio, by slope
TWs <- unique(summary_sed$TW)
cols <- viridis::viridis(length(Qps))
png(file.path(base_path, "Results", "Figure S10.png"), type = "cairo", height = 3.5,
    width = 6.5, res = 500, units = "in")
pts <- c(16, 15, 18)
par(mfrow = c(1,3), mar = c(3, 3, 2, 0.5), oma = c(1.5, 1.5, 0, 0))
for (j in 1:length(slopes)){
  sub <- filter(summary_sed, S == slopes[j], n_fp == 0.05, Area > 0)
  
  par(fig = c(1/3 * (j - 1), 1/3 * j, 0, 1), new = TRUE, mar = c(3, 3, 2, 0.5))
  plot(NA, ylim = c(1/13, 1), xlim = range(sub$Area / (sub$TW * 3 / 1000)), las = 1, xlab = "", ylab = "",
       main = paste0("Slope = ", slopes[j]), log = "y", yaxt = "n")
  axis(side = 2, at = 1/(1:13), labels = F, tcl = -0.1)
  axis(side = 2, at = c(1, 1/2, 1/5, 1/10), labels = c("1", "1/2", "1/5", "1/10"), las = 1)
  
  for (i in 1:length(Qps)){
    for(k in 1:length(TW)){
      sub2 <- filter(sub, Qp == Qps[i], TW == TWs[k]) %>%
        arrange(Area / TW)
      points(min_diff ~ I(Area / (TW * 3 / 1000)), sub2, col = adjustcolor(cols[i], alpha.f = 0.5), pch = pts[k], cex = 0.8)
      
      lo <- loess(min_diff ~ I(Area / (TW * 3 / 1000)), sub2, span = 0.5)
      pred <- predict(lo)
      lines(pred ~ I(sub2$Area / (3 * sub2$TW / 1000)), col = cols[i], lwd = 1, lty = k)
    }
  }
  
  add_label(-0.05, -1, paste0("(", letters[j], ")"))
  if (j == 3){
    legend("bottomleft", legend = TW, lty = 1:3, col = cols[1], title = "Top Width (m)", bty = "n", cex = 0.8,
           pch = 21:23, pt.bg = cols[1])
    legend("bottomright", legend = Qps, col = cols, title = expression("Flood Magnitude (x" * Q[bf]*")"), bty = "n", cex = 0.8,
           pch = 21, pt.bg = cols, lty = 1)
  }
  
  #Add subplot
  #if (j != 1){
    plt <- par("plt")
    fig <- par("fig")
    plt[1:2] <- plt[1:2] * (fig[2] - fig[1]) + fig[1]
    plt[3:4] <- plt[3:4] * (fig[4] - fig[3]) + fig[3]
    xl <- plt[2] - (plt[2] - plt[1]) * 0.4
    xr <- plt[2]
    yb <- plt[4] - (plt[4] - plt[3]) * 0.25
    yt <- plt[4]
    par(fig = c(xl, xr, yb, yt), new = TRUE, mar = c(0, 0, 0, 0))
    plot(NA, xlim = c(0.2, 20), ylim = c(1/4, 1.1), las = 1, ylab= "", xlab = "", log = "xy", cex.axis = 0.8,
         xaxt = "n", yaxt = "n")
    axis(side = 1, at = c(1, 10), cex.axis = 0.8, mgp = c(2, 0.5, 0), tcl = -0.3)
    axis(side = 1, at = c(seq(0.1, 1, 0.1), 2:10, 20), labels = FALSE, tcl = -0.1)
    axis(side = 2, at = 1/(1:4), mgp = c(2, 0.5, 0), cex.axis = 0.8, tcl = -0.3, las = 1,
         labels = c("1", "1/2", "1/3", "1/4"))
    for (i in 1:length(Qps)){
      for(k in 1:length(TWs)){
        sub2 <- filter(sub, Qp == Qps[i], TW == TWs[k]) %>%
          arrange(Area / TW)
        points(min_diff ~ I(Area / (TW * 3 / 1000)), sub2, col = adjustcolor(cols[i], alpha.f = 0.5), pch = pts[k], cex = 0.8)
      }
    }

}
mtext("Min Qs Ratio", side = 2, outer = TRUE, line = 0)
mtext(expression("Reconnected Floodplain Area ["*km^2*"] / Levee Top Width [km]"), side = 1, outer = TRUE, line = 0.5)
dev.off()

#########
#Effects of FP roughness
n_fp05 <- filter(summary_sed, n_fp == 0.05)
n_fp15 <- filter(summary_sed, n_fp == 0.15)

manning <- left_join(n_fp05, n_fp15, by = c("TW", "S", "Qp", "Qd", "Scenario")) %>%
  mutate(min_diff = abs(1/min_diff.x - 1/min_diff.y),
         max_diff = max_diff.x - max_diff.y, 
         min_diff_ratio = min_diff.y / min_diff.x,
         max_diff_ratio = max_diff.x / max_diff.y)

png(file.path(base_path, "Results", "Figure S12.png"), type = "cairo", height = 3.5,
    width = 5.5, res = 500, units = "in")
par(mfrow = c(1, 2), mar = c(3, 2, 2, 0.5), oma = c(1, 2.5, 0, 5), mgp = c(2, 0.8, 0), cex.axis = 0.8)
cols <- RColorBrewer::brewer.pal(length(slopes) + 1, "GnBu")[2:4]

#Compare meean change in setback
vioplot::vioplot(min_diff_ratio ~ S, manning, col = cols, las = 1,
        main = "Within Setback", ylim = c(1, 2.6), ylab = "", xlab = "")
axis(side = 1, at = 1:3, labels = slopes)
abline(h = 1, lwd = 2)
add_label(-0.18, -0.04, "(a)")

vioplot::vioplot(max_diff_ratio ~ S, manning, col = cols, las = 1, main = "Upstream",
                 ylim = c(1, 2.6), ylab = "", xlab = "")
abline(h = 1, lwd = 2)
vioplot::vioplot(max_diff_ratio ~ S, manning, col = cols, add = T)
axis(side = 1, at = 1:3, labels = slopes)
add_label(-0.18, -0.04, "(b)")

lines(c(4, 5), c(1, 1), lwd = 2, xpd = NA)
text(x = 4.5, y = 1.3, paste("Lower FP n =\nLarger \U0394Qs"), xpd = NA, adj = 0.5, cex = 0.8)
mtext(expression(frac("Low FP n Qs Ratio", "High FP n Qs Ratio")), side = 2, outer = TRUE, line = 0)
mtext("Slope", side = 1, outer = TRUE, line = -0.5)
dev.off()

