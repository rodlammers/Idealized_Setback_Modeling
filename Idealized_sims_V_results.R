#This code reads in the velocity change results that were saved previously and
#plots them. You must therefore run the code in "Idealized_sims_updated.R"
#before running the code in this file.
######################################################################
library(dplyr)
source("Plot Functions.R")

inputs_all <- read.csv("Inputs_all.csv")
base_path <- "Runs"

v_file <- list.files(file.path(base_path, "Results"), pattern = "Velocity_results_max", full.names = TRUE)

V_diff <- readRDS(v_file)

#Look at velocity
#Summary stats
summary_V <- V_diff %>%
  mutate(Length = if_else(Scenario == "baseline", 0, as.numeric(stringr::str_match(Scenario, "L_(.*?)km")[,2])) * 1000,
         Width_x = if_else(Scenario == "baseline", 0, 1 / as.numeric(stringr::str_match(Scenario, "_(.*?)x")[,2])),
         Width = Width_x * Length,
         Area = (Length - 2 * Width) * Width / 1000^2) %>% #Floodplain area in km2
  group_by(Run, Scenario) %>%
  summarize(end_RS = start_RS - first(Length),
            max_diff = max(V_diff),
            min_diff = min(V_diff),
            mean_setback = mean(V_diff[RS <= start_RS & RS >= (end_RS)]),
            tot_diff = sum(V_diff),
            dist_US = min(RS[which(V_diff < 0.05 & RS > start_RS)] - (start_RS)),
            dist_US = if_else(is.infinite(dist_US), max(RS) - start_RS, dist_US),
            DS_diff = V_diff[RS == 0],
            Length = first(Length),
            Width = first(Width),
            Area = first(Area),
            .groups = "keep") %>%
  left_join(inputs_all, by = "Run") %>%
  mutate(dist_US = if_else(dist_US == -1, L - start_RS, dist_US),
         River_area = Length * TW / 1000 ^ 2)

##Create full plots for each Run - 9 plots per figure, 1 figure per geometry, 9 total geometries
geoms <- unique(inputs_all$Geometry)
for (i in 1:length(geoms)){
  runs <- inputs_all$Run[inputs_all$Geometry == geoms[i]]
  
  png(file.path(base_path, "Results", paste0("Velocity_diff_geom_", geoms[i], "v4.png")), type = "cairo",
      height = 9, width = 9, res = 500, units = "in")
  par(mfrow = c(3, 3), mar = c(3, 3, 1.5, 2))
  for (j in 1:length(runs)){
    plot_run_vel2(V_diff, start_RS = 80000, run = runs[j], inputs_all, yrange = range(V_diff$V_diff), xrange = range(V_diff$RS) / 1000)
  }
  dev.off()
}

#Plotting
##############################
#summary plot of change in peak channel V
cols <- colorspace::lighten(viridis::viridis(3), amount = 0.2)
cols2 <- colorspace::lighten(viridis::viridis(3), amount = 0.5)
png(file.path(base_path, "Results", "Figure 7.png"), type = "cairo", height = 3.5,
    width = 3.5, res = 500, units = "in")
par(mfrow = c(1,1), mar = c(4, 4, 2, 2), oma = c(0, 0, 0, 0), cex = 0.8)

vioplot(min_diff ~ Qp + S, summary_V[summary_V$Area > 0, ], col = cols2,
        las = 1, xlab = "Slope", xaxt = "n", ylab = "",
        at = c(1:3, 4.5:6.5, 8:10), main = "Change in Peak Channel V", ylim = c(-1.6, 1.6),
        side = "left")
abline(h = seq(-1.5, 1.5, 0.5), col = "lightgray", lty = 2)
abline(h = 0, lwd = 2)
vioplot(min_diff ~ Qp + S, summary_V[summary_V$Area > 0, ], col = cols2,
        las = 1,  at = c(1:3, 4.5:6.5, 8:10), add = TRUE, side = "left")
axis(side = 1, at = c(2, 5.5, 9), labels = c(1e-4, 3e-4, 0.001), las = 1)


vioplot(max_diff ~ Qp + S, summary_V[summary_V$Area > 0, ], col = cols,
        las = 1, xaxt = "n",
        at = c(1:3, 4.5:6.5, 8:10), add = T,
        side = "right")

mtext(side = 2, expression("Min and Max" ~ Delta * "V [m/s]"), outer = TRUE, line = -1.2, cex = 0.8)
text(11.5, 0.8, "Upstream", font = 2, adj = 0.5, srt = 90, xpd = NA)
text(11.5, -0.8, "In Setback", font = 2, adj = 0.5, srt = 90, xpd = NA)
lines(c(11, 12), c(0, 0), lwd = 2, xpd = NA)
legend("bottomleft", legend = rev(Qps), fill = rev(cols),
       title = expression("Flood Peak (x" * Q[bf] *")"), bg = "white", 
       box.lwd = 0, inset = c(0.02, 0.02))
dev.off()


slopes <- sort(unique(summary_V$S))
widths <- unique(inputs_all$TW)
roughness <- unique(inputs_all$n_fp)
Qds <- unique(inputs_all$Qd)
Qps <- unique(inputs_all$Qp)

#Changes in velocity increases
TW <- unique(summary_V$TW)
cols <- viridis::viridis(length(Qps))
png(file.path(base_path, "Results", "Figure S7.png"), type = "cairo", height = 3.5,
    width = 6.5, res = 500, units = "in")
pts <- c(16, 15, 18)
par(mfrow = c(1,3), mar = c(3, 3, 2, 0.5), oma = c(1.5, 1.5, 0, 0))
for (j in 1:length(slopes)){
  sub <- filter(summary_V, S == slopes[j], n_fp == 0.05, Area > 0)
  
  par(fig = c(1/3 * (j - 1), 1/3 * j, 0, 1), new = TRUE, mar = c(3, 3, 2, 0.5))
  plot(NA, ylim = range(summary_V$max_diff[summary_V$n_fp == 0.05]), xlim = range(sub$Area / (sub$TW * 3 / 1000)), las = 1, xlab = "", ylab = "",
       main = paste0("Slope = ", slopes[j]))
  
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
  
  add_label(-0.05, -0.03, paste0("(", letters[j], ")"))
  if (j == 1){
    legend("topleft", legend = TW, lty = 1:3, col = cols[1], title = "Top Width (m)", bty = "n", cex = 0.8,
           pch = 21:23, pt.bg = cols[1])
    
  }else if (j == 2){
    legend("topleft", legend = Qps, col = cols, title = expression("Flood Magnitude (x" * Q[bf]*")"), bty = "n", cex = 0.8,
           pch = 21, pt.bg = cols, lty = 1)
  }
  
  #Add subplot
  if (j == 1){
    plt <- par("plt")
    fig <- par("fig")
    plt[1:2] <- plt[1:2] * (fig[2] - fig[1]) + fig[1]
    plt[3:4] <- plt[3:4] * (fig[4] - fig[3]) + fig[3]
    xl <- plt[2] - (plt[2] - plt[1]) * 0.4
    xr <- plt[2]
    yb <- plt[4] - (plt[4] - plt[3]) * 0.25
    yt <- plt[4]
    par(fig = c(xl, xr, yb, yt), new = TRUE, mar = c(0, 0, 0, 0))
    plot(NA, xlim = c(0.2, 20), ylim = c(-0.05, 1.05), las = 1, ylab= "", xlab = "", log = "x", cex.axis = 0.8,
         xaxt = "n", yaxt = "n")
    axis(side = 1, at = c(1, 10), cex.axis = 0.8, mgp = c(2, 0.5, 0), tcl = -0.3)
    axis(side = 1, at = c(seq(0.1, 1, 0.1), 2:10, 20), labels = FALSE, tcl = -0.1)
    axis(side = 2, at = seq(0, 1, 0.2), mgp = c(2, 0.5, 0), cex.axis = 0.8, tcl = -0.3, las = 1)
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
    xl <- plt[2] - (plt[2] - plt[1]) * 0.4
    xr <- plt[2]
    yb <- plt[3]
    yt <- plt[3] + (plt[4] - plt[3]) * 0.25
    par(fig = c(xl, xr, yb, yt), new = TRUE, mar = c(0, 0, 0, 0))
    plot(NA, xlim = c(0.2, 20), ylim = c(-0.05, 1.05), las = 1, ylab= "", xlab = "", log = "x", cex.axis = 0.8,
         xaxt = "n", yaxt = "n")
    axis(side = 3, at = c(1, 10), cex.axis = 0.8, mgp = c(2, 0.5, 0), tcl = -0.3)
    axis(side = 3, at = c(seq(0.1, 1, 0.1), 2:10, 20), labels = FALSE, tcl = -0.1)
    axis(side = 2, at = seq(0, 1, 0.2), mgp = c(2, 0.5, 0), cex.axis = 0.8, tcl = -0.3, las = 1)
    for (i in 1:length(Qps)){
      for(k in 1:length(TWs)){
        sub2 <- filter(sub, Qp == Qps[i], TW == TWs[k]) %>%
          arrange(Area / TW)
        points(max_diff ~ I(Area / (TW * 3 / 1000)), sub2, col = adjustcolor(cols[i], alpha.f = 0.5), pch = pts[k], cex = 0.8)
      }
    }
  }
  
}
mtext("Max V Diff [m/s]", side = 2, outer = TRUE, line = 0)
mtext(expression("Reconnected Floodplain Area ["*km^2*"] / Levee Top Width [km]"), side = 1, outer = TRUE, line = 0.5)
dev.off()

#############################################################################
#Changes in reductions in velocity
TW <- unique(summary_V$TW)
cols <- viridis::viridis(length(Qps))
png(file.path(base_path, "Results", "Figure S8.png"), type = "cairo", height = 3.5,
    width = 6.5, res = 500, units = "in")
pts <- c(16, 15, 18)
par(mfrow = c(1,3), mar = c(3, 3, 2, 0.5), oma = c(1.5, 1.5, 0, 0))
for (j in 1:length(slopes)){
  sub <- filter(summary_V, S == slopes[j], n_fp == 0.05, Area > 0)
  
  par(fig = c(1/3 * (j - 1), 1/3 * j, 0, 1), new = TRUE, mar = c(3, 3, 2, 0.5))
  plot(NA, ylim = range(summary_V$min_diff[summary_V$n_fp == 0.05]), xlim = range(sub$Area / (sub$TW * 3 / 1000)), las = 1, xlab = "", ylab = "",
       main = paste0("Slope = ", slopes[j]))
  
  for (i in 1:length(Qps)){
    for(k in 1:length(TW)){
      sub2 <- filter(sub, Qp == Qps[i], TW == TWs[k]) %>%
        arrange(Area / TW)
      points(min_diff ~ I(Area / (TW * 3 / 1000)), sub2, col = adjustcolor(cols[i], alpha.f = 0.5), pch = pts[k], cex = 0.8)
      
      lo <- loess(min_diff ~ I(Area / (TW * 3 / 1000)), sub2, span = 0.4)
      pred <- predict(lo)
      lines(pred ~ I(sub2$Area / (3 * sub2$TW / 1000)), col = cols[i], lwd = 1, lty = k)
    }
  }
  
  add_label(-0.05, -0.03, paste0("(", letters[j], ")"))
  if (j == 1){
    legend("bottomleft", legend = TW, lty = 1:3, col = cols[1], title = "Top Width (m)", bty = "n", cex = 0.8,
           pch = 21:23, pt.bg = cols[1])
    
  }else if (j == 2){
    legend("bottomleft", legend = Qps, col = cols, title = expression("Flood Size (x" * Q[bf]*")"), bty = "n", cex = 0.8,
           pch = 21, pt.bg = cols, lty = 1)
  }
  
  #Add subplot
  if (j == 3){
    plt <- par("plt")
    fig <- par("fig")
    plt[1:2] <- plt[1:2] * (fig[2] - fig[1]) + fig[1]
    plt[3:4] <- plt[3:4] * (fig[4] - fig[3]) + fig[3]
    xl <- plt[2] - (plt[2] - plt[1]) * 0.4
    xr <- plt[2]
    yb <- plt[4] - (plt[4] - plt[3]) * 0.25
    yt <- plt[4]
    par(fig = c(xl, xr, yb, yt), new = TRUE, mar = c(0, 0, 0, 0))
    plot(NA, xlim = c(0.2, 20), ylim = c(-1.05, 0.05), las = 1, ylab= "", xlab = "", log = "x", cex.axis = 0.8,
         xaxt = "n", yaxt = "n")
    axis(side = 1, at = c(1, 10), cex.axis = 0.8, mgp = c(2, 0.5, 0), tcl = -0.3)
    axis(side = 1, at = c(seq(0.1, 1, 0.1), 2:10, 20), labels = FALSE, tcl = -0.1)
    axis(side = 2, at = seq(0, -1, -0.2), mgp = c(2, 0.5, 0), cex.axis = 0.8, tcl = -0.3, las = 1)
    for (i in 1:length(Qps)){
      for(k in 1:length(TWs)){
        sub2 <- filter(sub, Qp == Qps[i], TW == TWs[k]) %>%
          arrange(Area / TW)
        points(min_diff ~ I(Area / (TW * 3 / 1000)), sub2, col = adjustcolor(cols[i], alpha.f = 0.5), pch = pts[k], cex = 0.8)
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
    plot(NA, xlim = c(0.2, 20), ylim = c(-1.05, 0.05), las = 1, ylab= "", xlab = "", log = "x", cex.axis = 0.8,
         xaxt = "n", yaxt = "n")
    axis(side = 3, at = c(1, 10), cex.axis = 0.8, mgp = c(2, 0.5, 0), tcl = -0.3)
    axis(side = 3, at = c(seq(0.1, 1, 0.1), 2:10, 20), labels = FALSE, tcl = -0.1)
    axis(side = 2, at = seq(0, -1, -0.2), mgp = c(2, 0.5, 0), cex.axis = 0.8, tcl = -0.3, las = 1)
    for (i in 1:length(Qps)){
      for(k in 1:length(TWs)){
        sub2 <- filter(sub, Qp == Qps[i], TW == TWs[k]) %>%
          arrange(Area / TW)
        points(min_diff ~ I(Area / (TW * 3 / 1000)), sub2, col = adjustcolor(cols[i], alpha.f = 0.5), pch = pts[k], cex = 0.8)
      }
    }
  }
  
}
mtext("Min V Diff [m/s]", side = 2, outer = TRUE, line = 0)
mtext(expression("Reconnected Floodplain Area ["*km^2*"] / Levee Top Width [km]"), side = 1, outer = TRUE, line = 0.5)
dev.off()

#############################
#Compare fp n effects
n_fp05 <- filter(summary_V, n_fp == 0.05)
n_fp15 <- filter(summary_V, n_fp == 0.15)

manning <- left_join(n_fp05, n_fp15, by = c("TW", "S", "Qp", "Qd", "Scenario")) %>%
  mutate(setback_diff = abs(mean_setback.x) - abs(mean_setback.y),
         min_diff = abs(min_diff.x) - abs(min_diff.y),
         max_diff = max_diff.x - max_diff.y)

png(file.path(base_path, "Results", "Figure S11.png"), type = "cairo", height = 3.5,
    width = 5.5, res = 500, units = "in")
par(mfrow = c(1, 2), mar = c(3, 2, 2, 0.5), oma = c(1, 2.5, 0, 5), mgp = c(2, 0.8, 0), cex.axis = 0.8)
cols <- RColorBrewer::brewer.pal(length(slopes) + 1, "GnBu")[2:4]

#Compare meean change in setback
vioplot::vioplot(min_diff ~ S, manning, col = cols, las = 1,
                 main = "Within Setback", ylim = c(0, 0.9), ylab = "", xlab = "")
axis(side = 1, at = 1:3, labels = slopes)
abline(h = 0, lwd = 2)
add_label(-0.18, -0.04, "(a)")

vioplot::vioplot(max_diff ~ S, manning, col = cols, las = 1, main = "Upstream",
                 ylim = c(0, 0.9), ylab = "", xlab = "")
abline(h = 0, lwd = 2)
vioplot::vioplot(max_diff ~ S, manning, col = cols, add = T)
axis(side = 1, at = 1:3, labels = slopes)
add_label(-0.18, -0.04, "(b)")

lines(c(4, 5), c(0, 0), lwd = 2, xpd = NA)
text(x = 4.5, y = 0.2, paste("Lower FP n =\nLarger \U0394V"), xpd = NA, adj = 0.5, cex = 0.8)
mtext(expression(paste("Low FP n ", Delta, "V -")), side = 2, line = 0.8, cex = 0.8, outer = T)
mtext(expression(paste("High FP n ", Delta, "V [m/s]")), side = 2, line = 0, cex = 0.8, outer = T)
mtext("Slope", side = 1, outer = TRUE, line = -0.5)
dev.off()


