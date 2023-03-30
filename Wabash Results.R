#Wabash Analysis

#Read in idealized summary results
summary_file <- list.files(file.path(base_path, "Results"), pattern = "Summary_WSE", full.names = TRUE)
summary <- readRDS(summary_file) %>%
  filter(Qd == 60, Qp == 4.5, S == 1e-4, n_fp == 0.05, Area > 0) %>%
  mutate(x_val = Area / (3 * TW / 1000)) %>%
  arrange(x_val)

#Get Wabash Results
wabash_area <- read.csv(file.path(base_path, "Wabash/Wabash_setback_area.csv")) %>%
  tidyr::pivot_longer(!RS, names_to = "Scenario", values_to = "Area") %>%
  group_by(Scenario) %>%
  summarize(start_RS = RS[first(which(Area > 0))],
            end_RS = RS[last(which(Area > 0))],
            Area = sum(Area))

wabash_wse <- read.csv(file.path(base_path, "Wabash/Wabash_WSE_diff.csv")) %>%
  tidyr::pivot_longer(!RS, names_to = "Scenario", values_to = "WS_diff") %>%
  mutate(#RS = RS * 1609.34, #convert from mi to m
    WS_diff = WS_diff / 3.28) %>% #convert from ft to m
  left_join(wabash_area, by = c("Scenario"))

#Get average channel top width within each setback area
wabash_TW <- read.csv(file.path(base_path, "Wabash/Wabash_bank_stations.csv")) %>%
  mutate(RB = as.numeric(if_else(RB == "Bridge", NA_character_, RB)),
         TW = LB - RB,
         L_levee = as.numeric(if_else(L_levee == "Bridge", NA_character_, L_levee)),
         levee_width = R_levee - L_levee) %>%
  right_join(read.csv(file.path(base_path, "Wabash/Wabash_setback_area.csv")) %>%
               tidyr::pivot_longer(!RS, names_to = "Scenario", values_to = "Area")) %>%
  group_by(Scenario) %>%
  summarize(TW = mean(TW[Area > 0]) / 3.28,
            levee_width = mean(levee_width[Area > 0]) / 3.28) #convert to m

#Summary stats
wabash_wse_summary <- wabash_wse %>%
  group_by(Scenario) %>%
  summarize(mean_setback = mean(WS_diff[RS <= first(start_RS) & RS >= first(end_RS)]),
            DS_diff = mean(WS_diff[RS <= first(end_RS) & RS > 79.78]),
            dist_US = min(RS[which(WS_diff > -0.1 & RS > first(start_RS))] - first(start_RS)) * 1609.34,
            Area = first(Area) / 247.105, #acres to km2
            L = as.numeric(stringr::str_match(first(Scenario), "L(.*?)_")[,2]),
            W = as.numeric(stringr::str_match(first(Scenario), "W(.*?)$")[,2])) %>%
  left_join(wabash_TW, by = "Scenario") %>%
  filter(W != 30)

source("Plot Functions.R")
png(file.path(base_path, "Results", "Wabash_WSE.png"), type = "cairo", height = 3.5,
    width = 6.5, res = 500, units = "in")
par(mfrow = c(1,3), mar = c(3, 4.1, 2, 0.5), oma = c(1.5, 0, 0, 0))
cols <- rev(viridis::rocket(length(unique(wabash_wse_summary$L))))
palette(cols)
#Downstream
plot(DS_diff ~ I(Area / levee_width * 1000), wabash_wse_summary, pch = 21, bg = as.factor(L), las = 1,
     main = "Downstream", ylab = "WSE Change Downstream [m]", xlim = c(0, 375))

lo <- loess(DS_diff ~ x_val, summary, span = 0.2)
pred <- predict(lo, se = T)

lines(summary$x_val, pred$fit, lwd = 2)
points(DS_diff ~ x_val, summary, pch = 16, cex = 0.9, col = "gray60")
points(DS_diff ~ I(Area / levee_width * 1000), wabash_wse_summary, pch = 21, bg = as.factor(L))

lines(x = c(0, 50), y = c(-.5, -.5), lwd = 2)
points(25, -0.5, pch = 16, cex = 0.9, col = "gray60")
text(x = 50, y = -.5, "Idealized Results", pos = 4)

add_label(-0.05, -0.03, "(a)")

#In setback
plot(mean_setback ~ I(Area / levee_width * 1000), wabash_wse_summary, pch = 21, bg = as.factor(L), las = 1, 
     main = "In Setback", ylab = "Mean WSE Change in Setback [m]", xlim = c(0, 375))

lo <- loess(mean_setback ~ x_val, summary, span = 0.2)
pred <- predict(lo, se = T)

lines(summary$x_val, pred$fit, lwd = 2)
points(mean_setback ~ x_val, summary, pch = 16, cex = 0.9, col = "gray60")
points(mean_setback ~ I(Area / levee_width * 1000), wabash_wse_summary, pch = 21, bg = as.factor(L))

add_label(-0.05, -0.03, "(b)")

#Upstream
plot(I(dist_US / 1000) ~ I(Area / levee_width * 1000), wabash_wse_summary, pch =21, bg = as.factor(L), las = 1,
     main = "Upstream", ylab = "Distance US with Lower WSE [km]", xlim = c(0, 375))


lo <- loess(I(dist_US / 1000) ~ x_val, summary, span = 0.2)
pred <- predict(lo, se = T)

lines(summary$x_val, pred$fit, lwd = 2)
points(I(dist_US / 1000) ~ x_val, summary, pch = 16, cex = 0.9, col = "gray60")
points(I(dist_US / 1000) ~ I(Area / levee_width * 1000), wabash_wse_summary, pch = 21, bg = as.factor(L))

add_label(-0.05, -0.03, "(c)")

legend("bottomright", legend = levels(as.factor(unique(wabash_wse_summary$L))), pch = 21, pt.bg = cols, title = "Wabash R. Results\nSetback Length [km]",
       bty = "n")

mtext(expression("Reconnected Floodplain Area ["*km^2*"] / Levee Top Width [km]"), side = 1, outer = TRUE, line = 0.5)
dev.off()


