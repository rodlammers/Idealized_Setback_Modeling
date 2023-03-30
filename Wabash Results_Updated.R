#Wabash Analysis
base_path <- "Runs"
library(dplyr)
source("Plot Functions.R")

#Read in idealized summary results
summary_file <- list.files(file.path(base_path, "Results"), pattern = "Summary_WSE", full.names = TRUE)
summary <- readRDS(summary_file) %>%
  filter(Qd == 60, Qp == 3.5, S %in% c(1e-4, 3e-4), n_fp == 0.05, Area > 0) %>%
  mutate(x_val = Area / (3 * TW / 1000)) %>%
  arrange(x_val)

#This section gets Wabash results from HEC-RAS output files. These files are too
#large and numerous to share, but the code is provided for informational purposes.
    # wabash_path <- "~/WORK/Natural Infrastructure/Levees/Idealized_Models/Wabash/RAS/Wabash_Setbacks_Rev4"
    # p_files <- list.files(wabash_path, pattern = "Rev4.p[0-9]", full.names = T)
    # p_files <- p_files[stringr::str_sub(p_files, -4) != ".hdf"]
    # p_files <- p_files[-77] #remove erroneous sim
    # 
    # plan_names <- sapply(p_files, function(x){
    #   file <- readLines(x)
    #   words <- stringr::str_extract_all(file[3], stringr::boundary("word"))
    #   name <- paste0(words[[1]][3], "_", words[[1]][4])
    #   return(name)
    # })
    # 
    # names(plan_names) <- (1:length(p_files))
    # plan_names <- plan_names[!(stringr::str_detect(plan_names, "R2") | 
    #                              stringr::str_detect(plan_names, "Rev2") |
    #                              stringr::str_detect(plan_names, "Ref2"))] #remove any with R2 or Rev2 or Ref2
    # 
    # results_files <- list.files(wabash_path, pattern = "Rev4.p[0-9]", full.names = T)
    # results_files <- results_files[stringr::str_sub(results_files, -4) == ".hdf"]
    # results_files <- results_files[as.numeric(names(plan_names))] #keep only ones with plan names
    # 
    # get_results <- function(file_name){
    #   
    #   f <- hdf5r::H5File$new(file_name, mode = "r")
    #   
    #   XS <- f[["Geometry/Cross Sections/Attributes"]]$read()
    #   
    #   Q <- f[["Results/Unsteady/Output/Output Blocks/Base Output/Unsteady Time Series/Cross Sections"]][["Flow"]]$read()
    #   WS <- f[["Results/Unsteady/Output/Output Blocks/Base Output/Unsteady Time Series/Cross Sections"]][["Water Surface"]]$read()
    #   V_tot <- f[["Results/Unsteady/Output/Output Blocks/Base Output/Unsteady Time Series/Cross Sections"]][["Velocity Total"]]$read()
    #   V_channel <- f[["Results/Unsteady/Output/Output Blocks/Base Output/Unsteady Time Series/Cross Sections"]][["Velocity Channel"]]$read()
    #   
    #   n_time <- ncol(Q)
    #   
    #   combined <- data.frame(Q = as.integer(round(as.vector(matrix(t(Q), ncol = 1)))),
    #                          WS = round(as.vector(matrix(t(WS), ncol = 1)), 3),
    #                          V_tot = round(as.vector(matrix(t(V_tot), ncol = 1)), 3),
    #                          V_channel = round(as.vector(matrix(t(V_channel), ncol = 1)), 3),
    #                          #RS = as.integer(rep(XS$RS, each = n_time)))
    #                          RS = (rep(XS$RS, each = n_time)),
    #                          River = rep(XS$River, each = n_time)) %>%
    #     filter(River == "Wabash")
    #   #levee = stringr::str_detect(file_name, pattern = "levee"))
    #   
    #   f$close_all()
    #   
    #   return(combined)
    #   
    # }
    # 
    # get_geometry <- function(file_name){
    #   f <- hdf5r::H5File$new(file_name, mode = "r")
    #   
    #   XS <- f[["Geometry/Cross Sections/Attributes"]]$read() %>%
    #     filter(River == "Wabash") %>%
    #     select(L_levee_sta = "Left Levee Sta",
    #            R_levee_sta = "Right Levee Sta",
    #            L_bank = "Left Bank",
    #            R_bank = "Right Bank",
    #            RS)
    #   
    #   f$close_all()
    #   
    #   return(XS)
    # }
    # 
    # results <- purrr::map2(results_files, plan_names, function(x, y){
    #   run <- stringr::str_sub(y, 1, 3)
    #   flood <- stringr::str_sub(y, -3, -1)
    #   flood <- ifelse(run == "8.5", 8.5, 
    #                    ifelse(flood == "_NA", 25, 8.5))
    #   run <- ifelse(run == "SB1" | run == "SB2", run, "Ref")
    #   get_results(x) %>%
    #     mutate(Run = run,
    #            Scenario = y,
    #            Flood = flood)
    # })
    # 
    # max_WSE <- lapply(results, function(x){
    #   group_by(x, Scenario, RS) %>%
    #     summarize(max_WS = max(WS) / 3.28, #convert from ft to m
    #               Run = first(Run),
    #               Flood = first(Flood),
    #               .groups = "keep") %>%
    #     mutate(RS = as.numeric(stringr::str_replace_all(RS, "\\*", ""))) %>%
    #     arrange(RS)
    # })
    # 
    # max_WSE <- do.call("rbind", max_WSE)
    # 
    # saveRDS(max_WSE, "Runs/Wabash/Wabash_max_WSE_03292023.Rds")
    # 
    # WSE_diff <- ungroup(max_WSE) %>%
    #   group_by(Flood) %>%
    #   mutate(WS_baseline = rep(max_WS[Run == "Ref"], length(unique(Scenario)))) %>%
    #   group_by(Scenario) %>%
    #   mutate(WS_diff = max_WS - WS_baseline) %>%
    #   ungroup()
    # 
    # max_Q <- lapply(results, function(x){
    #   group_by(x, Scenario, RS) %>%
    #     summarize(max_Q = max(Q) / 35.3, #convert from cfs to cms
    #               Run = first(Run),
    #               Flood = first(Flood),
    #               .groups = "keep") %>%
    #     mutate(RS = as.numeric(stringr::str_replace_all(RS, "\\*", ""))) %>%
    #     arrange(RS)
    # })
    # 
    # max_Q <- do.call("rbind", max_Q)
    # 
    # Q_diff <- ungroup(max_Q) %>%
    #   group_by(Flood) %>%
    #   mutate(Q_baseline = rep(max_Q[Run == "Ref"], length(unique(Scenario)))) %>%
    #   group_by(Scenario) %>%
    #   mutate(Q_diff = max_Q - Q_baseline) %>%
    #   ungroup()
    # 
    # #Get cross section station-elevation data
    # file1 <- hdf5r::H5File$new(results_files[1], mode = "r")
    # xs_attr <- file1[["Geometry/Cross Sections/Attributes"]]$read()
    # rows <- which(xs_attr$River == "Wabash")
    # xs_indices <- file1[["Geometry/Cross Sections/Station Elevation Info"]]$read()[,rows]
    # xs_data <- file1[["Geometry/Cross Sections/Station Elevation Values"]]$read()
    # xs_data <- xs_data[,(xs_indices[1,1] + 1):ncol(xs_data)]
    # RS_rep <- purrr::map2(xs_indices[2,], rev(filter(WSE_diff, Scenario == "25yr_Ref_NA")$RS), function(x, y){
    #   rep(y, x)})
    # xs_df <- data.frame(Sta = xs_data[1,],
    #                     Elev = xs_data[2,],
    #                     RS = unlist(RS_rep))
    # 
    # bed_z <- group_by(xs_df, RS) %>%
    #   summarize(z = min(Elev) / 3.28)
    # 
    # write.csv(bed_z, "Runs/Wabash/Bed_z.csv", row.names = F)
    # 
    # #For each XS, check if there is a right levee. If not, estimate the edge of the floodplain
    # #as the station with the elevation 15 feet greater than the bank station elevation
    # geom <- purrr::map2(results_files, plan_names, function(x, y){
    #   run <- stringr::str_sub(y, 1, 3)
    #   flood <- stringr::str_sub(y, -3, -1)
    #   flood <- ifelse(run == "8.5", 8.5, 
    #                   ifelse(flood == "_NA", 25, 8.5))
    #   run <- ifelse(run == "SB1" | run == "SB2", run, "Ref")
    #   get_geometry(x) %>%
    #     mutate(Run = run,
    #            Flood = flood,
    #            Scenario = y,
    #            RS = as.numeric(stringr::str_replace_all(RS, "\\*", "")))
    # })
    # 
    # geom <- do.call("rbind", geom)
    # 
    # base <- filter(geom, Scenario == "25yr_Ref_NA") %>%
    #   mutate(R_edge = NA)
    # 
    # for (i in 1:nrow(base)){
    #   RS_i <- base$RS[i]
    #   R_bank <- base$R_bank[i]
    #   
    #   if (is.nan(base$R_levee_sta[i])){
    #     xs <- filter(xs_df, RS == RS_i, Sta >= R_bank)
    #     bank_elev <- min(filter(xs, Sta == R_bank)$Elev)
    #     if (length(bank_elev) > 1){
    #       print(i)
    #     }
    #     index <- min(which(xs$Elev > (bank_elev + 15)))
    #     if(is.infinite(index)){
    #       index <- nrow(xs)
    #     }
    #     base$R_edge[i] <- xs$Sta[index]
    #   }
    # }
    # 
    # base <- base %>%
    #   select(L_levee_base = "L_levee_sta",
    #          R_levee_base = "R_levee_sta",
    #          R_edge,
    #          RS)
    # geom_comb <- left_join(geom, base, by = "RS") %>%
    #   mutate(L_diff = L_levee_base - L_levee_sta,
    #          Area = NA,
    #          Width = ifelse(is.nan(R_levee_sta), R_edge - L_levee_base, 
    #                         R_levee_sta - L_levee_base))
    # 
    # for (i in 2:nrow(geom_comb)){
    #   geom_comb$Area[i] <- (geom_comb$L_diff[i] + geom_comb$L_diff[i - 1]) / 2 * 
    #     (geom_comb$RS[i - 1] - geom_comb$RS[i]) * 5280
    # }
    # 
    # #########################
    # #Combine and summarize
    # results_comb <- left_join(WSE_diff, geom_comb, by = c("Scenario", "Run", "RS", "Flood"))
    # wabash_summary <- group_by(results_comb, Scenario) %>%
    #   summarize(start_RS = max(RS[Area > 0], na.rm = T),
    #             end_RS = min(RS[Area > 0], na.rm = T),
    #             mean_setback = mean(WS_diff[RS <= first(start_RS) & RS >= first(end_RS)]),
    #             DS_diff = mean(WS_diff[RS <= first(end_RS)]),
    #             dist_US = min(RS[which(WS_diff > -0.1 & RS >= first(start_RS))] - first(start_RS)) * 1609.34,
    #             Width = mean(Width[Area > 0], na.rm = T) / 3280, #ft to km
    #             Area = sum(Area, na.rm = TRUE) / (10.7639 * 1000^2), #ft^2 to km2)
    #             rel_area = Area / Width,
    #             L = (start_RS - end_RS) * 1.609, #mi to km
    #             Run = first(Run),
    #             Flood = first(Flood)) %>%
    #   filter(Scenario != "25yr_Ref_NA",
    #          Scenario != "8.5yr_Ref_Rev1_NA")
    # 
    # y25 <- filter(wabash_summary, Flood == 25)
    # y8 <- filter(wabash_summary, Flood == 8.5)
    # 
    # write.csv(y25, "Runs/Results/Wabash_summary.csv", row.names = F)
    # 
    # summary_low <- filter(summary, S == 1e-4)
    # summary_high <- filter(summary, S == 3e-4)

#Gets wabash summary results
wabash_summary <- read.csv("Runs/Wabash/Wabash_summary.csv")

png("Runs/Results/Figure 6.png", type = "cairo", height = 3.5,
    width = 6.5, res = 500, units = "in")
par(mfrow = c(1,3), mar = c(3, 4.1, 2, 0.5), oma = c(1.5, 0, 0, 0))
cols <- RColorBrewer::brewer.pal(3, "Set1")
palette(cols)

#Downstream
plot(DS_diff ~ rel_area, wabash_summary, pch = 21, bg = as.factor(Run), las = 1,
     main = "Downstream", ylab = "WSE Change Downstream [m]", xlim = c(0, 40),
     ylim = c(-0.03, 0.015))

lo <- loess(DS_diff ~ x_val, summary_low, span = 0.2)
pred <- predict(lo, se = T)
lines(summary_low$x_val, pred$fit, lwd = 2)

lo <- loess(DS_diff ~ x_val, summary_high, span = 0.2)
pred <- predict(lo, se = T)
lines(summary_high$x_val, pred$fit, lwd = 2, lty = 2)


points(DS_diff ~ x_val, summary, pch = summary$S * 1e4 + 15, cex = 0.9, col = "gray60")
points(DS_diff ~ rel_area, wabash_summary, pch = 21, bg = as.factor(Run))

lines(x = c(0, 50), y = c(-.5, -.5), lwd = 2)
points(25, -0.5, pch = 16, cex = 0.9, col = "gray60")
text(x = 50, y = -.5, "Idealized Results", pos = 4)

add_label(-0.05, -0.03, "(a)")

legend("bottomleft", legend = c("1e-4", "3e-4"), lty = 1:2, lwd = 2,
       title = "Idealized Slope", pch = c(NA, NA),
       bty = "n", text.col = "white")
legend("bottomleft", legend = c("1e-4", "3e-4"), pch = c(16, 18), col = "gray60", bty = "n",
       lwd = 1, lty = c(0, 0), title = "Idealized Slope")

#In setback
plot(mean_setback ~ rel_area, wabash_summary, pch = 21, bg = as.factor(Run), las = 1, 
     main = "In Setback", ylab = "Mean WSE Change in Setback [m]", xlim = c(0, 40),
     ylim = c(-1, 0))

lo <- loess(mean_setback ~ x_val, summary_low, span = 0.2)
pred <- predict(lo, se = T)
lines(summary_low$x_val, pred$fit, lwd = 2)

lo <- loess(mean_setback ~ x_val, summary_high, span = 0.2)
pred <- predict(lo, se = T)
lines(summary_high$x_val, pred$fit, lwd = 2, lty = 2)

points(mean_setback ~ x_val, summary, pch = summary$S * 1e4 + 15, cex = 0.9, col = "gray60")
points(mean_setback ~ rel_area, wabash_summary, pch = 21, bg = as.factor(Run))

add_label(-0.05, -0.03, "(b)")

#Upstream
plot(I(dist_US / 1000) ~ rel_area, wabash_summary, pch =21, bg = as.factor(Run), las = 1,
     main = "Upstream", ylab = "Distance US with Lower WSE [km]", xlim = c(0, 40),
     ylim = c(0, 40))


lo <- loess(I(dist_US / 1000) ~ x_val, summary_low, span = 0.2)
pred <- predict(lo, se = T)
lines(summary_low$x_val, pred$fit, lwd = 2)

lo <- loess(I(dist_US / 1000) ~ x_val, summary_high, span = 0.2)
pred <- predict(lo, se = T)
lines(summary_high$x_val, pred$fit, lwd = 2, lty = 2)

points(I(dist_US / 1000) ~ x_val, summary, pch = summary$S * 1e4 + 15, cex = 0.9, col = "gray60")
points(I(dist_US / 1000) ~ rel_area, wabash_summary, pch = 21, bg = as.factor(Run))

add_label(-0.05, -0.03, "(c)")

legend("bottomright", legend = c("US", "DS"), pch = 21, pt.bg = cols, title = "Wabash Setback Loc.",
       bty = "n")

mtext(expression("Reconnected Floodplain Area ["*km^2*"] / Levee Top Width [km]"), side = 1, outer = TRUE, line = 0.5)
dev.off()

################################################################################
###############################################################################
#Plot water surface slopes
max_WSE <- readRDS("Runs/Wabash/Wabash_max_WSE_03292023.Rds")
bed_z <- read.csv("Runs/Wabash/Bed_z.csv")

DS <- filter(y25, Scenario == "SB2_25yr_15km_16xBFW_NA")
US <- filter(y25, Scenario == "SB1_25yr_15km_16xBFW_NA")

base_max <- filter(max_WSE, Scenario == "25yr_Ref_NA")
base_slope <- base_max %>%
  ungroup() %>%
  mutate(Sf = abs(c(0, diff(max_WS)) / c(1, diff(RS)*1609)),
         Sf_avg = zoo::rollmean(Sf, k = 3, fill = NA))

abline(v = c(DS$start_RS, DS$end_RS), lty = 2)
abline(v = c(US$start_RS, US$end_RS), lty = 2)

US_slope <- filter(max_WSE, Scenario == "SB1_25yr_15km_16xBFW_NA") %>%
  ungroup() %>%
  mutate(Sf = abs(c(0, diff(max_WS)) / c(1, diff(RS)*1609)),
         Sf_avg = zoo::rollmean(Sf, k = 3, fill = NA))

US_avg_slope <- mean(filter(base_slope, RS <= US$start_RS, RS >= US$end_RS)$Sf_avg)
DS_avg_slope <- mean(filter(base_slope, RS <= DS$start_RS, RS >= DS$end_RS)$Sf_avg)
avg_slope <- mean(base_slope$Sf_avg, na.rm = T)

#Compare US and DS locations
png("Runs/Wabash/Figure S5.png", type = "cairo", height = 3.5,
    width = 6.5, res = 500, units = "in")
par(mfrow = c(1,1), mar = c(4, 4, 0.5, 0.5))
plot(z ~ RS , bed_z, type = "l", las = 1, ylim = c(92, max(max_WSE$max_WS)),
     ylab = "Elevation [m]", xlab = "River Station [km]", xaxt = "n")

rect(DS$end_RS, 0, DS$start_RS, 200, col = "gray80", border = NA)
rect(US$end_RS, 0, US$start_RS, 200, col = "gray80", border = NA)
lines(z ~ RS, bed_z)

abline(v = c(DS$start_RS + DS$dist_US / 1609, US$start_RS + US$dist_US / 1609), lty = 2)

lines(max_WS ~ RS, filter(max_WSE, Scenario == "SB2_25yr_15km_16xBFW_NA"), col = "darkviolet")
lines(max_WS ~ RS, filter(max_WSE, Scenario == "SB1_25yr_15km_16xBFW_NA"), col = "darkviolet")
lines(max_WS ~ RS, filter(max_WSE, Scenario == "25yr_Ref_NA"), col = "blue")

#Add slope values
text(y = 145, x = 0, bquote(bar(S[w])*"="*.(a), list(a = formatC(avg_slope, format = "e", digits = 2))), pos = 4, cex = 0.8)
text(y = 120, x = 70, bquote("DS"~bar(S[w])*"="*.(a), list(a = formatC(DS_avg_slope, format = "e", digits = 2))), pos = 2, cex = 0.8)
text(y = 137, x = 136, bquote("US"~bar(S[w])*"="*.(a), list(a = formatC(US_avg_slope, format = "e", digits = 2))), cex = 0.8)

axis(side = 1, at = seq(0, 350, 50) / 1.6, labels = seq(0, 350, 50))
dev.off()


##########################
#Bankfull determination - ~38,800 at Montezuma gage (1.23 yr return period)
rating <- dataRetrieval::readNWISmeas("03340500", expand = T) %>%
  mutate(chan_width = chan_width / 3.28, #ft to m
         gage_height_va = gage_height_va / 3.28, #ft to m
         discharge_va = discharge_va / 35.3, #cfs to cms
         chan_area = chan_area / (3.28^2)) #ft2 to m2

png("Runs/Wabash/Figure S4.png", type = "cairo", height = 5,
    width = 5, res = 500, units = "in")
par(mfrow = c(2,2), mar = c(4, 4, 0.5, 0.5), mgp = c(2.5, 0.8, 0), oma = c(0, 0, 0.5, 0))
plot(I(chan_width / gage_height_va) ~ discharge_va, rating, log = "xy", pch = 16,
     las = 1, ylab = "Width:Depth", xlab = expression("Q ("*m^3*"/s)"), xaxt = "n")
axis(side = 1, at = c(100, 1000), labels = c(expression(10^2), expression(10^3)))
axis(side = 1, at = outer(1:9, 10^(1:3)), tck = -0.02, labels = F)
abline(v = 38800 / 35.3, lty = 2)
legend("topleft", legend = "(a)", pch = NA, bty = "n", inset = c(-0.15, -0.15),
       xpd = NA)

plot(gage_height_va ~ discharge_va, rating, log = "xy", pch = 16, las = 1,
     ylab = "Stage (m)", xlab = expression("Q ("*m^3*"/s)"), xaxt = "n")
axis(side = 1, at = c(100, 1000), labels = c(expression(10^2), expression(10^3)))
axis(side = 1, at = outer(1:9, 10^(1:3)), tck = -0.02, labels = F)
abline(v = 38800 / 35.3, lty = 2)
legend("topleft", legend = "(b)", pch = NA, bty = "n", inset = c(-0.15, -0.15),
       xpd = NA)

plot(chan_width ~ discharge_va, rating, log = "xy", pch = 16,
     las = 1, ylab = "Width (m)", xlab = expression("Q ("*m^3*"/s)"), xaxt = "n")
axis(side = 1, at = c(100, 1000), labels = c(expression(10^2), expression(10^3)))
axis(side = 1, at = outer(1:9, 10^(1:3)), tck = -0.02, labels = F)
abline(v = 38800 / 35.3, lty = 2)
legend("topleft", legend = "(c)", pch = NA, bty = "n", inset = c(-0.15, -0.15),
       xpd = NA)

plot(chan_area ~ chan_width, rating, log = "xy", pch = 16, 
     las = 1, ylab = expression("Flow Area ("* m^2*")"),
     xlab = "Width (m)")
legend("topleft", legend = "(d)", pch = NA, bty = "n", inset = c(-0.15, -0.15),
       xpd = NA)
dev.off()

####################################################
#Plot discharge changes for two different scenarios
US_diff <- filter(Q_diff, Scenario == "SB1_25yr_15km_8xBFW_NA")
US_diff2 <- filter(Q_diff, Scenario == "SB1_25yr_5km_8xBFW_NA")

plot(Q_diff ~ I(RS*1.6), US_diff, type = "l")
rect(163.85 * 1.6, -10, 166.69 * 1.6, 60, col = "gray80", border = NA)
rect(158.03 * 1.6, -10, 166.69 * 1.6, 60, col = "gray80", border = NA)

lines(Q_diff ~ I(RS*1.6), US_diff2, col = "blue")


#####################################################
#Look at discharge changes more closely

baseline_Q <- results[[6]]
plot(filter(baseline_Q, RS == 0)$Q, type = "l")

DS_setback_Q <- results[[22]]
US_setback_Q <- results[[4]]

lines(filter(DS_setback_Q, RS == 0)$Q, col = "blue")
lines(filter(US_setback_Q, RS == 0)$Q, col = "green")

#Compare inflow and outflow just downstream of setback
plot(filter(baseline_Q, RS == 229.22)$Q, type = "l")
lines(filter(baseline_Q, RS == 158.03)$Q, lty = 2)
lines(filter(US_setback_Q, RS == 158.03)$Q, col = "red")


US_diff <- filter(Q_diff, Scenario == "SB1_25yr_15km_16xBFW_NA")
DS_diff <- filter(Q_diff, Scenario == "SB2_25yr_15km_16xBFW_NA")
plot(Q_diff ~ RS, US_diff, type = "l")
lines(Q_diff ~ RS, DS_diff, col = "blue")

abline(v = c(US$start_RS, DS$start_RS), lty = 2)
abline(v = c(DS$start_RS + DS$dist_US / 1609, US$start_RS + US$dist_US / 1609), col = "red")
