#sys.frame(1)$ofile

#' @param xfrac The fraction over from the left side.
#' @param yfrac The fraction down from the top.
#' @param label The text to label with.
#' @param pos Position to pass to text()
#' @param ... Anything extra to pass to text(), e.g. cex, col.
add_label <- function(xfrac, yfrac, label, pos = 4, ...) {
  u <- par("usr")
  x <- u[1] + xfrac * (u[2] - u[1])
  y <- u[4] - yfrac * (u[4] - u[3])
  text(x, y, label, pos = pos, xpd = NA, ...)
}

range02 <- function(x)(x + max(abs(x)))/(2 * max(abs(x)))

range01 <- function(x)(x-min(x))/diff(range(c(x, max(abs(x)))))

range03 <- function(x)(x-min(x))/diff(range(c(x, max(x))))

cRamp <- function(x, palette, alpha = 1){
  #cols <- colorRamp(c("red", "gray40", "blue"))(range02(x))
  #if (sum(x < 0) == length(x)){
    range <- range03(x)
  # }else{
  #   range <- range01(x)
  # }
  if (length(palette) > 1){
    cols <- colorRamp(palette)(range)
    cols <- apply(cols, 1, function(xt)rgb(xt[1], xt[2], xt[3], maxColorValue = 255))
    cols <- adjustcolor(cols, alpha.f = alpha)
  }else{
    if (tolower(palette) %in% c("viridis", "magma", "inferno", "plasma", "cividis")){
      cols <- colorRamp(viridis::viridis_pal(option = tolower(palette))(10))(range)
      cols <- apply(cols, 1, function(xt)rgb(xt[1], xt[2], xt[3], maxColorValue=255))
      cols <- adjustcolor(cols, alpha.f = alpha)
    }else if( palette == "custom"){
      cols <- colorRamp(colorspace::diverge_hcl(n = length(range), c = c(100, 0), 
                                               l = c(50, 90)))(range)
      cols <- apply(cols, 1, function(xt)rgb(xt[1], xt[2], xt[3], maxColorValue=255))
      cols <- adjustcolor(cols, alpha.f = alpha)
    }else{
      cols <- suppressWarnings(colorRamp(RColorBrewer::brewer.pal(11, palette))(range))
      cols <- apply(cols, 1, function(xt)rgb(xt[1], xt[2], xt[3], maxColorValue=255))
      cols <- adjustcolor(cols, alpha.f = alpha)
    }
  
  }
  return(cols)
}

cRamp_legend <- function(x, palette, alpha = 1){
  range <- seq(0, 1, length.out = x)
  
  if (tolower(palette) %in% c("viridis", "magma", "inferno", "plasma", "cividis")){
    cols <- colorRamp(viridis::viridis_pal(option = tolower(palette))(10))(range)
    cols <- apply(cols, 1, function(xt)rgb(xt[1], xt[2], xt[3], maxColorValue=255))
    cols <- adjustcolor(cols, alpha.f = alpha)

  }else{
    cols <- suppressWarnings(colorRamp(RColorBrewer::brewer.pal(11, palette))(range))
    cols <- apply(cols, 1, function(xt)rgb(xt[1], xt[2], xt[3], maxColorValue=255))
    cols <- adjustcolor(cols, alpha.f = alpha)
  }
  
  return(cols)
}

plot_colors <- function(alpha = 1, plot = FALSE, type = 1){
  colors <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2",
              "#D55E00", "#CC79A7", "#999999")
  
  colors <- adjustcolor(colors, alpha.f = alpha)
  
  names(colors) <- c("orange", "sky blue", "bluish green", "yellow", "blue",
                     "vermillion", "reddish purple", "gray")
  
  if (type == 2){
    colors <- c("#ff7f00","#1f78b4","#ffff33","#a6cee3","#33a02c","#e31a1c")
    colors <- adjustcolor(colors, alpha.f = alpha)
  }
  
  if (plot){
    par(mfrow = c(1,1), mar = c(0,0,0,0))
    plot(NA, xlim = c(0,1), ylim = c(0, length(colors) + 1), xaxt = "n",
         yaxt = "n", xlab = "", ylab = "", axes = FALSE)
    for (i in 1:length(colors)){
      lines(0:1, rep(i, 2), lwd = 3, col = colors[i])
      points(0.5, i, pch = 21, bg = colors[i], cex = 2)
      text(0.2, i, names(colors)[i], pos = 3)
    }
  }
  

  return(colors)
}

rodplot <- function(x = NULL, ..., col = "gray60"){

  if (!exists("outcex")){
    outcex = 1.3
  }
  
  #outbg <- adjustcolor(col, alpha.f = 0.7)
  
  boxplot(x, medlwd = 2, outpch = 21, col = col,
           whisklty = 1, staplelty = 0, outbg = adjustcolor(col, alpha.f = 0.7), ...)
}

lighter <- function(color, factor = 0.2) {
  ## converts color to hsv, multiplies v by factor, returns colors as hexcode
  x = rgb2hsv(col2rgb(color))
  v = pmax(pmin(x[3, ] + factor, 1), 0)
  s = pmax(pmin(x[2, ] + factor, 1), 0)
  hsv(h = x[1, ], s = s, v = v)
}

log_axis <- function(labels = TRUE, at = NULL, ...){
  
  #Create axis labels
  if (labels){
    labels <- do.call("expression", lapply(log10(at), function(i) bquote(10^.(i))))
  }
  
  axis(labels = labels, at = at, ...)
}


#Horizontal legend. From: https://stackoverflow.com/questions/21262472/adjust-spacing-between-text-in-horizontal-legend
f.horlegend <- function(pos, legend, xoff = 0, yoff = 0, 
                        lty = 0, lwd = 1, ln.col = 1, seg.len = 0.04, 
                        pch = NA, pt.col = 1, pt.bg = NA, pt.cex = par("cex"), pt.lwd = lwd, 
                        text.cex = par("cex"), text.col = par("col"), text.font = NULL, text.vfont = NULL, 
                        bty = "o", bbord = "black", bbg = par("bg"), blty = par("lty"), blwd = par("lwd"), bdens = NULL, bbx.adj = 0, bby.adj = 0.75 
) {
  
  ### get original par values and re-set them at end of function
  op <- par(no.readonly = TRUE)
  on.exit(par(op))
  
  ### new par with dimension [0,1]
  par(new=TRUE, xaxs="i", yaxs="i", xpd=TRUE)
  plot.new()
  
  ### spacing between legend elements
  d0 <- 0.01 * (1 + bbx.adj)
  d1 <- 0.01
  d2 <- 0.02
  pch.len <- 0.008
  ln.len <- seg.len/2
  
  n.lgd <- length(legend)
  
  txt.h <- strheight(legend[1], cex = text.cex, font = text.font, vfont = text.vfont) *(1 + bby.adj)
  i.pch <- seq(1, 2*n.lgd, 2)
  i.txt <- seq(2, 2*n.lgd, 2)
  
  ### determine x positions of legend elements
  X <- c(d0 + pch.len, pch.len + d1, rep(strwidth(legend[-n.lgd])+d2+pch.len, each=2))
  X[i.txt[-1]] <- pch.len+d1
  
  ### adjust symbol space if line is drawn
  if (any(lty != 0)) {
    lty <- rep(lty, n.lgd)[1:n.lgd]
    ln.sep <- rep(ln.len - pch.len, n.lgd)[lty]
    ln.sep[is.na(ln.sep)] <- 0
    X <- X + rep(ln.sep, each=2)
    lty[is.na(lty)] <- 0
  } 
  
  X <- cumsum(X)
  
  ### legend box coordinates
  bstart <- 0
  bend <- X[2*n.lgd]+strwidth(legend[n.lgd])+d0
  
  ### legend position
  if (pos == "top" | pos == "bottom" | pos == "center") x_corr <- 0.5 - bend/2 +xoff
  if (pos == "bottomright" | pos == "right" | pos == "topright") x_corr <- 1. - bend + xoff
  if (pos == "bottomleft" | pos == "left" | pos == "topleft") x_corr <- 0 + xoff
  
  if (pos == "bottomleft" | pos == "bottom" | pos == "bottomright") Y <- txt.h/2 + yoff
  if (pos == "left" | pos == "center" | pos =="right") Y <- 0.5 + yoff
  if (pos == "topleft" | pos == "top" | pos == "topright") Y <- 1  - txt.h/2 + yoff
  
  Y <- rep(Y, n.lgd)
  ### draw legend box
  if (bty != "n") rect(bstart+x_corr, Y-txt.h/2, x_corr+bend, Y+txt.h/2, border=bbord, col=bbg, lty=blty, lwd=blwd, density=bdens)
  
  ### draw legend symbols and text
  segments(X[i.pch]+x_corr-ln.len, Y, X[i.pch]+x_corr+ln.len, Y, col = ln.col, lty = lty, lwd = lwd)
  points(X[i.pch]+x_corr, Y, pch = pch, col = pt.col, bg = pt.bg, cex = pt.cex, lwd = pt.lwd)
  text(X[i.txt]+x_corr, Y, legend, pos=4, offset=0, cex = text.cex, col = text.col, font = text.font, vfont = text.vfont)
  
}
