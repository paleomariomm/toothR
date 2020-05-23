toothR <- function(image_url, tooth_type = c("UI", "UC", "UP", "UM", "LI", "LC", "LP", "LM")) {
  # Load required packages
  require(imager)
  require(grid)
  require(ggplot2)

  # Load data of the polynomial regressions
  #xy_coord_stack <- data("data/xy_coord_stack.rda")

  # Check if all the arguments are present
  if(missing(image_url)) stop("A valid image address is required")
  if(missing(tooth_type)) stop("A tooth type is required")

  # Check if tooth_type has one of the valid values
  tooth_type <- match.arg(tooth_type)

  # XY proportion per tooth type

  index_tooth <- list(UI = 0.5928,
                      LI = 0.6583,
                      UC = 0.5980,
                      LC = 0.5840,
                      UP = 0.6406,
                      LP = 0.6290,
                      UM = 0.7456,
                      LM = 0.6698)

  # Select points in the image
  img <- load.image(image_url)
  X11()
  plot(img)
  mtext("Step 1. Select dentine horn (1 click)", side = 3, line = 1.5)
  cusp_tip <- locator(n = 1, type = "o", col = "blue")
  points(cusp_tip, pch = 4, col = "blue", cex = 2)
  abline(h = cusp_tip[[2]], col = "red")
  mtext("Step 2. Select outer enamel surface over the red line (1 click)", line = 0.5)
  out_enamel <- locator(n = 1, type = "o", col = "blue")
  points(out_enamel, pch = 4, col = "blue", cex = 2)
  # Data frame of XY coordinates of cusp_tip and outer_enamel
  dh <-unlist(cusp_tip)
  oe <-unlist(out_enamel)
  landmark_XY <- data.frame(rbind(dh, oe))
  # Get image dimensions and isolate some coordinates from landmark_XY
  img_wd_px <- dim(img)[1] # image width in pixels
  img_hg_px <- dim(img)[2] # image height in pixels
  xpr100 <- landmark_XY[2,1]    # X coordinate of the Xprotoconid 100
  xpr0   <- landmark_XY[1,1]    # X coordinate of the Xprotoconid 0 (dentine horn)
  ypr0   <- landmark_XY[1,2]    # Y coordinate of the Yprotoconid 0 (dentine horn)

  # Resize and position of background image
  rG <- rasterGrob(img, interpolate = F,
                  hjust = (xpr0 + (xpr100 - xpr0)/2) / img_wd_px,
                  vjust = 1 - (ypr0 / img_hg_px) + ((((xpr100-xpr0)/img_hg_px) * index_tooth[[tooth_type]]) / 2),
                  width = 1 / ((xpr100 - xpr0) / img_wd_px))

  # Plot regression with the fitted microCT image of the tooth
  gg <- ggplot(data.frame(xy_coord_stack[[tooth_type]]), aes(x = X, y = Y)) +
    coord_fixed(ratio = index_tooth[[tooth_type]]) + # aquí poner la proporción por diente
    annotation_custom(rG, xmin = 0, xmax=Inf,
                      ymin = 0, ymax=Inf) +
    geom_point(alpha = .20, size = 1, shape = 19, color = "red") +
    stat_smooth(method = 'lm', formula = y ~ poly(x, 3)) +
    theme_minimal() +
    geom_hline(aes(yintercept = 0), color = "red", lty = 2) +
    geom_vline(aes(xintercept = 0), color = "red", lty = 2) +
    scale_x_continuous(limits = c(0, 100), expand = c(0, 0)) +
    scale_y_continuous(limits = c(0, 100), expand = c(0, 0))

  # Return a list of objects
  return(list("XY_Coordinates" = landmark_XY, "ggplot" = gg))
}
