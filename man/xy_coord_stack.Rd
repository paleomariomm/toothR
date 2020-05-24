\name{xy_coord_stack}
\alias{xy_coord_stack}
\title{XY coordinates of outer enamel}
\usage{
data(xy_coord_stack)
}
\description{
List composed of XY coordinates per tooth type.
}
\format{
   A list composed of 8 data frames. Each data frame corresponds with one tooth type.
   \describe{
   \item{\code{tooth_num}}{Number of tooth}
   \item{\code{sample}}{Point of the outer enamel where the XY coordinates has been obtained}
   \item{\code{X}}{X value of the point. Value range is from 0 to 100}
   \item{\code{Y}}{Y value of the point. Value range is from 0 to 100}
   }
}
