\name{toothR}
\alias{toothR}
\title{toothR - Reconstructing slightly worn human teeth}
\usage{
toothR(image_url, tooth_type)
}
\description{
Set a regression equation adapted to
}
\examples{
toothR(image_url = Images/microCT_1.png, tooth_type = "LM")
}
\arguments{
  \item{\code{image_url}}{Set the URL of the image of the tooth you would like to reconstruct}
  \item{\code{tooth_type}}{set the tooth type}
  \item{\code{interval}}{set the interval of the regression: \code{prediction} (default) / \code{confidence}}
  \item{\code{save_svg}}{select if the ggplot should be saved or not as SVG format: \code{yes} (default) / \code{no}}
}
