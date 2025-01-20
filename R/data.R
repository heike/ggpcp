#' Data set: NASA - Data Expo 2006
#'
#' The data are geographic and atmospheric measures on a very coarse
#' 24 by 24 grid covering Central America. This data was provided by
#' the NASA Langley Research Center Atmospheric Sciences Data Center
#' as part of the ASA Data Expo in 2006. Monthly averages of a set of
#' atmospheric measurements are provided for Jan 1995 to Dec 2000.
#' A subset of this data is available from the `GGally` package.
#'
#' @format A data frame with 41472 (= 24 x 24 x 72) rows and 15 variables:
#' @section Structural variables:
#' \describe{
#'     \item{time}{time index for each month from 1 (= Jan 1995) to 72 (= Dec 2000)}
#'     \item{id}{identifier for each grid point 1-1 to 24-24}
#'     \item{lat, long}{geographic latitude and longitude}
#'     \item{elevation}{altitude of the location in meters above sea level}
#'     \item{month, year, date}{year/month of each measurement}
#'   }
#' @section Measured variables:
#' \describe{
#'   \item{cloudlow, cloudmid, cloudhigh}{Cloud cover (in percent) at low, middle, and high levels.}
#'   \item{ozone}{mean ozone abundance (in dobson)}
#'   \item{pressure}{mean surface pressure (in millibars)}
#'   \item{surftemp, temperature}{mean surface/near surface air temperature (in Kelvin)}
#' }
#'
#' @source \url{https://community.amstat.org/jointscsg-section/dataexpo/dataexpo2006}
#' @docType data
#' @name nasa
#' @usage nasa
#' @examples
#' data(nasa)
#' library(ggplot2)
#' nasa |>
#'   dplyr::filter(id == "1-10") |>
#'   pcp_select(starts_with("cloud"), ozone, temperature) |>
#'   pcp_scale() |>
#'   ggplot(aes_pcp()) +
#'  geom_pcp(aes(colour=month))
"nasa"

#' Data set: Assessment of Carcinoma slides
#'
#' A differently formatted data is set available as `carcinoma` in package `poLCA`.
#' Here, pathologists' ratings are recorded
#' @format A data frame with 118 rows and 9 variables:
#' @section Overall structure:
#' \describe{
#'   \item{No}{slide number 1 through 126 (data for slides 14, 20, 21, 50, 75, 97, 109, and 125 are missing)}
#'   \item{Average}{average rating of all eight pathologists.}
#' }
#' @section Pathologist ratings:
#' \describe{
#'   \item{A}{scores 1 to 5 of pathologist's A evaluation (1) Negative; (2) Atypical Squamous Hyperplasia; (3) Carcinoma in Situ; (4) Squamous Carcinoma with Early Stromal Invasion; (5) Invasive Carcinoma.}
#'   \item{B}{scores by pathologist B.}
#'   \item{C}{scores by pathologist C.}
#'   \item{D}{scores by pathologist D.}
#'   \item{E}{scores by pathologist E.}
#'   \item{F}{scores by pathologist F.}
#'   \item{G}{scores by pathologist G.}
#' }
#' @source
#' Data published as Table 1 in Landis, J. Richard, and  Koch, Gary G. "An Application of Hierarchical Kappa-type Statistics in the Assessment of Majority Agreement among Multiple Observers." Biometrics 33.2 (1977): 363-74, \doi{10.2307/2529786}.
#'
#' Study and Design in Holmquist, Nelson D., McMahan C.A., Williams O. Dale. Variability in classification of carcinoma in situ of the uterine cervix. Arch Pathol. 1967 Oct;84(4):334-45. PMID: 6045443, \doi{10.1097/00006254-196806000-00023}.
#' @docType data
#' @name Carcinoma
#' @usage Carcinoma
#' @examples
#' library(ggplot2)
#' Carcinoma |>
#'   pcp_select(F, D, C, A, G, E, B, Average) |>
#'   pcp_scale(method="uniminmax") |>
#'   pcp_arrange() |>
#'   ggplot(aes_pcp()) +
#'     geom_pcp_axes() +
#'     geom_pcp(aes(colour = Average > 2)) +
#'     geom_pcp_boxes(colour="black", alpha=0) +
#'     geom_pcp_labels(aes(label = pcp_level), fill="white", alpha = 1) +
#'     theme_bw() +
#'     scale_x_discrete(expand = expansion(add=0.25)) +
#'     xlab("Pathologist") + ylab("Carcinoma score 1 (Negative) to 5 (Invasive Carcinoma)") +
#'     theme(axis.text.y=element_blank(), axis.ticks.y=element_blank(), legend.position="none")
"Carcinoma"
