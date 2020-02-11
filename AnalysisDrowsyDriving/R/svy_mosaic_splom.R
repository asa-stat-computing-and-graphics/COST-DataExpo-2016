#' Create a grid of 2x2 survey-weighted mosaic plots
#'
#' @param data a data frame
#' @param factors a vector of character strings corresponding to factor-type variables from the data frame
#' @param weight a character string indicating the name of the weight variable in the data frame.
#' @param svydesignObj a survey::svydesign object, used to create bivariate ssurvey::svyglm models and color the corresponding mosaics red/blue if there is a statistically significant correlation between the two variables
#' @param mar_par a numeric vector of length 4 controlling the margins of the plots
#' @return A grid of 2x2 mosaics, one for each pair of the input factors
#' @examples
#' data(list = c("GES2013.drivers.design", "GES2013.drivers"))
#' factors <- c("DROWSY", "HEAVY_TRUCK",  "INT_HWY",  "SEX_IM", "SPEEDREL")
#' svy_mosaic_splom(data = GES2013.drivers, factors = factors, weight = "WEIGHT", svydesignObj = GES2013.drivers.design)
#' @export
svy_mosaic_splom <- function (data,
                          factors = c(),
                          weight = "WEIGHT",
                          svydesignObj,
                          mar_par = rep(0.5, 4))
{
  require(grid)
  require(vcd)

  options(survey.lonely.psu="adjust")

  data$DUMMY <- 1
  factors <- c(weight, factors, "DUMMY")
  counter <- 1
  ind <- (length(factors)-2)
  mosaic.list <- vector("list", length = ind^2)
  for (i in 1:ind)
  {
    for (j in ind:1)
    {
      grid.newpage()
      if (i == j)
      {
        mosaic.list[[counter]] <- grid.text(paste0(factors[i+1]))
      } else if (i > j) {
        mosaic.list[[counter]] <- grid.text("")
      } else
      {
        NAS <- tapply(data[,weight],
                      data[, c(factors[j+1], factors[i+1], "DUMMY")], sum, na.rm=TRUE)
        fmla <- formula(paste0(factors[j+1], "~", factors[i+1]))
        model <- survey::svyglm(fmla,
                        family = quasibinomial(link = logit),
                        design = svydesignObj,
                        na.action = na.omit)
        if (coef(summary(model))[2, 4] <= .05)
        {
          if (coef(summary(model))[2, 1] >= 0) {
            mosaic.list[[counter]] <- grid.grabExpr(mosaic(fmla,
                                                           data    = NAS,
                                                           legend  = F,
                                                           labels  = F,
                                                           margins = mar_par,
                                                           gp      = gpar(fill = matrix(c("red", "blue", "blue", "red"), 2, 2))
                                                           )
                                                    )
          } else {
            mosaic.list[[counter]] <- grid.grabExpr(mosaic(fmla,
                                                           data    = NAS,
                                                           legend  = F,
                                                           labels  = F,
                                                           margins = mar_par,
                                                           gp      = gpar(fill = matrix(c("blue", "red", "red", "blue"), 2, 2))
                                                           )
                                                    )
          }
        }
        if (coef(summary(model))[2, 4] > .05)
        {
          mosaic.list[[counter]] <- grid.grabExpr(mosaic(fmla,
                                                         data    = NAS,
                                                         legend  = F,
                                                         labels  = F,
                                                         margins = mar_par,
                                                         newpage = F)
                                                  )
        }
      }
      counter <- counter + 1
    }
  }
  mosaic.list <- mosaic.list[!sapply(mosaic.list, is.null)]
  myfun <- get("grid.arrange", asNamespace("gridExtra"))
  do.call(myfun, args = c(mosaic.list,
                          ncol = ind))
}
