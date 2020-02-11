#' The length of a string (in characters).
#'
#' @param response a character string identifying the response variable
#' @param predictor_list a list whose elements are vectors of character strings specifying the desired predictor variables. A svyglm model is created for each element of the list.
#' @param svydesignObj a svydesign object from which to create the svyglm models
#' @param test.set a data frame whose variables match the svydesignObj, which is used to build ROC curves based on prediction
#' @param plot a Boolean indicating whether to plot the ROC curves for each model
#' @return A data frame of ROC curve values for each input formula
#' @seealso \code{\link{predROC}}
#' @examples
#' data(list = c("GES2013.drivers.design", "GES2013.drivers"))
#' GES2013.drivers$TIME_TRANSFORM <- sapply(GES2013.drivers$TIME_OF_WEEK, function (x) .0026*exp(3*cos(2*pi*(x-4)/24)) + .0037*cos(2*pi*floor(x/24)/7) + .0034)
#' GES2013.drivers$AGE_TRANSFORM <- GES2013.drivers$AGE_IM^2
#' GES2014.drivers$TIME_TRANSFORM <- sapply(GES2014.drivers$TIME_OF_WEEK, function (x) .0026*exp(3*cos(2*pi*(x-4)/24)) + .0037*cos(2*pi*floor(x/24)/7) + .0034)
#' GES2014.drivers$AGE_TRANSFORM <- GES2014.drivers$AGE_IM^2
#' GES2013.drivers.design <- update(GES2013.drivers.design,
#'                                                          TIME_TRANSFORM = GES2013.drivers$TIME_TRANSFORM,
#'                                                          AGE_TRANSFORM = GES2013.drivers$AGE_TRANSFORM)
#' response <- "DROWSY"
#' predictor_list <- list(c ("HEAVY_TRUCK",  "INT_HWY",  "SEX_IM", "SPEEDREL", "TIME_TRANSFORM", "AGE_TRANSFORM"),
#'                                  c ("HEAVY_TRUCK",  "INT_HWY",  "SEX_IM", "SPEEDREL"))
#' model_and_convert(response, predictor_list, GES2013.drivers.design, GES2014.drivers)
#' @export
model_and_convert <- function(response, predictor_list, svydesignObj, test.set, plot = T)
{

  require(ggplot2)

  options(survey.lonely.psu="adjust")
  pred <- vector("list")

  for (i in 1:length(predictor_list))
  {
    newData <- subset(test.set, select=c(predictor_list[[i]], response, "WEIGHT"))
    my_fmla <- formula(paste(response, "~", paste(predictor_list[[i]], collapse=' + ')))
    model <- survey::svyglm(my_fmla,
                         family = quasibinomial(link=logit),
                         design = svydesignObj)
    roc <- predROC(model, newData)
    auc <-  round(sum((sort(roc[, 1])[-1] - sort(roc[, 1])[-nrow(roc)]) * (sort(roc[, 2])[-1] + sort(roc[, 2])[-nrow(roc)])/2), digits=2)
    pred[[i]] <- cbind(roc, AUC=rep(paste("Model ", i, "\nAUC = ", auc), nrow(roc)))
  }

  myroc <- data.frame(matrix(unlist(lapply(pred, t)), ncol= 3, byrow=T), stringsAsFactors=FALSE)
  myroc[,1:2] <- matrix(as.numeric(unlist(myroc[,1:2])), ncol = 2)
  names(myroc) <- c("FPR","TPR","AUC")

  if (plot) {
    p <- ggplot(data = myroc, aes(x = FPR, y = TPR, group = AUC, color = AUC)) +
      # geom_line(aes(color=AUC), size=1.6) +
      geom_line(size = 1.6) +
      theme_bw() +
      # scale_color_manual(values = wes_palette("Moonrise2")) +
      geom_segment(x = 0, xend = 1, y = 0, yend = 1, color = "black", size = 0.25) +
      guides(colour = guide_legend(override.aes = list(size = 10))) +
      labs(x = "False Positive Rate", y = "True Positive Rate", title = "ROC Curves") +
      theme(legend.title=element_blank(), aspect.ratio=1, plot.title = element_text(hjust = 0.5))

    plot(p)
  }

  return(myroc)
}
