#' Produce a data frame of ROC curve values based on a svyglm object and a test set
#'
#' @param glm.obj a survey::svyglm object whose response is a binary numeric variable taking the values 0, 1 or NA
#' @param newData a data frame whose variables are compatible with the svyglm object, for use in prediction
#' @return A data frame of ROC curve values based on a svyglm object and a test set
#' @examples
#' data(list = c("GES2013.drivers.design", "GES2013.drivers"))
#' options(survey.lonely.psu = "adjust")
#' model <- survey::svyglm(DROWSY ~ HEAVY_TRUCK + INT_HWY + SEX_IM + SPEEDREL,
#'                             family = quasibinomial(link=logit),
#'                             design = GES2013.drivers.design)
#' predROC(model,GES2013.drivers)
#' @export
predROC <- function (glm.obj, newData, plot = TRUE)
{

  options(survey.lonely.psu="adjust")

  pred <- rep(NA, nrow(newData)); names(pred) <- rownames(newData)
  model_terms <- attributes(glm.obj$terms)$variables
  predictors <- as.character(model_terms[3:length(model_terms)])
  response <-  as.character(model_terms[2])
  newData <- newData[,c("WEIGHT",response,predictors)]
  xnn <- na.omit(newData)
  pred[-attr(xnn, "na.action")] <- predict(glm.obj, xnn)
  guess <- 1/(1+exp(-pred))
  dframe <- data.frame(response=ifelse(newData[, response]==0, -1, 1),
                       guess=guess,
                       WEIGHT=newData$WEIGHT)
  dframe <- na.omit(dframe)
  subset(WeightedROC::WeightedROC(dframe$guess, dframe$response, weight=dframe$WEIGHT), select=c(FPR, TPR))
}
