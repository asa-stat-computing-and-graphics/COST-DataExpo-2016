##devtools::install_github("tdhock/WeightedROC")
##devtools::install_github("wesanderson","karthik")

library(survey)
library(WeightedROC)
library(ggplot2)
library(wesanderson)
library(DT)
library(shiny)
library(lattice)
library(GGally)
library(grid)
library(gridExtra)
library(vcd)
library(Hmisc)
library(survey)

load("GES2013.drivers.rda")
load("GES2013.drivers.design.rda")
load("GES2014.drivers.rda")

GES2013.drivers$DUMMY <- 1

options(survey.lonely.psu="adjust")

timevars <- c("HOUR_NEWFIT","DAY_NEWFIT", "AGE_IM_SQ", "AGE_IM")

# func --------------------------------------------------------------------



shinyServer(
  function(input, output) {
    
     output$res1 <- renderPrint({
       input$check1
     })
    
     # Sorting asc
     observeEvent(input$a2z, {
       updateCheckboxGroupInput(
         session = session, inputId = "check2", choices = colnames(GES2013.drivers), selected = input$check2
       )
     })
     # Sorting desc
     observeEvent(input$z2a, {
       updateCheckboxGroupInput(
         session = session, inputId = "check2", choices = colnames(GES2013.drivers), selected = input$check2
       )
     })
     output$res2 <- renderPrint({
       input$check2
     })
     # Select all / Unselect all
     observeEvent(input$all, {
       if (is.null(input$check2)) {
         updateCheckboxGroupInput(
           session = session, inputId = "check2", selected = colnames(GES2013.drivers)
         )
       } else {
         updateCheckboxGroupInput(
           session = session, inputId = "check2", selected = ""
         )
       }
     })
  
  dat <- reactive({
    
    predROC <- function (glm.obj, newData, response)
    {
      pred <- rep(NA, nrow(newData)); names(pred) <- rownames(newData)
      xnn <- na.omit(newData)
      pred[-attr(xnn, "na.action")] <- predict(glm.obj, xnn)
      guess <- 1/(1+exp(-pred))
      dframe <- data.frame(RESPONSE = ifelse(newData[, response] == 0, -1, 1),
                           guess    = guess,
                           WEIGHT   = GES2014.drivers$WEIGHT)
      dframe <- na.omit(dframe)
      subset(WeightedROC(dframe$guess, 
                         dframe$RESPONSE, 
                         weight=dframe$WEIGHT), 
             select=c(FPR, TPR))
      # ggplot(roc, aes(x=FPR, y=TPR)) + geom_line(colour = "red")
    }
    
    model_and_convert <- function (response, predictors) {
      models <- list(
        model_1 = svyglm(formula(paste0(response, 
                                        " ~ ", 
                                        paste(predictors, collapse="*"))),
                         family = quasibinomial(link = logit),
                         design = GES2013.drivers.design), 
        model_2 = svyglm(formula(paste0(response, 
                                        " ~ ", 
                                        paste(predictors, collapse="*"), 
                                        "+", 
                                        paste(timevars, collapse="+"))),
                         family = quasibinomial(link = logit),
                         design = GES2013.drivers.design), 
        model_3 = svyglm(formula(paste0(response, 
                                        " ~ ", 
                                        paste(predictors, collapse="*"), 
                                        "+", 
                                        paste(paste(outer(input$check2, timevars, function(x,y) paste0(x, "*", y))), collapse="+"))),
                         family = quasibinomial(link = logit),
                         design = GES2013.drivers.design)
      )
      newData <- subset(GES2014.drivers, select=c(predictors, response, timevars, "WEIGHT"))
      roc <- lapply(models, function (x) predROC(x, newData, response))
      auc <- lapply(roc, function (x) round(sum((sort(x[, 1])[-1] - sort(x[, 1])[-nrow(x)]) * (sort(x[, 2])[-1] + sort(x[, 2])[-nrow(x)])/2), digits=2))
      rbind(cbind(roc[[1]], 
                  AUC = rep(paste("Time/age w/ interactions (AUC = ", auc[[1]], ")"), nrow(roc[[1]]))),
            cbind(roc[[2]], 
                  AUC = rep(paste("Time/age w/ interactions (AUC = ", auc[[2]], ")"), nrow(roc[[2]]))),
            cbind(roc[[3]], 
                  AUC = rep(paste("Time/age w/ interactions (AUC = ", auc[[3]], ")"), nrow(roc[[3]]))))
    }
    
     model_and_convert(input$check1, input$check2)
   })

  mosaics <- reactive({
    
    mosaicSplom <- function(response = "DROWSY", predictors = "DUMMY", mar_par = rep(0.5, 4))
    {
      factors <- c("WEIGHT", response, predictors, "DUMMY")
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
            NAS <- tapply(GES2013.drivers[,"WEIGHT"], 
                          GES2013.drivers[, c(factors[j+1], factors[i+1], "DUMMY")], sum, na.rm=TRUE)
            fmla <- formula(paste0(factors[j+1], "~", factors[i+1]))
            model <- svyglm(fmla,
                            family = quasibinomial(link = logit), 
                            design = GES2013.drivers.design,
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
      mosaic.list
    }
    mosaicSplom(input$check1, input$check2)
  })
  
  plot_switch <- eventReactive(input$input_action, {
    switch(input$plot_type,
           "ROC" = ggplot(dat(), aes(x = FPR, y = TPR)) +
             geom_line(aes(color=AUC), size=1.6) +
             theme_bw() +
             scale_color_manual(values = wes_palette("Moonrise2")) +
             geom_abline() +
             guides(colour = guide_legend(override.aes = list(size = 10))) +
             theme(legend.title=element_blank(), aspect.ratio=1) +
             labs(x = "False Positive Rate",
                  y = "True Positive Rate",
                  title="2014 ROC based on 2013 models"),
           "Scatterplot Matrix" = ggpairs(GES2013.drivers, 
                                          columns = c(input$check1, input$check2)),
           "Mosaic Matrix"      = do.call("grid.arrange", args = c(mosaics(), 
                                                                   ncol = length(c(input$check1, input$check2))))
    )
  })
  
  output$plot1 <- renderPlot({plot_switch()})
  
})