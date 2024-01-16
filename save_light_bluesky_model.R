library(readr)
library(readxl)

# This file fits the blue sky model and then saves a lightweight version of the model
# that can only be used for predictions. This is necessary so the R Shiny App
# doesn't take forever to load.

sf_history <- read_csv("./SF_Logistic_Data.csv")

model <- glm(Developed ~ Historic + Residential_Dummy + Zillow_Price_Real + 
               Const_FedReserve_Real + Envelope_1000 + Upzone_Ratio + zp_OfficeComm +
               zp_DensRestMulti + zp_FormBasedMulti + zp_PDRInd + zp_Public + 
               zp_Redev + zp_RH2 + zp_RH3_RM1, 
             data = sf_history, family = 'binomial')

stripGlmLR = function(cm) {
  cm$y = c()
  cm$model = c()
  
  cm$residuals = c()
  cm$fitted.values = c()
  cm$effects = c()
  cm$qr$qr = c()  
  cm$linear.predictors = c()
  cm$weights = c()
  cm$prior.weights = c()
  cm$data = c()
  
  
  cm$family$variance = c()
  cm$family$dev.resids = c()
  cm$family$aic = c()
  cm$family$validmu = c()
  cm$family$simulate = c()
  attr(cm$terms,".Environment") = c()
  attr(cm$formula,".Environment") = c()
  
  cm
}

model <- stripGlmLR(model)

saveRDS(model, "light_model.rds")
