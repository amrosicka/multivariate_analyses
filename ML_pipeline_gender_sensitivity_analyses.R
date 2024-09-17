#---------------------------------------------#
#      Risk Factors multivariate analyses     #
#   Sensitivity analyses: balanced samples    #
#                    2024                     #
#---------------------------------------------#

# 1. Preparation ----------------------------------------------------------
## Remove leftovers from previous analyses --------------------------------
rm(list=ls())
options(scipen = 999) # force R to not use scientific notations
cores <- parallel::detectCores(logical = FALSE) # Detect number of cores in computer for faster computations

## Set working directory --------------------------------------------------
# Replace with your WD and path to this folder
setwd("/Users/annamarierosicka/Library/CloudStorage/OneDrive-TrinityCollegeDublin/Neureka/Analysis_Pipelines/")
pathname_public <- "riskFactorsScripts/riskFactors-ANNAR/multivariate_analyses"
pathname <- "../../ML_output/sensitivity"

# Toggle if you want to update data or not (not doing so saves time)
update_data <- F

## Run the helper script first ---------------------------------------------
# This also loads and installs packages
source(paste0(pathname_public, "/helper_script.R"))

## Load the pre-processed datafiles ---------------------------------------
load(paste0(pathname_public, "/output/trainsets_final.RData"))
load(paste0(pathname_public, "/output/testsets_final.RData"))

# Calling different datasets for the sensitivity analyses:
trainSet_all_imp <- trainSet_all_matched_imp
testSet_all_imp <- testSet_all_matched_imp

# 2. Define and rename variables of interest ------------------------------
# Create feature lists for easier selection later
all_features <- names(trainSet_all_imp %>% select(-PlayerID, -cb, -stay, -modelFree, -transition))

# Define list of DVs to loop over
main_dvs <- c("mm", "sra", "srb", "MemoryProblems", "avg_z")

main_dv_names <- c(
  "Visual working memory",
  "Processing speed (~ Trails A)",
  "Cognitive flexibility (~ Trails B)",
  "Subjective memory problems",
  "Composite cognition (avg. z)"
)
all_dvs <- c("avg_z", "avg_z_all", "avg_z_simple", "mm", "sra", "srb", "binding_cost_nameable", "avgAll_Mean_proportion_of_selected_lures", "avgAll_Mean_proportion_of_other_misses", "MemoryProblems")

##### SUBSETTING FOR NOW ###########
main_dvs <- main_dvs[4:5]
main_dv_names <- main_dv_names[4:5]


# Define covariates lists to loop over
main_covariates <- c("Age", "Woman")
all_covariates <- c("LMIC country", "First language not EN", "Tablet user", "Android user", "Age", "Woman")

# Define features sets to loop ver
all_features <- all_features[!all_features %in% all_dvs] # everything but DVs
main_features_main_covariates <- c(all_features[!all_features %in% all_covariates], main_covariates) # main covariates and all features

core_features <- c(
  "Lower SES", "Depression", "Less education",
  "Small social network",
  "Less exercise", "Loneliness",
  "Hypertension", "Tinnitus",
  "Ever smoked", "Hearing handicap", "DFH"
)
core_features_main_covariates <- c(core_features, main_covariates)
interactions_gender <- c(core_features_main_covariates, paste0(core_features_main_covariates[-length(core_features_main_covariates)], " * Woman"))
interactions_age <- c(core_features_main_covariates, paste0(core_features_main_covariates[-length(core_features_main_covariates)+1], " * Age"))
interactions_ses <- c(core_features_main_covariates[2:length(core_features_main_covariates)], "Lower SES", paste0(core_features_main_covariates[-1], " * Lower SES"))
interactions_education <- c(core_features_main_covariates[1:2], core_features_main_covariates[4:length(core_features_main_covariates)], "Less education", paste0(core_features_main_covariates[-3], " * Less education"))
interactions_age_group <- c(core_features, c("Woman", "Over 48"), paste0(c(core_features, "Woman"), " * Over 48"))


# Updated version without rarely endorsed items
create_feature_matrix <- function(j, dataset) {
  switch(j,
         `3` = model.matrix(~ (`Lower SES` + Depression + `Less education` + `Small social network` +
                                 `Less exercise` + Loneliness + 
                                 Hypertension + Tinnitus + `Ever smoked` + `Hearing handicap` +
                                 DFH + Age) * `Woman`, data = dataset),
         `4` = model.matrix(~ (`Lower SES` + Depression + `Less education` + `Small social network` +
                                 `Less exercise` + Loneliness + 
                                 Hypertension + Tinnitus + `Ever smoked` + `Hearing handicap` +
                                 DFH + Woman) * `Age`, data = dataset),
         `5` = model.matrix(~ (Depression + `Less education` + `Small social network` +
                                 `Less exercise` + Loneliness + 
                                 Hypertension + Tinnitus + `Ever smoked` + `Hearing handicap` +
                                 DFH + Age + Woman) * `Lower SES`, data = dataset),
         `6` = model.matrix(~ (`Lower SES` + Depression + `Small social network` +
                                 `Less exercise` + Loneliness + 
                                 Hypertension + Tinnitus + `Ever smoked` + `Hearing handicap` +
                                 DFH + Age + Woman) * `Less education`, data = dataset),
         `7` = model.matrix(~ .^2, data = dataset %>% select(all_of(core_features_main_covariates))),
         `8` = model.matrix(~ (`Lower SES` + Depression + `Less education` + `Small social network` +
                                 `Less exercise` + Loneliness + 
                                 Hypertension + Tinnitus + `Ever smoked` + `Hearing handicap` +
                                 DFH + Woman) * `Over 48`, data = dataset)
  )
}

feature_lists <- list(main_covariates, core_features_main_covariates, interactions_gender)
feature_list_names <- c("main_covariates", "core_features_main_covariates", "interactions_gender")

# Define subsamples to loop over
subsamples <- c("all", "women", "men")
datasets <- list(trainSet_all_imp, trainSet_women_imp, trainSet_men_imp)

# 3. Train the model ------------------------------------------------------
## Specify model parameters for glmnet modelling --------------------------
# # ## Overnight models:
folds <- 10
outer_folds <- 10
repeats <- 10
# ## Works faster:
# folds <- 5
# outer_folds <- 5
# repeats <- 5
# ## For testing:
# folds <- 2
# outer_folds <- 2
# repeats <- 2

# tuning grid search parameters - so to explore more potential models; explore alpha/beta pairings
myGrid <- expand.grid(alpha = seq(0.0001, 0.9999, length = 20),
                      lambda = seq(0.0001, 0.9999, length = 20))

# Function to prepare outcome and control parameters
prepare_control_params <- function(dv, folds, repeats) {
  if (dv == "MemoryProblems") {
    list(
      # Memory Problems is a categorical variable and therefore needs be recoded as factor:
      outcome = function(outcome) factor(outcome[, 1], labels = c("No", "Yes")),
      # Set summary metric to be used to select the optimal model:
      metrika = "logLoss", # otherwise I got warnings
      # Set parameters:
      control = trainControl(
        method = "repeatedcv",
        number = folds,
        repeats = repeats,
        verboseIter = TRUE,
        savePredictions = "all", # setting this to "final" saves time but does not enable getting inner loop performance metrics 
        classProbs = TRUE,  # otherwise I got warnings
        summaryFunction = mnLogLoss  # otherwise I got warnings from nestcv.train() later:
        # ("In train.default(x = filtx, y = yfinal, method = method, weights = weights,  :
        # The metric "logLoss" was not in the result set. ROC will be used instead.")
        ####
        # Relevant comment from nestedcv vignette, https://cran.r-project.org/web/packages/nestedcv/vignettes/nestedcv.html#Notes_on_caret:
        # "When fitting classification models, the usual default metric for tuning model
        # hyperparameters in caret is Accuracy. However, with small datasets, accuracy
        # is disproportionately affected by changes in a single individual’s prediction
        # outcome from incorrect to correctly classified or vice versa. For this reason,
        # we suggest using logLoss with smaller datasets as it provides more stable measures
        # of model tuning behaviour. In nestedcv, when fitting classification models with
        # caret, the default metric is changed to use logLoss."
      )
    )
  } else {
    list(
      outcome = function(outcome) outcome[, 1],
      # Set summary metric to be used to select the optimal model:
      metrika = "RMSE",
      # Set parameters:
      control = trainControl(
        method = "repeatedcv",
        number = folds,
        repeats = repeats,
        verboseIter = TRUE,
        savePredictions = "all"  # setting this to "final" saves time but does not enable getting inner loop performance metrics 
      )
    )
  }
}


## Loop over datasets, dvs and feature lists -------------------------------
# Loop over datasets
for(k in seq_along(subsamples)) {
  
  # Define subsample
  subsample <- subsamples[k]
  dataset <- datasets[[k]]
  
  # Loop over DVs
  for (i in seq_along(main_dvs)) {
    
    # Define DV
    dv <- main_dvs[[i]]
    control_params <- prepare_control_params(dv, folds, repeats)
    
    # Loop over feature lists
    for (j in seq_along(feature_lists)) {
      
      # Define feature set name
      feature_list_name <- feature_list_names[[j]]
      
      # For models split by group we are only interested in the covariates + risk factors model
      ####### Covariates only model does not work with only a single predictor for now!!! ###################
      if (j != 2 & subsample %in% c("women", "men", "young", "old")) next
      # For age group interaction models we need to select the appropriate subset and no other!!
      if (j != 8 & subsample == "age" | j == 8 & subsample != "age") next
      
      # Print to output
      print(subsample)
      print(dv)
      print(feature_list_name)
      
      # Define feature list
      if (j %in% 3:8) {
        mat <- create_feature_matrix(as.character(j), dataset)
        features <- data.frame(mat, check.names = FALSE)[-1]
        names(features) <- gsub("`", "", names(features))
        names(features) <- sub(":", " * ", names(features))
      } else if (subsample %in% c("women", "men")) {
        features <- dataset %>% select(all_of(feature_lists[[j]][1:(length(feature_lists[[j]])-1)]))
      } else {
        features <- dataset %>% select(all_of(feature_lists[[j]]))
      }
      
      
      features <-  data.matrix(features)
      

      # Define outcome column
      outcome <- control_params$outcome(dataset %>% select({{ dv }}) %>% data.matrix())
      
      # Set control parameters - this will differ for classification and regression
      metrika <- control_params$metrika
      myControl <- control_params$control
  
      # Tuning both alpha and lambda
      set.seed(123, "L'Ecuyer-CMRG") # for reproducibility
      model_ncv <- nestcv.train(
          x = features,
          y = outcome,
          method = "glmnet",
          outer_method = "cv",
          n_outer_folds = outer_folds,
          trControl = myControl,
          tuneGrid = myGrid,
          metric = metrika,
          savePredictions = "final", # setting this to "all" requires more time but enables getting inner loop performance metrics
          outer_train_predict = TRUE,
          finalCV = TRUE, # useful if you want a metric for comparing models (testing) but to be conservative in your estimates, set to F
          cv.cores = cores # makes running the code faster unless more folds than cores
        )
      
      gc() # Clean up memory
      
      # save output
      saveRDS(model_ncv, paste(pathname,
                               "/output/", subsample, "_models/",
                               dv, "/model_", feature_list_names[[j]],
                               ".rds",
                               sep = ""
      ))
      
      # Model fit evaluation
      
      # Performance Assessment
      # outer_result: list of each outer fold containing predictions on left-out outer folds, caret result, no. of predictors
      # bestTune = best tuned parameters from each outer fold
      # finalTune = final parameters used for final model
      # final_fit = final fitted caret model using best tune parameters
      
      # Model fit evaluation
      # Save names
      metric_names <- if (dv == "MemoryProblems") {
        c("AUC", "Accuracy", "Balanced accuracy")
      } else {
        c("RMSE", "MAE", "PEARSON", "R_SQUARED")
      }
      
      # Save final metrics
      metrics <- if (dv == "MemoryProblems") {
        c(model_ncv$summary$metrics[["AUC"]], model_ncv$summary$metrics[["Accuracy"]], model_ncv$summary$metrics[["Balanced accuracy"]])
      } else {
        c(model_ncv$summary[["RMSE"]], model_ncv$summary[["MAE"]], sqrt(model_ncv$summary[["R.squared"]]), model_ncv$summary[["R.squared"]])
      }
      
      # Extract these metrics
      output <- data.frame(metric_names, metrics)
      write_csv(output, paste(pathname,
                              "/output/", subsample, "_models/",
                              dv, "/performance_metrics_", feature_list_names[[j]],
                              ".csv",
                              sep = ""
      ))
      
      # Save individual metrics
      individual_metrics <- if (dv == "MemoryProblems") {
        df <- data.frame(matrix(ncol = 3, nrow = 0))
        names(df) <- metric_names
        for(n in seq_along(model_ncv$outer_result)) {
          predicted <- model_ncv$outer_result[[n]]$preds$predy
          observed <- model_ncv$outer_result[[n]]$preds$testy
          predicted_prob <- model_ncv$outer_result[[n]]$preds$predyp
          
          TP <- sum(observed == "Yes" & predicted == "Yes")
          TN <- sum(observed == "No" & predicted == "No")
          FP <- sum(observed == "No" & predicted == "Yes")
          FN <- sum(observed == "Yes" & predicted == "No")
          
          accuracy <- (TP + TN) / (TP + TN + FP + FN)
          sensitivity <- TP / (TP + FN)
          specificity <- TN / (TN + FP)
          balanced_accuracy <- (sensitivity + specificity) / 2
          
          roc_obj <- roc(observed, predicted_prob,)
          auc_value <- pROC::auc(roc_obj)
          
          new <- c(auc_value, accuracy, balanced_accuracy)
          df[n,] <- new
        }
        df
      } else {
        df <- data.frame(matrix(ncol = 4, nrow = 0))
        names(df) <- metric_names
        for(n in seq_along(model_ncv$outer_result)) {
          new <- colMeans(model_ncv$outer_result[[n]]$fit$resample[1:3], na.rm = T)
          new <- c(mean(model_ncv$outer_result[[n]]$fit$resample$RMSE, na.rm = T),
                   mean(model_ncv$outer_result[[n]]$fit$resample$MAE, na.rm = T),
                   sqrt(mean(model_ncv$outer_result[[n]]$fit$resample$Rsquared, na.rm = T)),
                   mean(model_ncv$outer_result[[n]]$fit$resample$Rsquared, na.rm = T))
          df[n,] <- new
        }
        df
      }
      
      # Extract these metrics
      output <- data.frame(individual_metrics)
      write_csv(output, paste(pathname,
                              "/output/", subsample, "_models/",
                              dv, "/performance_metrics_by_fold_", feature_list_names[[j]],
                              ".csv",
                              sep = ""
      ))
      
      summary(model_ncv)
      # print("Performance metrics on inner CV held-out test folds")
      # print(innercv_summary(model_ncv)) # performance metrics on inner CV held-out testfolds
      print("Performance metrics on outer training folds")
      print(train_summary(model_ncv)) # performance metrics on outer training folds
      
      # Model Fit -
      # plot tuning parameter deviations on RMSE/logLoss
      # png(
      #   file = paste(
      #     pathname,
      #     "/figures/", subsample, "_models/",
      #     dv, "/tuning_parameters_", feature_list_names[[j]],
      #     ".png",
      #     sep = ""
      #   ),
      #   unit = "cm",
      #   width = 12,
      #   height = 12,
      #   res = 700
      # )
      # plot_caret(model_ncv$final_fit) # Plot the main tuning parameter
      # dev.off()
      
      # Variable Importance (Explanability)
      # https://cran.r-project.org/web/packages/nestedcv/vignettes/nestedcv_shap.html
      
      # check stability of coefficients (By default only the predictors chosen in the final model are shown)
      vs <- var_stability(model_ncv)
      vs$variable <- rownames(vs)
      saveRDS(
        vs,
        paste(
          pathname,
          "/output/", subsample, "_models/",
          dv, "/var_stability_", feature_list_names[[j]],
          ".rds",
          sep = ""
        )
      )
      
      
      # explain using SHAP values (# Can set NSIM to 5 repeats for speed)
      set.seed(123)  # for reproducibility
      shap <- explain(model_ncv, X = features, pred_wrapper = pred_train, nsim = 100)
      gc() # Clean up memory
      
      # Extract SHAP values
      saveRDS(shap, paste(pathname, "/output/", subsample, "_models/",
                          dv, "/shap_val_", feature_list_names[[j]],
                          ".rds", sep = ""))
      
      # plot SHAP values and variable stability
      # overlay directionality using colour
      p1 <- plot_var_stability(model_ncv, final = FALSE, direction = 1) +
        theme_main +
        ggtitle(paste0(main_dv_names[i], "\nvariable importance"))
      # option to show directionality with the sign of the variable importance:
      # plot_var_stability(model_ncv, final = FALSE, percent = F)
      
      # plot SHAP values alone
      p2 <- plot_shap_bar(shap, features, labels = c("Lower\nrisk", "Higher\nrisk")) +
        theme_main +
        ggtitle(paste0(main_dv_names[i], "\nmean shap values"))
      
      # combine the two
      shap_plot <- ggarrange(p1, p2, ncol=2)
      
      # set up figure size
      h <- if (j == 1) 5 else if (j == 4) 15 else 10
      
      # export plots
      ggsave(shap_plot,
             file = paste0(pathname, "/figures/", subsample, "_models/",
                           dv, "/shap_and_imp_", feature_list_names[[j]],
                           ".png"),
             height = h, width = 20, dpi = 300
      )
      
      ggsave(p2,
             file = paste0(pathname, "/figures/", subsample, "_models/",
                           dv, "/shap_val_", feature_list_names[[j]],
                           ".png"),
             height = h, width = 10, dpi = 300
      )
      
      # extract coefficients of final fitted model
      coef_list <- glmnet_coefs(model_ncv$final_fit$finalModel, s = model_ncv$finalTune$lambda)
      coef_list <- as.data.frame(coef_list)
      coef_list <- coef_list %>%
        mutate(var = rownames(.)) %>%
        select(var, val = coef_list) %>%
        mutate(valence = ifelse(val < 0, "Negative", "Positive")) %>%
        arrange(-abs(val)) %>%
        filter(var != "(Intercept)") # comment out
      rownames(coef_list) <- NULL
      write_csv(coef_list,
                paste(pathname,
                      "/output/", subsample, "_models/",
                      dv, "/coefs_", feature_list_names[[j]],
                      ".csv", sep = ""))
    }
  }
}


# 4. Checking for multicollinearity ---------------------------------------
## 4.1. Correlation matrices ----------------------------------------------
# All, DVs
d <- trainSet_all_imp %>%
  select(!!main_dvs, Age) %>%
  mutate(across(where(is.numeric), ~ as.numeric(.x))) %>%
  data.frame()

corrMatrix <- round(hetcor(d)$cor, 2)

# Get only lower triangle of corr matrix
corrMatrix[lower.tri(corrMatrix)] <- NA

# Attach variable names
colnames(corrMatrix) <- c(main_dv_names, "Age")
rownames(corrMatrix) <- c(main_dv_names, "Age")

# Melt data for plot purposes
melted_corrMatrix <- melt(corrMatrix, na.rm = T)

# Plot that
corrMatrix_plot <- melted_corrMatrix %>%
  ggplot(aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "#4477AA", high = "#85C660", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Correlation") +
  geom_text(aes(Var2, Var1, label = value), color = "black",
            family = font.family, size = 4) +
  theme_cowplot()+ 
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.grid.major = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.ticks = element_blank(),
        legend.justification = c(1, 0),
        legend.position = c(0.4, 0.7),
        legend.direction = "horizontal",
        axis.text.x = element_text(angle = 45, vjust = 1, size = 12, hjust = 1,
                                   family = font.family),
        axis.text.y = element_text(size = 12, family = font.family),
        legend.text = element_text(size = 12, family = font.family),
        legend.title = element_text(size = 12, family = font.family,face = "bold"),
        plot.background = element_rect(fill = "white")) +
  guides(fill = guide_colorbar(barwidth = 7, barheight = 1,
                               title.position = "top", title.hjust = 0.5))

corrMatrix_plot

ggsave(corrMatrix_plot, file = paste0(pathname, "/figures/corrMatrix_all_matched_imp.png"),
       height = 7, width = 7, dpi = 300)

# All, predictors
d <- trainSet_all_imp %>%
  select(!!core_features_main_covariates, -Woman) %>%
  mutate(across(where(is.numeric), ~ as.numeric(.x))) %>%
  data.frame()

corrMatrix <- round(hetcor(d)$cor, 2)

# Get only lower triangle of corr matrix
corrMatrix[lower.tri(corrMatrix)] <- NA

# Attach variable names
colnames(corrMatrix) <- core_features_main_covariates[1:length(core_features_main_covariates)-1]
rownames(corrMatrix) <- core_features_main_covariates[1:length(core_features_main_covariates)-1]

# Melt data for plot purposes
melted_corrMatrix <- melt(corrMatrix, na.rm = T)

# Plot that
corrMatrix_plot <- melted_corrMatrix %>%
  ggplot(aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "#4477AA", high = "#85C660", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Correlation") +
  geom_text(aes(Var2, Var1, label = value), color = "black",
            family = font.family, size = 4) +
  theme_cowplot()+ 
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.grid.major = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.ticks = element_blank(),
        legend.justification = c(1, 0),
        legend.position = c(0.4, 0.7),
        legend.direction = "horizontal",
        axis.text.x = element_text(angle = 45, vjust = 1, size = 12, hjust = 1,
                                   family = font.family),
        axis.text.y = element_text(size = 12, family = font.family),
        legend.text = element_text(size = 12, family = font.family),
        legend.title = element_text(size = 12, family = font.family,face = "bold"),
        plot.background = element_rect(fill = "white")) +
  guides(fill = guide_colorbar(barwidth = 7, barheight = 1,
                               title.position = "top", title.hjust = 0.5))

corrMatrix_plot

ggsave(corrMatrix_plot, file = paste0(pathname, "/figures/corrMatrix_all_RF.png"),
       height = 7, width = 7, dpi = 300)

# Women, DVs
d <- trainSet_women_imp %>%
  select(!!main_dvs, Age) %>%
  mutate(across(where(is.numeric), ~ as.numeric(.x))) %>%
  data.frame()

corrMatrix <- round(hetcor(d)$cor, 2)

# Get only lower triangle of corr matrix
corrMatrix[lower.tri(corrMatrix)] <- NA

# Attach variable names
colnames(corrMatrix) <- c(main_dv_names, "Age")
rownames(corrMatrix) <- c(main_dv_names, "Age")

# Melt data for plot purposes
melted_corrMatrix <- melt(corrMatrix, na.rm = T)

# Plot that
corrMatrix_plot <- melted_corrMatrix %>%
  ggplot(aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "#4477AA", high = "#85C660", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Correlation") +
  geom_text(aes(Var2, Var1, label = value), color = "black",
            family = font.family, size = 4) +
  theme_cowplot()+ 
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.grid.major = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.ticks = element_blank(),
        legend.justification = c(1, 0),
        legend.position = c(0.4, 0.7),
        legend.direction = "horizontal",
        axis.text.x = element_text(angle = 45, vjust = 1, size = 12, hjust = 1,
                                   family = font.family),
        axis.text.y = element_text(size = 12, family = font.family),
        legend.text = element_text(size = 12, family = font.family),
        legend.title = element_text(size = 12, family = font.family,face = "bold"),
        plot.background = element_rect(fill = "white")) +
  guides(fill = guide_colorbar(barwidth = 7, barheight = 1,
                               title.position = "top", title.hjust = 0.5))

corrMatrix_plot

ggsave(corrMatrix_plot, file = paste0(pathname, "/figures/corrMatrix_women_imp.png"),
       height = 7, width = 7, dpi = 300)

# Women, predictors
d <- trainSet_women_imp %>%
  select(!!core_features_main_covariates, -Woman) %>%
  mutate(across(where(is.numeric), ~ as.numeric(.x))) %>%
  data.frame()

corrMatrix <- round(hetcor(d)$cor, 2)

# Get only lower triangle of corr matrix
corrMatrix[lower.tri(corrMatrix)] <- NA

# Attach variable names
colnames(corrMatrix) <- core_features_main_covariates[1:length(core_features_main_covariates)-1]
rownames(corrMatrix) <- core_features_main_covariates[1:length(core_features_main_covariates)-1]

# Melt data for plot purposes
melted_corrMatrix <- melt(corrMatrix, na.rm = T)

# Plot that
corrMatrix_plot <- melted_corrMatrix %>%
  ggplot(aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "#4477AA", high = "#85C660", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Correlation") +
  geom_text(aes(Var2, Var1, label = value), color = "black",
            family = font.family, size = 4) +
  theme_cowplot()+ 
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.grid.major = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.ticks = element_blank(),
        legend.justification = c(1, 0),
        legend.position = c(0.4, 0.7),
        legend.direction = "horizontal",
        axis.text.x = element_text(angle = 45, vjust = 1, size = 12, hjust = 1,
                                   family = font.family),
        axis.text.y = element_text(size = 12, family = font.family),
        legend.text = element_text(size = 12, family = font.family),
        legend.title = element_text(size = 12, family = font.family,face = "bold"),
        plot.background = element_rect(fill = "white")) +
  guides(fill = guide_colorbar(barwidth = 7, barheight = 1,
                               title.position = "top", title.hjust = 0.5))

corrMatrix_plot

ggsave(corrMatrix_plot, file = paste0(pathname, "/figures/corrMatrix_women_RF.png"),
       height = 7, width = 7, dpi = 300)

# Men
d <- trainSet_men_imp %>%
  select(!!main_dvs, Age) %>%
  mutate(across(where(is.numeric), ~ as.numeric(.x))) %>%
  data.frame()

corrMatrix <- round(hetcor(d)$cor, 2)

# Get only lower triangle of corr matrix
corrMatrix[lower.tri(corrMatrix)] <- NA

# Attach variable names
colnames(corrMatrix) <- c(main_dv_names, "Age")
rownames(corrMatrix) <- c(main_dv_names, "Age")

library("reshape2")
# Melt data for plot purposes
melted_corrMatrix <- melt(corrMatrix, na.rm = T)

# Plot that
corrMatrix_plot <- melted_corrMatrix %>%
  ggplot(aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "#4477AA", high = "#85C660", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Correlation") +
  geom_text(aes(Var2, Var1, label = value), color = "black",
            family = font.family, size = 4) +
  theme_cowplot()+ 
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.grid.major = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.ticks = element_blank(),
        legend.justification = c(1, 0),
        legend.position = c(0.4, 0.7),
        legend.direction = "horizontal",
        axis.text.x = element_text(angle = 45, vjust = 1, size = 12, hjust = 1,
                                   family = font.family),
        axis.text.y = element_text(size = 12, family = font.family),
        legend.text = element_text(size = 12, family = font.family),
        legend.title = element_text(size = 12, family = font.family,face = "bold"),
        plot.background = element_rect(fill = "white")) +
  guides(fill = guide_colorbar(barwidth = 7, barheight = 1,
                               title.position = "top", title.hjust = 0.5))

corrMatrix_plot

ggsave(corrMatrix_plot, file = paste0(pathname, "/figures/corrMatrix_men_imp.png"),
       height = 7, width = 7, dpi = 300)

# Men, predictors
d <- trainSet_men_imp %>%
  select(!!core_features_main_covariates, -Woman) %>%
  mutate(across(where(is.numeric), ~ as.numeric(.x))) %>%
  data.frame()

corrMatrix <- round(hetcor(d)$cor, 2)

# Get only lower triangle of corr matrix
corrMatrix[lower.tri(corrMatrix)] <- NA

# Attach variable names
colnames(corrMatrix) <- core_features_main_covariates[1:length(core_features_main_covariates)-1]
rownames(corrMatrix) <- core_features_main_covariates[1:length(core_features_main_covariates)-1]

# Melt data for plot purposes
melted_corrMatrix <- melt(corrMatrix, na.rm = T)

# Plot that
corrMatrix_plot <- melted_corrMatrix %>%
  ggplot(aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "#4477AA", high = "#85C660", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Correlation") +
  geom_text(aes(Var2, Var1, label = value), color = "black",
            family = font.family, size = 4) +
  theme_cowplot()+ 
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.grid.major = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.ticks = element_blank(),
        legend.justification = c(1, 0),
        legend.position = c(0.4, 0.7),
        legend.direction = "horizontal",
        axis.text.x = element_text(angle = 45, vjust = 1, size = 12, hjust = 1,
                                   family = font.family),
        axis.text.y = element_text(size = 12, family = font.family),
        legend.text = element_text(size = 12, family = font.family),
        legend.title = element_text(size = 12, family = font.family,face = "bold"),
        plot.background = element_rect(fill = "white")) +
  guides(fill = guide_colorbar(barwidth = 7, barheight = 1,
                               title.position = "top", title.hjust = 0.5))

corrMatrix_plot

ggsave(corrMatrix_plot, file = paste0(pathname, "/figures/corrMatrix_men_RF.png"),
       height = 7, width = 7, dpi = 300)

## 4.1. VIF ------------------------------------------------------
#model_ncv$final_vars

model_avg_z <- lm(avg_z ~ `Lower SES` + Depression + `Less education` + `Small social network` +
                    `Less exercise` + Loneliness + Hypertension +
                    Tinnitus + `Ever smoked` + `Hearing handicap` + DFH + Age + Woman,
                  data = trainSet_all_imp)
model_MemoryProblems <- glm(MemoryProblems ~ `Lower SES` + Depression + `Less education` + `Small social network` +
                              `Less exercise` + Loneliness + Hypertension +
                              Tinnitus + `Ever smoked` + `Hearing handicap` + DFH + Age + Woman,
                            data = trainSet_all_imp, family = "binomial")
summary(model_avg_z)
summary(model_MemoryProblems)
car::vif(model_avg_z)
car::vif(model_MemoryProblems)
# Slightly different VIFs even though these are the same predictors and the same data, probably because, as per documentation:
# "If all terms in an unweighted linear model have 1 df, then the usual variance-inflation factors are calculated.
# If any terms in an unweighted linear model have more than 1 df, then generalized variance-inflation factors (Fox and Monette, 1992) are calculated."

# Women
model_avg_z_women <- lm(avg_z ~ `Lower SES` + Depression + `Less education` + `Small social network` +
                          `Less exercise` + Loneliness + Hypertension +
                          Tinnitus + `Ever smoked` + `Hearing handicap` + DFH + Age,
                        data = trainSet_women_imp)

model_MemoryProblems_women <- glm(MemoryProblems ~ `Lower SES` + Depression + `Less education` + `Small social network` +
                                    `Less exercise` + Loneliness + Hypertension +
                                    Tinnitus + `Ever smoked` + `Hearing handicap` + DFH + Age,
                                  data = trainSet_women_imp, family = "binomial")
summary(model_avg_z_women)
summary(model_MemoryProblems_women)
car::vif(model_avg_z_women)
car::vif(model_MemoryProblems_women)

# Men
model_avg_z_men <- lm(avg_z ~ `Lower SES` + Depression + `Less education` + `Small social network` +
                        `Less exercise` + Loneliness + Hypertension +
                        Tinnitus + `Ever smoked` + `Hearing handicap` + DFH + Age,
                      data = trainSet_men_imp)

model_MemoryProblems_men <- glm(MemoryProblems ~ `Lower SES` + Depression + `Less education` + `Small social network` +
                                  `Less exercise` + Loneliness + Hypertension +
                                  Tinnitus + `Ever smoked` + `Hearing handicap` + DFH + Age,
                                data = trainSet_men_imp, family = "binomial")
summary(model_avg_z_men)
summary(model_MemoryProblems_men)
car::vif(model_avg_z_men)
car::vif(model_MemoryProblems_men)

# Interaction models
model_avg_z_int <- lm(avg_z ~ (`Lower SES` + Depression + `Less education` + `Small social network` +
                                 `Less exercise` + Loneliness + Hypertension +
                                 Tinnitus + `Ever smoked` + `Hearing handicap` + DFH + Age) * Woman,
                      data = trainSet_all_imp)

model_MemoryProblems_int <- glm(MemoryProblems ~ (`Lower SES` + Depression + `Less education` + `Small social network` +
                                                    `Less exercise` + Loneliness + Hypertension +
                                                    Tinnitus + `Ever smoked` + `Hearing handicap` + DFH + Age) * Woman,
                                data = trainSet_all_imp, family = "binomial")
summary(model_avg_z_int)
summary(model_MemoryProblems_int)
car::vif(model_avg_z_int, type="predictor") # should set type="predictor"?
car::vif(model_MemoryProblems_int) 

# 5. Generate lolliplot plots ------------------------------------------------------

## Extract shap values -------------------------------------------------------
shap_final <- data.frame(matrix(, ncol = 5, nrow = 0))
names(shap_final) <- c("subsample", "features", "dv", "variable", "value")

# Loop over subsamples
for(k in seq_along(subsamples)) {
  subsample <- subsamples[k]
  dataset <- datasets[[k]]
  print(subsample)
  print("------------")
  
  for (j in seq_along(feature_lists)) {
    # For models split by group we are only interested in the covariates + risk factors model
    ####### Covariates only model does not work with only a single predictor for now!!! ###################
    if (j != 2 & subsample %in% c("women", "men", "young", "old")) next
    # For age group interaction models we need to select the appropriate subset and no other!!
    if (j != 8 & subsample == "age" | j == 8 & subsample != "age") next
    
    # Define feature set name
    feature_list_name <- feature_list_names[[j]]
    print(feature_list_name)
    
    # Define feature list
    if (j %in% 3:8) {
      mat <- create_feature_matrix(as.character(j), dataset)
      features <- data.frame(mat, check.names = FALSE)[-1]
      names(features) <- gsub("`", "", names(features))
      names(features) <- sub(":", " * ", names(features))
    } else if (subsample %in% c("women", "men")) {
      features <- dataset %>% select(all_of(feature_lists[[j]][1:(length(feature_lists[[j]])-1)]))
    } else {
      features <- dataset %>% select(all_of(feature_lists[[j]]))
    }
    
    
    features <-  data.matrix(features)
    
    # Load in shap values and direction signs
    shap_list <- list()
    signs_list <- matrix(, ncol = length(colnames(features)), nrow = 0)
    colnames(signs_list) <- colnames(features)
    for(i in seq_along(main_dvs)) {
      dv <- main_dvs[i]
      # Load shap value files
      files_list <- list.files(path = paste0(pathname,
                                             "/output/", subsample, "_models/",
                                             dv),
                               pattern = paste0("^shap_val_", feature_list_name, ".rds$"), full.names = TRUE)
      shap_list[[dv]] <- files_list %>%
        lapply(readRDS) %>%
        as.data.frame(.)
      print(dv)
      print(files_list)
      # Extract direction
      cor1 <- diag(suppressWarnings(cor(shap_list[[dv]], features)))
      sign1 <- sign(cor1)
      sign1[is.na(sign1)] <- 1
      
      signs_list <- rbind(signs_list, sign1)
      
      rownames(signs_list)[i] <- dv
    }
  
    signs_list <- rownames_to_column(as.data.frame(signs_list), var = "dv")
  
    # Turn into data frame
    shap_list <-  combine_data_tidy(shap_list)
  
    # Unify colnames
    names(shap_list) <- colnames(signs_list)
  
    # Calculate mean absolute shap values
    shap_groups <- shap_list %>%
      mutate(across(where(is.numeric), ~ abs(.)),
             dv = factor(dv, levels = main_dvs)) %>%
      group_by(dv) %>%
      summarise(across(everything(), mean),
                .groups = "drop")
    
    # Attach direction sign
    shap_direction <- shap_groups %>%
      select(dv) %>%
      cbind(., shap_groups[,2:length(names(shap_groups))] * signs_list[2:length(names(shap_groups))])
    
    
    # Pivot shap values data frame and attach names of subsample & feature set
    shap_long <- shap_direction %>%
      pivot_longer(!dv, names_to = "variable", values_to = "value") %>%
      mutate(subsample = subsample,
             features = feature_list_name) %>%
      select(subsample, features, everything())
    
    shap_final <-  rbind(shap_final, shap_long)
  }
  
}

# Generate colour coding & rename DFH
shap_final <- shap_final %>%
  mutate(colour = factor(
    case_when(variable %in% main_covariates ~ "Benchmark  ",
              variable %in% core_features ~ "Risk factors",
              TRUE ~ "Interactions"),
    levels = c("Benchmark  ", "Risk factors", "Interactions")
  ),
  variable = case_when(variable == "DFH" ~ "Dem. family history",
                       variable == "DFH * Woman" ~ "Dem. family history * Woman",
                       variable == "Small social network" ~ "Small soc. network",
                       variable == "Small social network * Woman" ~ "Small soc. network * Woman",
                       TRUE ~ variable)
  )


# Subset table for plots
shap_all <- shap_final %>% filter(subsample == "all")
shap_final <- shap_final %>% filter(subsample != "all")

## Plot shap values for age-stratified models --------------------------------------
# 
# # Order factor levels by shap value on subjective cognition for women to unify plots
# new_order <- shap_final  %>%
#   filter(dv == "MemoryProblems",
#          subsample == "young") %>%
#   select(variable, value) %>%
#   arrange(value) %>%
#   .[[1]]
# 
# shap_final <-  shap_final %>%
#   mutate(subsample = fct_rev(factor(subsample)),
#          variable = factor(variable, levels = new_order))
# 
# # Create shading variable for plots
# rects <- data.frame(
#   xstart = rep(seq(0.5, length(feature_lists[[2]])-1 - 0.5, 1), times = 4),
#   xend = rep(seq(1.5, length(feature_lists[[2]])-1 + 0.5, 1), times = 4)
# )
# Shade <- ifelse(((ceiling(rects$xstart) %% 2) == 0), "gray90", "white")
# rects <- cbind(rects, Shade)
# shap_final <- cbind(shap_final, rects)
# 
# 
# 
# # Plot composite obj. cognition
# avg_z_lolly <- shap_final %>%
#   filter(dv == "avg_z",
#          subsample %in% c("young", "old")) %>%
#   ggplot() +
#   scale_x_continuous(limits = c(-0.04, 0.26),
#                      breaks = seq(-0.05, 0.25, by = 0.05),
#                      expand = c(0, 0)
#   ) +
#   scale_x_continuous(expand = c(0, 0)) +
#   geom_rect(
#     aes(xmin = xstart,
#         xmax = xend,
#         ymin = -0.04,
#         ymax = 0.26,
#         fill = Shade
#     ),
#     color = NA,
#     alpha = 0.3,
#     show.legend = FALSE
#   ) +
#   scale_fill_identity() +
#   scale_x_discrete(expand = c(0, 0)) +
#   geom_hline(yintercept = 0, linetype = "dashed", size = 0.6) +
#   ggnewscale::new_scale_fill() +
#   geom_point(aes(x = variable, y = value, fill = subsample, colour = subsample),
#              size = 10, alpha = 0.7,
#              position = position_dodge(0.7)) +
#   geom_hline(yintercept = 0, linetype = "dashed", size = 0.6) +
#   geom_linerange(aes(y = value,
#                      ymax = value,
#                      ymin = 0,
#                      x = variable,
#                      colour = subsample),
#                  linewidth = 2,
#                  alpha = 1,
#                  position = position_dodge(0.7)) +
#   coord_flip()+
#   scale_y_continuous(limits = c(-0.04, 0.26),
#                      breaks = seq(-0.05, 0.25, by = 0.05)) +
#   scale_color_manual(values = my.palette) +
#   theme_classic() +
#   theme_main +
#   labs(color = "Gender", fill = "Gender") +
#   ggtitle("Worse objective cognition") +
#   xlab("") + ylab("") +
#   # For extracting the legend
#   theme(legend.margin = margin(t = 0, r = 15, b = 0.5, l = 0, unit = "cm"),
#         legend.position = "bottom")
# 
# # Plot memory problems
# MemoryProblems_lolly <-
#   shap_final %>%
#   filter(dv == "MemoryProblems") %>%
#   ggplot() +
#   scale_x_continuous(limits=c(NA, NA),
#                      breaks = seq(-0.10, 0.25, by = 0.05),
#                      expand = c(0, 0)
#   ) +
#   scale_x_continuous(expand = c(0, 0)) +
#   geom_rect(
#     aes(xmin = xstart,
#         xmax = xend,
#         ymin = -0.015,
#         ymax = 0.075,
#         fill = Shade
#     ),
#     color = NA,
#     alpha = 0.3,
#     show.legend = FALSE
#   ) +
#   scale_fill_identity() +
#   scale_x_discrete(expand = c(0, 0)) +
#   geom_hline(yintercept = 0, linetype = "dashed", size = 0.6) +
#   ggnewscale::new_scale_fill() +
#   geom_point(aes(x = variable, y = value, fill = subsample, colour = subsample),
#              size = 10, alpha = 0.7,
#              position = position_dodge(0.7)) +
#   geom_linerange(aes(y = value,
#                      ymax = value,
#                      ymin = 0,
#                      x = variable,
#                      colour = subsample),
#                  linewidth = 2,
#                  alpha = 1,
#                  position = position_dodge(0.7)) +
#   coord_flip()+
#   scale_y_continuous(limits = c(NA, NA),
#                      breaks = seq(-0.10, 0.25, by = 0.05)) +
#   scale_color_manual(values = my.palette) +
#   scale_fill_manual(values = my.palette) +
#   theme_classic() +
#   theme_main +
#   labs(color = "Gender", fill = "Gender") +
#   ggtitle("Subjective memory problems") +
#   xlab("") + ylab("")
# 
# 
# # Combine figure
# fig1 <- ggarrange(
#   MemoryProblems_lolly +
#     theme(
#       axis.ticks.y = element_blank(),
#       plot.margin = margin(t = 0.2, r = 0, b = 0, l = 0.4, unit = "cm")
#     ),
#   avg_z_lolly +
#     theme(plot.margin = margin(t = 0.2, r = 0.2, b = 0, l = 0, unit = "cm"),
#           axis.text.y = element_blank(),
#           axis.ticks.y = element_blank()),
#   legend = "bottom",
#   ncol = 2, nrow = 1,
#   #labels = c("A", "B"),
#   font.label = list(size = 30, family = font.family),
#   legend.grob = get_legend(avg_z_lolly),
#   widths = c(0.55, 0.45)
# ) +
#   theme(plot.background = element_rect(fill = "white"))
# fig1
# 
# ggsave(fig1,
#        file = paste0(pathname, "/figures/gender/fig1.png"),
#        width = 15, height = 10, dpi = 300)

## Plot shap values for full models --------------------------------------
# Create order objects for ordering plots
all_subj_main_covariates_order <- shap_all  %>%
  filter(dv == "MemoryProblems",
         features == "main_covariates") %>%
  select(variable, value) %>%
  arrange(value) %>%
  .[[1]]

all_obj_main_covariates_order <- shap_all  %>%
  filter(dv == "avg_z",
         features == "main_covariates") %>%
  select(variable, value) %>%
  arrange(value) %>%
  .[[1]]

all_subj_core_features_main_covariates_order <- shap_all  %>%
  filter(dv == "MemoryProblems",
         features == "core_features_main_covariates") %>%
  select(variable, value) %>%
  arrange(value) %>%
  .[[1]]

all_obj_core_features_main_covariates_order <- shap_all  %>%
  filter(dv == "avg_z",
         features == "core_features_main_covariates") %>%
  select(variable, value) %>%
  arrange(value) %>%
  .[[1]]

all_subj_interactions_gender_order <- shap_all  %>%
  filter(dv == "MemoryProblems",
         features == "interactions_gender") %>%
  select(variable, value) %>%
  arrange(value) %>%
  .[[1]]

all_obj_interactions_gender_order <- shap_all  %>%
  filter(dv == "avg_z",
         features == "interactions_gender") %>%
  select(variable, value) %>%
  arrange(value) %>%
  .[[1]]

# all_subj_interactions_age_order <- shap_all  %>%
#   filter(dv == "MemoryProblems",
#          features == "interactions_age") %>%
#   select(variable, value) %>%
#   arrange(value) %>%
#   .[[1]]
# 
# all_obj_interactions_age_order <- shap_all  %>%
#   filter(dv == "avg_z",
#          features == "interactions_age") %>%
#   select(variable, value) %>%
#   arrange(value) %>%
#   .[[1]]
# 
# all_subj_interactions_ses_order <- shap_all  %>%
#   filter(dv == "MemoryProblems",
#          features == "interactions_ses") %>%
#   select(variable, value) %>%
#   arrange(value) %>%
#   .[[1]]
# 
# all_obj_interactions_ses_order <- shap_all  %>%
#   filter(dv == "avg_z",
#          features == "interactions_ses") %>%
#   select(variable, value) %>%
#   arrange(value) %>%
#   .[[1]]
# 
# all_subj_all_interactions_order <- shap_all  %>%
#   filter(dv == "MemoryProblems",
#          features == "all_interactions") %>%
#   select(variable, value) %>%
#   arrange(value) %>%
#   .[[1]]
# 
# all_obj_all_interactions_order <- shap_all  %>%
#   filter(dv == "avg_z",
#          features == "all_interactions") %>%
#   select(variable, value) %>%
#   arrange(value) %>%
#   .[[1]]
# 
# all_subj_age_group_interactions_order <- shap_all  %>%
#   filter(dv == "MemoryProblems",
#          features == "interactions_age_group") %>%
#   select(variable, value) %>%
#   arrange(value) %>%
#   .[[1]]
# 
# all_obj_age_group_interactions_order <- shap_all  %>%
#   filter(dv == "avg_z",
#          features == "interactions_age_group") %>%
#   select(variable, value) %>%
#   arrange(value) %>%
#   .[[1]]

# Create shading variable for plots
rects <- data.frame(
  xstart = rep(seq(0.5, length(feature_lists[[3]]) - 0.5, 1), times = 4),
  xend = rep(seq(1.5, length(feature_lists[[3]]) + 0.5, 1), times = 4)
)
Shade <- ifelse(((ceiling(rects$xstart) %% 2) == 0), "gray90", "white")
rects <- cbind(rects, Shade)

### Covariates only models --------
# Plot composite obj. cognition
avg_z_lolly_benchmark <- shap_all %>%
  filter(features == "main_covariates",
         dv == "avg_z") %>%
  cbind(., rects[1:(length(feature_lists[[1]])),]) %>%
  mutate(variable = factor(variable, levels = all_obj_main_covariates_order)) %>%
  ggplot() +
  scale_x_continuous(limits = c(-0.05, 0.27),
                     breaks = seq(-0.05, 0.25, by = 0.05),
                     expand = c(0, 0)
  ) +
  scale_x_continuous(expand = c(0, 0)) +
  geom_rect(
    aes(xmin = xstart,
        xmax = xend,
        ymin = -0.05,
        ymax = 0.27,
        fill = Shade
    ),
    color = NA,
    alpha = 0.3,
    show.legend = FALSE
  ) +
  scale_fill_identity() +
  scale_x_discrete(expand = c(0, 0)) +
  geom_hline(yintercept = 0, linetype = "dashed", size = 0.6) +
  ggnewscale::new_scale_fill() +
  geom_point(aes(x = variable, y = value, fill = colour, colour = colour),
             size = 10, alpha = 0.4,
             position = position_dodge(0.7)) +
  geom_hline(yintercept = 0, linetype = "dashed", size = 0.6) +
  geom_linerange(aes(y = value,
                     ymax = value,
                     ymin = 0,
                     x = variable,
                     colour = colour),
                 linewidth = 2,
                 alpha = 1,
                 position = position_dodge(0.7)) +
  coord_flip()+
  scale_y_continuous(limits = c(-0.05, 0.27),
                     breaks = seq(-0.05, 0.25, by = 0.05)) +
  scale_color_manual(values = my.palette[c(7,8,9)]) +
  theme_classic() +
  theme_main +
  labs(color = "Gender", fill = "Gender") +
  ggtitle("Worse objective cognition") +
  xlab("") + ylab("") +
  # For extracting the legend
  theme(legend.margin = margin(t = 0, r = 15, b = 0.5, l = 0, unit = "cm"),
        legend.position = "none")

avg_z_lolly_benchmark

# Plot memory problems
MemoryProblems_lolly_benchmark <- shap_all %>%
  filter(features == "main_covariates",
         dv == "MemoryProblems") %>%
  cbind(., rects[1:(length(feature_lists[[1]])),]) %>%
  mutate(variable = factor(variable, levels = all_subj_main_covariates_order)) %>%
  ggplot() +
  scale_x_continuous(limits = c(-0.016, 0.07),
                     breaks = seq(-0.10, 0.25, by = 0.05),
                     expand = c(0, 0)
  ) +
  scale_x_continuous(expand = c(0, 0)) +
  geom_rect(
    aes(xmin = xstart,
        xmax = xend,
        ymin = -0.016,
        ymax = 0.07,
        fill = Shade
    ),
    color = NA,
    alpha = 0.3,
    show.legend = FALSE
  ) +
  scale_fill_identity() +
  scale_x_discrete(expand = c(0, 0)) +
  geom_hline(yintercept = 0, linetype = "dashed", size = 0.6) +
  ggnewscale::new_scale_fill() +
  geom_point(aes(x = variable, y = value, fill = colour, colour = colour),
             size = 10, alpha = 0.4,
             position = position_dodge(0.7)) +
  geom_linerange(aes(y = value,
                     ymax = value,
                     ymin = 0,
                     x = variable,
                     colour = colour),
                 linewidth = 2,
                 alpha = 1,
                 position = position_dodge(0.7)) +
  coord_flip()+
  scale_y_continuous(limits = c(-0.016, 0.07),
                     breaks = seq(-0.10, 0.25, by = 0.05)) +
  scale_color_manual(values = my.palette[c(7,8,9)]) +
  theme_classic() +
  theme_main +
  labs(color = "", fill = "") +
  ggtitle("Subjective memory problems") +
  xlab("") + ylab("")
MemoryProblems_lolly_benchmark

# Combine figure
fig2 <- ggarrange(
  avg_z_lolly_benchmark +
    theme(plot.margin = margin(t = 0.2, r = 0, b = 0, l = 0, unit = "cm"),
          axis.ticks.y = element_blank()),
  MemoryProblems_lolly_benchmark +
    theme(
      axis.ticks.y = element_blank(),
      plot.margin = margin(t = 0.2, r = 0.2, b = 0, l = 0, unit = "cm")
    ),
  legend = "none",
  ncol = 2, nrow = 1,
  #labels = c("A", "B"),
  font.label = list(size = 30, family = font.family),
  widths = c(0.60, 0.40)
) +
  theme(plot.background = element_rect(fill = "white"))
fig2

ggsave(fig2,
       file = paste0(pathname, "/figures/gender/fig2.png"),
       width = 12, height = 5, dpi = 300)

### Covariates + risk factors models --------
# Plot composite obj. cognition
avg_z_lolly_all <- shap_all %>%
  filter(features == "core_features_main_covariates",
         dv == "avg_z") %>%
  cbind(., rects[1:(length(feature_lists[[2]])),]) %>%
  mutate(variable = factor(variable, levels = all_obj_core_features_main_covariates_order )) %>%
  ggplot() +
  scale_x_continuous(limits = c(-0.05, 0.27),
                     breaks = seq(-0.05, 0.25, by = 0.05),
                     expand = c(0, 0)
  ) +
  scale_x_continuous(expand = c(0, 0)) +
  geom_rect(
    aes(xmin = xstart,
        xmax = xend,
        ymin = -0.05,
        ymax = 0.27,
        fill = Shade
    ),
    color = NA,
    alpha = 0.3,
    show.legend = FALSE
  ) +
  scale_fill_identity() +
  scale_x_discrete(expand = c(0, 0)) +
  geom_hline(yintercept = 0, linetype = "dashed", size = 0.6) +
  ggnewscale::new_scale_fill() +
  geom_point(aes(x = variable, y = value, fill = colour, colour = colour),
             size = 10, alpha = 0.4,
             position = position_dodge(0.7)) +
  geom_hline(yintercept = 0, linetype = "dashed", size = 0.6) +
  geom_linerange(aes(y = value,
                     ymax = value,
                     ymin = 0,
                     x = variable,
                     colour = colour),
                 linewidth = 2,
                 alpha = 1,
                 position = position_dodge(0.7)) +
  coord_flip()+
  scale_y_continuous(limits = c(-0.05, 0.27),
                     breaks = seq(-0.05, 0.25, by = 0.05)) +
  scale_color_manual(values = my.palette[c(7,8,9)]) +
  theme_classic() +
  theme_main +
  labs(color = "Gender", fill = "Gender") +
  ggtitle("Worse objective cognition") +
  xlab("") + ylab("") +
  # For extracting the legend
  theme(legend.margin = margin(t = 0, r = 15, b = 0.5, l = 0, unit = "cm"),
        legend.position = "none")

avg_z_lolly_all

# Plot memory problems
MemoryProblems_lolly_all <- shap_all %>%
  filter(features == "core_features_main_covariates",
         dv == "MemoryProblems") %>%
  cbind(., rects[1:(length(feature_lists[[2]])),]) %>%
  mutate(variable = factor(variable, levels = all_subj_core_features_main_covariates_order)) %>%
  ggplot() +
  scale_x_continuous(limits=c(-0.016, 0.07),
                     breaks = seq(-0.10, 0.25, by = 0.05),
                     expand = c(0, 0)
  ) +
  scale_x_continuous(expand = c(0, 0)) +
  geom_rect(
    aes(xmin = xstart,
        xmax = xend,
        ymin = -0.016,
        ymax = 0.07,
        fill = Shade
    ),
    color = NA,
    alpha = 0.3,
    show.legend = FALSE
  ) +
  scale_fill_identity() +
  scale_x_discrete(expand = c(0, 0)) +
  geom_hline(yintercept = 0, linetype = "dashed", size = 0.6) +
  ggnewscale::new_scale_fill() +
  geom_point(aes(x = variable, y = value, fill = colour, colour = colour),
             size = 10, alpha = 0.4,
             position = position_dodge(0.7)) +
  geom_linerange(aes(y = value,
                     ymax = value,
                     ymin = 0,
                     x = variable,
                     colour = colour),
                 linewidth = 2,
                 alpha = 1,
                 position = position_dodge(0.7)) +
  coord_flip()+
  scale_y_continuous(limits = c(-0.016, 0.07),
                     breaks = seq(-0.10, 0.25, by = 0.05)) +
  scale_color_manual(values = my.palette[c(7,8,9)]) +
  theme_classic() +
  theme_main +
  labs(color = "", fill = "") +
  ggtitle("Subjective memory problems") +
  xlab("") + ylab("")
MemoryProblems_lolly_all

# Combine figure
fig3 <- ggarrange(
  avg_z_lolly_all +
    theme(plot.margin = margin(t = 0.2, r = 0, b = 0, l = 0, unit = "cm"),
          axis.ticks.y = element_blank()),
  MemoryProblems_lolly_all +
    theme(
      axis.ticks.y = element_blank(),
      plot.margin = margin(t = 0.2, r = 0.2, b = 0, l = 0.4, unit = "cm")
    ),
  legend = "none",
  ncol = 2, nrow = 1,
  #labels = c("A", "B"),
  font.label = list(size = 30, family = font.family),
  widths = c(0.60, 0.40)
) +
  theme(plot.background = element_rect(fill = "white"))
fig3

ggsave(fig3,
       file = paste0(pathname, "/figures/gender/fig3.png"),
       width = 20, height = 10, dpi = 300)


### Gender interactions models --------
# Plot composite obj. cognition
avg_z_lolly_interactions <- shap_all %>%
  filter(features == "interactions_gender",
         dv == "avg_z") %>%
  cbind(., rects[1:(length(feature_lists[[3]])),]) %>%
  mutate(variable = factor(variable, levels = all_obj_interactions_gender_order),
         nullified = case_when(value == 0 ~ TRUE,
                               TRUE ~ FALSE)) %>%
  ggplot() +
  scale_x_continuous(limits = c(-0.05, 0.27),
                     breaks = seq(-0.05, 0.25, by = 0.05),
                     expand = c(0, 0)
  ) +
  scale_x_continuous(expand = c(0, 0)) +
  geom_rect(
    aes(xmin = xstart,
        xmax = xend,
        ymin = -0.05,
        ymax = 0.27,
        fill = Shade
    ),
    color = NA,
    alpha = 0.3,
    show.legend = FALSE
  ) +
  scale_fill_identity() +
  scale_x_discrete(expand = c(0, 0)) +
  geom_hline(yintercept = 0, linetype = "dashed", size = 0.6) +
  ggnewscale::new_scale_fill() +
  geom_point(aes(x = variable, y = value, fill = colour, colour = colour, shape = nullified),
             size = 10, alpha = 0.4, stroke = 2,
             position = position_dodge(0.7)) +
  scale_shape_manual(values = c(16, 1),
                     guide = NULL,
                     name = element_blank()) +
  geom_hline(yintercept = 0, linetype = "dashed", size = 0.6) +
  geom_linerange(aes(y = value,
                     ymax = value,
                     ymin = 0,
                     x = variable,
                     colour = colour),
                 linewidth = 2,
                 alpha = 1,
                 position = position_dodge(0.7)) +
  coord_flip()+
  scale_y_continuous(limits = c(-0.05, 0.27),
                     breaks = seq(-0.05, 0.25, by = 0.05)) +
  scale_color_manual(values = my.palette[c(7,8,9)]) +
  theme_classic() +
  theme_main +
  ggtitle("Worse objective cognition") +
  xlab("") + ylab("") +
  # For extracting the legend
  theme(legend.margin = margin(t = 0, r = 15, b = 0.5, l = 0, unit = "cm"),
        legend.position = "none")

avg_z_lolly_interactions

# Plot memory problems
MemoryProblems_lolly_interactions <- shap_all %>%
  filter(features == "interactions_gender",
         dv == "MemoryProblems") %>%
  cbind(., rects[1:(length(feature_lists[[3]])),]) %>%
  mutate(variable = factor(variable, levels = all_subj_interactions_gender_order),,
         nullified = case_when(value == 0 ~ TRUE,
                               TRUE ~ FALSE)) %>%
  ggplot() +
  scale_x_continuous(limits = c(-0.016, 0.07),
                     breaks = seq(-0.10, 0.25, by = 0.05),
                     expand = c(0, 0)
  ) +
  scale_x_continuous(expand = c(0, 0)) +
  geom_rect(
    aes(xmin = xstart,
        xmax = xend,
        ymin = -0.016,
        ymax = 0.07,
        fill = Shade
    ),
    color = NA,
    alpha = 0.3,
    show.legend = FALSE
  ) +
  scale_fill_identity() +
  scale_x_discrete(expand = c(0, 0)) +
  geom_hline(yintercept = 0, linetype = "dashed", size = 0.6) +
  ggnewscale::new_scale_fill() +
  geom_point(aes(x = variable, y = value, fill = colour, colour = colour, shape = nullified),
             size = 10, alpha = 0.4, stroke = 2,
             position = position_dodge(0.7)) +
  scale_shape_manual(values = c(16, 1),
                     guide = NULL,
                     name = element_blank()) +
  geom_linerange(aes(y = value,
                     ymax = value,
                     ymin = 0,
                     x = variable,
                     colour = colour),
                 linewidth = 2,
                 alpha = 1,
                 position = position_dodge(0.7)) +
  coord_flip()+
  scale_y_continuous(limits = c(-0.016, 0.07),
                     breaks = seq(-0.10, 0.25, by = 0.05)) +
  scale_color_manual(values = my.palette[c(7,8,9)]) +
  theme_classic() +
  theme_main +
  ggtitle("Subjective memory problems") +
  xlab("") + ylab("") +
  labs(color = "", fill = "")

MemoryProblems_lolly_interactions

# Combine figure
fig4 <- ggarrange(
  avg_z_lolly_interactions +
    theme(plot.margin = margin(t = 0.2, r = 0, b = 0, l = 0, unit = "cm"),
          axis.ticks.y = element_blank()),
  MemoryProblems_lolly_interactions +
    theme(
      axis.ticks.y = element_blank(),
      plot.margin = margin(t = 0.2, r = 0.2, b = 0, l = 0.4, unit = "cm")
    ),
  legend = "none",
  ncol = 2, nrow = 1,
  #labels = c("A", "B"),
  font.label = list(size = 30, family = font.family),
  widths = c(0.50, 0.50)
) +
  theme(plot.background = element_rect(fill = "white"))
fig4

ggsave(fig4,
       file = paste0(pathname, "/figures/gender/fig4.png"),
       width = 25, height = 13, dpi = 300)


### Create combined figure for paper, v1 ----------------
figure_sensitivity_shap <- ggarrange(
  avg_z_lolly_benchmark +
    theme(plot.margin = margin(t = 0.2, r = 0, b = 0, l = 8.1, unit = "cm"),
          axis.ticks.y = element_blank()),
  MemoryProblems_lolly_benchmark +
    theme(
      axis.ticks.y = element_blank(),
      plot.margin = margin(t = 0.2, r = 0.2, b = 0, l = 8.1, unit = "cm")
    ),
  avg_z_lolly_all +
    theme(plot.margin = margin(t = 0.2, r = 0, b = 0, l = 2.85, unit = "cm"),
          axis.ticks.y = element_blank()),
  MemoryProblems_lolly_all +
    theme(
      axis.ticks.y = element_blank(),
      plot.margin = margin(t = 0.2, r = 0.2, b = 0, l = 2.85, unit = "cm")
    ),
  avg_z_lolly_interactions +
    theme(plot.margin = margin(t = 0.2, r = 0, b = 0, l = 0, unit = "cm"),
          axis.ticks.y = element_blank()),
  MemoryProblems_lolly_interactions +
    theme(
      axis.ticks.y = element_blank(),
      plot.margin = margin(t = 0.2, r = 0.2, b = 0, l = 0, unit = "cm")
    ),
  legend.grob = get_legend(MemoryProblems_lolly_interactions +
                             labs(color = "Predictor type", fill = "Predictor type") +
                             theme(legend.margin = margin(t = 0.2, r = 14, b = 0.5, l = 0, unit = "cm"),
                                   legend.direction = "horizontal")),
  #common.legend = T,
  ncol = 2, nrow = 3,
  labels = c("A", "", "B", "", "C", ""),
  font.label = list(size = 30, family = font.family),
  heights = c(0.2, 0.50, 1.0),
  widths = c(0.55, 0.45)
) +
  theme(plot.background = element_rect(fill = "white"))
figure_sensitivity_shap

ggsave(figure_sensitivity_shap,
       file = paste0(pathname, "/figures/figure_sensitivity_shap.png"),
       width = 20, height = 25, dpi = 300)


## Plot shap values for gender-stratified models --------------------------------------
# Order factor levels by shap value on subjective cognition for women to unify plots
new_order <- shap_final  %>%
  filter(dv == "MemoryProblems",
         subsample == "women") %>%
  select(variable, value) %>%
  arrange(value) %>%
  .[[1]]

shap_final <-  shap_final %>%
  mutate(subsample = fct_rev(factor(subsample)),
         variable = factor(variable, levels = new_order))

# Create shading variable for plots
#### this will need updating in case we include age and other interactions
rects <- data.frame(
  xstart = rep(seq(0.5, length(feature_lists[[2]])-1 - 0.5, 1), times = 4),
  xend = rep(seq(1.5, length(feature_lists[[2]])-1 + 0.5, 1), times = 4)
)
Shade <- ifelse(((ceiling(rects$xstart) %% 2) == 0), "gray90", "white")
rects <- cbind(rects, Shade)
shap_final <- cbind(shap_final, rects)



# Plot composite obj. cognition
avg_z_lolly <- shap_final %>%
  filter(dv == "avg_z") %>%
  ggplot() +
  scale_x_continuous(limits = c(-0.04, 0.28),
                     breaks = seq(-0.05, 0.25, by = 0.05),
                     expand = c(0, 0)
  ) +
  scale_x_continuous(expand = c(0, 0)) +
  geom_rect(
    aes(xmin = xstart,
        xmax = xend,
        ymin = -0.04,
        ymax = 0.28,
        fill = Shade
    ),
    color = NA,
    alpha = 0.3,
    show.legend = FALSE
  ) +
  scale_fill_identity() +
  scale_x_discrete(expand = c(0, 0)) +
  geom_hline(yintercept = 0, linetype = "dashed", size = 0.6) +
  ggnewscale::new_scale_fill() +
  geom_point(aes(x = variable, y = value, fill = subsample, colour = subsample),
             size = 10, alpha = 0.7,
             position = position_dodge(0.7)) +
  geom_hline(yintercept = 0, linetype = "dashed", size = 0.6) +
  geom_linerange(aes(y = value,
                     ymax = value,
                     ymin = 0,
                     x = variable,
                     colour = subsample),
                 linewidth = 2,
                 alpha = 1,
                 position = position_dodge(0.7)) +
  coord_flip()+
  scale_y_continuous(limits = c(-0.04, 0.28),
                     breaks = seq(-0.05, 0.25, by = 0.05)) +
  scale_color_manual(values = my.palette) +
  theme_classic() +
  theme_main +
  labs(color = "Gender", fill = "Gender") +
  ggtitle("Worse objective cognition") +
  xlab("") + ylab("") +
  # For extracting the legend
  theme(legend.margin = margin(t = 0, r = 15, b = 0.5, l = 0, unit = "cm"),
        legend.position = "bottom")

# Plot memory problems
MemoryProblems_lolly <-
  shap_final %>%
  filter(dv == "MemoryProblems") %>%
  ggplot() +
  scale_x_continuous(limits=c(NA, NA),
                     breaks = seq(-0.10, 0.25, by = 0.05),
                     expand = c(0, 0)
  ) +
  scale_x_continuous(expand = c(0, 0)) +
  geom_rect(
    aes(xmin = xstart,
        xmax = xend,
        ymin = -0.015,
        ymax = 0.075,
        fill = Shade
    ),
    color = NA,
    alpha = 0.3,
    show.legend = FALSE
  ) +
  scale_fill_identity() +
  scale_x_discrete(expand = c(0, 0)) +
  geom_hline(yintercept = 0, linetype = "dashed", size = 0.6) +
  ggnewscale::new_scale_fill() +
  geom_point(aes(x = variable, y = value, fill = subsample, colour = subsample),
             size = 10, alpha = 0.7,
             position = position_dodge(0.7)) +
  geom_linerange(aes(y = value,
                     ymax = value,
                     ymin = 0,
                     x = variable,
                     colour = subsample),
                 linewidth = 2,
                 alpha = 1,
                 position = position_dodge(0.7)) +
  coord_flip()+
  scale_y_continuous(limits = c(NA, NA),
                     breaks = seq(-0.10, 0.25, by = 0.05)) +
  scale_color_manual(values = my.palette) +
  scale_fill_manual(values = my.palette) +
  theme_classic() +
  theme_main +
  labs(color = "Gender", fill = "Gender") +
  ggtitle("Subjective memory problems") +
  xlab("") + ylab("")


# Combine figure
fig1 <- ggarrange(
  MemoryProblems_lolly +
    theme(
      axis.ticks.y = element_blank(),
      plot.margin = margin(t = 0.2, r = 0, b = 0, l = 0.4, unit = "cm")
    ),
  avg_z_lolly +
    theme(plot.margin = margin(t = 0.2, r = 0.2, b = 0, l = 0, unit = "cm"),
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank()),
  legend = "bottom",
  ncol = 2, nrow = 1,
  #labels = c("A", "B"),
  font.label = list(size = 30, family = font.family),
  legend.grob = get_legend(avg_z_lolly),
  widths = c(0.55, 0.45)
) +
  theme(plot.background = element_rect(fill = "white"))
fig1

ggsave(fig1,
       file = paste0(pathname, "/figures/gender/fig1.png"),
       width = 15, height = 10, dpi = 300)



# 6. Extracting final model parameters ------------------------------------
feature_list_names_reordered <- sort(feature_list_names)
parameters_table <- matrix(ncol = 4, nrow = 2* length(feature_list_names_reordered))
colnames(parameters_table) <- c("dv", "features", "alpha", "lambda")

for (i in seq_along(main_dvs)) {
  dv <- main_dvs[i]
  # Load model files
  files_list <- list.files(
    path = paste0(
      pathname,
      "/output/all_models/",
      dv
    ),
    pattern = "model_",
    full.names = TRUE
  )
  files_list <- files_list[!str_detect(files_list, "age|education|all_int|ons_ses")]
  models_list <- files_list %>%
    lapply(readRDS)
  # Print model info
  print(dv)
  print(files_list)

  for(j in seq_along(feature_list_names_reordered)) {
    if (i == 1) {
      parameters_table[j,] <- c(dv, feature_list_names_reordered[j], as.numeric(models_list[[j]]$finalTune))
    } else {
      parameters_table[j +3 ,] <- c(dv, feature_list_names_reordered[j], as.numeric(models_list[[j]]$finalTune))
    }
  }
}

print(parameters_table)
rm(models_list)
parameters_table <- as.data.frame(parameters_table) %>%
  mutate(across(c(alpha, lambda),
                ~ round(as.numeric(.x), digits = 4)))

save_as_docx(flextable(parameters_table, cwidth = 1.5, cheight = 0.25),
             path = paste0(pathname, "/output/final_model_parameters.docx")
)



# 7. Calculating performance metrics ---------------------------------------
# Reorder feature lists alphabetically
feature_list_names_reordered <- sort(feature_list_names)

# Generate a list of names to loop over train and test datasets
subsets <- c("train", "test")

# Create a list of labels for the comparisons
list_of_comparison_names <- c("covariates_to_baseline", "baseline_to_interactions", "covariates_to_interactions") # Labels used internally throughout the code
titles_list <- c("Benchmark to risk factors", "Risk factors to\nrisk factors + gender interactions", "Benchmark to\nrisk factors + gender interactions") # This will be printed in the plots

# Extract observed data
avg_z_train <- trainSet_all_imp %>%
  select(avg_z)
MemoryProblems_train <- trainSet_all_imp %>%
  select(MemoryProblems)
avg_z_test <- testSet_all_imp %>%
  select(avg_z)
MemoryProblems_test <- testSet_all_imp %>%
  select(MemoryProblems)

# Generate an index for participant/observation
avg_z_train$obs <- as.numeric(rownames(avg_z_train))
MemoryProblems_train$obs <- as.numeric(rownames(MemoryProblems_train))
avg_z_test$obs <- as.numeric(rownames(avg_z_test))
MemoryProblems_test$obs <- as.numeric(rownames(MemoryProblems_test))

# Loop over saved models
if(update_data == TRUE) {
  performance_MemoryProblems <- performance_avg_z <- matrix(, ncol = 4, nrow = 0) %>%
    as.data.frame()
  names(performance_MemoryProblems) <- names(performance_avg_z) <- c("feature_list", "sample", "metric", "value")
  
  for (i in seq_along(main_dvs)) {
    dv <- main_dvs[i]
    if (dv == "avg_z") {
      actual_test <- avg_z_test
    } else {
      actual_test <- MemoryProblems_test
    }
    # Load model files
    files_list <- list.files(
      path = paste0(
        pathname,
        "/output/all_models/",
        dv
      ),
      pattern = "model_",
      full.names = TRUE
    )
    files_list <- files_list[!str_detect(files_list, "age|education|all_int|ons_ses")]
    models_list <- files_list %>%
      lapply(readRDS)
    # Print model info
    print(dv)
    print(files_list)
    
    for(j in seq_along(feature_list_names_reordered)) {
      feature_list_test <- if(j ==1) {
        testSet_all_imp %>%
          select(any_of(core_features_main_covariates))
      } else if (j ==2) {
        model.matrix(~ (`Lower SES` + Depression + `Less education` +
                          `Small social network` + `Less exercise` +
                          Loneliness + Hypertension + Tinnitus +
                          `Ever smoked` + `Hearing handicap` + DFH +
                          Age) * `Woman`, data = testSet_all_imp) %>%
          data.frame(., check.names = FALSE) %>%
          select(-`(Intercept)`) %>%
          rename_with(~ gsub("`", "", .x)) %>%
          rename_with(~ sub(":", " * ", .x))
      } else {
        testSet_all_imp %>%
          select(any_of(main_covariates))
      }
      
      # Generate predictions and performance metrics
      pred_test <- predict(models_list[[j]], as.matrix(feature_list_test))
      
      if (i == 1) {
        # Generate performance metrics for binary outcome
        # Train set - extract metrics from model object, left-out outer folds
        cat("\nTrain set performance for", feature_list_names_reordered[j], "model\n")
        pred_train <- models_list[[j]]$output$predy
        actual_train <- models_list[[j]]$output$testy
        pred_train_probs <- models_list[[j]]$output$predyp
        
        auc_value <- models_list[[j]]$summary$metrics[["AUC"]]
        accuracy <- models_list[[j]]$summary$metrics[["Accuracy"]]
        balanced_accuracy <- models_list[[j]]$summary$metrics[["Balanced accuracy"]]
        
        output_for_mnLogLoss <- data.frame(obs = actual_train,
                                           pred = pred_train,
                                           Yes = pred_train_probs,
                                           No = 1 - pred_train_probs)
        ll <- mnLogLoss(output_for_mnLogLoss, lev = levels(output_for_mnLogLoss$pred))
        
        performance_train <- c(auc_value, accuracy, balanced_accuracy, ll)
        
        cat("AUC: ", round(auc_value, 3),
            "\nAccuracy: ", round(accuracy, 3),
            "\nBalanced accuracy: ", round(balanced_accuracy, 3),
            "\nLog loss: ", round(ll, 3))
        
        # Generate performance metrics for binary outcome
        # Test set - calculate performance metrics manually by fitting final model to held-out data
        cat("\nTest set performance for", feature_list_names_reordered[j], "model\n")
        pred_test_probs <- predict(models_list[[j]], as.matrix(feature_list_test), type = "prob")[,2]
        
        TP <- sum(actual_test[[1]] == 1 & pred_test == "Yes")
        TN <- sum(actual_test[[1]] == 0 & pred_test == "No")
        FP <- sum(actual_test[[1]] == 0 & pred_test == "Yes")
        FN <- sum(actual_test[[1]] == 1 & pred_test == "No")
        
        accuracy <- (TP + TN) / (TP + TN + FP + FN)
        sensitivity <- TP / (TP + FN)
        specificity <- TN / (TN + FP)
        balanced_accuracy <- (sensitivity + specificity) / 2
        
        ROC <- suppressMessages(roc(actual_test[[1]], pred_test_probs, direction = "<"))
        auc_value <- pROC::auc(ROC)
        
        output_for_mnLogLoss <- data.frame(obs = factor(actual_test[[1]], labels = c("No", "Yes")),
                                           pred = pred_test,
                                           Yes = pred_test_probs,
                                           No = 1 - pred_test_probs)
        ll <- mnLogLoss(output_for_mnLogLoss, lev = levels(output_for_mnLogLoss$pred))
        
        performance_test <- c(auc_value, accuracy, balanced_accuracy, ll)
        
        cat("AUC: ", round(auc_value, 3),
            "\nAccuracy: ", round(accuracy, 3),
            "\nBalanced accuracy: ", round(balanced_accuracy, 3),
            "\nLog loss: ", round(ll, 3))
        
        # Assemble performance metrics
        performance_temp <- data.frame(feature_list = feature_list_names_reordered[j],
                                       sample = rep(c("train", "test"), each = 4),
                                       metric = rep(c("AUC", "Accuracy", "Balanced accuracy", "LogLoss"), times = 2),
                                       value = c(performance_train, performance_test))
        performance_MemoryProblems <- rbind(performance_MemoryProblems, performance_temp)
        
      } else {
        # Generate performance metrics for continuous outcome
        # Train set - extract metrics from model object, left-out outer folds
        cat("\nTrain set performance for", feature_list_names_reordered[j], "model\n")
        
        pred_train <- models_list[[j]]$output$predy
        actual_train <- models_list[[j]]$output$testy
        
        cor <- cor.test(pred_train, actual_train)$estimate
        mae <- caret::MAE(pred_train, actual_train)
        rmse <- caret::RMSE(pred_train, actual_train)
        rsq <- cor^2
        
        performance_train <- c(rmse, mae, cor, rsq)
        
        cat("MAE: ", round(mae, 3), "\nRMSE: ", round(rmse, 3),
            "\nPEARSON: ", round(cor, 3), "\nRsq: ", round(rsq, 3))
        
        
        # Generate performance metrics for continuous outcome
        # Test set - calculate performance metrics manually by fitting final model to held-out data
        cat("\nTest set performance for", feature_list_names_reordered[j], "model\n")
        
        cor <- cor.test(pred_test, actual_test[[1]])$estimate
        mae <- caret::MAE(pred_test, actual_test[[1]])
        rmse <- caret::RMSE(pred_test, actual_test[[1]])
        rsq <- cor^2
        
        performance_test <- c(rmse, mae, cor, rsq)
        
        cat("MAE: ", round(mae, 3), "\nRMSE: ", round(rmse, 3),
            "\nPEARSON: ", round(cor, 3), "\nRsq: ", round(rsq, 3))
        
        # Assemble performance metrics
        performance_temp <- data.frame(feature_list = feature_list_names_reordered[j],
                                       sample = rep(c("train", "test"), each = 4),
                                       metric = rep(c("RMSE", "MAE", "PEARSON", "R_SQUARED"), times = 2),
                                       value = c(performance_train, performance_test))
        performance_avg_z <- rbind(performance_avg_z, performance_temp)
        
      }
      
    }
    rm(models_list)
  }
  
  
  print(performance_MemoryProblems)
  print(performance_avg_z)
  
  # Attach features lists variable
  performance_list_MemoryProblems <- performance_MemoryProblems %>%
    mutate(feature_list = factor(feature_list),
           across(where(is.numeric), ~ round(., digits = 3))) %>%
    pivot_wider(names_from = metric,
                values_from = value) %>%
    arrange(sample)
  
  performance_list_avg_z <- performance_avg_z %>%
    mutate(feature_list = factor(feature_list),
           across(where(is.numeric), ~ round(., digits = 3))) %>%
    filter(metric != "PEARSON") %>%
    pivot_wider(names_from = metric,
                values_from = value) %>%
    arrange(sample)
  
  # Export results to docx
  save_as_docx(flextable(performance_list_avg_z, cwidth = 1, cheight = 0.25),
               path = paste0(pathname, "/output/model_performance/external_validation_avg_z.docx")
               
  )
  
  save_as_docx(flextable(performance_list_MemoryProblems, cwidth = 1, cheight = 0.25),
               path = paste0(pathname, "/output/model_performance/external_validation_MemoryProblems.docx")
  )
  
  # Export tables to speed up analyses
  write_csv(performance_avg_z, paste(pathname, "/output/model_performance/avg_z_perf_table.csv", sep = ""))
  write_csv(performance_MemoryProblems, paste(pathname, "/output/model_performance/MemoryProblems_perf_table.csv", sep = ""))
  
}
# 8. Permutation analyses ------------------------------------------------------
## 8.1 Preparation -------------------------------------------------------------

# Generate an empty list which will contain the predictions for each dataset
predictions_lists_by_dataset <- list()

# Loop over datasets
for (d in seq_along(subsets)) {
  dataset <- subsets[d]
  cat("Preparing prediction tables for the", dataset, "set...\n")
  
  # Load in feature sets for each model (raw data)
  covariates <- get(paste0(dataset, "Set_all_imp")) %>%
    select(any_of(main_covariates))
  
  baseline <- get(paste0(dataset, "Set_all_imp")) %>%
    select(any_of(core_features_main_covariates))
  
  interactions <- model.matrix(~ (`Lower SES` + Depression + `Less education` +
                                    `Small social network` + `Less exercise` +
                                    Loneliness + Hypertension + Tinnitus +
                                    `Ever smoked` + `Hearing handicap` + DFH +
                                    Age) * `Woman`,
                               data = get(paste0(dataset, "Set_all_imp"))) %>%
    data.frame(., check.names = FALSE) %>%
    select(-`(Intercept)`) %>%
    rename_with(~ gsub("`", "", .x)) %>%
    rename_with(~ sub(":", " * ", .x))
  
  # load in observed data (target)
  avg_z <- get(paste0("avg_z_", dataset))
  MemoryProblems <- get(paste0("MemoryProblems_", dataset))
  
  # Generate predictions based on all 3 models for both memory problems and objective cognition (if time allows)
  if(update_data == T) {
    # Print info to console
    cat("Updating predictions based on models...\n")
    # load the predictions
    predictions_table <- matrix(, ncol = 6, nrow = nrow(avg_z))
    colnames(predictions_table) <- c("baseline_MemoryProblems", "interactions_MemoryProblems", "covariates_MemoryProblems", "baseline_avg_z", "interactions_avg_z", "covariates_avg_z")
    predictions_table <- data.frame(predictions_table)
    predicted_probs_table <- matrix(, ncol = 3, nrow = nrow(avg_z))
    colnames(predicted_probs_table) <- c("baseline_predicted_probs", "interactions_predicted_probs", "covariates_predicted_probs")
    predicted_probs_table <- data.frame(predicted_probs_table)
    
    for (i in seq_along(main_dvs)) {
      dv <- main_dvs[i]
      # Print dv info
      cat("\n", dv, "\n")
      
      # Load model files
      files_list <- list.files(
        path = paste0(
          pathname,
          "/output/all_models/",
          dv
        ),
        pattern = "model_",
        full.names = TRUE
      )
      files_list <- files_list[!str_detect(files_list, "age|education|all_int|ons_ses")]
      models_list <- files_list %>%
        lapply(readRDS)
      
      # Create an empty matrix to store predictions
      predictions_table_temp <- matrix(, ncol = 3, nrow = nrow(avg_z))
      predicted_probs_table_temp <- matrix(, ncol = 3, nrow = nrow(avg_z))
      
      # Loop over models (feature lists)
      for(j in seq_along(feature_list_names_reordered)) {
        feature_list <- if(j ==1) {
          baseline
        } else if (j ==2) {
          interactions
        } else {
          covariates
        }
        
        # Print model info
        cat("\n Generating predictions for", feature_list_names_reordered[j], "model...\n")
        
        # Generate predictions and predicted probabilities
        predictions <- predict(models_list[[j]], as.matrix(feature_list))
        if (i == 1) {
          predicted_probs <- predict(models_list[[j]], as.matrix(feature_list), type = "prob")[,2]
          predictions_table_temp[,j] <- as.numeric(predictions)-1
          predicted_probs_table_temp[,j] <- predicted_probs
        } else {
          predictions_table_temp[,j] <- predictions
        }
        
      }
      # Saving prediction tables
      if (i == 1) {
        predictions_table[1:3] <- predictions_table_temp
        predicted_probs_table[1:3] <- predicted_probs_table_temp
      } else {
        predictions_table[4:6] <- predictions_table_temp
      }
    }
    
    rm(models_list)
    
    # Export tables to speed up analyses
    write_csv(predictions_table, paste(pathname, "/output/model_performance/predictions_table_", dataset, ".csv", sep = ""))
    write_csv(predicted_probs_table, paste(pathname, "/output/model_performance/predicted_probs_table_", dataset, ".csv", sep = ""))
  } else {
    # Otherwise load in model prediction tables from all 3 models for both memory problems and objective cognition (to speed up analyses)
    # Print info to console
    cat("Loading saved predictions...\n")
    predictions_table <- read.csv(paste(pathname, "/output/model_performance/predictions_table_", dataset, ".csv", sep = ""))
    predicted_probs_table <- read.csv(paste(pathname, "/output/model_performance/predicted_probs_table_", dataset, ".csv", sep = ""))
  }
  
  
  # Check the output
  # print(predictions_table)
  # print(predicted_probs_table)
  
  # Generate an index for participant/observation
  predictions_table <- as.data.frame(predictions_table)
  predictions_table$obs <- as.numeric(rownames(predictions_table))
  predicted_probs_table <- as.data.frame(predicted_probs_table)
  predicted_probs_table$obs <- as.numeric(rownames(predicted_probs_table))
  
  # Merge prediction tables for memory problems and objective cognition and pivot to long format so that they're easier to loop over
  # Note that I am using predicted probabilities to calculate AUC for memory problems, rather than predicted values
  predictions_avg_z <- predictions_table %>%
    select(obs, covariates_avg_z, baseline_avg_z, interactions_avg_z) %>%
    # Now snap and pivot
    pivot_longer(
      cols = !obs,
      names_to = "model",
      names_pattern = "(.*)_avg_z",
      values_to = "pred"
    ) %>%
    mutate(dv = "avg_z") %>%
    arrange(dv, model) %>%
    select(obs, dv, model, pred)
  
  predictions_MemoryProblems <- predicted_probs_table %>%
    # Now snap and pivot
    pivot_longer(
      cols = !obs,
      names_to = "model",
      names_pattern = "(.*)_predicted_probs",
      values_to = "pred"
    ) %>%
    mutate(dv = "MemoryProblems") %>%
    arrange(dv, model) %>%
    select(obs, dv, model, pred)
  
  predictions <- rbind(predictions_avg_z,
                       predictions_MemoryProblems)
  
  # Export to a list
  predictions_lists_by_dataset[[d]] <-  predictions
  
}

## 8.2 Generating permutations of differences ---------------------------------
# Code below is adapted from Sharon & Klaas and looped over train/test datasets

# Loop over datasets
for (d in seq_along(subsets)) {
  dataset <- subsets[d]
  cat("Permuting differences in performance metrics in the", dataset, "set...\n")
  
  # define predictions data frame for this data frame
  predictions <- predictions_lists_by_dataset[[d]]
  
  # load in observed data (target)
  avg_z <- get(paste0("avg_z_", dataset))
  MemoryProblems <- get(paste0("MemoryProblems_", dataset))
  
  # Here we create 1000 random sets of R2/AUC pairs and their respective R2/AUC differences.
  # In each pair, the performance metrics are based on a random mixture of predictions
  # taken from one or the other model (roughly 50% from each).
  # Null hypothesis: predictions from model 1 and model 2 do not differ.
  # If we just randomly shuffled the predictions instead, the R2s/AUCs would be too low
  # as they would be based on noise rather than on a model with any predictive value.
  
  # Create empty data frame that will eventually store the results 
  permutation_mat <- matrix(NA, nrow = 1000, ncol = 2)
  permutation_mat <- as.data.frame(permutation_mat)
  permutation_mat <- permutation_mat %>% select(iteration = V1, diff = V2)
  
  # Create multiple such empty data frames, one for each model
  AUCperm_list <- r2perm_list <- list(permutation_mat, permutation_mat, permutation_mat)
  
  # Create list of names for the models
  model_names_list <- c("covariates", "baseline", "interactions")
  
  # Loop over all possible model combinations
  # Out of 3 possible models, we always compare two, here titled "benchmark" & "final" model
  for (i in 1:length(model_names_list)) {
    
    # Set benchmark model name
    benchmark_model <- model_names_list[i]
    
    # Set final model name
    if (i == 3) {
      final_model <- model_names_list[1]
    } else {
      final_model <- model_names_list[i + 1]
    }
    
    # Name data frames that will store the results
    names(AUCperm_list)[i] <- names(r2perm_list)[i] <- paste0(benchmark_model, "_to_", final_model)
    cat("\nComparing ", benchmark_model, " to ", final_model)
    
    # Loop over 1000 random seeds
    for (k in 1:1000) {
      set.seed(k)
      
      # Print loop status to console - every 25% of iterations
      if (k %% 250 == 0) {
        cat("\n", k/1000*100, "% iterations done")
      }
      
      # Using the seed k, we now sample 50% of the observations.
      # Because the number of observations can be odd, 50% is not always a whole number.
      # Therefore, we randomize whether we round down or up to the nearest whole number.
      # E.g., if total N = 4001 --> for k = 1 we take n = 2001, for k = 2 we take n = 2000 etc.
      n_obs <- unique(predictions$obs)
      
      if (k %% 2 == 1) { # if k is an odd number, round up
        obs_IDs <- sample(n_obs,
                          round(length(n_obs)/2)+1,
                          replace = FALSE)
      } else { # otherwise round down
        obs_IDs <- sample(n_obs,
                          round(length(n_obs))/2,
                          replace = FALSE)
      }
      
      # Loop over dependent variables
      for(dv in main_dvs) {
        
        # Generate a subset of predictions for random mixture model 1
        # (with half predictions from benchmark and half predictions from final model)
        model1_benchmark_obs <- predictions %>%
          filter(obs %in% obs_IDs,
                 dv == !!dv,
                 model == benchmark_model)
        # get the remaining half of obs index for final model
        model1_final_obs <- predictions %>%
          filter(!obs %in% obs_IDs,
                 dv == !!dv,
                 model == final_model)
        
        # this now contains half of predictions of benchmark and the other half from final:
        model1 <- rbind(model1_benchmark_obs, model1_final_obs)
        
        # Generate a subset of predictions for random mixture model 2
        # (with the other half predictions from benchmark and the other half predictions from final model)
        model2_final_obs <- predictions %>%
          filter(obs %in% obs_IDs,
                 dv == !!dv,
                 model == final_model)
        model2_benchmark_obs <- predictions %>%
          filter(!obs %in% obs_IDs,
                 dv == !!dv,
                 model == benchmark_model)
        
        # this now contains the other half of predictions of benchmark and the other half from final
        model2 <- rbind(model2_final_obs, model2_benchmark_obs) 
        
        # Order both sets of predictions by observation number so as to match observed data
        model1 <- model1 %>% arrange(obs)
        model2 <- model2 %>% arrange(obs)
        
        # Save R2 or AUC difference
        if(dv == "avg_z") {
          # REGRESSION PROBLEM -> R SQARED
          # Estimate R2 for each random mixture model and observed data
          model1_r2 <- cor(model1$pred, avg_z[[1]])^2 # r-squared value for model 1 and actual y
          model2_r2 <- cor(model2$pred, avg_z[[1]])^2 # r-squared value for model 2 and actual y
          r2_diff <- model1_r2 - model2_r2 # r-squared difference
          
          # Attach to list
          r2perm_list[[i]][k, 1] <- k # Iteration number
          r2perm_list[[i]][k, 2] <- r2_diff # R2 difference value
          
        } else {
          # CLASSIFICATION PROBLEM -> AUC
          # Estimate AUC for each random mixture model and observed data
          roc_obj_1 <- suppressMessages(roc(MemoryProblems[[1]], model1$pred, direction = "<",))
          model1_AUC <- pROC::auc(roc_obj_1) # AUC value for model 1 and actual y
          
          roc_obj_2 <- suppressMessages(roc(MemoryProblems[[1]], model2$pred, direction = "<",))
          model2_AUC <- pROC::auc(roc_obj_2) # AUC value for model 2 and actual y
          
          AUC_diff <- model1_AUC - model2_AUC # AUC difference
          
          # Attach to list
          AUCperm_list[[i]][k, 1] <- k # Iteration number
          AUCperm_list[[i]][k, 2] <- AUC_diff # R2 difference value
          
        }
        
      }
    }
  }
  
  cat("\nExporting permuted differences to .csv...\n")
  # Save output
  # baseline to covariates
  write_csv(r2perm_list[[1]], paste(pathname, "/output/model_performance/", dataset, "_r2permutation_covariates_to_baseline.csv", sep = ""))
  write_csv(AUCperm_list[[1]], paste(pathname, "/output/model_performance/", dataset, "_AUCpermutation_covariates_to_baseline.csv", sep = ""))
  
  # baseline to interactions
  write_csv(r2perm_list[[2]], paste(pathname, "/output/model_performance/", dataset, "_r2permutation_baseline_to_interactions.csv", sep = ""))
  write_csv(AUCperm_list[[2]], paste(pathname, "/output/model_performance/", dataset, "_AUCpermutation_baseline_to_interactions.csv", sep = ""))
  
  # covariates to interactions
  write_csv(r2perm_list[[3]], paste(pathname, "/output/model_performance/", dataset, "_r2permutation_covariates_to_interactions.csv", sep = ""))
  write_csv(AUCperm_list[[3]], paste(pathname, "/output/model_performance/", dataset, "_AUCpermutation_covariates_to_interactions.csv", sep = ""))
  
  
}


## 8.3 Plot R2 / AUC differences ---------------------------------------------
# Function to plot histograms for R2 or AUC
plot_permutation_histogram <- function(perm_diff,
                                       true_diff,
                                       plot_title,
                                       binwidth,
                                       x_max,
                                       metric) {
  
  ggplot(perm_diff,
         aes(x = diff)) +
    geom_histogram(
      binwidth = binwidth,
      fill = palette_color, color = "gray20", alpha = 0.9) +
    xlab(paste0(toupper(metric), " Difference")) +
    ylab("Frequency") +
    geom_vline(aes(xintercept = true_diff),
               linetype = "dashed",
               linewidth = 0.9) +
    theme_bw() +
    ggtitle(plot_title) +
    scale_x_continuous(limit = c(NA, x_max)) +
    #scale_y_continuous(limit = c(0, NA), breaks = seq(0, 1000, by = 20)) +
    theme_main +
    theme(
      legend.position = "none",
      legend.background = element_blank()
    )
}



# Load previously exported true performance metrics for both train and test set
performance_avg_z <- read.csv(paste(pathname, "/output/model_performance/avg_z_perf_table.csv", sep = ""))
performance_MemoryProblems <- read.csv(paste(pathname, "/output/model_performance/MemoryProblems_perf_table.csv", sep = ""))

# Loop over datasets
for (d in seq_along(subsets)) {
  dataset <- subsets[d]
  cat("Plotting permutation analyses in the", dataset, "set...\n")
  
  # Define plot colour for the train set
  if (d == 1){
    palette_color <- my.palette[4]
  } else {
    # Define plot colour for the test set
    palette_color <- my.palette[6]
  }
  
  # Load permuted R2/AUC differences
  r2perm_covariates_to_baseline <- read.csv(paste(pathname, "/output/model_performance/", dataset, "_r2permutation_covariates_to_baseline.csv", sep = ""))
  r2perm_covariates_to_interactions <- read.csv(paste(pathname, "/output/model_performance/", dataset, "_r2permutation_covariates_to_interactions.csv", sep = ""))
  r2perm_baseline_to_interactions <- read.csv(paste(pathname, "/output/model_performance/", dataset, "_r2permutation_baseline_to_interactions.csv", sep = ""))
  AUCperm_covariates_to_baseline <- read.csv(paste(pathname, "/output/model_performance/", dataset, "_AUCpermutation_covariates_to_baseline.csv", sep = ""))
  AUCperm_covariates_to_interactions <- read.csv(paste(pathname, "/output/model_performance/", dataset, "_AUCpermutation_covariates_to_interactions.csv", sep = ""))
  AUCperm_baseline_to_interactions <- read.csv(paste(pathname, "/output/model_performance/", dataset, "_AUCpermutation_baseline_to_interactions.csv", sep = ""))
  
  # Gather the permuted differences in lists
  r2perm_list <- list(r2perm_covariates_to_baseline, r2perm_baseline_to_interactions, r2perm_covariates_to_interactions)
  AUCperm_list <- list(AUCperm_covariates_to_baseline, AUCperm_baseline_to_interactions, AUCperm_covariates_to_interactions)
  names(r2perm_list) <- names(AUCperm_list) <- list_of_comparison_names
  
  # Preparation for R2
  # Extract observed (true) R2 differences
  covariates_r2 <- performance_avg_z %>%
    filter(sample == dataset,
           metric == "R_SQUARED",
           feature_list == "main_covariates") %>%
    .[[1, 4]]
  
  baseline_r2 <- performance_avg_z %>%
    filter(sample == dataset,
           metric == "R_SQUARED",
           feature_list == "core_features_main_covariates") %>%
    .[[1, 4]]
  
  interactions_r2 <- performance_avg_z %>%
    filter(sample == dataset,
           metric == "R_SQUARED",
           feature_list == "interactions_gender") %>%
    .[[1, 4]]
  
  # Put them in a single list
  r2_true_diffs <- c(baseline_r2 - covariates_r2,
                     interactions_r2 - baseline_r2,
                     interactions_r2 - covariates_r2)
  
  
  # Plot R2 permutation histograms
  metric <- "r2"
  p_hist_covariates_to_baseline_r2 <- plot_permutation_histogram(r2perm_list[[1]],
                                                                 r2_true_diffs[1],
                                                                 plot_title = titles_list[1],
                                                                 binwidth = 0.0006,
                                                                 x_max = 0.04,
                                                                 metric)
  p_hist_baseline_to_interactions_r2 <- plot_permutation_histogram(r2perm_list[[2]],
                                                                   r2_true_diffs[2],
                                                                   plot_title = titles_list[2],
                                                                   binwidth = 0.0002,
                                                                   x_max = 0.01,
                                                                   metric)
  p_hist_covariates_to_interactions_r2 <- plot_permutation_histogram(r2perm_list[[3]],
                                                                     r2_true_diffs[3],
                                                                     plot_title = titles_list[3],
                                                                     binwidth = 0.0006,
                                                                     x_max = 0.045,
                                                                     metric)
  # Combined figure for R2:
  combined_fig_r2 <- ggarrange(
    p_hist_covariates_to_baseline_r2,
    p_hist_baseline_to_interactions_r2,
    legend = "none",
    ncol = 1, nrow = 2,
    labels = c("A", "C"),
    font.label = list(size = 30, family = font.family)
  )
  
  # Preparation for AUC
  # Extract observed (true) AUC differences
  covariates_AUC <- performance_MemoryProblems %>%
    filter(sample == dataset,
           metric == "AUC",
           feature_list == "main_covariates") %>%
    .[[1, 4]]
  
  baseline_AUC <- performance_MemoryProblems %>%
    filter(sample == dataset,
           metric == "AUC",
           feature_list == "core_features_main_covariates") %>%
    .[[1, 4]]
  
  interactions_AUC <- performance_MemoryProblems %>%
    filter(sample == dataset,
           metric == "AUC",
           feature_list == "interactions_gender") %>%
    .[[1, 4]]
  
  # Put them in a single list
  AUC_true_diffs <- c(baseline_AUC - covariates_AUC,
                      interactions_AUC - baseline_AUC,
                      interactions_AUC - covariates_AUC)
  
  
  # Plot R2 permutation histograms
  metric <- "AUC"
  p_hist_covariates_to_baseline_AUC <- plot_permutation_histogram(AUCperm_list[[1]],
                                                                  AUC_true_diffs[1],
                                                                  plot_title = titles_list[1],
                                                                  binwidth = 0.002,
                                                                  x_max = 0.17,
                                                                  metric)
  p_hist_baseline_to_interactions_AUC <- plot_permutation_histogram(AUCperm_list[[2]],
                                                                    AUC_true_diffs[2],
                                                                    plot_title = titles_list[2],
                                                                    binwidth = 0.0002,
                                                                    x_max = 0.01,
                                                                    metric)
  p_hist_covariates_to_interactions_AUC <- plot_permutation_histogram(AUCperm_list[[3]],
                                                                      AUC_true_diffs[3],
                                                                      plot_title = titles_list[3],
                                                                      binwidth = 0.002,
                                                                      x_max = 0.16,
                                                                      metric)
  
  # Combined figure for AUC:
  combined_fig_AUC <- ggarrange(
    p_hist_covariates_to_baseline_AUC,
    p_hist_baseline_to_interactions_AUC,
    legend = "none",
    ncol = 1, nrow = 2,
    labels = c("B", "D"),
    font.label = list(size = 30, family = font.family)
  )
  
  # Create combined plot for paper, add column labels
  combined_fig <- ggarrange(
    annotate_figure(combined_fig_r2,
                    top = text_grob("Objective cognition",
                                    face = "bold",
                                    family = font.family,
                                    size = 30)) +
      theme(plot.margin = margin(t = 0.2, r = 0.5, b = 0, l = 0.2, unit = "cm"),
            plot.background = element_rect(fill = "white")),
    annotate_figure(combined_fig_AUC,
                    top = text_grob("Subj. memory problems",
                                    face = "bold",
                                    family = font.family,
                                    size = 30)) +
      theme(plot.margin = margin(t = 0.2, r = 0.2, b = 0, l = 0.5, unit = "cm"),
            plot.background = element_rect(fill = "white")),
    legend = "none",
    ncol = 2, nrow = 1,
    font.label = list(size = 30,
                      family = font.family)
  ) + theme(plot.background = element_rect(fill = "white"))
  
  # Save the final plot
  ggsave(combined_fig,
         file = paste0(pathname, "/figures/permutation_analyses/permutation_analyses_", dataset, ".png"),
         width = 12, height = 10, dpi = 300)
  
}

## 8.4 Run permutation tests ----------------------------------------------------
# Custom function to compare true difference with permuted differences
diff_comparison <- function(perm_data, true_diff) {
  mean(perm_data$diff >= true_diff)
}

# Print results
# Loop over datasets
for (d in seq_along(subsets)) {
  dataset <- subsets[d]
  cat("\n-----------------------------\n")
  cat("\nCalculating p for permutation analysis in", dataset, "set...\n")
  
  # Load previously exported true performance metrics
  performance_avg_z <- read.csv(paste(pathname, "/output/model_performance/avg_z_perf_table.csv", sep = ""))
  performance_MemoryProblems <- read.csv(paste(pathname, "/output/model_performance/MemoryProblems_perf_table.csv", sep = ""))
  
  # Preparation for R2
  # Extract observed (true) R2 differences
  covariates_r2 <- performance_avg_z %>%
    filter(sample == dataset,
           metric == "R_SQUARED",
           feature_list == "main_covariates") %>%
    .[[1, 4]]
  
  baseline_r2 <- performance_avg_z %>%
    filter(sample == dataset,
           metric == "R_SQUARED",
           feature_list == "core_features_main_covariates") %>%
    .[[1, 4]]
  
  interactions_r2 <- performance_avg_z %>%
    filter(sample == dataset,
           metric == "R_SQUARED",
           feature_list == "interactions_gender") %>%
    .[[1, 4]]
  
  # Put them in a single list
  r2_true_diffs <- c(baseline_r2 - covariates_r2,
                     interactions_r2 - baseline_r2,
                     interactions_r2 - covariates_r2)
  
  
  # Preparation for AUC
  # Extract observed (true) AUC differences
  covariates_AUC <- performance_MemoryProblems %>%
    filter(sample == dataset,
           metric == "AUC",
           feature_list == "main_covariates") %>%
    .[[1, 4]]
  
  baseline_AUC <- performance_MemoryProblems %>%
    filter(sample == dataset,
           metric == "AUC",
           feature_list == "core_features_main_covariates") %>%
    .[[1, 4]]
  
  interactions_AUC <- performance_MemoryProblems %>%
    filter(sample == dataset,
           metric == "AUC",
           feature_list == "interactions_gender") %>%
    .[[1, 4]]
  
  # Put them in a single list
  AUC_true_diffs <- c(baseline_AUC - covariates_AUC,
                      interactions_AUC - baseline_AUC,
                      interactions_AUC - covariates_AUC)
  
  # Load previously exported permuted R2/AUC differences
  r2perm_covariates_to_baseline <- read.csv(paste(pathname, "/output/model_performance/", dataset, "_r2permutation_covariates_to_baseline.csv", sep = ""))
  r2perm_covariates_to_interactions <- read.csv(paste(pathname, "/output/model_performance/", dataset, "_r2permutation_covariates_to_interactions.csv", sep = ""))
  r2perm_baseline_to_interactions <- read.csv(paste(pathname, "/output/model_performance/", dataset, "_r2permutation_baseline_to_interactions.csv", sep = ""))
  AUCperm_covariates_to_baseline <- read.csv(paste(pathname, "/output/model_performance/", dataset, "_AUCpermutation_covariates_to_baseline.csv", sep = ""))
  AUCperm_covariates_to_interactions <- read.csv(paste(pathname, "/output/model_performance/", dataset, "_AUCpermutation_covariates_to_interactions.csv", sep = ""))
  AUCperm_baseline_to_interactions <- read.csv(paste(pathname, "/output/model_performance/", dataset, "_AUCpermutation_baseline_to_interactions.csv", sep = ""))
  
  # R2
  cat("\nResults for R2")
  cat("\nComparing", titles_list[1])
  cat("\n", diff_comparison(r2perm_covariates_to_baseline, r2_true_diffs[1]))
  cat("\nComparing", titles_list[2])
  cat("\n", diff_comparison(r2perm_covariates_to_interactions, r2_true_diffs[2]))
  cat("\nComparing", titles_list[3])
  cat("\n", diff_comparison(r2perm_baseline_to_interactions, r2_true_diffs[3]))
  # AUC
  cat("\nResults for AUC")
  cat("\nComparing", titles_list[1])
  cat("\n", diff_comparison(AUCperm_covariates_to_baseline, AUC_true_diffs[1]))
  cat("\nComparing", titles_list[2])
  cat("\n", diff_comparison(AUCperm_covariates_to_interactions, AUC_true_diffs[2]))
  cat("\nComparing", titles_list[3])
  cat("\n", diff_comparison(AUCperm_baseline_to_interactions, AUC_true_diffs[3]))
  
}

