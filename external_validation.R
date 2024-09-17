#---------------------------------------------#
#      Risk Factors multivariate analyses     #
#          External model validation          #
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
pathname <- "../../ML_output"

# Toggle if you want to update data or not (not doing so saves time)
update_data <- F

## Run the helper script first ---------------------------------------------
# This also loads and installs packages
source(paste0(pathname_public, "/helper_script.R"))

## Load the pre-processed datafiles ---------------------------------------
load(paste0(pathname_public, "/output/trainsets_final.RData"))
load(paste0(pathname_public, "/output/testsets_final.RData"))

# Extract observed data
avg_z_train <- trainSet_all_imp %>%
  select(avg_z)
MemoryProblems_train <- trainSet_all_imp %>%
  select(MemoryProblems)
avg_z_test <- testSet_all_imp %>%
  select(avg_z)
MemoryProblems_test <- testSet_all_imp %>%
  select(MemoryProblems)

## Define list of DVs to loop over -----------------------------------------
main_dvs <- c("mm", "sra", "srb", "MemoryProblems", "avg_z")
main_dv_names <- c(
  "Visual working memory",
  "Processing speed (~ Trails A)",
  "Cognitive flexibility (~ Trails B)",
  "Subjective memory problems",
  "Composite cognition (avg. z)"
)
##### SUBSETTING FOR NOW ###########
main_dvs <- main_dvs[4:5]
main_dv_names <- main_dv_names[4:5]

# Define feature sets
main_covariates <- c("Age", "Woman")
core_features <- c(
  "Lower SES", "Depression", "Less education",
  "Small social network",
  "Less exercise", "Loneliness",
  "Hypertension", "Tinnitus",
  "Ever smoked", "Hearing handicap", "DFH"
)
core_features_main_covariates <- c(core_features, main_covariates)
feature_list_names <- c("main_covariates", "core_features_main_covariates", "interactions_gender")

# Reorder feature lists alphabetically
feature_list_names_reordered <- sort(feature_list_names)

# Generate a list of names to loop over train and test datasets
subsets <- c("train", "test")

# Create a list of labels for the comparisons
list_of_comparison_names <- c("covariates_to_baseline", "baseline_to_interactions", "covariates_to_interactions") # Labels used internally throughout the code
titles_list <- c("Benchmark to risk factors", "Risk factors to\nrisk factors + gender interactions", "Benchmark to\nrisk factors + gender interactions") # This will be printed in the plots

# Generate an index for participant/observation
avg_z_train$obs <- as.numeric(rownames(avg_z_train))
MemoryProblems_train$obs <- as.numeric(rownames(MemoryProblems_train))
avg_z_test$obs <- as.numeric(rownames(avg_z_test))
MemoryProblems_test$obs <- as.numeric(rownames(MemoryProblems_test))

# 2. Calculate performance metrics ---------------------------------------
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

# 3. Permutation analyses -----------------------------------------------------
## 3.1 Preparation ------------------------------------------------------------

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

## 3.2 Generating permutations of differences ---------------------------------
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


## 3.3 Plot R2 / AUC differences ---------------------------------------------
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
                                                                  x_max = 0.16,
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


## 3.4 Run permutation tests ----------------------------------------------------
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

