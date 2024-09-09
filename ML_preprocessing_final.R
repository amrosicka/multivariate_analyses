#---------------------------------------------#
#      Risk Factors multivariate analyses     #
#             Data preprocessing              #
#                    2024                     #
#---------------------------------------------#

# 1. Preparation ----------------------------------------------------------
## Remove leftovers from previous analyses --------------------------------
rm(list=ls())
options(scipen = 999) # force R to not use scientific notations

## Set working directory --------------------------------------------------
# Replace with your WD and path to this folder
setwd("/Users/annamarierosicka/Library/CloudStorage/OneDrive-TrinityCollegeDublin/Neureka/Analysis_Pipelines/")
pathname <- "riskFactorsScripts/riskFactors-ANNAR/multivariate_analyses"

# Run the helper script first
# This also loads and installs packages
source(paste0("riskFactorsScripts/riskFactors-ANNAR/multivariate_analyses", "/helper_script.R"))

# Load the pre-processed datafiles ---------------------------------------
# Partial completers
load("riskFactorsScripts/processedData/RF_partial_completers.rds")

# Subset to given time range as data gets updated
RF_partial_completers <- RF_partial_completers %>%
  filter(as.Date(str_sub(gsub("_", "", as.character(timestamp))), 1, 8, format = "%Y%m%d") <= as.Date("2024-05-09"))

# Check time range & number of participants
print("When was the data collected?")
print(summary(as.Date(str_sub(gsub("_", "", as.character(RF_partial_completers$timestamp))), 1, 8, format = "%Y%m%d")))
nrow(RF_partial_completers) # 10,660

# Rename variables to make them easier to understand
RF_partial_completers <- RF_partial_completers %>%
  dplyr::rename("Depression" = CES_D.t.TotalScore,
                "Less exercise" = GODIN.t.TotalScore,
                "Hearing handicap" = HHI.t.TotalScore,
                "Diabetes" = LDRF.t.Diabetes_coded.factor,
                "Hypertension" = LDRF.t.Hypertension_coded.factor,
                "Ever smoked" = LDRF.t.Smoking_coded.factor,
                "History of stroke" = LDRF.t.Stroke_coded.factor,
                "Tinnitus" = LDRF.t.Tinnitus_coded.factor,
                "Small social network" = LUBBEN.t.TotalScore,
                "Lower SES" = SES.t.TotalScore,
                "Loneliness" = UCLA.t.TotalScore,
                "Age" = age,
                "Less education" = education_coded,
                "Worse ATOA" = ATOA.t.TotalScore,
                "Hearing loss uncorrected" = LDRF.Hearing_loss_uncorrected.factor,
                "ARHL only" = LDRF.ARHL_only.factor,
                "Cognitive fluctuations" = MCF.t.TotalScore,
                "Hearing aid user" = LDRF.t.HearingAid_coded.factor)

# 2. Data preparation ---------------------------------------

## Create OS variable & tablets variable for exploration ---------------------------------------
tablet_devices <- c("Pad", "pad", "tab", "Tab", "TAB", "Surface Pro", "Lenovo TB", "LENOVO TB", "Lenovo YT", "LGE Nexus", "Amazon", "Acer", "Argos", "Nexus 7", "Chromebook", "samsung GT-P5210", "neocore neocore_N1G1")
tablet_os <- c("Fire", "Chrome") # helps filter out all detectable tablet devices

RF_partial_completers <- RF_partial_completers %>%
  mutate(
    `Tablet user` = case_when(
      str_detect(device, paste(tablet_devices, collapse = "|")) ~ "Yes",
      str_detect(os, paste(tablet_os, collapse = "|")) ~ "Yes",
      TRUE ~ "No/Unknown"
    ),
    # detect operating system
    os_type = case_when(
      str_detect(os, "iOS") ~ "iOS",
      str_detect(os, "Android") ~ "Android",
      TRUE ~ "iPadOS"
    )
  )

# Check out device counts
RF_partial_completers %>%
  group_by(device) %>%
  reframe(
    count = length(device),
    `Tablet user` = `Tablet user`
  ) %>%
  group_by(device) %>%
  slice(1) %>%
  arrange(desc(count))


## Create LMIC/HIC country variable for exploration ---------------------------------------
# Add indicator of country income
# Specify the source URL
url <- "https://datacatalogfiles.worldbank.org/ddh-published/0037712/DR0090755/CLASS.xlsx"

# Download the file
# download.file(url, paste0(pathname, "/country_income_list.xlsx"), mode = "wb")
# Read it
country_income_list <- read_excel(path = paste0(pathname, "/country_income_list.xlsx"))

# Select LMIC countries
LMIC_list <- country_income_list %>%
  filter(`Income group` != "High income") %>%
  select(Economy) %>%
  .[[1]] %>%
  c(., "Turkey", "Viet Nam", "Iran, Islamic Republic Of") # Some countries are named differently in Neureka!

RF_partial_completers <- RF_partial_completers %>%
  mutate(country_income = case_when(
    country %in% LMIC_list ~ "LMIC",
    country == "" ~ NA_character_,
    TRUE ~ "HIC"
  ))

# Check out country counts
RF_partial_completers %>%
  group_by(country) %>%
  reframe(
    count = length(country),
    country_income = country_income
  ) %>%
  group_by(country) %>%
  slice(1) %>%
  arrange(desc(count))

# Check out proportion of LMICs/HICs
RF_partial_completers %>%
  group_by(country_income) %>%
  summarise(n = length(country_income)) %>%
  mutate(prop = round(n / sum(n) * 100, 2)) %>%
  arrange(desc(n))
# There are 275/10,296 participants from LMIC (i.e., 2.6% of all) in the sample


## Filter out duplicate/obsolete variables ---------------------------------------
RF_partial_completers <- RF_partial_completers %>%
  select(
    -device, -os, -country,
    -ends_with("tamp"), -colourblind, -education, -gender, -DFH.t.MemoryProblems, -starts_with("DFH.t"),
    -LDRF.t.HeightCM, -LDRF.t.WeightKG, -LDRF.t.BMI_metric, -LDRF.t.Hypertension, -LDRF.t.Diabetes, -LDRF.t.Stroke,
    -LDRF.t.Smoking, -LDRF.t.HearingLoss, -LDRF.t.CauseOfHearingLoss, -LDRF.t.HearingAid, 
    -LDRF.t.HearingLossRel, -LDRF.t.Tinnitus, -LDRF.t.Hearing_loss_coded, -LDRF.Hearing_loss_uncorrected, -LDRF.ARHL_only,
    -LDRF.t.Smoking_coded, -LDRF.t.Tinnitus_coded, -LDRF.t.Hypertension_coded, -LDRF.t.Stroke_coded, -LDRF.t.HearingAid_coded,
    -LDRF.t.Diabetes_coded, -MCF.t.CogFlucCat
  )


## Dummy code categorical variables ---------------------
view(dfSummary(RF_partial_completers))

# Define list of categorical variables for later
cat_variables <- RF_partial_completers %>%
  select(!where(is.numeric), -PlayerID,
         -country_income, -os_type, -language, -gender_group) %>%
  colnames(.)

# Recode variables
RF_partial_completers <- RF_partial_completers %>%
  mutate(
    `Woman` = case_when(
      gender_group == "cisgender\nfemale" ~ 1,
      TRUE ~ 0
    ),
    `Trans/non-binary` = case_when(
      gender_group == "non-\ncisgender" ~ 1,
      TRUE ~ 0
    )
  ) %>%
  select(-gender_group) %>%
  mutate(
    across(where(is.factor), ~ case_when(
      as.character(.x) == "2" ~ 1,
      as.character(.x) == "0" ~ 0,
      TRUE ~ NA_real_
    )),
    `Tablet user` = case_when(
      `Tablet user` == "Yes" ~ 1,
      TRUE ~ 0
    ),
    `LMIC country` = case_when(
      country_income == "LMIC" ~ 1,
      TRUE ~ 0
    ),
    `Android user` = case_when(
      os_type == "Android" ~ 1,
      TRUE ~ 0
    ),
    `First language not EN` = case_when(
      language == "english" ~ 0,
      TRUE ~ 1
    )
  ) %>%
  select(-country_income, -os_type, -language)

# Update list of categorical variables for later
cat_variables_gender <- c(
  cat_variables, "Woman", "Tablet user",
  "LMIC country", "Android user", "First language not EN", "group"
)

# cat_variables_age <- cat_variables_gender

# cat_variables_age <- c(
#   cat_variables, "Woman", "Trans/non-binary", "Tablet user",
#   "LMIC country", "Android user", "First language not EN", "group"
# )

# 3. Descriptive statistics & missing data --------------------------------
## Get descriptive statistics ------------------------
view(dfSummary(RF_partial_completers, round.digits = 5))

## Inspect data regarding missingness -------------------------------------
missingData <- (colMeans(is.na(RF_partial_completers))) * 100
missingData <- as.data.frame(missingData)
# less than 5%
print("less than 5%")
filter(missingData, missingData > 0 & missingData < 5) # only education
# 5-15%
print("5-15%")
filter(missingData, missingData > 5 & missingData <= 15) # 12 variables
# 15-25%
print("15-25%")
filter(missingData, missingData > 15 & missingData <= 25) # 0 variables
# 25-50%
print("25-50%")
filter(missingData, missingData > 25 & missingData <= 50) # 16 variables including sra, srb, cb, mm, ATOA (which was buggy) and other questionnaires that followed ATOA in the same block, importantly these include:
# SES, exercise, social network, and loneliness!!
# >50%
print(">50%")
filter(missingData, missingData > 50) # BMI (only calculated in newer versions of the app due to previous bug)

## Create a subset of participants with complete data on cognitive measures (=DVs) --------
RF_full_DV_data <- RF_partial_completers %>%
  dplyr::select(PlayerID, mm, srb, sra, MemoryProblems) %>%
  na.omit() %>%
  # Re-attach other variables
  left_join(RF_partial_completers)

# Inspect the number of participants excluded at this stage
nrow(RF_full_DV_data)
100-nrow(RF_full_DV_data)/nrow(RF_partial_completers)*100
nrow(RF_partial_completers)-nrow(RF_full_DV_data)

## Inspect data regarding missingness ---------------
missingData <- (colMeans(is.na(RF_full_DV_data))) * 100
missingData <- as.data.frame(missingData)
# less than 5%
print("less than 5%")
filter(missingData, missingData > 0 & missingData < 5) # 15 variables
# 5-15%
print("5-15%")
filter(missingData, missingData > 5 & missingData <= 15) # 1 variable
# 15-25%
print("15-25%")
filter(missingData, missingData > 15 & missingData <= 25) # 0 variables
# 25-50%
print("25-50%")
filter(missingData, missingData > 25 & missingData <= 50) # 4 variables - all related to cannon blast which is not a problem
# >50%
print(">50%")
filter(missingData, missingData > 50) # only BMI

## Filter out rarely endorsed items (>50%) -----------------------------------
RF_full_DV_data <- RF_full_DV_data %>%
  select(-BMI_fixed)
RF_partial_completers <- RF_partial_completers %>%
  select(-BMI_fixed)

## Split data by gender ----------------------------------------------------
RF_full_DV_women <- RF_full_DV_data %>%
  filter(`Woman` == 1,
         `Trans/non-binary` == 0) %>%
  select(-`Trans/non-binary`)
RF_full_DV_men <- RF_full_DV_data %>%
  filter(`Woman` == 0,
         `Trans/non-binary` == 0) %>%
  select(-`Trans/non-binary`)
# Unsplit data for descriptive statis:
RF_full <- RF_full_DV_data %>%
  filter(`Trans/non-binary` == 0) %>%
  select(-`Trans/non-binary`)

## Generate descriptive statistics ------------------------------------------
view(dfSummary(RF_full, round.digits = 5))
view(dfSummary(RF_full_DV_women, round.digits = 5))
view(dfSummary(RF_full_DV_men, round.digits = 5))

# Inspect the number of participants excluded at this stage
nrow(RF_full)
100-nrow(RF_full)/nrow(RF_full_DV_data)*100
nrow(RF_full_DV_data)-nrow(RF_full)

## Compare gender groups on important variables -------------------------------
var_continuous <- c("Age", "`Less education`", "Depression", "`Hearing handicap`", "`Lower SES`", "`Less exercise`", "`Small social network`", "Loneliness", "mm", "sra", "srb")
var_categorical <- c("MemoryProblems", "History of stroke", "DFH", "Hypertension", "Ever smoked", "Diabetes", "Tinnitus")

RF_temp <- RF_full %>%
  mutate(across(c(`Less education`, `Lower SES`, `Less exercise`, `Small social network`, mm),
                ~ -1*.x))
# t-test and histogram of continuous variables
for (v in var_continuous) {
  print(t.test(as.formula(paste(v, "~ Woman")), data = RF_full))
  if (v == "sra") {
    label = "Processing speed (~ Trails A)"
    limits_manual = c(0, NA)
  } else if (v == "srb") {
    label = "Cognitive flexibility (~ Trails B)"
    limits_manual = c(0, NA)
  } else if (v == "mm") {
    label = "Visual working memory"
    limits_manual = c(0, 1)
  } else {
    label = str_remove_all(v, "`")
    limits_manual = c(NA, NA)
  }
  fig <- ggplot(RF_temp, aes_string(x = v, fill = "as.factor(Woman)")) + # should use tidy evaluation if I figure out how to implement it
    geom_histogram(colour = "white") +
    scale_fill_manual(values = my.palette[c(2, 1)]) +
    facet_wrap(~Woman) +
    theme_minimal() +
    theme_main +
    xlab(label) +
    scale_x_continuous(limits = limits_manual) +
    theme(legend.position = "none",
          strip.background = element_blank(),
          strip.text.x = element_blank(),
          axis.title.x = element_text(size = 30, face = "bold"))
  print(v)
  print(fig)
  filename <- paste(pathname, "/figures/feature_histograms/", v, "_by_gender.png", sep = "")
  ggsave(filename, fig, 
         height = 5, width = 8, dpi = 300)
}

# chi-sq and table of categorical variables
for (v in var_categorical) {
  cat("\n", "Variable:", v, "\n")
  print(chisq.test(RF_full$Woman, RF_full[[v]], correct = FALSE))
  print(table(RF_full$Woman, RF_full[[v]]))
  print(prop.table(table(RF_full$Woman, RF_full[[v]])))
}

# ## Split data by age groups ------------------------------------------------------
# # Extract median for median split
# med_age <- median(RF_full$Age)
# med_age #48
# 
# # Create categories
# RF_full_age_groups <- RF_full %>%
#   mutate(
#     # 3 groups
#     age_group = factor(
#       case_when(
#         Age > 59 ~ "over 59",
#         Age < 40 ~ "under 40",
#         TRUE ~ "40-59"
#       ),
#       levels = c("under 40", "40-59", "over 59")
#     ),
#     # median split
#     age_group_median = factor(
#       case_when(
#         Age > med_age ~ "49+",
#         TRUE ~ "under 49"
#       ),
#       levels = c("under 49", "49+")
#     )
#   )
# 
# # Describe 3 age groups
# RF_full_age_groups %>%
#   group_by(age_group) %>%
#   summarise(count = length(age_group)) %>%
#   arrange(desc(count))
# 
# # 1 40-59      2325
# # 2 under 40   1728
# # 3 over 59     942
# 
# # Describe median split age groups
# RF_full_age_groups %>%
#   group_by(age_group_median) %>%
#   summarise(count = length(age_group_median)) %>%
#   arrange(desc(count))
# 
# # under 49   2611
# # 49+        2384
# 
# # The latter are the ones we'll retain
# 
# # Do the split
# RF_young <- RF_full_age_groups %>%
#   filter(age_group_median == "under 49") %>%
#   select(-age_group, -age_group_median)
# 
# RF_old <- RF_full_age_groups %>%
#   filter(age_group_median != "under 49") %>%
#   select(-age_group, -age_group_median)
# 
# 
# ## Compare age groups on important variables -------------------------------------
# 
# # correlation and scatterplot of continuous variables
# for (v in var_continuous[2:length(var_continuous)]) {
#   print(cor.test(as.formula(paste("~ Age + ", v)), data = RF_full))
#   fig <- ggplot(RF_full, aes_string(y = v, x = "Age", colour = "as.factor(Woman)")) + # should use tidy evaluation if I figure out how to implement it
#     geom_point(size = 3,
#                alpha = 0.2) +
#     scale_colour_manual(values = my.palette[c(2, 1)]) +
#     stat_smooth(method = "lm",
#                 formula = y ~ x + I(x^2),
#                 linewidth = 3) +
#     theme_minimal() +
#     theme_main +
#     theme(legend.position = "none",
#           strip.background = element_blank(),
#           strip.text.x = element_blank(),
#           axis.title.y = element_text(size = 30, face = "bold"))
#   print(fig)
#   filename <- paste(pathname, "/figures/feature_histograms/", v, "_by_age.png", sep = "")
#   ggsave(filename, fig, 
#          height = 5, width = 8, dpi = 300)
# }
# 
# # chi-sq and table of categorical variables
# for (v in var_categorical) {
#   
#   data <- RF_full %>%
#     select(Age, all_of(v))
#   
#   names(data) <- c("Age", "variable")
#   
#   fig <- data %>%
#     group_by(Age) %>%
#     summarise(prop = sum(variable == 1)/length(variable)) %>%
#     ggplot(aes(x = Age,
#                y = prop)) +
#     geom_bar(position = position_stack(),
#              colour = "white", linewidth = 0.5,
#              stat = "identity", fill = my.palette[4]) + 
#     xlab('Age') +
#     ylab("Proportion of people") +
#     ggtitle(v)+
#     scale_x_continuous(limits = c(17, NA),
#                        breaks = seq(20, 90, by = 10)) +
#     scale_y_continuous(limits = c(0, 1))+
#     theme_cowplot() +
#     theme(legend.position = "none",
#           plot.title = element_text(size = 28, face = "bold", hjust = 0.5, family = font.family),
#           axis.text=element_text(size=20, family = font.family),
#           axis.title=element_text(size=20,face="bold", family = font.family),
#           plot.background = element_rect(fill = "white"))
#   
#   print(fig)
#   
#   filename <- paste(pathname, "/figures/feature_histograms/", v, "_by_age_proportions.png", sep = "")
#   ggsave(filename, fig, 
#          height = 5, width = 8, dpi = 300)  
#   if (v == "History of stroke") {
#     v <- "`History of stroke`"
#   }
#   if (v == "Ever smoked") {
#     v <- "`Ever smoked`"
#   }
#   cat("\n", "Variable:", v, "\n")
#   print(t.test(as.formula(paste("Age ~", v)), data = RF_full))
#   fig <- ggplot(RF_full, aes_string(x = v, y = "Age", fill = "as.factor(Woman)", group = v)) + # should use tidy evaluation if I figure out how to implement it
#     geom_violin(position = position_dodge(width = .75),
#                 linewidth = .5,
#                 color = "BLACK",
#                 alpha = 0.5) +
#     geom_boxplot(notch = TRUE,
#                  outlier.shape = NA,
#                  alpha = 0.7,
#                  width = .5,
#                  colour = "BLACK") +
#     scale_x_continuous(labels = c("No", "Yes"),
#                        breaks = c(0, 1)) +
#     theme_minimal() +
#     theme_main +
#     facet_wrap(~Woman) +
#     scale_fill_manual(values = my.palette[c(2, 1)]) +
#     theme(legend.position = "none",
#           strip.background = element_blank(),
#           strip.text.x = element_blank(),
#           axis.title.x = element_text(size = 30, face = "bold"))
#   
#   print(fig)
#   filename <- paste(pathname, "/figures/feature_histograms/", v, "_by_age.png", sep = "")
#   ggsave(filename, fig, 
#          height = 5, width = 8, dpi = 300)
#   
# }
# 


# 4. Test/train dataset segmentation --------------------------------------
var_continuous <- c("Age", "`Less education`", "mm", "cb", "sra", "srb")

## Unsplit data -------------------------------------------------
var_categorical <- c("MemoryProblems", "Woman")
# Stratified sampling by age, gender, and education
set.seed(123)
RF_stratified_all <- stratified(RF_full, c("Age", "Woman"), 0.8, bothSets = T)
# cca 80:20 split of whole sample
trainSet <- RF_stratified_all$SAMP1
trainSet$group <- "train"
testSet <- RF_stratified_all$SAMP2
testSet$group <- "test"
RF_stratified_all <- rbind(trainSet, testSet)

# test if stratified sampling worked - see if there are differences in those variables between test/train set
# t-test and histogram of continuous variables
for (v in var_continuous) {
  print(t.test(as.formula(paste(v, "~ group")), data = RF_stratified_all))
  print(ggplot(RF_stratified_all, aes_string(x = v)) + # should use tidy evaluation if I figure out how to implement it
          geom_histogram() +
          facet_wrap(~group))
}

# chi-sq and table of categorical variables
for (v in var_categorical) {
  cat("\n", "Variable:", v, "\n")
  print(chisq.test(RF_stratified_all$group, RF_stratified_all[[v]], correct = FALSE))
  print(table(RF_stratified_all$group, RF_stratified_all[[v]]))
}

# No sig. differences in either stratification variables or cognitive DVs

## Gender split -------------------------------------------------
RF_stratified_women <- RF_stratified_all %>%
  filter(`Woman` == 1)
RF_stratified_men <- RF_stratified_all %>%
  filter(`Woman` == 0)


## Check stratification for women ----------------------------------------------------
# test if stratified sampling worked - see if there are differences in those variables between test/train set
# t-test and histogram of continuous variables
for (v in var_continuous) {
  print(t.test(as.formula(paste(v, "~ group")), data = RF_stratified_women))
  print(ggplot(RF_stratified_women, aes_string(x = v)) + # should use tidy evaluation if I figure out how to implewoment it
          geom_histogram() +
          facet_wrap(~group))
}

# chi-sq and table of categorical variables
cat("\n", "Variable:", "Memory Problems", "\n")
print(chisq.test(RF_stratified_women$group, RF_stratified_women$MemoryProblems, correct = FALSE))
print(table(RF_stratified_women$group, RF_stratified_women$MemoryProblems))

# No sig. differences in cognitive DVs

## Check stratification for men ----------------------------------------------------
# test if stratified sampling worked - see if there are differences in those variables between test/train set
# t-test and histogram of continuous variables
for (v in var_continuous) {
  print(t.test(as.formula(paste(v, "~ group")), data = RF_stratified_men))
  print(ggplot(RF_stratified_men, aes_string(x = v)) + # should use tidy evaluation if I figure out how to implement it
          geom_histogram() +
          facet_wrap(~group))
}

# chi-sq and table of categorical variables
cat("\n", "Variable:", "Memory Problems", "\n")
print(chisq.test(RF_stratified_men$group, RF_stratified_men$MemoryProblems, correct = FALSE))
print(table(RF_stratified_men$group, RF_stratified_men$MemoryProblems))

# No sig. differences in cognitive DVs

## OLD CODE: Unsplit data ------------------------------------------------------------
# RF_stratified_all <- rbind(RF_stratified_women, RF_stratified_men)
# 
# var_categorical <- c("MemoryProblems", "Woman")
# 
# # test if stratified sampling worked - see if there are differences in those variables between test/train set
# # t-test and histogram of continuous variables
# for (v in var_continuous) {
#   print(t.test(as.formula(paste(v, "~ group")), data = RF_stratified_all))
#   print(ggplot(RF_stratified_all, aes_string(x = v)) + # should use tidy evaluation if I figure out how to implement it
#           geom_histogram() +
#           facet_wrap(~group))
# }
# 
# # chi-sq and table of categorical variables
# for (v in var_categorical) {
#   cat("\n", "Variable:", v, "\n")
#   print(chisq.test(RF_stratified_all$group, RF_stratified_all[[v]], correct = FALSE))
#   print(table(RF_stratified_all$group, RF_stratified_all[[v]]))
# }
# 
# # No sig. differences in either stratification variables or cognitive DVs

## OLD CODE: Age split ---------------------------------------------------------------
# var_categorical <- c("MemoryProblems", "Woman")
### OLD CODE: Younger --------------------------------------------------------
# # Stratified sampling by age, gender, and education
# set.seed(123)
# RF_stratified_young <- stratified(RF_young, c("Age", "Woman"), 0.8, bothSets = T)
# # cca 80:20 split of whole sample
# trainSet <- RF_stratified_young$SAMP1
# trainSet$group <- "train"
# testSet <- RF_stratified_young$SAMP2
# testSet$group <- "test"
# RF_stratified_young <- rbind(trainSet, testSet)
# 
# # test if stratified sampling worked - see if there are differences in those variables between test/train set
# # t-test and histogram of continuous variables
# 
# for (v in var_continuous) {
#   print(t.test(as.formula(paste(v, "~ group")), data = RF_stratified_young))
#   print(ggplot(RF_stratified_young, aes_string(x = v)) + # should use tidy evaluation if I figure out how to implement it
#           geom_histogram() +
#           facet_wrap(~group))
# }
# 
# # chi-sq and table of categorical variables
# for (v in var_categorical) {
#   cat("\n", "Variable:", v, "\n")
#   print(chisq.test(RF_stratified_young$group, RF_stratified_young[[v]], correct = FALSE))
#   print(table(RF_stratified_young$group, RF_stratified_young[[v]]))
# }
# 
# # No sig. differences in either stratification variables or cognitive DVs except cannon blast
# 
### OLD CODE: Older -----------------------------------------------------
# # Stratified sampling by age, gender, and education
# set.seed(123)
# RF_stratified_old <- stratified(RF_old, c("Age", "Woman"), 0.8, bothSets = T)
# # cca 80:20 split of whole sample
# trainSet <- RF_stratified_old$SAMP1
# trainSet$group <- "train"
# testSet <- RF_stratified_old$SAMP2
# testSet$group <- "test"
# RF_stratified_old <- rbind(trainSet, testSet)
# 
# # test if stratified sampling worked - see if there are differences in those variables between test/train set
# # t-test and histogram of continuous variables
# for (v in var_continuous) {
#   print(t.test(as.formula(paste(v, "~ group")), data = RF_stratified_old))
#   print(ggplot(RF_stratified_old, aes_string(x = v)) + # should use tidy evaluation if I figure out how to implement it
#           geom_histogram() +
#           facet_wrap(~group))
# }
# 
# # chi-sq and table of categorical variables
# for (v in var_categorical) {
#   cat("\n", "Variable:", v, "\n")
#   print(chisq.test(RF_stratified_old$group, RF_stratified_old[[v]], correct = FALSE))
#   print(table(RF_stratified_old$group, RF_stratified_old[[v]]))
# }
# # No sig. differences in other cognitive DVs
# 
# # Unsplit data again
# RF_stratified_age <- RF_stratified_young %>%
#   mutate('Over 48' = 0) %>%
#   rbind(RF_stratified_old %>%
#           mutate('Over 48' = 1))
# 
# # test if stratified sampling worked - see if there are differences in those variables between test/train set
# # t-test and histogram of continuous variables
# for (v in var_continuous) {
#   print(t.test(as.formula(paste(v, "~ group")), data = RF_stratified_age))
#   print(ggplot(RF_stratified_age, aes_string(x = v)) + # should use tidy evaluation if I figure out how to implement it
#           geom_histogram() +
#           facet_wrap(~group))
# }
# 
# # chi-sq and table of categorical variables
# for (v in var_categorical) {
#   cat("\n", "Variable:", v, "\n")
#   print(chisq.test(RF_stratified_age$group, RF_stratified_age[[v]], correct = FALSE))
#   print(table(RF_stratified_age$group, RF_stratified_age[[v]]))
# }
# 
# # No sig. differences in either stratification variables or cognitive DVs

## Subset train and test sets ----------------------------------------
trainSet_women <- RF_stratified_women %>% filter(group == "train")
testSet_women <- RF_stratified_women %>% filter(group == "test")

trainSet_men <- RF_stratified_men %>% filter(group == "train")
testSet_men <- RF_stratified_men %>% filter(group == "test")

trainSet_all <- RF_stratified_all %>% filter(group == "train")
testSet_all <- RF_stratified_all %>% filter(group == "test")

# trainSet_young <- RF_stratified_young %>% filter(group == "train")
# testSet_young <- RF_stratified_young %>% filter(group == "test")
# 
# trainSet_old <- RF_stratified_old %>% filter(group == "train")
# testSet_old <- RF_stratified_old %>% filter(group == "test")
# 
# trainSet_age <- RF_stratified_age %>% filter(group == "train")
# testSet_age <- RF_stratified_age %>% filter(group == "test")

## Create age-matched subset of the women sample: train set -------------
trainSet_all_matched <- trainSet_all

set.seed(123)
trainSet_all_matched$match <- case_when(trainSet_all_matched$Woman == 1 ~ 0,
                                        TRUE ~ 1)

## Matching by age
# Constructing a pre-match MatchIt object
matching.output <- matchit(match ~ Age, data = trainSet_all_matched,
                           method=NULL, distance="glm")
# Check balancing
summary(matching.output)

# 1:1 Nearest matching w/o replacement, to implement nearest neighbor matching
matching.output1 <- matchit(match ~ Age, data = trainSet_all_matched,
                            method="nearest", distance="glm") 
# Check balancing of matching
summary(matching.output1) 

## Matched subjects
trainSet_all_matched <- match.data(matching.output1)

## Create age-matched subset of the women sample: test set -------------
testSet_all_matched <- testSet_all

set.seed(123)
testSet_all_matched$match <- case_when(testSet_all_matched$Woman == 1 ~ 0,
                                       TRUE ~ 1)

## Matching by age
# Constructing a pre-match MatchIt object
matching.output <- matchit(match ~ Age, data = testSet_all_matched,
                           method=NULL, distance="glm")
# Check balancing
summary(matching.output)

# 1:1 Nearest matching w/o replacement, to implement nearest neighbor matching
matching.output1 <- matchit(match ~ Age, data = testSet_all_matched,
                            method="nearest", distance="glm") 
# Check balancing of matching
summary(matching.output1) 

## Matched subjects
testSet_all_matched <- match.data(matching.output1)

# Print age matching info for train set
print("Women")
print(mean((trainSet_all_matched %>% filter(Woman == 1))$Age))
print(sd((trainSet_all_matched %>% filter(Woman == 1))$Age))
print("Men")
print(mean((trainSet_all_matched %>% filter(Woman == 0))$Age))
print(sd((trainSet_all_matched %>% filter(Woman == 0))$Age))

# Print age matching info for test set
print("Women")
print(mean((testSet_all_matched %>% filter(Woman == 1))$Age))
print(sd((testSet_all_matched %>% filter(Woman == 1))$Age))
print("Men")
print(mean((testSet_all_matched %>% filter(Woman == 0))$Age))
print(sd((testSet_all_matched %>% filter(Woman == 0))$Age))

# Remove redundant objects
rm(matching.output, matching.output1)

testSet_all_matched <- testSet_all_matched %>%
  select(-match, -distance, -weights, -subclass)

trainSet_all_matched <- trainSet_all_matched %>%
  select(-match, -distance, -weights, -subclass)

## Create non-matched random subset -----------------------------------
set.seed(123)
trainSet_women_equalN <- trainSet_women %>%
  filter(PlayerID %in% sample(trainSet_women$PlayerID, size = nrow(trainSet_men)))
set.seed(123)
testSet_women_equalN <- testSet_women %>%
  filter(PlayerID %in% sample(testSet_women$PlayerID, size = nrow(testSet_men)))

# Merge
trainSet_all_equalN <- rbind(trainSet_men, trainSet_women_equalN)
testSet_all_equalN <- rbind(testSet_men, testSet_women_equalN)



# 5. Missing data imputation ----------------------------------------------
# Impute missing data for training dataset and then carry over estimated parameters to the test dataset
# This is because predicting outcomes for any new observation should be done with whatever variables are available,
# i.e. even if data is missing in the given observation, without the medians/modes of a test set
## Unsplit data -------------------------------------------------
# What are the variables that contain at least 1 NA in the train set?

# Print the % of NAs
# continuous train set
trainSet_all %>%
  select(!any_of(cat_variables_gender)) %>%
  summarise_all(~ sum(is.na(.)) / n() * 100) %>%
  gather(key = "Variable", value = "Percent_NA") %>%
  filter(Percent_NA > 0)
# categorical train set
trainSet_all %>%
  select(any_of(cat_variables_gender)) %>%
  summarise_all(~ sum(is.na(.)) / n() * 100) %>%
  gather(key = "Variable", value = "Percent_NA") %>%
  filter(Percent_NA > 0)
# continuous test set
testSet_all %>%
  select(!any_of(cat_variables_gender)) %>%
  summarise_all(~ sum(is.na(.)) / n() * 100) %>%
  gather(key = "Variable", value = "Percent_NA") %>%
  filter(Percent_NA > 0)
# categorical test set
testSet_all %>%
  select(any_of(cat_variables_gender)) %>%
  summarise_all(~ sum(is.na(.)) / n() * 100) %>%
  gather(key = "Variable", value = "Percent_NA") %>%
  filter(Percent_NA > 0)

# Next, impute missing data
# Here we take the mode for categorical variables and median for numeric/ordinal variables

# Extract column names
# continuous
cols_with_nas_num <- trainSet_all %>%
  select(!any_of(cat_variables_gender) &
           where(~ !all(!is.na(.)))) %>%
  colnames(.)
# categorical
cols_with_nas_cat <- trainSet_all %>%
  select(any_of(cat_variables_gender) &
           where(~ !all(!is.na(.)))) %>%
  colnames(.)

# Impute the median for continuous data and mode for categorical data
trainSet_all_imp <- trainSet_all %>%
  mutate_at(vars(all_of(cols_with_nas_num)), ~ ifelse(is.na(.x), median(.x, na.rm = TRUE), .x)) %>%
  mutate_at(vars(all_of(cols_with_nas_cat)), ~ ifelse(is.na(.x), calc_mode(.x), .x))

trainSet_all_imp[rowSums(is.na(trainSet_all_imp)) > 0, ] # double check if df contains any NAs in rows
colnames(trainSet_all_imp)[colSums(is.na(trainSet_all_imp)) > 0] # which column contains Na

# Extend to test set
# Here we take the mode for categorical variables and median for numeric/ordinal variables FROM THE TRAINING SET AND CARRY THEM OVER

# Test set
# Extract column names
# continuous
cols_with_nas_num <- testSet_all %>%
  select(!any_of(cat_variables_gender) &
           where(~ !all(!is.na(.)))) %>%
  colnames(.)
# categorical
cols_with_nas_cat <- testSet_all %>%
  select(any_of(cat_variables_gender) &
           where(~ !all(!is.na(.)))) %>%
  colnames(.)

# get median / mode of missing data variables from training set
medians <- trainSet_all %>%
  select(all_of(cols_with_nas_num)) %>%
  summarise_all(list(~ median(., na.rm = TRUE)))
mode <- trainSet_all %>%
  select(all_of(cols_with_nas_cat)) %>%
  summarise_all(list(calc_mode))

testSet_all_imp <- testSet_all

for (i in 1:length(cols_with_nas_num)) { # impute for numeric variables
  testSet_all_imp[[cols_with_nas_num[i]]] <- ifelse(is.na(testSet_all_imp[[cols_with_nas_num[i]]]),
                                                    medians[[cols_with_nas_num[i]]],
                                                    testSet_all_imp[[cols_with_nas_num[i]]]
  )
}

for (i in 1:length(cols_with_nas_cat)) { # impute for categorical variables
  testSet_all_imp[[cols_with_nas_cat[i]]] <- ifelse(is.na(testSet_all_imp[[cols_with_nas_cat[i]]]),
                                                    mode[[cols_with_nas_cat[i]]],
                                                    testSet_all_imp[[cols_with_nas_cat[i]]]
  )
}

testSet_all_imp[rowSums(is.na(testSet_all_imp)) > 0, ] # check if df contains any NAs in rows
colnames(testSet_all_imp)[colSums(is.na(testSet_all_imp)) > 0] # which column contains Na

## Women ----------------------------------------------------------------
# What are the variables that contain at least 1 NA in the train set?

# Print the % of NAs
# continuous train set
trainSet_women %>%
  select(!any_of(cat_variables_gender)) %>%
  summarise_all(~ sum(is.na(.)) / n() * 100) %>%
  gather(key = "Variable", value = "Percent_NA") %>%
  filter(Percent_NA > 0)
# categorical train set
trainSet_women %>%
  select(any_of(cat_variables_gender)) %>%
  summarise_all(~ sum(is.na(.)) / n() * 100) %>%
  gather(key = "Variable", value = "Percent_NA") %>%
  filter(Percent_NA > 0)
# continuous test set
testSet_women %>%
  select(!any_of(cat_variables_gender)) %>%
  summarise_all(~ sum(is.na(.)) / n() * 100) %>%
  gather(key = "Variable", value = "Percent_NA") %>%
  filter(Percent_NA > 0)
# categorical test set
testSet_women %>%
  select(any_of(cat_variables_gender)) %>%
  summarise_all(~ sum(is.na(.)) / n() * 100) %>%
  gather(key = "Variable", value = "Percent_NA") %>%
  filter(Percent_NA > 0)

# Next, impute missing data
# Here we take the mode for categorical variables and median for numeric/ordinal variables

# Extract column names
# continuous
cols_with_nas_num <- trainSet_women %>%
  select(!any_of(cat_variables_gender) &
           where(~ !all(!is.na(.)))) %>%
  colnames(.)
# categorical
cols_with_nas_cat <- trainSet_women %>%
  select(any_of(cat_variables_gender) &
           where(~ !all(!is.na(.)))) %>%
  colnames(.)

# Impute the median for continuous data and mode for categorical data
trainSet_women_imp <- trainSet_women %>%
  mutate_at(vars(all_of(cols_with_nas_num)), ~ ifelse(is.na(.x), median(.x, na.rm = TRUE), .x)) %>%
  mutate_at(vars(all_of(cols_with_nas_cat)), ~ ifelse(is.na(.x), calc_mode(.x), .x))

trainSet_women_imp[rowSums(is.na(trainSet_women_imp)) > 0, ] # double check if df contains any NAs in rows
colnames(trainSet_women_imp)[colSums(is.na(trainSet_women_imp)) > 0] # which column contains Na

# Extend to test set
# Here we take the mode for categorical variables and median for numeric/ordinal variables FROM THE TRAINING SET AND CARRY THEM OVER

# Test set
# Extract column names
# continuous
cols_with_nas_num <- testSet_women %>%
  select(!any_of(cat_variables_gender) &
           where(~ !all(!is.na(.)))) %>%
  colnames(.)
# categorical
cols_with_nas_cat <- testSet_women %>%
  select(any_of(cat_variables_gender) &
           where(~ !all(!is.na(.)))) %>%
  colnames(.)

# get median / mode of missing data variables from training set
medians <- trainSet_women %>%
  select(all_of(cols_with_nas_num)) %>%
  summarise_all(list(~ median(., na.rm = TRUE)))
mode <- trainSet_women %>%
  select(all_of(cols_with_nas_cat)) %>%
  summarise_all(list(calc_mode))

testSet_women_imp <- testSet_women

for (i in 1:length(cols_with_nas_num)) { # impute for numeric variables
  testSet_women_imp[[cols_with_nas_num[i]]] <- ifelse(is.na(testSet_women_imp[[cols_with_nas_num[i]]]),
                                                      medians[[cols_with_nas_num[i]]],
                                                      testSet_women_imp[[cols_with_nas_num[i]]]
  )
}

for (i in 1:length(cols_with_nas_cat)) { # impute for categorical variables
  testSet_women_imp[[cols_with_nas_cat[i]]] <- ifelse(is.na(testSet_women_imp[[cols_with_nas_cat[i]]]),
                                                      mode[[cols_with_nas_cat[i]]],
                                                      testSet_women_imp[[cols_with_nas_cat[i]]]
  )
}

testSet_women_imp[rowSums(is.na(testSet_women_imp)) > 0, ] # check if df contains any NAs in rows
colnames(testSet_women_imp)[colSums(is.na(testSet_women_imp)) > 0] # which column contains Na


## Men ----------------------------------------------------------------
# What are the variables that contain at least 1 NA in the train set?

# Print the % of NAs
# continuous train set
trainSet_men %>%
  select(!any_of(cat_variables_gender)) %>%
  summarise_all(~ sum(is.na(.)) / n() * 100) %>%
  gather(key = "Variable", value = "Percent_NA") %>%
  filter(Percent_NA > 0)
# categorical train set
trainSet_men %>%
  select(any_of(cat_variables_gender)) %>%
  summarise_all(~ sum(is.na(.)) / n() * 100) %>%
  gather(key = "Variable", value = "Percent_NA") %>%
  filter(Percent_NA > 0)
# continuous test set
testSet_men %>%
  select(!any_of(cat_variables_gender)) %>%
  summarise_all(~ sum(is.na(.)) / n() * 100) %>%
  gather(key = "Variable", value = "Percent_NA") %>%
  filter(Percent_NA > 0)
# categorical test set
testSet_men %>%
  select(any_of(cat_variables_gender)) %>%
  summarise_all(~ sum(is.na(.)) / n() * 100) %>%
  gather(key = "Variable", value = "Percent_NA") %>%
  filter(Percent_NA > 0)

# Next, impute missing data
# Here we take the mode for categorical variables and median for numeric/ordinal variables

# Extract column names
# continuous
cols_with_nas_num <- trainSet_men %>%
  select(!any_of(cat_variables_gender) &
           where(~ !all(!is.na(.)))) %>%
  colnames(.)
# categorical
cols_with_nas_cat <- trainSet_men %>%
  select(any_of(cat_variables_gender) &
           where(~ !all(!is.na(.)))) %>%
  colnames(.)

# Impute the median for continuous data and mode for categorical data
trainSet_men_imp <- trainSet_men %>%
  mutate_at(vars(all_of(cols_with_nas_num)), ~ ifelse(is.na(.x), median(.x, na.rm = TRUE), .x)) %>%
  mutate_at(vars(all_of(cols_with_nas_cat)), ~ ifelse(is.na(.x), calc_mode(.x), .x))

trainSet_men_imp[rowSums(is.na(trainSet_men_imp)) > 0, ] # double check if df contains any NAs in rows
colnames(trainSet_men_imp)[colSums(is.na(trainSet_men_imp)) > 0] # which column contains Na

# Extend to test set
# Here we take the mode for categorical variables and median for numeric/ordinal variables FROM THE TRAINING SET AND CARRY THEM OVER

# Test set
# Extract column names
# continuous
cols_with_nas_num <- testSet_men %>%
  select(!any_of(cat_variables_gender) &
           where(~ !all(!is.na(.)))) %>%
  colnames(.)
# categorical
cols_with_nas_cat <- testSet_men %>%
  select(any_of(cat_variables_gender) &
           where(~ !all(!is.na(.)))) %>%
  colnames(.)

# get median / mode of missing data variables from training set
medians <- trainSet_men %>%
  select(all_of(cols_with_nas_num)) %>%
  summarise_all(list(~ median(., na.rm = TRUE)))
mode <- trainSet_men %>%
  select(all_of(cols_with_nas_cat)) %>%
  summarise_all(list(calc_mode))

testSet_men_imp <- testSet_men

for (i in 1:length(cols_with_nas_num)) { # impute for numeric variables
  testSet_men_imp[[cols_with_nas_num[i]]] <- ifelse(is.na(testSet_men_imp[[cols_with_nas_num[i]]]),
                                                    medians[[cols_with_nas_num[i]]],
                                                    testSet_men_imp[[cols_with_nas_num[i]]]
  )
}

for (i in 1:length(cols_with_nas_cat)) { # impute for categorical variables
  testSet_men_imp[[cols_with_nas_cat[i]]] <- ifelse(is.na(testSet_men_imp[[cols_with_nas_cat[i]]]),
                                                    mode[[cols_with_nas_cat[i]]],
                                                    testSet_men_imp[[cols_with_nas_cat[i]]]
  )
}

testSet_men_imp[rowSums(is.na(testSet_men_imp)) > 0, ] # check if df contains any NAs in rows
colnames(testSet_men_imp)[colSums(is.na(testSet_men_imp)) > 0] # which column contains Na

## Age-matched subset -------------------------------------------------
# What are the variables that contain at least 1 NA in the train set?

# Print the % of NAs
# continuous train set
trainSet_all_matched %>%
  select(!any_of(cat_variables_gender)) %>%
  summarise_all(~ sum(is.na(.)) / n() * 100) %>%
  gather(key = "Variable", value = "Percent_NA") %>%
  filter(Percent_NA > 0)
# categorical train set
trainSet_all_matched %>%
  select(any_of(cat_variables_gender)) %>%
  summarise_all(~ sum(is.na(.)) / n() * 100) %>%
  gather(key = "Variable", value = "Percent_NA") %>%
  filter(Percent_NA > 0)
# continuous test set
testSet_all_matched %>%
  select(!any_of(cat_variables_gender)) %>%
  summarise_all(~ sum(is.na(.)) / n() * 100) %>%
  gather(key = "Variable", value = "Percent_NA") %>%
  filter(Percent_NA > 0)
# categorical test set
testSet_all_matched %>%
  select(any_of(cat_variables_gender)) %>%
  summarise_all(~ sum(is.na(.)) / n() * 100) %>%
  gather(key = "Variable", value = "Percent_NA") %>%
  filter(Percent_NA > 0)

# Next, impute missing data
# Here we take the mode for categorical variables and median for numeric/ordinal variables

# Extract column names
# continuous
cols_with_nas_num <- trainSet_all_matched %>%
  select(!any_of(cat_variables_gender) &
           where(~ !all(!is.na(.)))) %>%
  colnames(.)
# categorical
cols_with_nas_cat <- trainSet_all_matched %>%
  select(any_of(cat_variables_gender) &
           where(~ !all(!is.na(.)))) %>%
  colnames(.)

# Impute the median for continuous data and mode for categorical data
trainSet_all_matched_imp <- trainSet_all_matched %>%
  mutate_at(vars(all_of(cols_with_nas_num)), ~ ifelse(is.na(.x), median(.x, na.rm = TRUE), .x)) %>%
  mutate_at(vars(all_of(cols_with_nas_cat)), ~ ifelse(is.na(.x), calc_mode(.x), .x))

trainSet_all_matched_imp[rowSums(is.na(trainSet_all_matched_imp)) > 0, ] # double check if df contains any NAs in rows
colnames(trainSet_all_matched_imp)[colSums(is.na(trainSet_all_matched_imp)) > 0] # which column contains Na

# Extend to test set
# Here we take the mode for categorical variables and median for numeric/ordinal variables FROM THE TRAINING SET AND CARRY THEM OVER

# Test set
# Extract column names
# continuous
cols_with_nas_num <- testSet_all_matched %>%
  select(!any_of(cat_variables_gender) &
           where(~ !all(!is.na(.)))) %>%
  colnames(.)
# categorical
cols_with_nas_cat <- testSet_all_matched %>%
  select(any_of(cat_variables_gender) &
           where(~ !all(!is.na(.)))) %>%
  colnames(.)

# get median / mode of missing data variables from training set
medians <- trainSet_all_matched %>%
  select(all_of(cols_with_nas_num)) %>%
  summarise_all(list(~ median(., na.rm = TRUE)))
mode <- trainSet_all_matched %>%
  select(all_of(cols_with_nas_cat)) %>%
  summarise_all(list(calc_mode))

testSet_all_matched_imp <- testSet_all_matched

for (i in 1:length(cols_with_nas_num)) { # impute for numeric variables
  testSet_all_matched_imp[[cols_with_nas_num[i]]] <- ifelse(is.na(testSet_all_matched_imp[[cols_with_nas_num[i]]]),
                                                    medians[[cols_with_nas_num[i]]],
                                                    testSet_all_matched_imp[[cols_with_nas_num[i]]]
  )
}

for (i in 1:length(cols_with_nas_cat)) { # impute for categorical variables
  testSet_all_matched_imp[[cols_with_nas_cat[i]]] <- ifelse(is.na(testSet_all_matched_imp[[cols_with_nas_cat[i]]]),
                                                    mode[[cols_with_nas_cat[i]]],
                                                    testSet_all_matched_imp[[cols_with_nas_cat[i]]]
  )
}

testSet_all_matched_imp[rowSums(is.na(testSet_all_matched_imp)) > 0, ] # check if df contains any NAs in rows
colnames(testSet_all_matched_imp)[colSums(is.na(testSet_all_matched_imp)) > 0] # which column contains Na

## Non-matched subset -------------------------------------------------
# What are the variables that contain at least 1 NA in the train set?

# Print the % of NAs
# continuous train set
trainSet_all_equalN %>%
  select(!any_of(cat_variables_gender)) %>%
  summarise_all(~ sum(is.na(.)) / n() * 100) %>%
  gather(key = "Variable", value = "Percent_NA") %>%
  filter(Percent_NA > 0)
# categorical train set
trainSet_all_equalN %>%
  select(any_of(cat_variables_gender)) %>%
  summarise_all(~ sum(is.na(.)) / n() * 100) %>%
  gather(key = "Variable", value = "Percent_NA") %>%
  filter(Percent_NA > 0)
# continuous test set
testSet_all_equalN %>%
  select(!any_of(cat_variables_gender)) %>%
  summarise_all(~ sum(is.na(.)) / n() * 100) %>%
  gather(key = "Variable", value = "Percent_NA") %>%
  filter(Percent_NA > 0)
# categorical test set
testSet_all_equalN %>%
  select(any_of(cat_variables_gender)) %>%
  summarise_all(~ sum(is.na(.)) / n() * 100) %>%
  gather(key = "Variable", value = "Percent_NA") %>%
  filter(Percent_NA > 0)

# Next, impute missing data
# Here we take the mode for categorical variables and median for numeric/ordinal variables

# Extract column names
# continuous
cols_with_nas_num <- trainSet_all_equalN %>%
  select(!any_of(cat_variables_gender) &
           where(~ !all(!is.na(.)))) %>%
  colnames(.)
# categorical
cols_with_nas_cat <- trainSet_all_equalN %>%
  select(any_of(cat_variables_gender) &
           where(~ !all(!is.na(.)))) %>%
  colnames(.)

# Impute the median for continuous data and mode for categorical data
trainSet_all_equalN_imp <- trainSet_all_equalN %>%
  mutate_at(vars(all_of(cols_with_nas_num)), ~ ifelse(is.na(.x), median(.x, na.rm = TRUE), .x)) %>%
  mutate_at(vars(all_of(cols_with_nas_cat)), ~ ifelse(is.na(.x), calc_mode(.x), .x))

trainSet_all_equalN_imp[rowSums(is.na(trainSet_all_equalN_imp)) > 0, ] # double check if df contains any NAs in rows
colnames(trainSet_all_equalN_imp)[colSums(is.na(trainSet_all_equalN_imp)) > 0] # which column contains Na

# Extend to test set
# Here we take the mode for categorical variables and median for numeric/ordinal variables FROM THE TRAINING SET AND CARRY THEM OVER

# Test set
# Extract column names
# continuous
cols_with_nas_num <- testSet_all_equalN %>%
  select(!any_of(cat_variables_gender) &
           where(~ !all(!is.na(.)))) %>%
  colnames(.)
# categorical
cols_with_nas_cat <- testSet_all_equalN %>%
  select(any_of(cat_variables_gender) &
           where(~ !all(!is.na(.)))) %>%
  colnames(.)

# get median / mode of missing data variables from training set
medians <- trainSet_all_equalN %>%
  select(all_of(cols_with_nas_num)) %>%
  summarise_all(list(~ median(., na.rm = TRUE)))
mode <- trainSet_all_equalN %>%
  select(all_of(cols_with_nas_cat)) %>%
  summarise_all(list(calc_mode))

testSet_all_equalN_imp <- testSet_all_equalN

for (i in 1:length(cols_with_nas_num)) { # impute for numeric variables
  testSet_all_equalN_imp[[cols_with_nas_num[i]]] <- ifelse(is.na(testSet_all_equalN_imp[[cols_with_nas_num[i]]]),
                                                    medians[[cols_with_nas_num[i]]],
                                                    testSet_all_equalN_imp[[cols_with_nas_num[i]]]
  )
}

for (i in 1:length(cols_with_nas_cat)) { # impute for categorical variables
  testSet_all_equalN_imp[[cols_with_nas_cat[i]]] <- ifelse(is.na(testSet_all_equalN_imp[[cols_with_nas_cat[i]]]),
                                                    mode[[cols_with_nas_cat[i]]],
                                                    testSet_all_equalN_imp[[cols_with_nas_cat[i]]]
  )
}

testSet_all_equalN_imp[rowSums(is.na(testSet_all_equalN_imp)) > 0, ] # check if df contains any NAs in rows
colnames(testSet_all_equalN_imp)[colSums(is.na(testSet_all_equalN_imp)) > 0] # which column contains Na



# 
## Younger ----------------------------------------------------------------
# 
# # Print the % of NAs
# # continuous train set
# trainSet_young %>%
#   select(!any_of(cat_variables_age)) %>%
#   summarise_all(~ sum(is.na(.)) / n() * 100) %>%
#   gather(key = "Variable", value = "Percent_NA") %>%
#   filter(Percent_NA > 0)
# # categorical train set
# trainSet_young %>%
#   select(any_of(cat_variables_age)) %>%
#   summarise_all(~ sum(is.na(.)) / n() * 100) %>%
#   gather(key = "Variable", value = "Percent_NA") %>%
#   filter(Percent_NA > 0)
# # continuous test set
# trainSet_young %>%
#   select(!any_of(cat_variables_age)) %>%
#   summarise_all(~ sum(is.na(.)) / n() * 100) %>%
#   gather(key = "Variable", value = "Percent_NA") %>%
#   filter(Percent_NA > 0)
# # categorical test set
# trainSet_young %>%
#   select(any_of(cat_variables_age)) %>%
#   summarise_all(~ sum(is.na(.)) / n() * 100) %>%
#   gather(key = "Variable", value = "Percent_NA") %>%
#   filter(Percent_NA > 0)
# 
# # Next, impute missing data
# # Here we take the mode for categorical variables and median for numeric/ordinal variables
# 
# # Extract column names
# # continuous
# cols_with_nas_num <- trainSet_young %>%
#   select(!any_of(cat_variables_age) &
#            where(~ !all(!is.na(.)))) %>%
#   colnames(.)
# # categorical
# cols_with_nas_cat <- trainSet_young %>%
#   select(any_of(cat_variables_age) &
#            where(~ !all(!is.na(.)))) %>%
#   colnames(.)
# 
# # Impute the median for continuous data and mode for categorical data
# trainSet_young_imp <- trainSet_young %>%
#   mutate_at(vars(all_of(cols_with_nas_num)), ~ ifelse(is.na(.x), median(.x, na.rm = TRUE), .x)) %>%
#   mutate_at(vars(all_of(cols_with_nas_cat)), ~ ifelse(is.na(.x), calc_mode(.x), .x))
# 
# trainSet_young_imp[rowSums(is.na(trainSet_young_imp)) > 0, ] # double check if df contains any NAs in rows
# colnames(trainSet_young_imp)[colSums(is.na(trainSet_young_imp)) > 0] # which column contains Na
# 
# # Extend to test set
# # Here we take the mode for categorical variables and median for numeric/ordinal variables FROM THE TRAINING SET AND CARRY THEM OVER
# 
# # Test set
# # Extract column names
# # continuous
# cols_with_nas_num <- testSet_young %>%
#   select(!any_of(cat_variables_age) &
#            where(~ !all(!is.na(.)))) %>%
#   colnames(.)
# # categorical
# cols_with_nas_cat <- testSet_young %>%
#   select(any_of(cat_variables_age) &
#            where(~ !all(!is.na(.)))) %>%
#   colnames(.)
# 
# # get median / mode of missing data variables from training set
# medians <- trainSet_young %>%
#   select(all_of(cols_with_nas_num)) %>%
#   summarise_all(list(~ median(., na.rm = TRUE)))
# mode <- trainSet_young %>%
#   select(all_of(cols_with_nas_cat)) %>%
#   summarise_all(list(calc_mode))
# 
# testSet_young_imp <- testSet_young
# 
# for (i in 1:length(cols_with_nas_num)) { # impute for numeric variables
#   testSet_young_imp[[cols_with_nas_num[i]]] <- ifelse(is.na(testSet_young_imp[[cols_with_nas_num[i]]]),
#                                                       medians[[cols_with_nas_num[i]]],
#                                                       testSet_young_imp[[cols_with_nas_num[i]]]
#   )
# }
# 
# for (i in 1:length(cols_with_nas_cat)) { # impute for categorical variables
#   testSet_young_imp[[cols_with_nas_cat[i]]] <- ifelse(is.na(testSet_young_imp[[cols_with_nas_cat[i]]]),
#                                                       mode[[cols_with_nas_cat[i]]],
#                                                       testSet_young_imp[[cols_with_nas_cat[i]]]
#   )
# }
# 
# testSet_young_imp[rowSums(is.na(testSet_young_imp)) > 0, ] # check if df contains any NAs in rows
# colnames(testSet_young_imp)[colSums(is.na(testSet_young_imp)) > 0] # which column contains Na
# 
# 
## Older ----------------------------------------------------------------
# # Print the % of NAs
# # continuous train set
# trainSet_old %>%
#   select(!any_of(cat_variables_age)) %>%
#   summarise_all(~ sum(is.na(.)) / n() * 100) %>%
#   gather(key = "Variable", value = "Percent_NA") %>%
#   filter(Percent_NA > 0)
# # categorical train set
# trainSet_old %>%
#   select(any_of(cat_variables_age)) %>%
#   summarise_all(~ sum(is.na(.)) / n() * 100) %>%
#   gather(key = "Variable", value = "Percent_NA") %>%
#   filter(Percent_NA > 0)
# # continuous test set
# trainSet_old %>%
#   select(!any_of(cat_variables_age)) %>%
#   summarise_all(~ sum(is.na(.)) / n() * 100) %>%
#   gather(key = "Variable", value = "Percent_NA") %>%
#   filter(Percent_NA > 0)
# # categorical test set
# trainSet_old %>%
#   select(any_of(cat_variables_age)) %>%
#   summarise_all(~ sum(is.na(.)) / n() * 100) %>%
#   gather(key = "Variable", value = "Percent_NA") %>%
#   filter(Percent_NA > 0)
# 
# # Next, impute missing data
# # Here we take the mode for categorical variables and median for numeric/ordinal variables
# 
# # Extract column names
# # continuous
# cols_with_nas_num <- trainSet_old %>%
#   select(!any_of(cat_variables_age) &
#            where(~ !all(!is.na(.)))) %>%
#   colnames(.)
# # categorical
# cols_with_nas_cat <- trainSet_old %>%
#   select(any_of(cat_variables_age) &
#            where(~ !all(!is.na(.)))) %>%
#   colnames(.)
# 
# # Impute the median for continuous data and mode for categorical data
# trainSet_old_imp <- trainSet_old %>%
#   mutate_at(vars(all_of(cols_with_nas_num)), ~ ifelse(is.na(.x), median(.x, na.rm = TRUE), .x)) %>%
#   mutate_at(vars(all_of(cols_with_nas_cat)), ~ ifelse(is.na(.x), calc_mode(.x), .x))
# 
# trainSet_old_imp[rowSums(is.na(trainSet_old_imp)) > 0, ] # double check if df contains any NAs in rows
# colnames(trainSet_old_imp)[colSums(is.na(trainSet_old_imp)) > 0] # which column contains Na
# 
# # Extend to test set
# # Here we take the mode for categorical variables and median for numeric/ordinal variables FROM THE TRAINING SET AND CARRY THEM OVER
# 
# # Test set
# # Extract column names
# # continuous
# cols_with_nas_num <- testSet_old %>%
#   select(!any_of(cat_variables_age) &
#            where(~ !all(!is.na(.)))) %>%
#   colnames(.)
# # categorical
# cols_with_nas_cat <- testSet_old %>%
#   select(any_of(cat_variables_age) &
#            where(~ !all(!is.na(.)))) %>%
#   colnames(.)
# 
# # get median / mode of missing data variables from training set
# medians <- trainSet_old %>%
#   select(all_of(cols_with_nas_num)) %>%
#   summarise_all(list(~ median(., na.rm = TRUE)))
# mode <- trainSet_old %>%
#   select(all_of(cols_with_nas_cat)) %>%
#   summarise_all(list(calc_mode))
# 
# testSet_old_imp <- testSet_old
# 
# for (i in 1:length(cols_with_nas_num)) { # impute for numeric variables
#   testSet_old_imp[[cols_with_nas_num[i]]] <- ifelse(is.na(testSet_old_imp[[cols_with_nas_num[i]]]),
#                                                     medians[[cols_with_nas_num[i]]],
#                                                     testSet_old_imp[[cols_with_nas_num[i]]]
#   )
# }
# 
# for (i in 1:length(cols_with_nas_cat)) { # impute for categorical variables
#   testSet_old_imp[[cols_with_nas_cat[i]]] <- ifelse(is.na(testSet_old_imp[[cols_with_nas_cat[i]]]),
#                                                     mode[[cols_with_nas_cat[i]]],
#                                                     testSet_old_imp[[cols_with_nas_cat[i]]]
#   )
# }
# 
# testSet_old_imp[rowSums(is.na(testSet_old_imp)) > 0, ] # check if df contains any NAs in rows
# colnames(testSet_old_imp)[colSums(is.na(testSet_old_imp)) > 0] # which column contains Na
# 
## Unsplit age data -------------------------------------------------
# # What are the variables that contain at least 1 NA in the train set?
# 
# # Print the % of NAs
# # continuous train set
# trainSet_age %>%
#   select(!any_of(cat_variables_age)) %>%
#   summarise_all(~ sum(is.na(.)) / n() * 100) %>%
#   gather(key = "Variable", value = "Percent_NA") %>%
#   filter(Percent_NA > 0)
# # categorical train set
# trainSet_age %>%
#   select(any_of(cat_variables_age)) %>%
#   summarise_all(~ sum(is.na(.)) / n() * 100) %>%
#   gather(key = "Variable", value = "Percent_NA") %>%
#   filter(Percent_NA > 0)
# # continuous test set
# testSet_age %>%
#   select(!any_of(cat_variables_age)) %>%
#   summarise_all(~ sum(is.na(.)) / n() * 100) %>%
#   gather(key = "Variable", value = "Percent_NA") %>%
#   filter(Percent_NA > 0)
# # categorical test set
# testSet_age %>%
#   select(any_of(cat_variables_age)) %>%
#   summarise_all(~ sum(is.na(.)) / n() * 100) %>%
#   gather(key = "Variable", value = "Percent_NA") %>%
#   filter(Percent_NA > 0)
# 
# # Next, impute missing data
# # Here we take the mode for categorical variables and median for numeric/ordinal variables
# 
# # Extract column names
# # continuous
# cols_with_nas_num <- trainSet_age %>%
#   select(!any_of(cat_variables_age) &
#            where(~ !all(!is.na(.)))) %>%
#   colnames(.)
# # categorical
# cols_with_nas_cat <- trainSet_age %>%
#   select(any_of(cat_variables_age) &
#            where(~ !all(!is.na(.)))) %>%
#   colnames(.)
# 
# # Impute the median for continuous data and mode for categorical data
# trainSet_age_imp <- trainSet_age %>%
#   mutate_at(vars(all_of(cols_with_nas_num)), ~ ifelse(is.na(.x), median(.x, na.rm = TRUE), .x)) %>%
#   mutate_at(vars(all_of(cols_with_nas_cat)), ~ ifelse(is.na(.x), calc_mode(.x), .x))
# 
# trainSet_age_imp[rowSums(is.na(trainSet_age_imp)) > 0, ] # double check if df contains any NAs in rows
# colnames(trainSet_age_imp)[colSums(is.na(trainSet_age_imp)) > 0] # which column contains Na
# 
# # Extend to test set
# # Here we take the mode for categorical variables and median for numeric/ordinal variables FROM THE TRAINING SET AND CARRY THEM OVER
# 
# # Test set
# # Extract column names
# # continuous
# cols_with_nas_num <- testSet_age %>%
#   select(!any_of(cat_variables_age) &
#            where(~ !all(!is.na(.)))) %>%
#   colnames(.)
# # categorical
# cols_with_nas_cat <- testSet_age %>%
#   select(any_of(cat_variables_age) &
#            where(~ !all(!is.na(.)))) %>%
#   colnames(.)
# 
# # get median / mode of missing data variables from training set
# medians <- trainSet_age %>%
#   select(all_of(cols_with_nas_num)) %>%
#   summarise_all(list(~ median(., na.rm = TRUE)))
# mode <- trainSet_age %>%
#   select(all_of(cols_with_nas_cat)) %>%
#   summarise_all(list(calc_mode))
# 
# testSet_age_imp <- testSet_age
# 
# for (i in 1:length(cols_with_nas_num)) { # impute for numeric variables
#   testSet_age_imp[[cols_with_nas_num[i]]] <- ifelse(is.na(testSet_age_imp[[cols_with_nas_num[i]]]),
#                                                     medians[[cols_with_nas_num[i]]],
#                                                     testSet_age_imp[[cols_with_nas_num[i]]]
#   )
# }
# 
# for (i in 1:length(cols_with_nas_cat)) { # impute for categorical variables
#   testSet_age_imp[[cols_with_nas_cat[i]]] <- ifelse(is.na(testSet_age_imp[[cols_with_nas_cat[i]]]),
#                                                     mode[[cols_with_nas_cat[i]]],
#                                                     testSet_age_imp[[cols_with_nas_cat[i]]]
#   )
# }
# 
# testSet_age_imp[rowSums(is.na(testSet_age_imp)) > 0, ] # check if df contains any NAs in rows
# colnames(testSet_age_imp)[colSums(is.na(testSet_age_imp)) > 0] # which column contains Na

# 6. Drop rarely endorsed items --------------------------------------
# First check the descriptive stats of these
nearZeroVar(trainSet_all_imp, names = T, saveMetrics = T)
nearZeroVar(trainSet_women_imp, names = T, saveMetrics = T)
nearZeroVar(trainSet_men_imp, names = T, saveMetrics = T)
nearZeroVar(trainSet_all_equalN_imp, names = T, saveMetrics = T)
nearZeroVar(trainSet_all_matched_imp, names = T, saveMetrics = T)

# Diabetes is not so rarely endorsed for men but for women yes, excluding for both
# Export variable names to drop the features
nzv_all <- nearZeroVar(trainSet_all_imp, names = T) 

# Get rid of rarely endorsed items in train/test sets
trainSet_all_imp <- trainSet_all_imp %>% select(-all_of(nzv_all))
testSet_all_imp <- testSet_all_imp %>% select(-all_of(nzv_all))

trainSet_women_imp <- trainSet_women_imp %>% select(-all_of(nzv_all))
testSet_women_imp <- testSet_women_imp %>% select(-all_of(nzv_all))

trainSet_men_imp <- trainSet_men_imp %>% select(-all_of(nzv_all))
testSet_men_imp <- testSet_men_imp %>% select(-all_of(nzv_all))

trainSet_all_equalN_imp <- trainSet_all_equalN_imp %>% select(-all_of(nzv_all))
testSet_all_equalN_imp <- testSet_all_equalN_imp %>% select(-all_of(nzv_all))

trainSet_all_matched_imp <- trainSet_all_matched_imp %>% select(-all_of(nzv_all))
testSet_all_matched_imp <- testSet_all_matched_imp %>% select(-all_of(nzv_all))

# 7. Scaling variables ----------------------------------------------------
## OLD CODE: Normalization (min-max scaler with limits set to 0.05, 0.95) -----------
# Set aside continuous variables
# train_continuous_subset_women <- trainSet_women_imp %>%
#   select(!any_of(c(cat_variables_gender, "mm", "sra", "srb", "PlayerID")))
# 
# test_continuous_subset_women <- testSet_women_imp %>%
#   select(!any_of(c(cat_variables_gender, "mm", "sra", "srb", "PlayerID")))
# 
# train_continuous_subset_men <- trainSet_men_imp %>%
#   select(!any_of(c(cat_variables_gender, "mm", "sra", "srb", "PlayerID")))
# 
# test_continuous_subset_men <- testSet_men_imp %>%
#   select(!any_of(c(cat_variables_gender, "mm", "sra", "srb", "PlayerID")))
# 
# train_continuous_subset_all <- trainSet_all_imp %>%
#   select(!any_of(c(cat_variables_gender, "mm", "sra", "srb", "PlayerID")))
# 
# test_continuous_subset_all <- testSet_all_imp %>%
#   select(!any_of(c(cat_variables_gender, "mm", "sra", "srb", "PlayerID")))
# 
# # Define the preProcess object with custom range for each data subset
# preProc_women_imp <- train_continuous_subset_women %>%
#   preProcess(method = c("range"), rangeBounds = c(0.05, 0.95))
# preProc_men_imp <- train_continuous_subset_men %>%
#   preProcess(method = c("range"), rangeBounds = c(0.05, 0.95))
# preProc_all_imp <- train_continuous_subset_all %>%
#   preProcess(method = c("range"), rangeBounds = c(0.05, 0.95))
# 
# # Apply the transformations to both train and test sets
# trainSet_women_imp <- trainSet_women_imp %>%
#   select(PlayerID, any_of(cat_variables_gender), mm, sra, srb) %>%
#   cbind(predict(preProc_women_imp, train_continuous_subset_women))
# 
# testSet_women_imp <- testSet_women_imp %>%
#   select(PlayerID, any_of(cat_variables_gender), mm, sra, srb) %>%
#   cbind(predict(preProc_women_imp, test_continuous_subset_women))
# 
# trainSet_men_imp <- trainSet_men_imp %>%
#   select(PlayerID, any_of(cat_variables_gender), mm, sra, srb) %>%
#   cbind(predict(preProc_men_imp, train_continuous_subset_men))
# 
# testSet_men_imp <- testSet_men_imp %>%
#   select(PlayerID, any_of(cat_variables_gender), mm, sra, srb) %>%
#   cbind(predict(preProc_men_imp, test_continuous_subset_men))
# 
# trainSet_all_imp <- trainSet_all_imp %>%
#   select(PlayerID, any_of(cat_variables_gender), mm, sra, srb) %>%
#   cbind(predict(preProc_all_imp, train_continuous_subset_all))
# 
# testSet_all_imp <- testSet_all_imp %>%
#   select(PlayerID, any_of(cat_variables_gender), mm, sra, srb) %>%
#   cbind(predict(preProc_all_imp, test_continuous_subset_all))
# 
# # Additional scaling for dependent variables
# train_means_women_imp <- trainSet_women_imp %>%
#     select(mm, sra, srb) %>%
#     summarise_all(list(~ mean(., na.rm = TRUE)))
# train_stds_women_imp <- trainSet_women_imp %>%
#     select(mm, sra, srb) %>%
#     summarise_all(list(~ sd(., na.rm = TRUE)))
# 
# train_means_men_imp <- trainSet_men_imp %>%
#     select(mm, sra, srb) %>%
#     summarise_all(list(~ mean(., na.rm = TRUE)))
# train_stds_men_imp <- trainSet_men_imp %>%
#     select(mm, sra, srb) %>%
#     summarise_all(list(~ sd(., na.rm = TRUE)))
# 
# train_means_all_imp <- trainSet_all_imp %>%
#     select(mm, sra, srb) %>%
#     summarise_all(list(~ mean(., na.rm = TRUE)))
# train_stds_all_imp <- trainSet_all_imp %>%
#     select(mm, sra, srb) %>%
#     summarise_all(list(~ sd(., na.rm = TRUE)))
# 
# # scale the train sets
# trainSet_women_imp <- trainSet_women_imp %>%
#   mutate(across(c(mm, sra, srb), ~ (.x - train_means_women_imp$.x) / train_stds_women_imp$.x))
# trainSet_men_imp <- trainSet_men_imp %>%
#   mutate(across(c(mm, sra, srb), ~ (.x - train_means_men_imp$.x) / train_stds_men_imp$.x))
# trainSet_all_imp <- trainSet_all_imp %>%
#   mutate(across(c(mm, sra, srb), ~ (.x - train_means_all_imp$.x) / train_stds_all_imp$.x))
# # scale the test sets
# testSet_women_imp <- testSet_women_imp %>%
#   mutate(across(c(mm, sra, srb), ~ (.x - train_means_women_imp$.x) / train_stds_women_imp$.x))
# testSet_men_imp <- testSet_men_imp %>%
#   mutate(across(c(mm, sra, srb), ~ (.x - train_means_men_imp$.x) / train_stds_men_imp$.x))
# testSet_all_imp <- testSet_all_imp %>%
#   mutate(across(c(mm, sra, srb), ~ (.x - train_means_all_imp$.x) / train_stds_all_imp$.x))

## Scaling (standardizing) -----------------------------------------
# Scale (standardize) numeric variables using train set scale to avoid leakage
# scaled_train =  (train - train_mean) / train_std_deviation
# scaled_test = (test - train_mean) / train_std_deviation
# get the means and standard deviations of numeric items of the train set
train_means_women_imp <- trainSet_women_imp %>%
  select(!any_of(c(cat_variables_gender, "PlayerID"))) %>%
  summarise_all(list(~ mean(., na.rm = TRUE)))
train_stds_women_imp <- trainSet_women_imp %>%
  select(!any_of(c(cat_variables_gender, "PlayerID"))) %>%
  summarise_all(list(~ sd(., na.rm = TRUE)))

train_means_men_imp <- trainSet_men_imp %>%
  select(!any_of(c(cat_variables_gender, "PlayerID"))) %>%
  summarise_all(list(~ mean(., na.rm = TRUE)))
train_stds_men_imp <- trainSet_men_imp %>%
  select(!any_of(c(cat_variables_gender, "PlayerID"))) %>%
  summarise_all(list(~ sd(., na.rm = TRUE)))

train_means_all_imp <- trainSet_all_imp %>%
  select(!any_of(c(cat_variables_gender, "PlayerID"))) %>%
  summarise_all(list(~ mean(., na.rm = TRUE)))
train_stds_all_imp <- trainSet_all_imp %>%
  select(!any_of(c(cat_variables_gender, "PlayerID"))) %>%
  summarise_all(list(~ sd(., na.rm = TRUE)))

train_means_all_matched_imp <- trainSet_all_matched_imp %>%
  select(!any_of(c(cat_variables_gender, "PlayerID"))) %>%
  summarise_all(list(~ mean(., na.rm = TRUE)))
train_stds_all_matched_imp <- trainSet_all_matched_imp %>%
  select(!any_of(c(cat_variables_gender, "PlayerID"))) %>%
  summarise_all(list(~ sd(., na.rm = TRUE)))

train_means_all_equalN_imp <- trainSet_all_equalN_imp %>%
  select(!any_of(c(cat_variables_gender, "PlayerID"))) %>%
  summarise_all(list(~ mean(., na.rm = TRUE)))
train_stds_all_equalN_imp <- trainSet_all_equalN_imp %>%
  select(!any_of(c(cat_variables_gender, "PlayerID"))) %>%
  summarise_all(list(~ sd(., na.rm = TRUE)))

#
# cat_variables_age <- c(cat_variables_age, "Over 48")
#
# train_means_young_imp <- trainSet_young_imp %>%
#   select(!any_of(c(cat_variables_age, "PlayerID"))) %>%
#   summarise_all(list(~ mean(., na.rm = TRUE)))
# train_stds_young_imp <- trainSet_young_imp %>%
#   select(!any_of(c(cat_variables_age, "PlayerID"))) %>%
#   summarise_all(list(~ sd(., na.rm = TRUE)))
#
# train_means_old_imp <- trainSet_old_imp %>%
#   select(!any_of(c(cat_variables_age, "PlayerID"))) %>%
#   summarise_all(list(~ mean(., na.rm = TRUE)))
# train_stds_old_imp <- trainSet_old_imp %>%
#   select(!any_of(c(cat_variables_age, "PlayerID"))) %>%
#   summarise_all(list(~ sd(., na.rm = TRUE)))
#
# train_means_age_imp <- trainSet_age_imp %>%
#   select(!any_of(c(cat_variables_age, "PlayerID"))) %>%
#   summarise_all(list(~ mean(., na.rm = TRUE)))
# train_stds_age_imp <- trainSet_age_imp %>%
#   select(!any_of(c(cat_variables_age, "PlayerID"))) %>%
#   summarise_all(list(~ sd(., na.rm = TRUE)))
#

# scale the train sets
trainSet_women_imp <- trainSet_women_imp %>%
  mutate(across(!any_of(c(cat_variables_gender, "PlayerID")), ~ (.x - train_means_women_imp$.x) / train_stds_women_imp$.x))

trainSet_men_imp <- trainSet_men_imp %>%
  mutate(across(!any_of(c(cat_variables_gender, "PlayerID")), ~ (.x - train_means_men_imp$.x) / train_stds_men_imp$.x))

trainSet_all_imp <- trainSet_all_imp %>%
  mutate(across(!any_of(c(cat_variables_gender, "PlayerID")), ~ (.x - train_means_all_imp$.x) / train_stds_all_imp$.x))

trainSet_all_equalN_imp <- trainSet_all_equalN_imp %>%
  mutate(across(!any_of(c(cat_variables_gender, "PlayerID")), ~ (.x - train_means_all_equalN_imp$.x) / train_stds_all_equalN_imp$.x))

trainSet_all_matched_imp <- trainSet_all_matched_imp %>%
  mutate(across(!any_of(c(cat_variables_gender, "PlayerID")), ~ (.x - train_means_all_matched_imp$.x) / train_stds_all_matched_imp$.x))



# trainSet_young_imp <- trainSet_young_imp %>%
#   mutate(across(!any_of(c(cat_variables_age, "PlayerID")), ~ (.x - train_means_young_imp$.x) / train_stds_young_imp$.x))
#
# trainSet_old_imp <- trainSet_old_imp %>%
#   mutate(across(!any_of(c(cat_variables_age, "PlayerID")), ~ (.x - train_means_old_imp$.x) / train_stds_old_imp$.x))
#
# trainSet_age_imp <- trainSet_age_imp %>%
#   mutate(across(!any_of(c(cat_variables_age, "PlayerID")), ~ (.x - train_means_age_imp$.x) / train_stds_age_imp$.x))


# scale the test sets
testSet_women_imp <- testSet_women_imp %>%
  mutate(across(!any_of(c(cat_variables_gender, "PlayerID")), ~ (.x - train_means_women_imp$.x) / train_stds_women_imp$.x))

testSet_men_imp <- testSet_men_imp %>%
  mutate(across(!any_of(c(cat_variables_gender, "PlayerID")), ~ (.x - train_means_men_imp$.x) / train_stds_men_imp$.x))

testSet_all_imp <- testSet_all_imp %>%
  mutate(across(!any_of(c(cat_variables_gender, "PlayerID")), ~ (.x - train_means_all_imp$.x) / train_stds_all_imp$.x))

testSet_all_matched_imp <- testSet_all_matched_imp %>%
  mutate(across(!any_of(c(cat_variables_gender, "PlayerID")), ~ (.x - train_means_all_matched_imp$.x) / train_stds_all_matched_imp$.x))

testSet_all_equalN_imp <- testSet_all_equalN_imp %>%
  mutate(across(!any_of(c(cat_variables_gender, "PlayerID")), ~ (.x - train_means_all_equalN_imp$.x) / train_stds_all_equalN_imp$.x))


# testSet_young_imp <- testSet_young_imp %>%
#   mutate(across(!any_of(c(cat_variables_age, "PlayerID")), ~ (.x - train_means_young_imp$.x) / train_stds_young_imp$.x))
#
# testSet_old_imp <- testSet_old_imp %>%
#   mutate(across(!any_of(c(cat_variables_age, "PlayerID")), ~ (.x - train_means_old_imp$.x) / train_stds_old_imp$.x))
#
# testSet_age_imp <- testSet_age_imp %>%
#   mutate(across(!any_of(c(cat_variables_age, "PlayerID")), ~ (.x - train_means_age_imp$.x) / train_stds_age_imp$.x))

# 8. Generating cognitive composite scores --------------------------------
# Apply the function to each dataset
trainSet_women_imp <- compute_avg_z(trainSet_women_imp)
testSet_women_imp <- compute_avg_z(testSet_women_imp)

trainSet_men_imp <- compute_avg_z(trainSet_men_imp)
testSet_men_imp <- compute_avg_z(testSet_men_imp)

trainSet_all_imp <- compute_avg_z(trainSet_all_imp)
testSet_all_imp <- compute_avg_z(testSet_all_imp)

trainSet_all_equalN_imp <- compute_avg_z(trainSet_all_equalN_imp)
testSet_all_equalN_imp <- compute_avg_z(testSet_all_equalN_imp)

trainSet_all_matched_imp <- compute_avg_z(trainSet_all_matched_imp)
testSet_all_matched_imp <- compute_avg_z(testSet_all_matched_imp)

# Which gender is better?
t.test(avg_z ~ Woman, data = trainSet_all_imp)

full_imp <- rbind(trainSet_all_imp %>%
                    mutate(group = "train"),
                  testSet_all_imp %>%
                    mutate(group = "test"))

# Generate histogram for supplement
ggplot(full_imp, aes(x = avg_z, fill = as.factor(Woman))) + # should use tidy evaluation if I figure out how to implement it
  geom_histogram(colour = "white") +
  scale_fill_manual(values = my.palette[c(2, 1)]) +
  facet_wrap(~Woman) +
  theme_minimal() +
  theme_main +
  xlab("Composite obj. cognition (avg. z)") +
  theme(legend.position = "none",
        strip.background = element_blank(),
        strip.text.x = element_blank(),
        axis.title.x = element_text(size = 30, face = "bold"))
ggsave(paste(pathname, "/figures/feature_histograms/avg_z_by_gender.png", sep = ""),
       height = 5, width = 8, dpi = 300)


# 
# trainSet_young_imp <- compute_avg_z(trainSet_young_imp)
# testSet_young_imp <- compute_avg_z(testSet_young_imp)
# 
# trainSet_old_imp <- compute_avg_z(trainSet_old_imp)
# testSet_old_imp <- compute_avg_z(testSet_old_imp)
# 
# trainSet_age_imp <- compute_avg_z(trainSet_age_imp)
# testSet_age_imp <- compute_avg_z(testSet_age_imp)

## Old code: PCA-based component ----------------------------------------------------
### women -------------------------------------------------
# # Create correlation matrix for PCA: women
# corr_train_data_women_imp <- trainSet_women_imp %>%
#   select(mm, sra, srb) %>%
#   mutate(across(where(is.numeric), ~ as.numeric(.x))) %>%
#   data.frame()
# colnames(corr_train_data_women_imp) <- c("Visual working memory", "Processing speed",
#                                 "Cognitive flexibility")
# 
# het_mat_women_imp <- hetcor(corr_train_data_women_imp)$cor
# 
# 
# # Testing assumptions for PCA
# KMO(het_mat_women_imp) # between 0.58 (cognitive flexibility) and 0.85 (visual working memory), overall MSA = 0.62 -> our sample size might be small
# cortest.bartlett(het_mat_women_imp, n = nrow(corr_train_data_women_imp)) # is significant so our correlation matrix is not an identity matrix
# det(het_mat_women_imp) # determinant sufficiently high (0.3254554) -> multicollinearity not a problem
# 
# # Double check number of components
# fa.parallel(corr_train_data_women_imp, fm = "ml", fa = "pc")
# # seems that 1 component is OK
# 
# # Run the PCA
# principal <- principal(corr_train_data_women_imp, nfactors = 1, rotate = "oblimin")
# print(principal, sort=T)
# fa.diagram(principal, cut = .15, simple = FALSE, adj=3, digits = 2)
# 
# # Extract individual scores & weights
# scores_women_imp <- data.frame(principal$scores)
# weights_women_imp <- data.frame(principal$weights)
# colnames(scores_women_imp) <- colnames(weights_women_imp) <- "PC_composite_cognition"
# 
# trainSet_women_imp <- trainSet_women_imp %>%
#   cbind(., scores_women_imp)
# 
# write.csv(weights_women_imp, file=
#             paste0(pathname, "/output/weights_women_imp.csv"))
# 
# # Carry over weights from train set to test set
# corr_test_data_women_imp <- testSet_women_imp %>%
#   select(mm, sra, srb) %>%
#   mutate(across(where(is.numeric), ~ as.numeric(.x)))
# 
# PCA_result <- corr_test_data_women_imp*t(weights_women_imp)[col(corr_test_data_women_imp)]
# PC_composite_cognition <- rowSums(PCA_result[])
# 
# testSet_women_imp <- testSet_women_imp %>%
#   mutate(PC_composite_cognition = PC_composite_cognition)
# 
### men -------------------------------------------------
# # Create correlation matrix for PCA: men
# corr_train_data_men_imp <- trainSet_men_imp %>%
#   select(mm, sra, srb) %>%
#   mutate(across(where(is.numeric), ~ as.numeric(.x))) %>%
#   data.frame()
# colnames(corr_train_data_men_imp) <- c("Visual working memory", "Processing speed",
#                                           "Cognitive flexibility")
# 
# het_mat_men_imp <- hetcor(corr_train_data_men_imp)$cor
# 
# 
# # Testing assumptions for PCA
# KMO(het_mat_men_imp) # between 0.58 (cognitive flexibility) and 0.85 (visual working memory), overall MSA = 0.62 -> our sample size might be small
# cortest.bartlett(het_mat_men_imp, n = nrow(corr_train_data_men_imp)) # is significant so our correlation matrix is not an identity matrix
# det(het_mat_men_imp) # determinant sufficiently high (0.3254554) -> multicollinearity not a problem
# 
# # Double check number of components
# fa.parallel(corr_train_data_men_imp, fm = "ml", fa = "pc")
# # seems that 1 component is OK
# 
# # Run the PCA
# principal <- principal(corr_train_data_men_imp, nfactors = 1, rotate = "oblimin")
# print(principal, sort=T)
# fa.diagram(principal, cut = .15, simple = FALSE, adj=3, digits = 2)
# 
# # Extract individual scores & weights
# scores_men_imp <- data.frame(principal$scores)
# weights_men_imp <- data.frame(principal$weights)
# colnames(scores_men_imp) <- colnames(weights_men_imp) <- "PC_composite_cognition"
# 
# trainSet_men_imp <- trainSet_men_imp %>%
#   cbind(., scores_men_imp)
# 
# write.csv(weights_men_imp, file=
#             paste0(pathname, "/output/weights_men_imp.csv"))
# 
# # Carry over weights from train set to test set
# corr_test_data_men_imp <- testSet_men_imp %>%
#   select(mm, sra, srb) %>%
#   mutate(across(where(is.numeric), ~ as.numeric(.x)))
# 
# PCA_result <- corr_test_data_men_imp*t(weights_men_imp)[col(corr_test_data_men_imp)]
# PC_composite_cognition <- rowSums(PCA_result[])
# 
# testSet_men_imp <- testSet_men_imp %>%
#   mutate(PC_composite_cognition = PC_composite_cognition)
# 
### Young -------------------------------------------------
# # Create correlation matrix for PCA: women
# corr_train_data_young_imp <- trainSet_young_imp %>%
#   select(mm, sra, srb) %>%
#   mutate(across(where(is.numeric), ~ as.numeric(.x))) %>%
#   data.frame()
# colnames(corr_train_data_young_imp) <- c("Visual working memory", "Processing speed",
#                                           "Cognitive flexibility")
# 
# het_mat_young_imp <- hetcor(corr_train_data_young_imp)$cor
# 
# 
# # Testing assumptions for PCA
# KMO(het_mat_young_imp) # between 0.58 (cognitive flexibility) and 0.85 (visual working memory), overall MSA = 0.62 -> our sample size might be small
# cortest.bartlett(het_mat_young_imp, n = nrow(corr_train_data_young_imp)) # is significant so our correlation matrix is not an identity matrix
# det(het_mat_young_imp) # determinant sufficiently high (0.3254554) -> multicollinearity not a problem
# 
# # Double check number of components
# fa.parallel(corr_train_data_young_imp, fm = "ml", fa = "pc")
# # seems that 1 component is OK
# 
# # Run the PCA
# principal <- principal(corr_train_data_young_imp, nfactors = 1, rotate = "oblimin")
# print(principal, sort=T)
# fa.diagram(principal, cut = .15, simple = FALSE, adj=3, digits = 2)
# 
# # Extract individual scores & weights
# scores_young_imp <- data.frame(principal$scores)
# weights_young_imp <- data.frame(principal$weights)
# colnames(scores_young_imp) <- colnames(weights_young_imp) <- "PC_composite_cognition"
# 
# trainSet_young_imp <- trainSet_young_imp %>%
#   cbind(., scores_young_imp)
# 
# write.csv(weights_young_imp, file=
#             paste0(pathname, "/output/weights_young_imp.csv"))
# 
# # Carry over weights from train set to test set
# corr_test_data_young_imp <- testSet_young_imp %>%
#   select(mm, sra, srb) %>%
#   mutate(across(where(is.numeric), ~ as.numeric(.x)))
# 
# PCA_result <- corr_test_data_young_imp*t(weights_young_imp)[col(corr_test_data_young_imp)]
# PC_composite_cognition <- rowSums(PCA_result[])
# 
# testSet_young_imp <- testSet_young_imp %>%
#   mutate(PC_composite_cognition = PC_composite_cognition)
# 
### Old -------------------------------------------------
# # Create correlation matrix for PCA: women
# corr_train_data_old_imp <- trainSet_old_imp %>%
#   select(mm, sra, srb) %>%
#   mutate(across(where(is.numeric), ~ as.numeric(.x))) %>%
#   data.frame()
# colnames(corr_train_data_old_imp) <- c("Visual working memory", "Processing speed",
#                                           "Cognitive flexibility")
# 
# het_mat_old_imp <- hetcor(corr_train_data_old_imp)$cor
# 
# 
# # Testing assumptions for PCA
# KMO(het_mat_old_imp) # between 0.58 (cognitive flexibility) and 0.85 (visual working memory), overall MSA = 0.62 -> our sample size might be small
# cortest.bartlett(het_mat_old_imp, n = nrow(corr_train_data_old_imp)) # is significant so our correlation matrix is not an identity matrix
# det(het_mat_old_imp) # determinant sufficiently high (0.3254554) -> multicollinearity not a problem
# 
# # Double check number of components
# fa.parallel(corr_train_data_old_imp, fm = "ml", fa = "pc")
# # seems that 1 component is OK
# 
# # Run the PCA
# principal <- principal(corr_train_data_old_imp, nfactors = 1, rotate = "oblimin")
# print(principal, sort=T)
# fa.diagram(principal, cut = .15, simple = FALSE, adj=3, digits = 2)
# 
# # Extract individual scores & weights
# scores_old_imp <- data.frame(principal$scores)
# weights_old_imp <- data.frame(principal$weights)
# colnames(scores_old_imp) <- colnames(weights_old_imp) <- "PC_composite_cognition"
# 
# trainSet_old_imp <- trainSet_old_imp %>%
#   cbind(., scores_old_imp)
# 
# write.csv(weights_old_imp, file=
#             paste0(pathname, "/output/weights_old_imp.csv"))
# 
# # Carry over weights from train set to test set
# corr_test_data_old_imp <- testSet_old_imp %>%
#   select(mm, sra, srb) %>%
#   mutate(across(where(is.numeric), ~ as.numeric(.x)))
# 
# PCA_result <- corr_test_data_old_imp*t(weights_old_imp)[col(corr_test_data_old_imp)]
# PC_composite_cognition <- rowSums(PCA_result[])
# 
# testSet_old_imp <- testSet_old_imp %>%
#   mutate(PC_composite_cognition = PC_composite_cognition)

# 9. Export preprocessed data -------------------------------------------------------------
# save(trainSet_women_imp, trainSet_men_imp, trainSet_all_imp, trainSet_young_imp,
#      trainSet_old_imp, trainSet_age_imp,
#      file = paste0(pathname, "/output/trainsets.RData"))
# save(testSet_women_imp, testSet_men_imp, testSet_all_imp, testSet_young_imp,
#      testSet_old_imp, testSet_age_imp,
#      file = paste0(pathname, "/output/testsets.RData"))

save(trainSet_women_imp, trainSet_men_imp, trainSet_all_imp, trainSet_all_equalN_imp, trainSet_all_matched_imp, file = paste0(pathname, "/output/trainsets_final.RData"))
save(testSet_women_imp, testSet_men_imp, testSet_all_imp, testSet_all_equalN_imp, testSet_all_matched_imp, file = paste0(pathname, "/output/testsets_final.RData"))

# 
# save(trainSet_women_imp, trainSet_men_imp, trainSet_all_imp, file = paste0(pathname, "/output/trainsets_minmax.RData"))
# save(testSet_women_imp, testSet_men_imp, testSet_all_imp, file = paste0(pathname, "/output/testsets_minmax.RData"))

