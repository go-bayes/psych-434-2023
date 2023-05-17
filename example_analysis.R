# PSYCH 434: Example script for assessment 3 and 5.
# May 2023
# questions: joseph.bulbulia@vuw.ac.nz
# Running this command will download the functions and packages you need to complete this worksheet.
# You many find the code by pointing your browser to the webpage that is contained in the link

# Before running this source code, make sure to update to the current version of R, and to update all exisiting packages.
source("https://raw.githubusercontent.com/go-bayes/templates/main/functions/funs.R")


# experimental functions
source(
  "https://raw.githubusercontent.com/go-bayes/templates/main/functions/experimental_funs.R"
)

############## ############## ############## ############## ############## ############## ############## ########
#########  ############## ############## IMPORT DATA ##############  ############## ############## ##############
############## ############## ############## ############## ############## ############## ############## ########


#  If you haven't already, you should have created a folder called "data", in your Rstudio project. If not, download this file, add it to your the folder called "data" in your Rstudio project. # "https://www.dropbox.com/s/vwqijg4ha17hbs1/nzavs_dat_synth_t10_t12?dl=0"


# This will read the synthetic data into Rstudio.  Note that the arrow package allows us to have lower memory demands in the storage and retrieval of data.
nzavs_synth <-
  arrow::read_parquet(here::here("data", "nzavs_dat_synth_t10_t12"))




## use colnames to inspect the variables
colnames(nzavs_synth)


#  MAKE SURE TO FAMILIARISE YOURSELF ABOUT THE VARIABLES HERE:
# https://github.com/go-bayes/psych-434-2023/blob/main/data/readme.qmd

#########################
#########################
#########################
# DATA WRANGLING
#########################
#########################
#########################

# Start with the nzavs_synth dataset
dt_start <- nzavs_synth |>

  # Create a new column 'kessler_6_sum' by summing across Kessler distress scale items
  mutate(kessler_6_sum = round(rowSums(across(
    # Specify the Kessler scale items
    c(
      kessler_depressed,
      # During the last 30 days, how often did you feel so depressed that nothing could cheer you up?
      kessler_hopeless,
      # During the last 30 days, how often did you feel hopeless?
      kessler_nervous,
      # During the last 30 days, how often did you feel nervous?
      kessler_effort,
      # During the last 30 days, how often did you feel that everything was an effort?
      kessler_restless,
      # During the last 30 days, how often did you feel restless or fidgety?
      kessler_worthless  # During the last 30 days, how often did you feel worthless?
    )
  )),
  0)) |>  # Round the sum to the nearest whole number) |>


  # Create a categorical variable 'kessler_6_coarsen' based on the sum of Kessler scale items
  mutate(
    kessler_6_coarsen = cut(
      kessler_6_sum,
      breaks = c(0, 5, 13, 24),
      # Define thresholds for categories
      include.lowest = TRUE,
      include.highest = TRUE,
      na.rm = TRUE,
      right = FALSE
    )
  ) |>

  # Create a new column 'kessler_6' as the average of sum scores of Kessler scale items
  mutate(kessler_6  = mean(rowSums(across(
    # Specify the Kessler scale items
    c(
      kessler_depressed,
      # During the last 30 days, how often did you feel so depressed that nothing could cheer you up?
      kessler_hopeless,
      # During the last 30 days, how often did you feel hopeless?
      kessler_nervous,
      # During the last 30 days, how often did you feel nervous?
      kessler_effort,
      # During the last 30 days, how often did you feel that everything was an effort?
      kessler_restless,
      # During the last 30 days, how often did you feel restless or fidgety ?
      kessler_worthless  # During the last 30 days, how often did you feel worthless?
    )
  )))) |>

  # If desired, create a 't2_meaning' column based on the average of 'meaning_purpose' and 'meaning_sense'
  dplyr::mutate(meaning = mean(rowSums(across(
    c(meaning_purpose ,  # My life has a clear sense of purpose.
      meaning_sense)
  )) , # I have a good sense of what makes my life meaningful.))),
  na.rm = TRUE)) |>

  # Transform 'hours_exercise' by applying the log function to compress its scale
  mutate(hours_exercise_log = log(hours_exercise + 1)) |> # Add 1 to avoid undefined log(0). Hours spent exercising/physical activity

  # Coarsen 'hours_exercise' into categories
  mutate(
    hours_exercise_coarsen = cut(
      hours_exercise,
      # Hours spent exercising/ physical activity
      breaks = c(-1, 1, 2, 7, 200),
      labels = c(
        "inactive",
        "somewhat_active",
        "active",
        "extremely_active"
      ),
      # Define thresholds for categories
      levels = c("(-1,1]", "(1,3]", "(3,7]", "(7,200]"),
      ordered = TRUE
    )
  ) |>

  # Create a binary 'urban' variable based on the 'rural_gch2018' variable
  mutate(urban = factor(
    ifelse(
      rural_gch2018 == "medium_urban_accessibility" |
        # Define urban condition
        rural_gch2018 == "high_urban_accessibility",
      "urban",
      # Label 'urban' if condition is met
      "rural"  # Label 'rural' if condition is not met
    )
  ))




# do some checks
levels(dt_start$hours_exercise_coarsen)
table(dt_start$hours_exercise_coarsen)
max( dt_start$hours_exercise)
min( dt_start$hours_exercise)

# checks
table(is.na(dt_start$kessler_6_coarsen))
table(is.na(dt_start$hours_exercise_coarsen))

# justification for transforming exercise" has a very long tail
hist(dt_start$hours_exercise, breaks = 1000)

# consider only those cases below < or = to 20
hist(subset(dt_start, hours_exercise <= 20)$hours_exercise)


# inspect kessler 6
table(dt_start$kessler_6_coarsen)
table(dt_start$hours_exercise_coarsen)

# trick
hist( as.numeric(dt_start$kessler_6_coarsen) )
hist( as.numeric(dt_start$hours_exercise_coarsen))




#########################
#########################
#########################
# CFA  FOR KESSLER 6
#########################
#########################
#########################

# Suppose we have reason to think Kessler 6 isn't one thing.
# Let's put our factor analysis skills to work
# Here we will use the paramters and see packages for R (part of the Easystats suite)

# for efa/cfa
if (!require(psych)) {
  install.packages("psych")
  library("psych")
}

# for reporting
if (!require(parameters)) {
  install.packages("parameters")
  library("parameters")
}

# for graphing
if (!require(see)) {
  install.packages("see")
  library("see")
}

# for graphing
if (!require(lavaan)) {
  install.packages("lavaan")
  library("lavaan")
}

dt_only_k6 <- dt_start |> select(kessler_depressed, kessler_effort,kessler_hopeless,
                                 kessler_worthless, kessler_nervous,
                                 kessler_restless)


# Check factor structure
performance::check_factorstructure(dt_only_k6)


# exploratory factor analysis
# explore a factor structure made of 2 latent variables
efa <- psych::fa(dt_only_k6, nfactors = 2) %>%
  model_parameters(sort = TRUE, threshold = "max")

efa

# fa -- there is no agreed method!
# method of agreement:

n <- n_factors(dt_only_k6)

# summary
n

# view data
as.data.frame(n)

# plot of smmary
plot(n) + theme_modern()


## CFA

# first we partition the data,  set seed for reproducability

partitions <- datawizard::data_partition(dt_only_k6, training_proportion = 0.7, seed = 123)
training <- partitions$p_0.7
test <- partitions$test

# create cfa structurees from efa models

# one factor
structure_k6_one <- psych::fa(training, nfactors = 1) %>%
  efa_to_cfa()

# two factor model
structure_k6_two <- psych::fa(training, nfactors = 2) %>%
  efa_to_cfa()

# three structure model
structure_k6_three <- psych::fa(training, nfactors = 3) %>%
  efa_to_cfa()

# inspect models
structure_k6_one
structure_k6_two
structure_k6_three

# fit and compare models
one_latent <- suppressWarnings(lavaan::cfa(structure_k6_one, data = test))
two_latents <- suppressWarnings(lavaan::cfa(structure_k6_two, data = test))
three_latents <- suppressWarnings(lavaan::cfa(structure_k6_three, data = test))


compare <- performance::compare_performance(one_latent, two_latents, three_latents, verbose = FALSE)


# view table
as.data.frame(compare)

# view as html table
as.data.frame(compare)|>
  kbl(format = "markdown")

# Interpret table
# using Goodness-of-Fit indices.

# Chi-square (Chi2): this test assesses the difference between the observed covariance matrix and the covariance matrix predicted by the model. A non-significant chi-square (i.e., p-value > .05) indicates a good fit, but this test is sensitive to sample size. Lower chi-square values indicate better fit.
#
# Goodness of Fit Index (GFI), Adjusted Goodness of Fit Index (AGFI), Normed Fit Index (NFI), Non-Normed Fit Index (NNFI), Comparative Fit Index (CFI), Incremental Fit Index (IFI), Relative Noncentrality Index (RNI): these are incremental fit indices. Values closer to 1 indicate a better fit.
#
# Root Mean Square Error of Approximation (RMSEA): this index is a measure of fit per degrees of freedom, correcting for model complexity. Lower values (usually below .06) suggest a better fit.
#
# Standardized Root Mean square Residual (SRMR): this is the standardised difference between the observed correlation and the predicted correlation. Lower values (usually below .08) indicate a better fit.
#
# Akaike Information Criterion (AIC) and Bayesian Information Criterion (BIC): These are used to compare models, with lower values indicating a better fit.
#
# RESULTS
#
# The Chi-square is higher in the three-factor model (747.872) than in the two-factor model (317.971), but lower than the one-factor model (1359.717). This indicates that the two-factor model fits the data better than the other two.
#
# The GFI, AGFI, NFI, NNFI, CFI, IFI, RNI are all closer to 1 for the two-factor model than for either the one-factor or the three-factor model. This suggests the two-factor model is the best fit.
#
# The RMSEA for the two-factor model (0.051) is lower than for both the one-factor (0.103) and three-factor models (0.083). Lower RMSEA values indicate a better model fit, meaning the two-factor model remains preferable. The p-value for the RMSEA of the three-factor model is less than .001, which is not ideal.
#
# The SRMR is lowest for the two-factor model (0.023) compared to both the one-factor (0.049) and three-factor models (0.038), once again suggesting the two-factor model is the best fit.
#
# AIC and BIC are similar for all models, so they do not provide a clear preference in this case. these values are typically used to compare models with different numbers of parameters, so they may not be the most critical indices in this case.
#
# Overall, based on these results, the two-factor model still appears to provide a better fit to the data than the one-factor or the three-factor models according to these indices. Therefore, the two-factor model should still be preferred.
#
# The general principle here is that adding more factors (or latent variables) can sometimes improve the model fit, but it can also lead to overfitting, where the model becomes too complex and may not generalize well to other samples. The key is to find the simplest model that provides a good fit to the data, and in this case, that seems to be the two-factor model.

# **** We should not think of KESSLER 6 as one thing ****


# So let's create new variables, anxiety and depression

# Reminder of the factor structure
structure_k6_two

# new dataset
dt_start2 <- dt_start |>
  mutate(depression_latent = mean(rowSums(across( c(kessler_depressed, kessler_hopeless, kessler_effort)), na.rm=TRUE)) ) |>
  mutate(anxiety_latent  = mean(rowSums(across( c(kessler_depressed, kessler_hopeless, kessler_effort)), na.rm=TRUE)) )



#########################
#########################
#########################
# Change in exposure
#########################
#########################
#########################


# inspect change in the exposure

# We only inspect change between the baseline condition and the exposure year

# full sample
dt_exposure <- dt_start2 |>

  # select baseline year and exposure year
  filter(wave == "2018" | wave == "2019") |>

  # select variables of interest
  select(id, wave, hours_exercise_coarsen,  eth_cat) |>

  # the categorical variable needs to be numeric for us to use msm package to investigate change
  mutate(hours_exercise_coarsen_n = as.numeric(hours_exercise_coarsen))



#  maybe consider people going from active to vary active
out <- msm::statetable.msm(round(hours_exercise_coarsen_n, 0), id, data = dt_exposure)

out

# for a function I wrote to create state tables
state_names <- c("Inactive", "Somewhat Active", "Active", "Extremely Active")

# transition table


transition_table(out, states_names)


# Maori only

dt_exposure_maori <- dt_exposure |>
  filter(eth_cat == "māori")

# lower support
out <- msm::statetable.msm(round(hours_exercise_coarsen_n, 0), id, data = dt_exposure_maori)


out <- data.frame(out)

# european
dt_exposure_euro <- dt_exposure |>
  filter(eth_cat == "euro")

# lower support
out <- msm::statetable.msm(round(hours_exercise_coarsen_n, 0), id, data = dt_exposure_euro)



transition_table( data.frame(out), state_names)


# |                     | Inactive | Somewhat Active | Active | Extremely Active |
# |---------------------|----------|-----------------|--------|------------------|
# | **Inactive**        | 415      | 187             | 403    | 105              |
# | **Somewhat Active** | 187      | 213             | 471    | 103              |
# | **Active**          | 293      | 424             | 2837   | 939              |
# | **Extremely Active**| 66       | 67              | 753    | 1178             |


# This transition matrix describes the shifts in physical activity levels from one state to another for the European ethnic subgroup between the baseline wave and the following wave. The numbers in the cells represent the number of individuals who transitioned from one state (rows) to another (columns). For example, 415 individuals remained inactive from the baseline wave to the following wave, while 187 individuals transitioned from being inactive to somewhat active.

transition_table <- function(data, state_names = NULL){

  # Ensure the data is a dataframe
  if (!is.data.frame(data)) {
    data <- as.data.frame(data)
  }

  # Check if state names are provided
  if(is.null(state_names)){
    state_names <- paste0("State ", sort(unique(c(data$from, data$to))))
  }

  # Convert the data frame to a wide format
  df <- data %>%
    pivot_wider(names_from = to, values_from = Freq) %>%
    mutate(from = factor(from, levels = sort(unique(from)))) %>%
    arrange(from) %>%
    mutate(from = state_names[from]) %>%
    setNames(c("From", state_names))

  # Create the markdown table using knitr's kable function
  markdown_table <- df %>%
    kable(format = "markdown", align = 'c')

  # Create the explanation
  explanation <- paste(
    "This transition matrix describes the shifts from one state to another between the baseline wave and the following wave.",
    "The numbers in the cells represent the number of individuals who transitioned from one state (rows) to another (columns).",
    "For example, the cell in the first row and second column shows the number of individuals who transitioned from the first state (indicated by the left-most cell in the row) to the second state.",
    "The top left cell shows the number of individuals who remained in the first state.")

  list(explanation = explanation, table = markdown_table)
}

# Test the function
data <- data.frame(
  from = rep(1:4, each = 4),
  to = rep(1:4, 4),
  Freq = c(415, 187, 293, 66, 187, 213, 424, 67, 403, 471, 2837, 753, 105, 103, 939, 1178)
)
data
state_names <- c("Inactive", "Somewhat Active", "Active", "Extremely Active")
result <- transition_table(out, state_names)
result
# Print the explanation and table
print(result$table)
cat(result$explanation)



############## ############## ############## ############## ############## ############## ############## ########
####  ####  ####  CREATE DATA FRAME FOR ANALYSIS ####  ####  ################## ############## ######## #########
############## ############## ############## ############## ############## ############## ############# #########

# To find out more about our dataset go here:
# https://github.com/go-bayes/psych-434-2023/blob/main/data/readme.qmd



# recall from the previous lecture that confounders are variables that may be associated with both an exposure and an outcome.


# I have created a function that will put the data into the correct shape. Here are the steps.

# Step 1: choose baseline variables.  here we select standard demographic variablees plus personality variables.

colnames(nzavs_synth)
head(nzavs_synth)

baseline_vars = c(
  "edu",
  "male",
  "eth_cat",
  "employed",
  "gen_cohort",
  "nz_dep2018",
  "nzsei13",
  "partner",
  "parent",
  "pol_orient",
  "rural_gch2018",
  "agreeableness",
  "conscientiousness",
  "extraversion",
  "honesty_humility",
  "openness",
  "neuroticism",
  "modesty",
  "religion_identification_level"
)


## Step 2, select the exposure variable.  This is the "cause"

exposure_var = c("perfectionism", "perfectionism_coarsen")


## step 3. select the outcome variable.  These are the outcomes.
outcome_vars_reflective = c("meaning_purpose",
                            "meaning_sense")

colnames(nzavs_synth)


# the function "create_wide_data" should be in your environment.
# If not, make sure to run the first line of code in this script once more.  You may ignore the warnings. or uncomment and run the code below
# source("https://raw.githubusercontent.com/go-bayes/templates/main/functions/funs.R")

prep_reflective <-
  create_wide_data(
    dat_long = nzavs_synth,
    #nzavs_synth,
    baseline_vars = baseline_vars,
    exposure_var = exposure_var,
    outcome_vars = outcome_vars_reflective
  )


colnames(prep_reflective)

# I have created a function that will allow you to take a data frame and
# create a table
baseline_table(prep_reflective, output_format = "markdown")


# if you just want a nice html table, do this:
library(table1) # should be in your environment

# get data into shape
dt_new <- prep_reflective %>%
  select(starts_with("t0")) %>%
  rename_all(~ stringr::str_replace(., "^t0_", "")) %>%
  mutate(wave = factor(rep("baseline", nrow(prep_reflective)))) |>
  janitor::clean_names(case = "screaming_snake")


# create a formula string

baseline_vars_names <- dt_new %>%
  select(-WAVE) %>%
  colnames()

table_baseline_vars <-
  paste(baseline_vars_names, collapse = "+")

formula_string_table_baseline <-
  paste("~", table_baseline_vars, "|WAVE")

formula_string_table_baseline

table1::table1(as.formula(formula_string_table_baseline),
               data = dt_new,
               overall = FALSE)


# another method for making a table
x <- table1::table1(as.formula(formula_string_table_baseline),
                    data = dt_new,
                    overall = FALSE)

# some options, see: https://cran.r-project.org/web/packages/kableExtra/vignettes/awesome_table_in_html.html
table1::t1kable(x, format = "html", booktabs = TRUE) |>
  kable_material(c("striped", "hover"))



### ### ### ### ### ### SUBGROUP DATA ANALYSIS: DATA WRANGLING  ### ### ### ###

dt_8 <- prep_reflective |>
  mutate(id = factor(1:nrow(prep_reflective))) |>
  # mutate(t1_perfectionism = round(t1_perfectionism)) |> # we create a three-level exposure to enable clear causal contrasts. We could also use a continous variable
  # mutate(
  #   t1_perfectionism_coarsen = cut(
  #     t1_perfectionism,
  #     breaks = c(1, 4, 5, 7),
  #     include.lowest = TRUE,
  #     include.highest = TRUE,
  #     na.rm = TRUE,
  #     right = FALSE
  #   ),
  #   t1_perfectionism_coarsen = factor(
#     t1_perfectionism_coarsen,
#     levels = c("[1,4)", "[4,5)", "[5,7]"),
#     labels = c("low", "medium", "high"),
#     ordered = TRUE
#   )
#) |>
mutate(
  t0_eth_cat = as.factor(t0_eth_cat),
  t0_rural_gch2018 = as.factor(t0_rural_gch2018),
  t0_gen_cohort = as.factor(t0_gen_cohort)
) |>
  dplyr::filter(t0_eth_cat == "euro" |
                  t0_eth_cat == "māori") |> # Too few asian and pacific

  group_by(id) |>
  dplyr::mutate(t2_meaning = mean(c(t2_meaning_purpose,
                                    t2_meaning_sense),
                                  na.rm = TRUE)) |>
  ungroup() |>
  # transform numeric variables into z scores (improves estimation)
  dplyr::mutate(across(where(is.numeric), ~ scale(.x), .names = "{col}_z")) %>%
  dplyr::select(-t0_rural_gch2018) |>
  # select only factors and numeric values that are z-scores
  select(id, # category is too sparse
         where(is.factor),
         t1_perfectionism, # for comparison
         ends_with("_z"), ) |>
  # tidy data frame so that the columns are ordered by time (useful for more complex models)
  relocate(id, .before = starts_with("t1_"))   |>
  relocate(starts_with("t0_"), .before = starts_with("t1_"))  |>
  relocate(starts_with("t2_"), .after = starts_with("t1_")) |>
  droplevels()



# view object
skimr::skim(dt_8)


# in a non-snythetic dataset we would inspect for missingness
# you should report missingness in real data, and describe how you will handle missing data.  we're talking about this in my lab today after class. All are invited.

naniar::vis_miss(dt_8)

# save your dataframe for future use

# make dataframe
dt_8 = as.data.frame(dt_8)

# save data
saveRDS(dt_8, here::here("data", "dt_8"))


# read -- you may start here if you need to repeat the analysis
dt_8 <- readRDS(here::here("data", "dt_8"))


## Check the ethnicity levels

# find names
levels_list <- unique(dt_8[["t0_eth_cat"]])

# we have in this dataset we have 2 levels of ethnicity.
levels_list


colnames(dt_8)



####### PROPENSITY SCORES AND WEIGHTING #####


# Next we generate propensity scores.  Instead of modelling the outcome (t2_y) we will model the exposure (t1_x) as predicted by baseline indicators (t0_c) that we assume may be associated with the outcome and the exposure.

# First step, obtain the baseline variables. note that we must remove "t0_eth_cat" because we are performing separate weighting for each stratum within this variable. here's the code:


baseline_vars_reflective_propensity = dt_8 |>
  dplyr::select(starts_with("t0"), -t0_eth_cat) |> colnames()

baseline_vars_reflective_propensity

# only for gcomp without stratification (used later)
baseline_vars_full = dt_8 |>
  dplyr::select(starts_with("t0")) |> colnames()


# define our exposure
X <- "t1_perfectionism_coarsen"

# define subclasses
S <- "t0_eth_cat"


# next we use our trick for creating a formula string, which will reduce our work
formula_str_prop <-
  paste(X,
        "~",
        paste(baseline_vars_reflective_propensity, collapse = "+"))

formula_str_prop

# I have created a function called "match_mi_general" that will perform the matching for us.
# For the purposes of our work, we will examine the "ATE" or average treatment effect.
# Additionally, this week we will only use the coarsened treatment variable

# we need our data to be a data frame, you did this before but do again :)


dt_8 <- data.frame(dt_8)



# Noah Greifer recommends trying several approaches, so let us try four matching approaches, as defined by the "method" option in the code:
# the methods we will try are: # tried cbps, ps, ebal, bart, and energy. of these energy worked best
# see: https://ngreifer.github.io/WeightIt/
# we might try cbps, ps, bart, and energy, super. Of these, energy worked best:


# to forshadow, "energy" works best for us.

dt_match <- match_mi_general(
  data = dt_8,
  X = X,
  baseline_vars = baseline_vars_reflective_propensity,
  subgroup = "t0_eth_cat",
  estimand = "ATE",
  #focal = "high", # for use with ATT
  method = "energy"
)


saveRDS(dt_match, here::here("data", "dt_match"))

dt_match <- readRDS(here::here("data", "dt_match"))


# next we inspect balance. "Max.Diff.Adj" should ideally be less than .05

bal.tab(dt_match$euro, thresholds = c(m = .05))   #  good
bal.tab(dt_match$māori, thresholds = c(m = .05))  # ok # Note we do have balance on the coarsened exposure
#bal.tab(dt_match$pacific)  # not good but won't use in contrasts
#bal.tab(test$asian)  # not good but won't use in contrasts


# this blows up :)
# dt_match_ebal <- match_mi_general(
#   data = dt_8,
#   X = X,
#   baseline_vars = baseline_vars_reflective_propensity,
#   subgroup = "t0_eth_cat",
#   estimand = "ATE",
#   method = "ebal"
#)

# not good
dt_match_ps <- match_mi_general(
  data = dt_8,
  X = X,
  baseline_vars = baseline_vars_reflective_propensity,
  subgroup = "t0_eth_cat",
  estimand = "ATE",
  method = "ps"
)

# check balance
bal.tab(dt_match_ps$euro, thresholds = c(m = .05)) # not good
bal.tab(dt_match_ps$māori, thresholds = c(m = .05)) # not good


# other options
# see "weightit" documentation
dt_match_cbps <- match_mi_general(
  data = dt_8,
  X = X,
  baseline_vars = baseline_vars_reflective_propensity,
  subgroup = "t0_eth_cat",
  estimand = "ATE",
  method = "cbps"
)

# check balance
bal.tab(dt_match_cbps$euro) # not good
bal.tab(dt_match_cbps$māori) # not good

library("SuperLearner")

dt_match_super <- match_mi_sub(
  data = dt_8,
  X = X,
  baseline_vars = baseline_vars_reflective_propensity,
  subgroup = "t0_eth_cat",
  estimand = "ATE",
  method = "super",
  super = TRUE,
  SL.library = c("SL.glm", "SL.ranger",
                 "SL.glm.interaction")
)


# check balance
bal.tab(dt_match_super$euro) #  good
bal.tab(dt_match_super$māori) # not good

# save data
saveRDS(dt_match_super, here::here("data", "dt_match_super"))

# check balance
bal.tab(dt_match$euro)
bal.tab(dt_match$māori)

# code for summar
sum_e <- summary(dt_match$euro)
sum_m <- summary(dt_match$māori)
# sum_p <- summary(dt_match$pacific)
# sum_a <- summary(dt_match$asian)
sum_e
sum_m

plot(sum_e)
plot(sum_m)
#plot(sum_p)
#plot(sum_a)

love.plot(dt_match$euro,
          binary = "std",
          thresholds = c(m = .1))

love.plot(dt_match$māori,
          binary = "std",
          thresholds = c(m = .1))

love.plot(dt_match$pacific,
          binary = "std",
          thresholds = c(m = .1))
love.plot(dt_match$asian,
          binary = "std",
          thresholds = c(m = .1))


# prepare data
dt_ref_e <- subset(dt_8, t0_eth_cat == "euro")
dt_ref_e$weights <- dt_match$euro$weights

# prepare data
dt_ref_m <- subset(dt_8, t0_eth_cat == "māori")
dt_ref_m$weights <- dt_match$māori$weights

# combine
dt_ref_all <- rbind(dt_ref_e, dt_ref_m)

# call dataframe `df`
df = dt_ref_all


# Let's calculate the ATE for the entire group, ignoring the subclasses.
# let's make the contrasts between low and high perfectionism.
baseline_vars_reflective_propensity
baseline_vars_full

#  GENERAL ATE (Not adjusting for subgroups)
mod_ref_meaning   <- gcomp_sim(
  df = df,
  # note change
  Y = "t2_meaning_z",
  X = X,
  baseline_vars = baseline_vars_full,
  treat_1 = "high",
  treat_0 = "low",
  estimand = "ATE",
  scale = "RD",
  type = "RD",
  nsims = 1000,
  cores = 8,
  family = gaussian,
  weights = TRUE,
  continuous_X = FALSE,
  splines = FALSE,
  new_name = "t2_meaning_z (composite)"
)

# ATE. we will cover "evalues" next week


### SUBGROUP analysis
df = dt_ref_all
Y = "t2_meaning_z"
X = "t1_perfectionism_coarsen"
baseline_vars = baseline_vars_reflective_propensity
treat_0 = "low"
treat_1 = "high"
estimand = "ATE"
scale = "RD"
nsims = 1000
family = "gaussian"
continuous_X = FALSE
splines = FALSE
cores = parallel::detectCores()
S = "t0_eth_cat"

# not we interact the subclass X treatment X covariates

formula_str <-
  paste(
    Y,
    "~",
    S,
    "*",
    "(",
    X ,
    "*",
    "(",
    paste(baseline_vars_reflective_propensity, collapse = "+"),
    ")",
    ")"
  )

formula_str


# fit model
fit_all_all  <- glm(
  as.formula(formula_str),
  weights = weights,
  # weights = if (!is.null(weight_var)) weight_var else NULL,
  family = family,
  data = df
)

summary(fit_all_all)

coefs <- coef(fit_all_all)
table(is.na(coefs))#     t0_eth_catmāori:t1_perfectionism_coarsen.Q:t0_gen_cohort.C

# #FALSE  TRUE
# 344     4

insight::get_varcov(fit_all_all)

# simulate coefficients
sim_model_all <- sim(fit_all_all, n = nsims, vcov = "HC1")


# simulate effect as modified in europeans
sim_estimand_all_e <- sim_ame(
  sim_model_all,
  var = X,
  cl = cores,
  subset = t0_eth_cat == "euro",
  verbose = FALSE
)

sim_estimand_all_e <-
  transform(sim_estimand_all_e, RD = `E[Y(low)]` - `E[Y(high)]`)
sim_estimand_all_e


# simulate effect as modified in māori
sim_estimand_all_m <- sim_ame(
  sim_model_all,
  var = X,
  cl = cores,
  subset = t0_eth_cat == "māori",
  verbose = FALSE
)

# combine
sim_estimand_all_m <-
  transform(sim_estimand_all_m, RD = `E[Y(low)]` - `E[Y(high)]`)


# summary
summary(sim_estimand_all_e)
summary(sim_estimand_all_m)

# rearrange
names(sim_estimand_all_e) <-
  paste(names(sim_estimand_all_e), "e", sep = "_")

names(sim_estimand_all_m) <-
  paste(names(sim_estimand_all_m), "m", sep = "_")


est_all <- cbind(sim_estimand_all_m, sim_estimand_all_e)
est_all <- transform(est_all, `RD_m - RD_e` = RD_m - RD_e)


# view summary
summary(est_all)




###### ###### ###### TRY ONLY G-COMPUTATION (leaving out Propensity Scores) ###### ######

# check formula:
formula_str

# fit model
fit_all_r <- glm(as.formula(formula_str),
                 #  weights = weights,  # remove weights
                 family = family,
                 data = df)

# simulate coefficients
sim_model_r <- sim(fit_all_r, n = nsims, vcov = "HC1")


# simulate effect as modified in europeans
sim_estimand_r_e <- sim_ame(
  sim_model_r,
  var = X,
  cl = cores,
  subset = t0_eth_cat == "euro",
  verbose = FALSE
)

# wrangle
sim_estimand_r_e <-
  transform(sim_estimand_r_e, RD = `E[Y(low)]` - `E[Y(high)]`)
sim_estimand_r_e


# simulate effect as modified in māori
sim_estimand_r_m <- sim_ame(
  sim_model_r,
  var = X,
  cl = cores,
  subset = t0_eth_cat == "māori",
  verbose = FALSE
)

# wrangle
sim_estimand_r_m <-
  transform(sim_estimand_r_m, RD = `E[Y(low)]` - `E[Y(high)]`)


# view
summary(sim_estimand_r_e)
summary(sim_estimand_r_m)

# rearrange
names(sim_estimand_r_e) <-
  paste(names(sim_estimand_r_e), "e", sep = "_")

names(sim_estimand_r_m) <-
  paste(names(sim_estimand_r_m), "m", sep = "_")


est_r <- cbind(sim_estimand_r_e, sim_estimand_r_m)
est_r <- transform(est_r, `RD_m - RD_e` = RD_m - RD_e)

# doubly robust
summary(est_all)

# g-computation
summary(est_r)


# only propensity score (no regression stratification)

# fit model
fit_all_p <- glm(
  t2_meaning_z  ~ t1_perfectionism_coarsen * t0_eth_cat,
  ## note we do not have covariates -- don't need them because we have weighted by the covariates on the exposure to acheive balance.
  weights = weights,
  family = family,
  data = df
)

# simulate coefficients
sim_model_p <- sim(fit_all_p, n = nsims, vcov = "HC1")


# simulate effect as modified in europeans
sim_estimand_p_e <- sim_ame(
  sim_model_p,
  var = X,
  cl = cores,
  subset = t0_eth_cat == "euro",
  verbose = FALSE
)

sim_estimand_p_e <-
  transform(sim_estimand_p_e, RD = `E[Y(low)]` - `E[Y(high)]`)

# simulate effect as modified in māori
sim_estimand_p_m <- sim_ame(
  sim_model_p,
  var = X,
  cl = cores,
  subset = t0_eth_cat == "māori",
  verbose = FALSE
)

# wrangle
sim_estimand_p_m <-
  transform(sim_estimand_p_m, RD = `E[Y(low)]` - `E[Y(high)]`)


# wrangle

# wrangle
names(sim_estimand_p_e) <-
  paste(names(sim_estimand_p_e), "e", sep = "_")

names(sim_estimand_p_m) <-
  paste(names(sim_estimand_p_m), "m", sep = "_")


est_p <- cbind(sim_estimand_p_e, sim_estimand_p_m)
est_p <- transform(est_p, `RD_m - RD_e` = RD_m - RD_e)


# summary of all three approaches are similar
summary(est_all)
summary(est_r)
summary(est_p)




### ### ### ### ### CONTINUOUS EXPOSURE   ### ### ### ### ### ### ### ###

X_cont <- "t1_perfectionism_z"


# Only use Engergy balancing for a continuous exposure
dt_match_cont <- match_mi_general(
  data = dt_8,
  X = X_cont,
  baseline_vars = baseline_vars_reflective_propensity,
  subgroup = "t0_eth_cat",
  estimand = "ATE",
  #focal = "high", # for use with ATT
  method = "energy"
)

saveRDS(dt_match_cont, here::here("data", "dt_match_cont"))
dt_match_cont <- readRDS(here::here("data", "dt_match_cont"))


bal.tab(dt_match_cont$euro, thresholds = c(m = .05))   #  good
bal.tab(dt_match_cont$māori, thresholds = c(m = .05))  # ok


# prepare data
dt_ref_e_cont <- subset(dt_8, t0_eth_cat == "euro")
dt_ref_e_cont$weights <- dt_match_cont$euro$weights

# prepare data
dt_ref_m_cont <- subset(dt_8, t0_eth_cat == "māori")
dt_ref_m_cont$weights <- dt_match_cont$māori$weights


# combine
dt_ref_all_cont <- rbind(dt_ref_e_cont, dt_ref_m_cont)

df = dt_ref_all_cont


# Let's calculate the ATE for the entire group, ignoring the subclasses.
# let's make the contrasts between low and high perfectionism.

mod_ref_meaning_cont   <- gcomp_sim(
  df = dt_ref_all_cont,
  # note change
  Y = "t2_meaning_z",
  X = X_cont,
  baseline_vars = baseline_vars_full,
  treat_1 = "high",
  treat_0 = "low",
  estimand = "ATE",
  scale = "RD",
  type = "RD",
  nsims = 1000,
  cores = 8,
  family = gaussian,
  weights = TRUE,
  continuous_X = TRUE,
  splines = FALSE,
  new_name = "t2_meaning_z (composite)"
)


mod_ref_meaning_cont

formula_str_cont <-
  paste(
    Y,
    "~",
    S,
    "*",
    "(",
    X_cont ,
    "*",
    "(",
    paste(baseline_vars_reflective_propensity, collapse = "+"),
    ")",
    ")"
  )


formula_str_cont


# fit model
fit_all_r_cont <- glm(as.formula(formula_str_cont),
                      #  weights = weights,  # remove weights
                      family = family,
                      data = dt_ref_all_cont)

# simulate coefficients
sim_model_r_cont <- sim(fit_all_r_cont, n = nsims, vcov = "HC3")


# simulate effect as modified in europeans
sim_estimand_r_e_cont <- sim_ame(
  sim_model_r_cont,
  var = X_cont,
  cl = cores,
  subset = t0_eth_cat == "euro",
  verbose = FALSE
)

summary(sim_estimand_r_e_cont)



# simulate effect as modified in māori
sim_estimand_r_m_cont <- sim_ame(
  sim_model_r_cont,
  var = X_cont,
  cl = cores,
  subset = t0_eth_cat == "māori",
  verbose = FALSE
)
sim_estimand_r_m_cont <- transform(sim_estimand_r_m_cont)
summary(sim_estimand_r_m_cont)



names(sim_estimand_r_e_cont) <-
  paste(names(sim_estimand_r_e_cont), "e", sep = "_")

names(sim_estimand_r_m_cont) <-
  paste(names(sim_estimand_r_m_cont), "m", sep = "_")

summary(sim_estimand_r_e_cont)

summary(sim_estimand_r_m_cont)
summary(est_r_cont)
sim_estimand_r_e_cont
names(sim_estimand_r_e_cont) <- "estimate_e"
names(sim_estimand_r_m_cont) <- "estimate_m"
est_r_cont <-
  cbind(sim_estimand_r_e_cont, sim_estimand_r_m_cont)
est_r_cont <-
  transform(est_r_cont, `estimate_m - estimate_e` = estimate_m - estimate_e)

# no difference
summary(est_r_cont)



# What do we make of this?
# the estimand is different here. We are considering people who are moving from average perfectionsim to +1 standard deviation higher.

# i.e. from
mean(dt_ref_all_cont$t1_perfectionism) #[1] 3.020397

# to

sd(dt_ref_all_cont$t1_perfectionism) + mean(dt_ref_all_cont$t1_perfectionism) #4.426781

# so this is in the range of low to medium in our coarsen variable.  Note that in this range of the data, the causal effects are similar for maori and nz europeans.



#################### THE FOLLOWING IS JUST FOR INTEREST. IT IS A MULTI-LEVEL MODEL, WHICH WOULD BE STANDARD IN LONGITUDINAL PSYCHOLOGY.  HOWEVER IT IS UNCLEAR WHAT WE LEARN FROM IT #################### #################### ###############

#####. multi-level model
##### This is how we would generally model "change over time"


dt_ml <- nzavs_synth |>
  mutate(time = as.numeric(wave) - 1) |>
  group_by(id, wave) |>
  dplyr::mutate(meaning = mean(c(meaning_purpose,
                                 meaning_sense),
                               na.rm = TRUE)) |>
  ungroup() |>
  # transform numeric variables into z scores (improves estimation)
  dplyr::mutate(across(where(is.numeric), ~ scale(.x), .names = "{col}_z")) %>%
  # select only factors and numeric values that are z-scores
  select(id,
         where(is.factor),
         perfectionism, # for comparison
         time,
         ends_with("_z")) |>
  data.frame()

baseline_vars_ml = c(
  "eth_cat",
  "edu_z",
  "male",
  "employed_z",
  "gen_cohort",
  "nz_dep2018_z",
  "nzsei13_z",
  "partner_z",
  "parent_z",
  "pol_orient_z",
  "rural_gch2018",
  "agreeableness_z",
  "conscientiousness_z",
  "extraversion_z",
  "honesty_humility_z",
  "openness_z",
  "neuroticism_z",
  "modesty_z",
  "religion_identification_level_z"
)

Y_ml = "meaning_z"
X_ml = "perfectionism_z"

formula_str_ml <-
  paste(
    Y_ml,
    "~",
    "time",
    "*",
    "(",
    X_ml ,
    "*",
    "(",
    paste(baseline_vars_ml, collapse = "+"),
    ")",
    "+",
    "(1|id)",
    ")"
  )

formula_str_ml

library(lme4)
as.formula(formula_str_ml)

model_ml <- lmer(as.formula(formula_str_ml), data = dt_ml)


tab_ml <-
  parameters::model_parameters(model_ml, effects = "fixed")
tab_ml
plot(tab_ml)


library(ggeffects)


graph_ml <-
  plot(ggeffects::ggpredict(model_ml, terms = c("time", "perfectionism_z", "eth_cat")))


# note we see regression to the mean.  this is common. but we do not have causal effects.
graph_ml


######################################## ################### ########################################
######################################## STUDENT PROBLEM SET ########################################
######################################## ################### ########################################


# Your task: model the (conditional) ATE of hours_exercise on meaning of life, as these effects are modified by euro or māori ethnicity.
# Find a sensible cut points for hours_exercise and model the causal effect of moving from low exercise to active on life meaning

















######################################## ################### ########################################
########################################       SOLUTION     ########################################
######################################## ################### ########################################



# find meaningful cut points
quantile(round(nzavs_synth$hours_exercise, 1))

#.0-2 = low,
# 2-7 = some
# => 7 = active


# prepare data
dt_prep <- nzavs_synth |>
  mutate(hours_exercise = round(hours_exercise)) |> # we create a three-level exposure to enable clear causal contrasts.
  mutate(
    hours_exercise_coarsen = cut(
      hours_exercise,
      breaks = c(0, 2, 7, 200),
      include.lowest = TRUE,
      include.highest = TRUE,
      na.rm = TRUE,
      right = FALSE
    ),
    hours_exercise_coarsen = factor(
      hours_exercise_coarsen,
      levels = c("[0,2)", "[2,7)", "[7,200]"),
      labels = c("low", "medium", "high"),
      ordered = TRUE
    )
  )




# we only inspect change between the baseline condition and the exposure year
dt_exposure_check <- dt_prep |>
  filter(wave == "2018" | wave == "2019") |>
  select(id, wave, hours_exercise, hours_exercise_coarsen, eth_cat) |> # the categorical variable needs to be numeric for us to use msm package to investigate change
  mutate(hours_exercise_coarsen_n = as.numeric(hours_exercise_coarsen))


# next we check for change in the exposure
msm::statetable.msm(round(hours_exercise_coarsen_n, 0), id, data = dt_exposure_check) |>
  kbl() |>
  kable_paper(full_width = F)



# lets break this down by ethnicity
dt_exposure_check_e <-
  dt_exposure_check |> filter(eth_cat == "euro")
dt_exposure_check_m <-
  dt_exposure_check |> filter(eth_cat == "māori")

# euro coarsen
msm::statetable.msm(round(hours_exercise_coarsen_n, 0), id, data = dt_exposure_check_e) |>
  kbl() |>
  kable_paper(full_width = F)

# maori coarsen
msm::statetable.msm(round(hours_exercise_coarsen_n, 0), id, data = dt_exposure_check_m) |>
  kbl() |>
  kable_paper(full_width = F)


# data wrangling

# step 1: choose baseline variables.  here we select standard demographic variablees plus personality variables.

baseline_vars = c(
  "edu",
  "male",
  "eth_cat",
  "employed",
  "gen_cohort",
  "nz_dep2018",
  "nzsei13",
  "partner",
  "parent",
  "pol_orient",
  "rural_gch2018",
  "agreeableness",
  "conscientiousness",
  "extraversion",
  "honesty_humility",
  "openness",
  "neuroticism",
  "modesty",
  "religion_identification_level"
)


## Step 2, select the exposure variable.  This is the "cause"

exposure_var = c("hours_exercise_coarsen")


## step 3. select the outcome variable.  These are the outcomes.
outcome_vars = c("hlth_fatigue")


# the function "create_wide_data" should be in your environment.

#If not, make sure to run the first line of code in this script once more.  You may ignore the warnings. or uncomment and run the code below
# source("https://raw.githubusercontent.com/go-bayes/templates/main/functions/funs.R")

prep_execercise <-
  create_wide_data(
    dat_long = dt_prep,
    #nzavs_synth,
    baseline_vars = baseline_vars,
    exposure_var = exposure_var,
    outcome_vars = outcome_vars
  )

prep_execercise


# I have created a function that will allow you to take a data frame and
# create a table
baseline_table(prep_execercise, output_format = "markdown")

colnames(prep_execercise)
### ### ### ### ### ### SUBGROUP DATA ANALYSIS: DATA WRANGLING  ### ### ### ###

dt_x <- prep_execercise |>
  mutate(id = factor(1:nrow(prep_execercise))) |>
  mutate(
    t0_eth_cat = as.factor(t0_eth_cat),
    t0_rural_gch2018 = as.factor(t0_rural_gch2018),
    t0_gen_cohort = as.factor(t0_gen_cohort)
  ) |>
  dplyr::filter(t0_eth_cat == "euro" |
                  t0_eth_cat == "māori") |> # Too few asian and pacific
  mutate(t0_urban = factor(
    ifelse(
      t0_rural_gch2018 == "medium_urban_accessibility" |
        t0_rural_gch2018 == "high_urban_accessibility",
      "urban",
      "rural"
    )
  )) |>
  # group_by(id) |>
  # dplyr::mutate(t2_meaning = mean(c(t2_meaning_purpose,
  #                                   t2_meaning_sense),
  #                                 na.rm = TRUE)) |>
  # ungroup() |>
  # transform numeric variables into z scores (improves estimation)
  dplyr::mutate(across(where(is.numeric), ~ scale(.x), .names = "{col}_z")) %>%
  dplyr::select(-t0_rural_gch2018) |>
  # select only factors and numeric values that are z-scores
  select(id, # category is too sparse
         where(is.factor), # for comparison
         ends_with("_z")) |>
  # tidy data frame so that the columns are ordered by time (useful for more complex models)
  relocate(id, .before = starts_with("t1_"))   |>
  relocate(starts_with("t0_"), .before = starts_with("t1_"))  |>
  relocate(starts_with("t2_"), .after = starts_with("t1_")) |>
  droplevels()





# view object
skimr::skim(dt_x)


# in a non-snythetic dataset we would inspect for missingness
# you should report missingness in real data, and describe how you will handle missing data.  we're talking about this in my lab today after class. All are invited.

naniar::vis_miss(dt_x)

# save your dataframe for future use

# make dataframe
dt_x = as.data.frame(dt_x)

# save data
saveRDS(dt_x, here::here("data", "dt_x"))


# read -- you may start here if you need to repeat the analysis
dt_x <- readRDS(here::here("data", "dt_x"))


## Check the ethnicity levels

# find names
levels_list <- unique(dt_x[["t0_eth_cat"]])

# we have in this dataset we have 2 levels of ethnicity.
levels_list



####### PROPENSITY SCORES AND WEIGHTING #####


# Next we generate propensity scores.  Instead of modelling the outcome (t2_y) we will model the exposure (t1_x) as predicted by baseline indicators (t0_c) that we assume may be associated with the outcome and the exposure.

# first step, obtain the baseline variables. note that we must remove "t0_eth_cat" because we are performing separate weighting for each stratum within this variable. here's the code:


baseline_vars_reflective_propensity_x = dt_x |>
  dplyr::select(starts_with("t0"), -t0_eth_cat) |> colnames()

baseline_vars_reflective_propensity_x

# define our exposure
X <- "t1_hours_exercise_coarsen"

# define subclasses
S <- "t0_eth_cat"

# next we use our trick for creating a formula string, which will reduce our work
formula_str_prop_x <-
  paste(X,
        "~",
        paste(baseline_vars_reflective_propensity_x, collapse = "+"))

formula_str_prop_x

# I have created a function called "match_mi_general" that will perform the matching for us.
# For the purposes of our work, we will examine the "ATE" or average treatment effect.
# Additionally, this week we will only use the coarsened treatment variable


baseline_vars_reflective_propensity_x
# to forshadow, "energy" works best for us.

dt_match_x <- match_mi_general(
  data = dt_x,
  X = X,
  baseline_vars = baseline_vars_reflective_propensity_x,
  subgroup = "t0_eth_cat",
  estimand = "ATE",
  #focal = "high", # for use with ATT
  method = "energy"
)

saveRDS(dt_match_x, here::here("data", "dt_match_x"))


# next we inspect balance. "Max.Diff.Adj" should ideally be less than .05

bal.tab(dt_match_x$euro, thresholds = c(m = .05))   #  good
bal.tab(dt_match_x$māori, thresholds = c(m = .05))  # good


# match
dt_match_ebal_x <- match_mi_general(
  data = dt_x,
  X = X,
  baseline_vars = baseline_vars_reflective_propensity_x,
  subgroup = "t0_eth_cat",
  estimand = "ATE",
  method = "ebal"
)

bal.tab(dt_match_ebal_x$euro, thresholds = c(m = .05))   #  good
bal.tab(dt_match_ebal_x$māori, thresholds = c(m = .05))  # good


# not good
dt_match_ps_x <- match_mi_general(
  data = dt_x,
  X = X,
  baseline_vars = baseline_vars_reflective_propensity_x,
  subgroup = "t0_eth_cat",
  estimand = "ATE",
  method = "ps"
)

bal.tab(dt_match_ps_x$euro, thresholds = c(m = .05)) # not good
bal.tab(dt_match_ps_x$māori, thresholds = c(m = .05)) # not good



# compare first energy
sum_e_x <- summary(dt_match_x$euro)
sum_m_x <- summary(dt_match_x$māori)

plot(sum_e_x)
plot(sum_m_x)


love.plot(dt_match_x$euro,
          binary = "std",
          thresholds = c(m = .1))
love.plot(dt_match_x$māori,
          binary = "std",
          thresholds = c(m = .1))


sum_e_x <- summary(dt_match_x$euro)
sum_m_x <- summary(dt_match_x$māori)

plot(sum_e_x)
plot(sum_m_x)



# next look at ebal -- which looks a little better

sum_e_bal_x <- summary(dt_match_ebal_x$euro)
sum_m_bal_x <- summary(dt_match_ebal_x$māori)
plot(sum_e_bal_x)
plot(sum_m_bal_x)


love.plot(dt_match_ebal_x$euro,
          binary = "std",
          thresholds = c(m = .1))
love.plot(dt_match_ebal_x$māori,
          binary = "std",
          thresholds = c(m = .1))



# prepare data
dt_ref_e_x <- subset(dt_x, t0_eth_cat == "euro")
dt_ref_e_x$weights <- dt_match_ebal_x$euro$weights

# prepare data
dt_ref_m_x <- subset(dt_x, t0_eth_cat == "māori")
dt_ref_m_x$weights <- dt_match_ebal_x$māori$weights

# combine
dt_xx  <- rbind(dt_ref_e_x, dt_ref_m_x)


### subgroup analysis
df = dt_xx
df$weights


Y = "t2_hlth_fatigue_z"
X = "t1_hours_exercise_coarsen"
baseline_vars = baseline_vars_reflective_propensity_x
treat_0 = "low"
treat_1 = "high"
estimand = "ATE"
scale = "RD"
nsims = 1000
family = "gaussian"
continuous_X = FALSE
splines = FALSE
cores = parallel::detectCores()
S = "t0_eth_cat"

# not we interact the subclass X treatment X covariates

formula_str <-
  paste(
    Y,
    "~",
    S,
    "*",
    "(",
    X ,
    "*",
    "(",
    paste(baseline_vars_reflective_propensity_x, collapse = "+"),
    ")",
    ")"
  )

formula_str


# fit model
fit_all_all_x  <-
  glm(# t2_hlth_fatigue_z ~ t0_eth_cat *  t1_hours_exercise_coarsen,
    formula_str,
    weights = weights,
    # weights = if (!is.null(weight_var)) weight_var else NULL,
    family = family,
    data = dt_xx)

# this tells us little
summary(fit_all_all_x)

# check that all parameters have been estimated.
coefs <- coef(fit_all_all_x)
table(is.na(coefs))#


# simulate coefficients
sim_model_all_x <- sim(fit_all_all_x, n = nsims, vcov = "HC3")


# simulate effect as modified in europeans
sim_estimand_all_e <- sim_ame(
  sim_model_all_x,
  var = X,
  cl = cores,
  subset = t0_eth_cat == "euro",
  verbose = FALSE
)

# wrangle
sim_estimand_all_e <-
  transform(sim_estimand_all_e, RD = `E[Y(low)]` - `E[Y(high)]`)
sim_estimand_all_e


# simulate effect as modified in māori
sim_estimand_all_m <- sim_ame(
  sim_model_all_x,
  var = X,
  cl = cores,
  subset = t0_eth_cat == "māori",
  verbose = FALSE
)

# wrangle
sim_estimand_all_m <-
  transform(sim_estimand_all_m, RD = `E[Y(low)]` - `E[Y(high)]`)

# wrangle

names(sim_estimand_all_e) <-
  paste(names(sim_estimand_all_e), "e", sep = "_")

names(sim_estimand_all_m) <-
  paste(names(sim_estimand_all_m), "m", sep = "_")


est_all <- cbind(sim_estimand_all_m, sim_estimand_all_e)
est_all <- transform(est_all, `RD_m - RD_e` = RD_m - RD_e)


# summary
summary(est_all)
