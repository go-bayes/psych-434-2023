# PSYCH 434: Example script for assessment 3 and 5.
# May 2023
# questions: joseph.bulbulia@vuw.ac.nz
# Running this command will download the functions and packages you need to complete this worksheet.
# You many find the code by pointing your browser to the webpage that is contained in the link

# QEUSTION 1. DOES EXCERCISE AFFECT DEPRESSION AMONG MAORI AND NZEUROPEANS? DO THESE EFFECTS DIFFER AMONG MAORI AND NZEUROPEANS?

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


# create sum score of kessler 6
dt_start <- nzavs_synth %>%
  arrange(id, wave) %>%
  rowwise() %>%
  mutate(kessler_6  = mean(
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
    ))) |>
  mutate(kessler_6_sum = round(
    sum(c (kessler_depressed,
                   kessler_hopeless,
                   kessler_nervous,
                   kessler_effort,
                   kessler_restless,
                   kessler_worthless)),
    digits = 0
  )) |>  ungroup() |>
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
  # Transform 'hours_exercise' by applying the log function to compress its scale
  mutate(hours_exercise_log = log(hours_exercise + 1)) |> # Add 1 to avoid undefined log(0). Hours spent exercising/physical activity

  # Coarsen 'hours_exercise' into categories
  mutate(
    hours_exercise_coarsen = cut(
      hours_exercise,
      # Hours spent exercising/ physical activity
      breaks = c(-1, 3, 8, 200),
      labels = c(
        "inactive",
        "active",
        "very_active"      ),
      # Define thresholds for categories
      levels = c("(-1,2]", "(2,8]", "(8,200]"),
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
efa <- psych::fa(dt_only_k6, nfactors = 3) %>%
  model_parameters(sort = TRUE, threshold = "max")

efa

# This output presents the results of an exploratory factor analysis (EFA), a statistical method used to discover the underlying structure of a relatively large set of variables. It's often used when you don't have a specific hypothesis about what latent factors (unobservable variables) might be influencing the observed variables in your dataset.
#
# In this analysis, we've requested three factors (latent variables), and the table presents the loadings of each observed variable on each of these factors. The loadings can be interpreted as the correlations between the observed variables and the latent factors.
#
# Here's how to interpret the output:
#
#   The variables kessler_depressed, kessler_worthless, and kessler_hopeless load strongly on the first latent factor (MR1), and do not significantly load on the other two. This suggests that these three variables share some common underlying factor.
#
# The variable kessler_nervous loads exclusively on the second latent factor (MR2), suggesting it might represent a different latent construct.
#
# The variables kessler_restless and kessler_effort load on the third latent factor (MR3), which could represent yet another underlying construct.
#
# The "Complexity" column indicates the complexity of each item. Complexity 1 indicates that the item is influenced mostly by a single factor.
#
# The "Uniqueness" column represents the proportion of variance in each variable that is not explained by the factors. For example, the uniqueness of kessler_depressed is 0.33, which means that 33% of the variance in this variable is not accounted for by the three factors.
#
# Lastly, the total variance explained by the three latent factors is 66.05%, with MR1 explaining 35.14%, MR2 explaining 17.17%, and MR3 explaining 13.73%. This indicates that about two-thirds of the variance in the six observed variables can be explained by the three latent factors extracted in the analysis.


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


# one factor
structure_k6_one <- psych::fa(dt_only_k6, nfactors = 1) |>
  efa_to_cfa()

# two factor model
structure_k6_two <- psych::fa(dt_only_k6, nfactors = 2) |>
  efa_to_cfa()

# three structure model
structure_k6_three <- psych::fa(dt_only_k6, nfactors = 3) %>%
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
# This table provides the results of three different Confirmatory Factor Analysis (CFA) models: one that specifies a single latent factor, one that specifies two latent factors, and one that specifies three latent factors. The results include a number of goodness-of-fit statistics, which can be used to assess how well each model fits the data.
#
# One_latent Model: This model assumes that there is only one underlying latent factor contributing to all variables. This model has a chi-square statistic of 1359.7 with 14 degrees of freedom, which is highly significant (p<0.001), indicating a poor fit of the model to the data. Other goodness-of-fit indices like GFI, AGFI, NFI, NNFI, and CFI are all high (above 0.9), generally indicating good fit, but these indices can be misleading in the presence of large sample sizes. RMSEA is above 0.1 which indicates a poor fit. The SRMR is less than 0.08 which suggests a good fit, but given the high Chi-square and RMSEA values, we can't solely rely on this index. The Akaike information criterion (AIC), Bayesian information criterion (BIC) and adjusted BIC are used for comparing models, with lower values indicating better fit.
#
# Two_latents Model: This model assumes that there are two underlying latent factors. The chi-square statistic is lower than the one-factor model (317.97 with 13 df), suggesting a better fit. The p-value is still less than 0.05, indicating a statistically significant chi-square, which typically suggests a poor fit. However, all other fit indices (GFI, AGFI, NFI, NNFI, and CFI) are above 0.9 and the RMSEA is 0.051, which generally indicate good fit. The SRMR is also less than 0.08 which suggests a good fit. This model has the lowest AIC and BIC values among the three models, indicating the best fit according to these criteria.
#
# Three_latents Model: This model assumes three underlying latent factors. The chi-square statistic is 747.87 with 12 df, higher than the two-factor model, suggesting a worse fit to the data. Other fit indices such as GFI, AGFI, NFI, NNFI, and CFI are below 0.97 and the RMSEA is 0.083, which generally indicate acceptable but not excellent fit. The SRMR is less than 0.08 which suggests a good fit. The AIC and BIC values are higher than the two-factor model but lower than the one-factor model, indicating a fit that is better than the one-factor model but worse than the two-factor model.
#
# In summary, based on these results, the two-latents model seems to provide the best fit to the data among the three models, according to most of the fit indices and the AIC and BIC. However, all models have significant chi-square statistics, which suggests some degree of misfit. It's also important to consider the substantive interpretation of the factors, to make sure the model makes sense theoretically. You could also consider more complex models, if appropriate.
# **** We should not think of KESSLER 6 as one thing ****

# So let's create new variables, anxiety and depression

# Reminder of the factor structure
structure_k6_two


# get two factors
dt_start2 <- dt_start |>
  arrange(id, wave) |>
  rowwise() |>
  mutate(
    kessler_latent_depression = mean(c(kessler_depressed, kessler_hopeless, kessler_effort), na.rm = TRUE),
    kessler_latent_anxiety  = mean(c(kessler_effort, kessler_nervous, kessler_restless), na.rm = TRUE)
  ) |> ungroup()



# You are using the dplyr pipeline (%>%) to first arrange the dt_start data frame by the id and wave columns. Then, for each row (rowwise()), you calculate two new variables:
#
# kessler_latent_depression, which is the mean of kessler_depressed, kessler_hopeless, and kessler_effort, and
# kessler_latent_anxiety, which is the mean of kessler_effort, kessler_nervous, and kessler_restless.
# The na.rm = TRUE argument ensures that NA values are ignored in the computation of the mean. Finally, you are using ungroup() to remove the rowwise grouping.
#
# This will result in a data frame dt_start2 with two new columns that represent the mean of the specified Kessler scale items for each row in the data frame. The assumption here is that these means capture some latent (unobservable) characteristics, namely depression and anxiety.
#
# Note: creating these latent variables by simply taking the mean of the observed variables is a bit of a simplification. It is only acceptable for a quick exploratory analysis, but in a full analysis, you'd want to confirm these latent factors statistically, for instance using factor analysis or structural equation modeling, as you did previously. Also, ensure that these constructs are theoretically meaningful and in line with previous research.

hist(dt_start2$kessler_latent_anxiety)

hist(dt_start2$kessler_latent_depression)



#
# table( dt_start_2$kessler_latent_anxiety == dt_start2$kessler_latent_anxiety )
#


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
  mutate(hours_exercise_coarsen_n = as.numeric(hours_exercise_coarsen)) |>
  droplevels()


# check
dt_exposure |>
  tabyl(hours_exercise_coarsen_n, eth_cat,  wave )

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
out_m <- msm::statetable.msm(round(hours_exercise_coarsen_n, 0), id, data = dt_exposure_maori)

# with this little support we might consider parametric models
t_tab_m<- transition_table( out_m, state_names)

#interpretation
cat(t_tab_m$explanation)
print(t_tab_m$table)



out <- data.frame(out)

# european
dt_exposure_euro <- dt_exposure |>
  filter(eth_cat == "euro")

# lower support
out_e <- msm::statetable.msm(round(hours_exercise_coarsen_n, 0), id, data = dt_exposure_euro)



t_tab_e <- transition_table( out_e, state_names)

#interpretation
cat(t_tab_e$explanation)

# table
print(t_tab_e$table)

# |                     | Inactive | Somewhat Active | Active | Extremely Active |
# |---------------------|----------|-----------------|--------|------------------|
# | **Inactive**        | 415      | 187             | 403    | 105              |
# | **Somewhat Active** | 187      | 213             | 471    | 103              |
# | **Active**          | 293      | 424             | 2837   | 939              |
# | **Extremely Active**| 66       | 67              | 753    | 1178             |


# This transition matrix describes the shifts in physical activity levels from one state to another for the European ethnic subgroup between the baseline wave and the following wave. The numbers in the cells represent the number of individuals who transitioned from one state (rows) to another (columns). For example, 415 individuals remained inactive from the baseline wave to the following wave, while 187 individuals transitioned from being inactive to somewhat active.



############## ############## ############## ############## ############## ############## ############## ########
####  ####  ####  CREATE DATA FRAME FOR ANALYSIS ####  ####  ################## ############## ######## #########
############## ############## ############## ############## ############## ############## ############# #########

# To find out more about our dataset go here:
# https://github.com/go-bayes/psych-434-2023/blob/main/data/readme.qmd



# recall from the previous lecture that confounders are variables that may be associated with both an exposure and an outcome.


# I have created a function that will put the data into the correct shape. Here are the steps.

# Step 1: choose baseline variables.  here we select standard demographic variablees plus personality variables.


colnames(dt_start)

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
 # "rural_gch2018",
   "urban",
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
outcome_vars_reflective = c("kessler_latent_anxiety",
                            "kessler_latent_depression")



# the function "create_wide_data" should be in your environment.
# If not, make sure to run the first line of code in this script once more.  You may ignore the warnings. or uncomment and run the code below
# source("https://raw.githubusercontent.com/go-bayes/templates/main/functions/funs.R")
dt_prepare <-
  create_wide_data(
    dat_long = dt_start2,
    baseline_vars = baseline_vars,
    exposure_var = exposure_var,
    outcome_vars = outcome_vars_reflective
  )



# I have created a function that will allow you to take a data frame and
# create a table
baseline_table(dt_prepare, output_format = "markdown")


# if you just want a nice html table, do this:
library(table1) # should be in your environment

# get data into shape
dt_new <- dt_prepare %>%
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

dt <- dt_prepare|>
  mutate(id = factor(1:nrow(prep_reflective))) |>
  mutate(
  t0_eth_cat = as.factor(t0_eth_cat),
  t0_urban = as.factor(t0_urban),
  t0_gen_cohort = as.factor(t0_gen_cohort)
) |>
  dplyr::filter(t0_eth_cat == "euro" |
                t0_eth_cat == "māori") |> # Too few asian and pacific
  ungroup() |>
  # transform numeric variables into z scores (improves estimation)
  dplyr::mutate(across(where(is.numeric), ~ scale(.x), .names = "{col}_z")) %>%
  # select only factors and numeric values that are z-scores
  select(id, # category is too sparse
         where(is.factor),
         ends_with("_z"), ) |>
  # tidy data frame so that the columns are ordered by time (useful for more complex models)
  relocate(id, .before = starts_with("t1_"))   |>
  relocate(starts_with("t0_"), .before = starts_with("t1_"))  |>
  relocate(starts_with("t2_"), .after = starts_with("t1_")) |>
  droplevels()



# view object
skimr::skim(dt)

# quick cross table
table( dt$t1_hours_exercise_coarsen, dt$t0_eth_cat )


# checks
hist(dt$t2_kessler_latent_depression_z)
hist(dt$t2_kessler_latent_anxiety_z)




dt |>
  tabyl(t0_eth_cat, t1_hours_exercise_coarsen ) |>
  kbl(format = "markdown")


# in a non-snythetic dataset we would inspect for missingness
# you should report missingness in real data, and describe how you will handle missing data.  we're talking about this in my lab today after class. All are invited.

naniar::vis_miss(dt)

# save your dataframe for future use

# make dataframe
dt = as.data.frame(dt)

# save data
saveRDS(dt, here::here("data", "dt"))


# read -- you may start here if you need to repeat the analysis
dt <- readRDS(here::here("data", "dt"))


# useful
levels_list <- unique(dt_8[["t0_eth_cat"]])


# Here ends the data wrangling --------------------------------------------







# propensity scores -------------------------------------------------------



# Next we generate propensity scores.  Instead of modelling the outcome (t2_y) we will model the exposure (t1_x) as predicted by baseline indicators (t0_c) that we assume may be associated with the outcome and the exposure.

# First step, obtain the baseline variables. note that we must remove "t0_eth_cat" because we are performing separate weighting for each stratum within this variable. here's the code:


baseline_vars_reflective_propensity = dt|>
  dplyr::select(starts_with("t0"), -t0_eth_cat) |> colnames()

# check
baseline_vars_reflective_propensity


# define our exposure
X <- "t1_hours_exercise_coarsen"

# define subclasses
S <- "t0_eth_cat"


# next we use our trick for creating a formula string, which will reduce our work
formula_str_prop <-
  paste(X,
        "~",
        paste(baseline_vars_reflective_propensity, collapse = "+"))

# this shows the exposure variable as predicted by the baseline confounders.
formula_str_prop


# Make sure data is in a data frame format
dt <- data.frame(dt)


colnames()

# Noah Greifer recommends trying several approaches, so let us try thre matching approaches, as defined by the "method" option in the code:
# the methods we will try are: # tried cbps, ps, ebal, bart, and energy. of these energy worked best
# see: https://ngreifer.github.io/WeightIt/
# we might try cbps, ps, bart, and energy, super. Of these, energy worked best:


# energy often works well for matching

dt_match_anxiety <- match_mi_general(
  data = dt,
  X = X,
  baseline_vars = baseline_vars_reflective_propensity,
  subgroup = "t0_eth_cat",
  estimand = "ATE",
  #focal = "high", # for use with ATT
  method = "energy"
)


saveRDS(dt_match_energy, here::here("data", "dt_match_energy"))

dt_match_energy <- readRDS(here::here("data", "dt_match_energy"))


# next we inspect balance. "Max.Diff.Adj" should ideally be less than .05

# very. good
bal.tab(dt_match_energy$euro)   #  good

#thresholds = c(m = .05)
bal.tab(dt_match_energy$māori)  # less good


dt_match_ebal <- match_mi_general(
  data = dt,
  X = X,
  baseline_vars = baseline_vars_reflective_propensity,
  subgroup = "t0_eth_cat",
  estimand = "ATE",
  method = "ebal"
)


bal.tab(dt_match_ebal$euro)   #  very good
bal.tab(dt_match_ebal$māori)  #very good

# not good
dt_match_ps <- match_mi_general(
  data = dt,
  X = X,
  baseline_vars = baseline_vars_reflective_propensity,
  subgroup = "t0_eth_cat",
  estimand = "ATE",
  method = "ps"
)

# check balance
bal.tab(dt_match_ps$euro) # not good
bal.tab(dt_match_ps$māori) # not good




# code for summar
sum_e <- summary(dt_match_energy$euro)
sum_m <- summary(dt_match_energy$māori)
# sum_p <- summary(dt_match$pacific)
# sum_a <- summary(dt_match$asian)
sum_e
sum_m

dev.off()
plot(sum_e)
plot(sum_m)
#plot(sum_p)
#plot(sum_a)

love.plot(dt_match_energy$euro,
          binary = "std",
          thresholds = c(m = .1))

love.plot(dt_match_energy$māori,
          binary = "std",
          thresholds = c(m = .1))



# prepare data post-weighting ---------------------------------------------


# prepare data
dt_ref_e <- subset(dt, t0_eth_cat == "euro")
dt_ref_e$weights <- dt_match_ebal$euro$weights

# prepare data
dt_ref_m <- subset(dt, t0_eth_cat == "māori")
dt_ref_m$weights <- dt_match_ebal$māori$weights

# combine data into one data frame
dt_ref_all <- rbind(dt_ref_e, dt_ref_m)

# call dataframe `df`
df = dt_ref_all



# ANXIETY ANALYSIS --------------------------------------------------------

levels(df$t1_hours_exercise_coarsen)

### SUBGROUP analysis
df <-  dt_ref_all
Y <-  "t2_kessler_latent_anxiety_z"
X <- "t1_hours_exercise_coarsen" # already defined above
baseline_vars = baseline_vars_reflective_propensity
treat_0 = "inactive"
treat_1 = "very_active"
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


coefs <- coef(fit_all_all)
table(is.na(coefs))#     t0_eth_catmāori:t1_perfectionism_coarsen.Q:t0_gen_cohort.C

# #FALSE  TRUE
# 344     4

insight::get_varcov(fit_all_all)

# simulate coefficients
conflicts_prefer(clarify::sim)
sim_model_all <- sim(fit_all_all, n = nsims, vcov = "HC1")


# simulate effect as modified in europeans
sim_estimand_all_e <- sim_ame(
  sim_model_all,
  var = X,
  cl = cores,
  subset = t0_eth_cat == "euro",
  verbose = FALSE
)


# note contrast of interest
sim_estimand_all_e <-
  transform(sim_estimand_all_e, RD = `E[Y(inactive)]` - `E[Y(very_active)]`)
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
  transform(sim_estimand_all_m, RD = `E[Y(inactive)]` - `E[Y(very_active)]`)


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

# This table provides estimated levels of anxiety, in standard deviation units, for different levels of activity for two groups: Māori (indicated by "_m") and NZ Europeans (indicated by "_e").
#
# The expectations are named as `E[Y(<level of activity>)]_group`, where the level of activity can be `inactive`, `active`, or `very_active`.
#
# Here is a breakdown of the table:
#
#   1. For the Māori group (`_m`):
#
#   - `E[Y(inactive)]_m`: When inactive, the expected level of anxiety is 0.209 standard deviations, with a 95% confidence interval from 0.092 to 0.324.
# - `E[Y(active)]_m`: When active, the expected level of anxiety decreases to 0.121 standard deviations, with a 95% confidence interval from 0.048 to 0.201.
# - `E[Y(very_active)]_m`: When very active, the expected level of anxiety further decreases to 0.087 standard deviations, with a 95% confidence interval from -0.023 to 0.192.
# - `RD_m`: The risk difference (RD) between inactive and very active Māori individuals is 0.122 standard deviations, with a 95% confidence interval from -0.051 to 0.276. This indicates a decrease in anxiety when individuals move from an inactive to a very active lifestyle.
#
# 2. For the NZ European group (`_e`):
#
#   - `E[Y(inactive)]_e`: When inactive, the expected level of anxiety is 0.039 standard deviations, with a 95% confidence interval from -0.004 to 0.079.
# - `E[Y(active)]_e`: When active, the expected level of anxiety slightly decreases to -0.001 standard deviations, with a 95% confidence interval from -0.023 to 0.021.
# - `E[Y(very_active)]_e`: When very active, the expected level of anxiety further decreases to -0.068 standard deviations, with a 95% confidence interval from -0.107 to -0.031.
# - `RD_e`: The risk difference (RD) between inactive and very active NZ European individuals is 0.107 standard deviations, with a 95% confidence interval from 0.049 to 0.166. Similar to the Māori group, this indicates a decrease in anxiety when individuals move from an inactive to a very active lifestyle.
#
# The last row, `RD_m - RD_e`, represents the difference in risk differences between Māori and NZ Europeans. It's 0.015 standard deviations with a 95% confidence interval from -0.163 to 0.185. This is not statistically significant (the confidence interval contains 0), suggesting that the difference in anxiety reduction from being inactive to very active is not significantly different between the two groups.
#
# As with the previous interpretation, these findings should be treated as estimates with some degree of uncertainty, as reflected in the confidence intervals. They suggest a trend, but are subject to statistical variability. It's also important to note that these estimates assume a causal relationship between physical activity and anxiety, and that other factors are held constant, which may not be the case in reality. Other factors might also be influencing these observed relationships.


# depression analysis -----------------------------------------------------


### SUBGROUP analysis
df <-  dt_ref_all
Y <-  "t2_kessler_latent_depression_z"
X <- "t1_hours_exercise_coarsen" # already defined above
baseline_vars = baseline_vars_reflective_propensity
treat_0 = "inactive"
treat_1 = "very_active"
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


coefs <- coef(fit_all_all)
table(is.na(coefs))#     t0_eth_catmāori:t1_perfectionism_coarsen.Q:t0_gen_cohort.C

# #FALSE  TRUE
# 344     4

insight::get_varcov(fit_all_all)

# simulate coefficients
conflicts_prefer(clarify::sim)
sim_model_all <- sim(fit_all_all, n = nsims, vcov = "HC1")


# simulate effect as modified in europeans
sim_estimand_all_e_d <- sim_ame(
  sim_model_all,
  var = X,
  cl = cores,
  subset = t0_eth_cat == "euro",
  verbose = FALSE
)


# note contrast of interest
sim_estimand_all_e_d <-
  transform(sim_estimand_all_e_d, RD = `E[Y(inactive)]` - `E[Y(very_active)]`)
sim_estimand_all_e_d


# simulate effect as modified in māori
sim_estimand_all_m_d <- sim_ame(
  sim_model_all,
  var = X,
  cl = cores,
  subset = t0_eth_cat == "māori",
  verbose = FALSE
)

# combine
sim_estimand_all_m_d <-
  transform(sim_estimand_all_m_d, RD = `E[Y(inactive)]` - `E[Y(very_active)]`)


# summary
summary(sim_estimand_all_e_d)
summary(sim_estimand_all_m_d)

# rearrange
names(sim_estimand_all_e_d) <-
  paste(names(sim_estimand_all_e_d), "e", sep = "_")

names(sim_estimand_all_m_d) <-
  paste(names(sim_estimand_all_m_d), "m", sep = "_")


est_all_d <- cbind(sim_estimand_all_m_d, sim_estimand_all_e_d)
est_all_d <- transform(est_all_d, `RD_m - RD_e` = RD_m - RD_e)


# view summary
summary(est_all_d)


# This table provides estimated levels of depression, in standard deviation units, for different levels of activity for two groups: Māori (indicated by "_m") and NZ Europeans (indicated by "_e").
#
# The expectations are named as `E[Y(<level of activity>)]_group`, where the level of activity can be `inactive`, `active`, or `very_active`.
#
# Here is a breakdown of the results.
#
#   1. For the Māori group (`_m`):
#
#   - `E[Y(inactive)]_m`: When inactive, the expected level of depression is 0.23 standard deviations, with a 95% confidence interval from 0.116 to 0.356.
# - `E[Y(active)]_m`: When active, the expected level of depression decreases to 0.193 standard deviations, with a 95% confidence interval from 0.108 to 0.282.
# - `E[Y(very_active)]_m`: When very active, the expected level of depression further decreases to 0.133 standard deviations, with a 95% confidence interval from 0.009 to 0.262.
# - `RD_m`: The risk difference (RD) between inactive and very active Māori individuals is 0.097 standard deviations, with a 95% confidence interval from -0.068 to 0.274. This indicates a decrease in depression when individuals move from an inactive to a very active lifestyle.
#
# 2. For the NZ European group (`_e`):
#
#   - `E[Y(inactive)]_e`: When inactive, the expected level of depression is 0.034 standard deviations, with a 95% confidence interval from -0.012 to 0.078.
# - `E[Y(active)]_e`: When active, the expected level of depression slightly decreases to -0.006 standard deviations, with a 95% confidence interval from -0.03 to 0.016.
# - `E[Y(very_active)]_e`: When very active, the expected level of depression further decreases to -0.046 standard deviations, with a 95% confidence interval from -0.086 to -0.007.
# - `RD_e`: The risk difference (RD) between inactive and very active NZ European individuals is 0.081 standard deviations, with a 95% confidence interval from 0.02 to 0.138. Similar to the Māori group, this indicates a decrease in depression when individuals move from an inactive to a very active lifestyle.
#
# The last row, `RD_m - RD_e`, represents the difference in risk differences between Māori and NZ Europeans. It's 0.017 standard deviations with a 95% confidence interval from -0.152 to 0.204. This is not statistically significant (the confidence interval contains 0), suggesting that the difference in depression reduction from being inactive to very active is not significantly different between the two groups.
#
# These are estimates and subject to statistical uncertainty. While they suggest a trend, the wide confidence intervals indicate that these estimates come with a degree of uncertainty.

