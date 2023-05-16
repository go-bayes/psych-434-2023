
# PSYCH 434 WEEK 7: INTRODUCTION TO CAUSAL INFERENCE
# PART 1: CAUSAL ESTIMATION
# PART 2: PRACTICING DAGS
# May 2023
# questions: joseph.bulbulia@vuw.ac.nz


# Running this command will download the functions and packages you need to complete this worksheet.
# You many find the code by pointing your browser to the webpage that is contained in the link

# Before running this source code, make sure to update to the current version of R, and to update all exisiting packages.

source("https://raw.githubusercontent.com/go-bayes/templates/main/functions/funs.R")




## Part 1. Draw a Dag




## Problem 1: bilingualism on cognitive abilities
## Using the DAG below as a starting point, create a DAG in R using the `ggdag` package that represents the causal relationships among bilingualism (B), cognitive abilities (C), and socioeconomic status (S).

# Install the package if not already installed
if (!require(ggdag)) {
  install.packages("ggdag")
}

set.seed(123)
library(ggdag)
dag1 <- dagify(Y2 ~ A1 + Y0 + A0 +  La0 + Lb0 + Lc0 + A0 + Y0,
               A1 ~ La0 + Lb0 + Lc0 + A0 + Y0,
               labels = c(
                 Y2 = "t2/outcome",
                 A1 = "t1/exposure",
                 A0 = "t0/baseline_exposure",
                 Y0 = "t0/baseline_outcome",
                 La0 = "t0/baseline_confounder_1",
                 Lb0 = "t0/baseline_confounder_2",
                 Lc0 = "t0/baseline_confounder_3"
               ),
               exposure = "A1",
               outcome = "Y2")


dag1 |>
  ggdag_adjustment_set()

dag1 |>
  ggdag(text = FALSE,
        use_labels = "label")

dag1 |>
  ggdag_adjustment_set()


#
# # inspect
# tidy_dagitty(dag1)
#
# dag1_t <- tidy_dagitty(dag1)
#
# # plot
# ggdag(dag1_t) + theme_dag_blank()
# #
# # view
# ggdag::ggdag_paths(dag1_t)
#
# # inspect
# ggdag_parents(dag1_t, "A1")

# find adjustment set: adjusting for S is sufficient to control for confounding (on the model's assumptions)
# ggdag_adjustment_set(dag1_t)


# aside customise
ggdag_paths(dag1, text = FALSE, use_labels = "label", shadow = TRUE) +
  theme_dag(base_size = 14) +
  theme(legend.position = "none", strip.text = element_blank()) +
  # set node aesthetics
  scale_color_manual(values = "#0072B2", na.value = "grey80") +
  # set label aesthetics
  scale_fill_manual(values = "#0072B2", na.value = "grey80") +
  # set arrow aesthetics
  ggraph::scale_edge_color_manual(values = "#0072B2", na.value = "grey80") +
  ggtitle("Open paths from smoking to A1 to Y2")




## TASK  Create a Dag for the Study described below on perfectionism
## hint

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

exposure_var = c("perfectionism")


## step 3. select the outcome variable.  These are the outcomes.
outcome_vars_reflective = c("meaning_purpose",
                            "meaning_sense")



### ANSWER HERE






















######### PART 2: DATA EXCERCISE ##############


# Create a folder called "data", in your Rstudio project. Download this file, add it to your the folder called "data" in your Rstudio project.
# "https://www.dropbox.com/s/vwqijg4ha17hbs1/nzavs_dat_synth_t10_t12?dl=0"



# This will read the synthetic data into Rstudio.  Note that the arrow package allows us to have lower memory demands in the storage and retrieval of data.
nzavs_synth <-
  arrow::read_parquet(here::here("data", "nzavs_dat_synth_t10_t12"))


# you may inspect the data (optional)
## one nice method
skimr::skim(nzavs_synth)

## inspect colnames
colnames(nzavs_synth)

## inspect data properties
str(nzavs_synth)




# next we will select variables for baseline confounders. recall from lecture that these are variables that may be associated with both an exposure and an outcome.  To find out more about these variables go here:

## https://github.com/go-bayes/psych-434-2023/blob/main/data/readme.qmd



# I have created a function that will put the data into the correct shape. Here are the steps.

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

exposure_var = c("perfectionism")


## step 3. select the outcome variable.  These are the outcomes.
outcome_vars_reflective = c("meaning_purpose",
                            "meaning_sense")



# the function "create_wide_data" should be in your environment. If not, make sure to run the first line of code in this script once more.  You may ignore the warnings.

# IGNORE WARNING

prep_reflective <-
  create_wide_data(
    dat_long = nzavs_synth,
    #nzavs_synth,
    baseline_vars = baseline_vars,
    exposure_var = exposure_var,
    outcome_vars = outcome_vars_reflective
  )


# check. Note that any column that is the exposure or an outcome is added to "t0_".  This ensures the strongest possible confounding control, as described by VanderWeele:
# https://cdn1.sph.harvard.edu/wp-content/uploads/sites/603/2020/09/OutcomeWide_StatisticalScience.pdf

str(prep_reflective)



# if the data is not working, you much run the code below to make the object in an object of the class dataframe.
# prep_reflective <- as.data.frame(prep_reflective)



# create composite scores for the constructs. make sure the tidyverse library is loaded:

library(tidyverse) # should be loaded

dt_ref <- prep_reflective |>
  mutate(id = factor(1:nrow(prep_reflective))) |>
  mutate(t1_perfectionism = round(t1_perfectionism)) |> # we create a three-level exposure to enable clear causal contrasts. We could also use a continous variable.
  mutate(
    t1_perfectionism_coarsen = cut(
      t1_perfectionism,
      breaks = c(1, 4, 5, 7),
      include.lowest = TRUE,
      include.highest = TRUE,
      na.rm = TRUE,
      right = FALSE
    )
  ) |>
  mutate(
    t0_eth_cat = as.factor(t0_eth_cat),
    t0_rural_gch2018 = as.factor(t0_rural_gch2018),
    t0_gen_cohort = as.factor(t0_gen_cohort)
  ) |>
  group_by(id) |>
  dplyr::mutate(t2_meaning = mean(c(t2_meaning_purpose,
                                    t2_meaning_sense),
                                  na.rm = TRUE)) |>
  ungroup() |>
  # transform numeric variables into z scores (improves estimation)
  dplyr::mutate(across(where(is.numeric), ~ scale(.x), .names = "{col}_z")) %>%
  # select only factors and numeric values that are z-scores
  select(id,
         where(is.factor),
         t1_perfectionism, # for comparison
         ends_with("_z"), ) |>
  # tidy data frame so that the columns are ordered by time (useful for more complex models)
  relocate(id, .before = starts_with("t1_"))   |>
  relocate(starts_with("t0_"), .before = starts_with("t1_"))  |>
  relocate(starts_with("t2_"), .after = starts_with("t1_"))


# inspect
levels(dt_ref$t1_perfectionism_coarsen)


# rename levels
dt_ref$t1_perfectionism_coarsen <-
  factor(
    dt_ref$t1_perfectionism_coarsen,
    levels = c("[1,4)", "[4,5)", "[5,7]"),
    labels = c("low", "medium", "high"),
    ordered = TRUE
  )



# view object
skimr::skim(dt_ref)


# save your dataframe for future use

# make dataframe
dt_ref = as.data.frame(dt_ref)

# save data
saveRDS(dt_ref, here::here("data", "dt_ref"))

# read -- you may start here if you need to repeat the analysis

dt_ref <- readRDS(here::here("data", "dt_ref"))


## NEXT we write two simple models, one with a continous exposure and one with a categorical exposure.
# To make this easier we will assign key columns to variables

# this is the continuous exposure
X = "t1_perfectionism_z"

# this is a categorical exposure
X_pc <- "t1_perfectionism_coarsen"


# set our outcome variable:
Y = "t2_meaning_z" #note that we have created all numeric values into z-scores.  This will facilitate estimation and also interpretation. The outcome is expressed in standard deviation units


# Get baseline names
baseline_vars_reflective_cont = dt_ref |>
  dplyr::select(starts_with("t0")) |> colnames()

# See what we have created:  These are all the "t0_" variables.
baseline_vars_reflective_cont


# to run these models our data need to be a dataframe (not a tibble or another kind of obect)
# above we've made the data a dataframe, but lets repeat in case you skipped that steip

dt_ref = as.data.frame(dt_ref)


# for simplicity
df = dt_ref

# create our formula string, this time for the categorical variable.
formula_str_X <-
  paste(Y,
        "~",
        X ,
        "*",
        "(",
        paste(baseline_vars_reflective_cont, collapse = "+"),
        ")")
formula_str_X

## regression based control


# fit model
m1 <- glm(as.formula(formula_str_X),
          # shortcut
          #  weights = weights, # will use weights with propensity score models
          family = "gaussian",
          data = df)

# we can look at the coefficients of this model, but again, it would be a mistake to interpret them
summary(m1)



# another way of presenting the model
# run to install latest version of easystats.
# parameters::model_parameters(m1)


# simulate coefficients for the continuous exposure
library(clarify)
nsims = 200
sim_model_r <- sim(m1, n = nsims)


# set to number of cores on your machine, e.g.
cores = 2

# simulate effect as modified
sim_estimand_r <- sim_ame(sim_model_r,
                          var = X,
                          cl = cores,
                          verbose = FALSE)


# this is the difference in expectations between everyone in the population being subject to a one standard deviation increase in perfectionism
# and everyone being subject to an average level of perfectionism.


# Estimate  2.5 % 97.5 %
#   dY/d(t1_perfectionism_z)   -0.160 -0.178 -0.140

summary(sim_estimand_r)


# In this case we will use the coarsened variable.

# create our formula string:
formula_str_X_pc <-
  paste(Y,
        "~",
        X_pc ,
        "*",
        "(",
        paste(baseline_vars_reflective_cont, collapse = "+"),
        ")")

formula_str_X_pc



# fit model
m2 <- glm(as.formula(formula_str_X_pc),
          # shortcut
          #  weights = weights, # will use weights with propensity score models
          family = "gaussian",
          data = df)

# we can look at the coefficients of this model, but again it would be a mistake to interpret them

summary(m2)

# simulate coefficients
library(clarify)
nsims = 200
sim_model_r2 <- sim(m2, n = nsims)


# set to number of cores on your machine, e.g.
cores = 4

# simulate effect as modified
sim_estimand_r2 <- sim_ame(sim_model_r2,
                           var = X_pc,
                           cl = cores,
                           verbose = FALSE)




summary(sim_estimand_r2)


# suppose we want to contrast everyone being assigned to medium with everyone being assigned to high.

sim_estimand_r2_focal <-
  transform(sim_estimand_r2, RD = `E[Y(low)]` - `E[Y(high)]`)


# RD describes the causal contrast on the risk difference scale.
summary(sim_estimand_r2)


# The ATE for the effect of moving from low to high is .26 a standard deviation unit in meaning.  What this means is that the expected loss of meaning from perfectionism is about a quarter standard deviation


# > summary(sim_estimand_r2)
# Estimate   2.5 %  97.5 %
#   E[Y(low)]      0.0747  0.0519  0.0950
# E[Y(medium)]  -0.1220 -0.1534 -0.0788
# E[Y(high)]    -0.1872 -0.2408 -0.1360


# We interpret these contrasts as the expected effects were everyone "low" verses everyone assigned "medium" versus everyone assigned "high"




# 1.  model the causal effect (ATE) of graditude on life satisfaction Do this in two ways: first model the causal effect of a standard deviation increase in gratitude from average gratitude. 2. model the causal effect of movement from medium gratitude to high gratitude. Experts tell us low is <5, medium is 5 <6, and high is >6.  Briefly, report your findings.












######################## solution to data excercise ####################


# inspect column names
colnames(nzavs_synth)

# what is the mean of gratitude?
mean(nzavs_synth$gratitude, na.rm = TRUE)

# what is the standard deviation?
sd(nzavs_synth$gratitude, na.rm = TRUE)

# inspect responses
hist(nzavs_synth$gratitude, breaks = 100)


# data wrangling

# step 1: choose baseline variables.

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


## step 2: select exposure

exposure_var = c("gratitude")


## step 3. select the outcome variable.  see, the "readme" file for information on the labels, and also for information on the NZAVS
# https://github.com/go-bayes/psych-434-2023/tree/main/data

outcome_vars_reflective = c("lifesat_satlife",
                            "lifesat_ideal")


# step 4. prepare data
# the function "create_wide_data" should be in your environment. If not, make sure to run the first line of code in this script once more.  You may ignore the warnings.

prep_data <-
  create_wide_data(
    dat_long = nzavs_synth,
    #nzavs_synth,
    baseline_vars = baseline_vars,
    exposure_var = exposure_var,
    outcome_vars = outcome_vars_reflective
  )


# check. Note that any column that is the exposure or an outcome is added to "t0_".  This ensures the strongest possible confounding control, as described by VanderWeele:
# https://cdn1.sph.harvard.edu/wp-content/uploads/sites/603/2020/09/OutcomeWide_StatisticalScience.pdf

str(prep_data)



# create composite scores for the constructs. make sure the tidyverse library is loaded:

dt <- prep_data |>
  mutate(id = factor(1:nrow(prep_data))) |>
  mutate(t1_gratitude = round(t1_gratitude)) |> # we create a three-level exposure to enable clear causal contrasts. We could also use a continous variable.
  mutate(
    t1_gratitude_coarsen = cut(
      t1_gratitude,
      breaks = c(1, 5, 6, 7),
      include.lowest = TRUE,
      include.highest = TRUE,
      na.rm = TRUE,
      right = FALSE
    )
  ) |>
  mutate(
    t0_eth_cat = as.factor(t0_eth_cat),
    t0_rural_gch2018 = as.factor(t0_rural_gch2018),
    t0_gen_cohort = as.factor(t0_gen_cohort)
  ) |>
  group_by(id) |>
  dplyr::mutate(t2_lifsat = mean(c(t2_lifesat_satlife,
                                  t2_lifesat_ideal),
                                  na.rm = TRUE)) |>
  ungroup() |>
  # transform numeric variables into z scores (improves estimation)
  dplyr::mutate(across(where(is.numeric), ~ scale(.x), .names = "{col}_z")) %>%
  # select only factors and numeric values that are z-scores
  select(id,
         where(is.factor),
         t1_gratitude, # for continuous model, if we do not wish use SD units
         ends_with("_z"), ) |>
  # tidy data frame so that the columns are ordered by time (useful for more complex models)
  relocate(id, .before = starts_with("t1_"))   |>
  relocate(starts_with("t0_"), .before = starts_with("t1_"))  |>
  relocate(starts_with("t2_"), .after = starts_with("t1_"))


# inspect
levels(dt$t1_gratitude_coarsen)

table(dt$t1_gratitude_coarsen)


# rename levels
dt$t1_gratitude_coarsen <-
  factor(
    dt$t1_gratitude_coarsen,
    levels = c("[1,5)", "[5,6)", "[6,7]"),
    labels = c("low", "medium", "high"),
    ordered = TRUE
  )


table(dt$t1_gratitude_coarsen)

# view object
skimr::skim(dt)

# make dataframe
dt = as.data.frame(dt)

colnames(dt)

## NEXT we write two simple models, one with a continous exposure and one with a categorical exposure.
# To make this easier we will assign key columns to variables

# this is the continuous exposure
X = "t1_gratitude_z"

# this is a categorical exposure
X_c <- "t1_gratitude_coarsen"


# set our outcome variable:
Y = "t2_lifsat_z" #note that we have created all numeric values into z-scores.  This will facilitate estimation and also interpretation. The outcome is expressed in standard deviation units


# Get baseline names
baseline_vars_mine = dt |>
  dplyr::select(starts_with("t0")) |> colnames()

# See what we have created:  These are all the "t0_" variables.
baseline_vars_mine


# to run these models our data need to be a dataframe (not a tibble or another kind of obect)
# above we've made the data a dataframe, but lets repeat in case you skipped that steip

dt = as.data.frame(dt)


# create our formula string, this time for the categorical variable.
formula_str_X <-
  paste(Y,
        "~",
        X ,
        "*",
        "(",
        paste(baseline_vars_mine, collapse = "+"),
        ")")
formula_str_X

## regression based control


# fit model
m1_mine <- glm(as.formula(formula_str_X),
          # shortcut
          #  weights = weights, # will use weights with propensity score models
          family = "gaussian",
          data = dt)

# we can look at the coefficients of this model, but again, it would be a mistake to interpret them
summary(m1_mine)



# another way of presenting the model
# run to install latest version of easystats.
# parameters::model_parameters(m1_mine)


# simulate coefficients for the continuous exposure
library(clarify)
nsims = 200
sim_model_r_mine <- sim(m1_mine, n = nsims)


# set to number of cores on your machine, e.g.
cores = 2

# simulate effect as modified
sim_model_r_mine <- sim_ame(sim_model_r_mine,
                          var = X,
                          cl = cores,
                          verbose = FALSE)


# this is the difference in expectations between everyone in the population being subject to a one standard deviation increase in perfectionism
# and everyone being subject to an average level of perfectionism.


summary(sim_model_r_mine)

# > summary(sim_model_r_mine)
# Estimate 2.5 % 97.5 %
#   dY/d(t1_gratitude_z)    0.115 0.097  0.134
#


# Brief interpretation

# - The sample consists of a representative group of New Zealanders (N = 10,000), with synthesized data from respondents to the NZAVS across three waves (2018-2020).
# - G-computation was employed to estimate the causal effect (ATE) of a one standard deviation change in baseline gratitude on life satisfaction.
# - The projected marginal effect of gratitude on life satisfaction for the relevant population is an increase of 0.115 standard deviation units.
# - The model's assumptions include positivity, conditional exchangeability based on baseline control variables, and consistency. Additionally, the model assumes no interference, accurate variable measurement, and correct statistical model specification.
# - A primary limitation of this model is its assumption that sample attrition is unbiased.
# - This result is significant because it demonstrates the potential positive impact of gratitude on life satisfaction, highlighting the importance of fostering gratitude in daily life and informing potential interventions to improve overall well-being.


# b. Next we model the causal effect of moving from medium gratitude to high gratitude on life satisfaction.



# In this case we will use the coarsened variable.

# create our formula string:
formula_str_X_c <-
  paste(Y,
        "~",
        X_c ,
        "*",
        "(",
        paste(baseline_vars_mine, collapse = "+"),
        ")")

formula_str_X_c



# fit model
m2_mine  <- glm(as.formula(formula_str_X_c),
          # shortcut
          #  weights = weights, # will use weights with propensity score models
          family = "gaussian",
          data = dt)

# we can look at the coefficients of this model, but again it would be a mistake to interpret them

summary(m2_mine)

# simulate coefficients
library(clarify)
nsims = 200
sim_model_r2_mine <- sim(m2_mine, n = nsims)


# set to number of cores on your machine, e.g.
cores = 2

# simulate effect as modified
sim_estimand_r2_mine <- sim_ame(sim_model_r2_mine,
                           var = X_c,
                           cl = cores,
                           verbose = FALSE)




summary(sim_estimand_r2_mine)


# suppose we want to contrast everyone being assigned to medium with everyone being assigned to high.

sim_estimand_r2_mine_focal <-
  transform(sim_estimand_r2, RD = `E[Y(medium)]` - `E[Y(high)]`)


# RD describes the causal contrast on the risk difference scale.
summary(sim_estimand_r2_mine_focal)



# > summary(sim_estimand_r2_mine_focal)
# Estimate    2.5 %   97.5 %
#   E[Y(low)]     0.07471  0.05191  0.09501
# E[Y(medium)] -0.12200 -0.15339 -0.07879
# E[Y(high)]   -0.18718 -0.24076 -0.13596
# RD            0.06518  0.00223  0.12873




sim_estimand_r2_mine_ad_hoc <-
  transform(sim_estimand_r2, RD = `E[Y(low)]` - `E[Y(high)]`)


# RD describes the causal contrast on the risk difference scale.
summary(sim_estimand_r2_mine_ad_hoc)


# > summary(sim_estimand_r2_mine_ad_hoc)
# Estimate   2.5 %  97.5 %
#   E[Y(low)]      0.0747  0.0519  0.0950
# E[Y(medium)]  -0.1220 -0.1534 -0.0788
# E[Y(high)]    -0.1872 -0.2408 -0.1360
# RD             0.2619  0.2071  0.3115


# Brief interpretation

# - The sample consists of a representative group of New Zealanders, with synthesised data from respondents to the NZAVS across three waves (2018-2020).
# - The continuous gratitude exposure variable was trichotomised into low (1 to <5), medium (5 to <6), and high (6-7) levels.
# - G-computation was employed to estimate the causal risk differences of moving between gratitude levels on life satisfaction.
# - The estimated causal risk difference for moving from low to high gratitude levels is 0.2619 (95% CI: 0.2071 - 0.3115).
# - The causal risk difference for moving from medium to high gratitude levels, which was the study's original focus, is 0.06518 (95% CI: 0.00223 - 0.12873).
# - The model's assumptions include positivity, conditional exchangeability based on baseline control variables, and consistency. Additionally, the model assumes no interference, accurate variable measurement, and correct statistical model specification.
# - A primary limitation of this model is its assumption that sample attrition is unbiased.
# - The results emphasize the importance of moving from medium to high gratitude levels in order to improve life satisfaction, providing a targeted area for potential interventions and well-being improvement strategies. Were there interventions that could move people from low to high levels of gratitude, we would expect even larger causal effects.





######### PART 2: CAUSAL GRAPHS &  EXCERCISE ##############


########################################### DAGS #####################################################################

# First GGDAG exercises.
# also load the ggdag package
if(!require(ggdag)){
  # Install the package if it is not already installed
  install.packages("ggdag")
}
# load the ggdag package
library(ggdag)


## Motivating Examples

# 1. **Bilingualism and cognitive abilities:** does bilingualism improve cognitive abilitiaes in children, and what factors might contribute to the observed relationship?
#   2. **Cultural values and well-being:** how do cultural values influence well-being, and are there confounding factors that might explain the association?
#   3. **Acculturation and mental health:** do different acculturation strategies impact mental health outcomes among migrants, and how can we account for potential confounders?
#   4. **Social media use and life satisfaction:** does social media use affect life satisfaction, or are there underlying factors driving the relationship?
#   5. **Parenting styles and children's self-esteem:** How do parenting styles influence children's self-esteem, and what role do cultural and demographic factors play?
#   6. **Migration and stress:** Is there a causal link between migration and stress levels, and how can we address potential confounding variables?
#   7. **Religion and well-being:** How does religious affiliation or practice affect well-being, and what factors may explain the observed associations?
#   8. **Ethnic identity and self-esteem:** What is the relationship between ethnic identity and self-esteem, and how can we account for potential confounders?
#   9. **Language proficiency and job satisfaction:** Does language proficiency have a causal impact on job satisfaction among migrants, and what factors may influence the relationship?
#   10. **Education and intercultural competence:** How does education level affect intercultural competence, and how can we address potential confounding factors?



## Problem 1: bilingualism on cognitive abilities

## Using the DAG below as a starting point, create a DAG in R using the `ggdag` package that represents the causal relationships among bilingualism (B), cognitive abilities (C), and socioeconomic status (S).


# Assume:
# S -> B -> C
# S ------> C

## Problem 2: Cultural values on well-being*

## Create a DAG in R using the `ggdag` package that represents the causal relationships among cultural values (V), well-being (W), and social support (SS)


# Assume
# V -> W
# V -> SS
# SS -> W


## Problem 3: acculturation and mental health

## Create a DAG that represents the causal relationships among acculturation (A), mental health (MH), language proficiency (LP), and social network size (SN).

## assume

# A -> MH
# A -> LP
# LP -> MH
# A -> SN
# SN -> MH


## Problem 4: Social media use and life satisfaction

# Create a DAG that represents the causal relationships among social media use (SM), life satisfaction (LS), and perceived social support (PSS).

## assume

# SM -> LS
# PSS -> LS
# PSS <- SM


## Problem 5: parenting styles on children's self-esteem

# Create a that represents the causal relationships among parenting styles (PS), children's self-esteem (SE), and children's academic achievement (AA).

# assume:
# PS -> SE
# PS -> AA
# SE -> AA


## Problem 6: Migration on distress
# Create a DAG that represents the causal relationships among migration status (M), stress levels (S), social support (SS), and coping strategies (CS).

# assume
#
# M -> S
# M -> SS
# SS -> S
# M -> CS
# CS -> S


# problem 7: religion on well-being

# create a DAG that represents the causal relationships among religiosity (R), well-being (W), and social support (SS).

# assume:
# R -> W
# R -> SS
# SS -> W


## Problem 8: ethnic identity on self-esteem

# Create a DAG that represents the causal relationships among ethnic identity (EI), self-esteem (SE), and social support (SS).

## assume:
# EI -> SE
# EI -> SS
# SS -> SE


## Problem 9: Language proficiency on job satisfaction

# create a DAG in R that represents the causal relationships among language proficiency (LP), job satisfaction (JS), and job opportunities (JO).

# assume
# LP -> JS
# LP -> JO
# JO -> JS


## Problem 10: education on intercultural competence

# Create a DAG that represents the causal relationships among education (E), intercultural competence (IC), and cultural exposure (CE).

# assume:
# E -> IC
# E -> CE
# CE -> IC



## Solutions
library(ggdag)
library(ggplot2)

#set the default theme for all plots

theme_set(theme_dag_blank())


## Problem 1: bilingualism on cognitive abilities
## Using the DAG below as a starting point, create a DAG in R using the `ggdag` package that represents the causal relationships among bilingualism (B), cognitive abilities (C), and socioeconomic status (S).



dag1 <- dagify(C ~ B + S,
               B ~ S,
               coords = list(x = c(S = 1, B = 2, C = 3),
                             y = c(S = 1, B = 2, C = 1)),
               exposure = "B",
               outcome = "C")

# inspect
tidy_dagitty(dag1)

dag1_t <- tidy_dagitty(dag1)

# plot
ggdag(dag1_t)

# view
ggdag::ggdag_paths(dag1_t)

# inspect
ggdag_parents(dag1_t, "B")

# find adjustment set: adjusting for S is sufficient to control for confounding (on the model's assumptions)
ggdag_adjustment_set(dag1_t)



## Problem 2: Cultural values and well-being
## Create a DAG in R using the `ggdag` package that represents the causal relationships among cultural values (V), well-being (W), and social support (SS)

dag2 <- dagify(W ~ V + SS,
               SS ~ V,
               coords = list(x = c(V = 1, SS = 2, W = 3),
                             y = c(V = 1, SS = 2, W = 1)),
               exposure = "V",
               outcome = "W")
ggdag(dag2)


ggdag_adjustment_set(dag2)


## Problem 3: acculturation and mental health
## Create a DAG that represents the causal relationships among acculturation (A), mental health (MH), language proficiency (LP), and social network size (SN).


dag3 <- dagify(MH ~ A + LP + SN,
               LP ~ A,
               SN ~ A,
               coords = list(x = c(A = 1, LP = 2, SN = 2, MH = 3),
                             y = c(A = 1, LP = 2, SN = 1, MH = 1)),
               exposure = "A",
               outcome = "MH")
ggdag(dag3)


ggdag_adjustment_set(dag3)


## Problem 4: Social media use and life satisfaction
# Create a DAG that represents the causal relationships among social media use (SM), life satisfaction (LS), and perceived social support (PSS).

dag4 <- dagify(LS ~ SM + PSS,
               PSS ~ SM,
               coords = list(x = c(SM = 1, PSS = 2, LS = 3),
                             y = c(SM = 1, PSS = 2, LS = 1)),
               exposure = "SM",
               outcome = "LS")
ggdag(dag4)

ggdag_adjustment_set(dag4)


## Problem 5: parenting styles on children's self-esteem
# Create a that represents the causal relationships among parenting styles (PS), children's self-esteem (SE), and children's academic achievement (AA).


dag5 <- dagify(SE ~ PS,
               AA ~ PS + SE,
               coords = list(x = c(PS = 1, SE = 2, AA = 3),
                             y = c(PS = 1, SE = 2, AA = 1)),
               exposure = "PS",
               outcome = "SE")
ggdag(dag5)

ggdag_adjustment_set(dag5)

# if we condition on AA PS and SE are no longer "d-separated" (which is bad)
ggdag::ggdag_dseparated(dag5)


## Problem 6: Migration on distress
# Create a DAG that represents the causal relationships among migration status (M), stress levels (S), social support (SS), and coping strategies (CS).


dag6 <- dagify(S ~ M + SS + CS,
               SS ~ M,
               CS ~ M,
               exposure = "M",
               outcome = "S")
ggdag(dag6)


ggdag_adjustment_set(dag6)


## Problem 7: religion on well-being
# create a DAG that represents the causal relationships among religiosity (R), well-being (W), and social support (SS).

dag7 <- dagify(W ~ R + SS,
               SS ~ R,
               coords = list(x = c(R = 1, SS = 2, W = 3),
                             y = c(R = 1, SS = 1, W = 1)),
               exposure = "R",
               outcome = "W")
ggdag(dag7)

ggdag_adjustment_set(dag7)

# do not condition on a mediator
ggdag::ggdag_dseparated(dag7)

## Problem 8: ethnic identity on self-esteem
# Create a DAG that represents the causal relationships among ethnic identity (EI), self-esteem (SE), and social support (SS).


dag8 <- dagify(SE ~ EI + SS,
               SS ~ EI,
               coords = list(x = c(EI = 1, SS = 2, SE = 3),
                             y = c(EI = 1, SS = 1, SE = 1)),
               exposure = "EI",
               outcome = "SE")
ggdag(dag8)

ggdag_adjustment_set(dag8)

## Problem 9: Language proficiency on job satisfaction
# create a DAG in R that represents the causal relationships among language proficiency (LP), job satisfaction (JS), and job opportunities (JO).

dag9 <- dagify(JS ~ LP + JO,
               JO ~ LP,
               coords = list(x = c(LP = 1, JO = 2, JS = 3),
                             y = c(LP = 1, JO = 1, JS = 1)),
               exposure = "LP",
               outcome = "JS")
ggdag(dag9)

ggdag_adjustment_set(dag9)




## Problem 10: education on intercultural competence
# Create a DAG that represents the causal relationships among education (E), intercultural competence (IC), and cultural exposure (CE).

dag10 <- dagify(IC ~ E + CE,
                CE ~ E,
                coords = list(x = c(E = 1, CE = 2, IC = 3),
                              y = c(E = 1, CE = 1, IC = 1)),
                exposure = "E",
                outcome = "IC")
ggdag(dag10)

ggdag_adjustment_set(dag10)



## Create dag

dag3 <- dagify(Y ~ A + L1 + L2 + L3 + L4,
               A ~ L1 + L2 + L3,
               L2 ~ L3,
               exposure = "A",
               outcome = "Y")

ggdag(dag3) + theme_dag_blank()


ggdag_adjustment_set(dag3)+ theme_dag_blank()


