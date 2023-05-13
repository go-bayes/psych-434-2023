# PSYCH 434 WEEK 8: subsample estimation
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

hist(scale(nzavs_synth$hlth_sleep_hours))

# This will read the synthetic data into Rstudio.  Note that the arrow package allows us to have lower memory demands in the storage and retrieval of data.
nzavs_synth <-
  arrow::read_parquet(here::here("data", "nzavs_dat_synth_t10_t12"))


# You may inspect the data (optional)
## one nice method
skimr::skim(nzavs_synth)

## inspect colnames
colnames(nzavs_synth)

## inspect data properties
str(nzavs_synth)


# We will make the coarsen variable for the exposure early

nzavs_synth <- nzavs_synth |>
  mutate(perfectionism = round(perfectionism)) |> # we create a three-level exposure to enable clear causal contrasts.
  mutate(
    perfectionism_coarsen = cut(
      perfectionism,
      breaks = c(1, 4, 5, 7),
      include.lowest = TRUE,
      include.highest = TRUE,
      na.rm = TRUE,
      right = FALSE
    ),
    perfectionism_coarsen = factor(
      perfectionism_coarsen,
      levels = c("[1,4)", "[4,5)", "[5,7]"),
      labels = c("low", "medium", "high"),
      ordered = TRUE
    )
  )


############## ############## ############## ############## ############## ############## ############## ########
############## ############## ############## Checks Exposure ############## ############## ######## ##############

# Today will we again be looking at the Average Treatment Effect of perfectionism on life meaning in two different "treatment groups" - Māori and New Zealand Europeans. We'll assess evidence for whether ATE differs between the groups.

# Recall that for an association to be causal, a change in the exposure must affect the world
# Lets look at how much the exposure has changed. No change, no effects!

# inspect change in the exposure
library(msm) # this will allow us to quicky inspect the data for instances of change.

# We only inspect change between the baseline condition and the exposure year
dt_18_19 <- nzavs_synth |>
  filter(wave == "2018" | wave == "2019") |>
  select(id, wave, perfectionism, perfectionism_coarsen, eth_cat) |> # the categorical variable needs to be numeric for us to use msm package to investigate change
  mutate(perfectionism_coarsen_n = as.numeric(perfectionism_coarsen))



# Consider the original response scale

msm::statetable.msm(round(perfectionism, 0), id, data = dt_18_19) |>
  kbl() |>
  kable_paper(full_width = F)


# We can count the instances

msm::statetable.msm(round(perfectionism, 0), id, data = dt_18_19) |>
  data.frame() |>  # adding this code does the trick of counting
  kbl() |>
  kable_paper(full_width = F)



# Lets look at the coarsened variable

msm::statetable.msm(round(perfectionism_coarsen_n, 0), id, data = dt_18_19) |>
  kbl() |>
  kable_paper(full_width = F)


# Here we see better coverage
# Recall that clinical psychologists look at 4 and 5 on the 1-7 perfectionism scale as cut off points,

msm::statetable.msm(round(perfectionism_coarsen_n, 0), id, data = dt_18_19) |>
  data.frame() |>
  kbl() |>
  kable_paper(full_width = F)


# lets break this down by ethnicity
dt_18_19_e <- dt_18_19 |> filter(eth_cat == "euro")
dt_18_19_m <- dt_18_19 |> filter(eth_cat == "māori")



# euro continuous
msm::statetable.msm(round(perfectionism, 0), id, data = dt_18_19_e) |>
  kbl() |>
  kable_paper(full_width = F)

# euro coarsen
msm::statetable.msm(round(perfectionism_coarsen_n, 0), id, data = dt_18_19_e) |>
  kbl() |>
  kable_paper(full_width = F)

#  māori continuous
msm::statetable.msm(round(perfectionism, 0), id, data = dt_18_19_m) |>
  kbl() |>
  kable_paper(full_width = F)

# māori coarsen -- again this looks a little better
msm::statetable.msm(round(perfectionism_coarsen_n, 0), id, data = dt_18_19_m) |>
  kbl() |>
  kable_paper(full_width = F)


# we can see there are not many "natural experiments" among māori
# this is a challenge in observational cultural research -- even with large samples (we started with N = 10,000!) we did not have many "natural experiments"

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
  rename_all( ~ stringr::str_replace(., "^t0_", "")) %>%
  mutate(wave = factor(rep("baseline", nrow(prep_reflective)))) |>
  janitor::clean_names(case = "screaming_snake")


# create a formula string

baseline_vars_names <- dt_new %>%
  select(-WAVE) %>%
  colnames()

table_baseline_vars <- paste(baseline_vars_names, collapse = "+")

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
  mutate(t0_urban = factor(
    ifelse(
      t0_rural_gch2018 == "medium_urban_accessibility" |
        t0_rural_gch2018 == "high_urban_accessibility",
      "urban",
      "rural"
    )
  )) |>
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
         ends_with("_z"),) |>
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

dt_match<- readRDS(here::here("data", "dt_match"))


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
  t2_meaning_z  ~ t1_perfectionism_coarsen * t0_eth_cat, ## note we do not have covariates -- don't need them because we have weighted by the covariates on the exposure to acheive balance.
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
est_r_cont <- cbind(sim_estimand_r_e_cont, sim_estimand_r_m_cont)
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


tab_ml <- parameters::model_parameters(model_ml, effects = "fixed")
tab_ml
plot(tab_ml)


library(ggeffects)


graph_ml <- plot(ggeffects::ggpredict(model_ml, terms = c("time", "perfectionism_z", "eth_cat")))


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
fit_all_all_x  <- glm(# t2_hlth_fatigue_z ~ t0_eth_cat *  t1_hours_exercise_coarsen,
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


