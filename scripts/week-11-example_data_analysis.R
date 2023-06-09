
# functions
source("https://raw.githubusercontent.com/go-bayes/templates/main/functions/funs.R")


# experimental functions (more functions)
source(
  "https://raw.githubusercontent.com/go-bayes/templates/main/functions/experimental_funs.R"
)



# import data
nzavs_synth <-
  arrow::read_parquet(here::here("data", "nzavs_dat_synth_t10_t12"))


#
hist( nzavs_synth$support_help)
hist( nzavs_synth$support_rnoguidance)
hist( nzavs_synth$support_turnto)

mean( nzavs_synth$support_help)
mean( nzavs_synth$support_rnoguidance)
mean( nzavs_synth$support_turnto)

sd( nzavs_synth$support_help)
sd( nzavs_synth$support_rnoguidance)
sd( nzavs_synth$support_turnto)

# create sum score of meaning of life
dt_start <- nzavs_synth %>%
  arrange(id, wave) %>%
  rowwise() %>%
  mutate(lifesat_composite  = mean(c(lifesat_satlife,
    lifesat_ideal), na.rm = TRUE ),

    support_composite = mean(c(support_help,support_rnoguidance,support_turnto), na.rm=TRUE )) |>  ungroup() |>

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
  )) |>






## look at religious change


dt_exposure <- dt_start2 |>

  # select baseline year and exposure year
  filter(wave == "2018" | wave == "2019") |>

  # select variables of interest
  select(id, wave, religion_religious,  eth_cat) |>

  # the categorical variable needs to be numeric for us to use msm package to investigate change
  mutate(religion_religious_n = as.numeric(religion_religious)) |>
  droplevels()



# check change

out <- msm::statetable.msm(round(hours_exercise_coarsen_n, 0), id, data = dt_exposure)



# names
state_names <- c("Not Religious", "Religious")


#
transition_table(out, state_names)


#interpretation
cat(t_tab_m$explanation)
print(t_tab_m$table)



# make wide data


# here are some plausible baseline confounders
baseline_vars = c(
  "edu",
  "male",
  "eth_cat",
  "employed",
  "gen_cohort",
  "nz_dep2018", # nz dep
 # "nzsei13", # occupational prestige
  "partner",
  "parent",
  "pol_orient",
 # "rural_gch2018",
   "urban", # use the two level urban varaible.
  "agreeableness",
  "conscientiousness",
  "extraversion",
  "honesty_humility",
  "openness",
  "neuroticism",
  "modesty")


## Step 2, select the exposure variable.  This is the "cause"
exposure_var = c("religion_religious")


## step 3. select the outcome variable.  These are the outcomes.
outcome_vars_reflective = c("lifesat_composite")



# prepare your data for analysis

dt_prepare <-
  create_wide_data(
    dat_long = dt_start2,
    baseline_vars = baseline_vars,
    exposure_var = exposure_var,
    outcome_vars = outcome_vars_reflective
  )


# make your table


# get data into shape
dt_new <- dt_prepare %>%
  select(starts_with("t0")) %>%
  rename_all( ~ stringr::str_replace(., "^t0_", "")) %>%
  mutate(wave = factor(rep("baseline", nrow(dt_prepare)))) |>
  janitor::clean_names(case = "screaming_snake")


# create a formula string
baseline_vars_names <- dt_new %>%
  select(-WAVE) %>%
  colnames()

table_baseline_vars <-
  paste(baseline_vars_names, collapse = "+")

formula_string_table_baseline <-
  paste("~", table_baseline_vars, "|WAVE")

table1::table1(as.formula(formula_string_table_baseline),
               data = dt_new,
               overall = FALSE)



## THEN FOR THE ANALYSIS.... tba



dt <- dt_prepare|>
  mutate(id = factor(1:nrow(dt_prepare))) |>
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


