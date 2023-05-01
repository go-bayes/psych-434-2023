
# PSYCH 434 WEEK 7: INTRODUCTION TO CAUSAL INFERENCE
# below




# save
arrow::write_parquet(my_data_updated, here::here("data", "nzavs_dat_synth_t10_t12"))


nzavs_synth <-
  arrow::read_parquet(here::here("data", "nzavs_dat_synth_t10_t12"))

str(nzavs_synth)

## select variables for domains of outcome ## always to be determined with expert


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



exposure_var = c("perfectionism")


outcome_vars_reflective = c(
  # "gratitude",
  # "pwi_health",
  # "pwi_relationships",
  # "pwi_security",
  # "pwi_standardliving",
  # "lifesat_satlife",
  # "lifesat_ideal",
  "meaning_purpose",
  "meaning_sense"
)


# make sure not to include
exclude_vars = c("year_measured")





prep_reflective <-
  create_wide_data(
    dat_long = nzavs_synth, #nzavs_synth,
    baseline_vars = baseline_vars,
    exposure_var = exposure_var,
    outcome_vars = outcome_vars_reflective
  )

# check
str(prep_reflective)

# prep_reflective <- as.data.frame(prep_reflective)

str(prep_reflective)


# create composite scores for the constructs
dt_ref <- prep_reflective |>
  mutate(id = factor(1:nrow(prep_reflective))) |>
  mutate(t1_perfectionism = round(t1_perfectionism)) |>
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
    t0_gen_cohort = as.factor(t0_gen_cohort)  ) |>
  group_by(id) |>
  # dplyr::mutate(t2_pwi = mean(
  #   c(
  #     t2_pwi_health,
  #     t2_pwi_relationships,
  #     t2_pwi_security,
  #     t2_pwi_standardliving
  #   ),
  #   na.rm = TRUE
  # )) |>
  # dplyr::mutate(t2_lifesat = mean(c(t2_lifesat_satlife,
  #                                   t2_lifesat_ideal),
#                                 na.rm = TRUE)) |>
dplyr::mutate(t2_meaning = mean(c(t2_meaning_purpose,
                                  t2_meaning_sense),
                                na.rm = TRUE)) |>
  ungroup() |>
  # transform numeric variables into z scores (improves estimation)
  dplyr::mutate(across(where(is.numeric), ~ scale(.x), .names = "{col}_z")) %>%
  # select only factors and numeric values that are z-scores
  select(id,
         where(is.factor),
         t1_perfectionism,# for comparison
         ends_with("_z"),) |>
  # tidy data frame so that the columns are ordered by time (useful for more complex models)
  relocate(id, .before = starts_with("t1_"))  %>%
  relocate(starts_with("t0_"), .before = starts_with("t1_"))  %>%
  relocate(starts_with("t2_"), .after = starts_with("t1_"))

levels(dt_ref$t1_perfectionism_coarsen)
dt_ref$t1_perfectionism_coarsen <-
  factor(
    dt_ref$t1_perfectionism_coarsen,
    levels = c("[1,4)", "[4,5)", "[5,7]"),
    labels = c("low", "medium", "high"),
    ordered = TRUE
  )

# view
library(skimr)
skimr::skim(dt_ref)

min(dt_ref$t1_perfectionism)

# Models
X = "t1_perfectionism_z"
X_pc <- "t1_perfectionism_coarsen"

# Get baseline names
baseline_vars_reflective_cont = dt_ref |>
  dplyr::select(starts_with("t0")) |> colnames() # note we added quartiles for this analysis to demonstrate interest of different causal estimands


baseline_vars_reflective_propensity = dt_ref |>
  dplyr::select(starts_with("t0"), -t0_eth_cat) |> colnames()

baseline_vars_reflective_cont

formula_str_prop <- paste(X_pc, "~", paste(baseline_vars_reflective_propensity, collapse = "+"))

Y = "t2_meaning_z"
X = "t1_perfectionism_z"
df = as.data.frame(dt_ref)
baseline_vars = baseline_vars_reflective_cont

baseline_vars

levels_list <- unique(df[["t0_eth_cat"]])
levels_list
# matching
# match_mi <- match_mi(data = df, X, basline_vars = baseline_vars_reflective_propensity,
#                              estimand = "ATE",  method = "ebal", subgroup = "t0_eth_cat")


baseline_vars_reflective_propensity
# tried cbps, ps, bart, and energy. of these energy worked best
library("CBPS")

dt_ref <- data.frame( dt_ref )
dt_match <- match_mi_general(
  data = dt_ref,
  X = X_pc,
  baseline_vars = baseline_vars_reflective_propensity,
  subgroup = "t0_eth_cat",
  estimand = "ATE",
  #focal = "high",
  method = "energy"
)

saveRDS(dt_match, here::here("data", "dt_match"))

bal.tab(dt_match$euro)
bal.tab(dt_match$māori)
bal.tab(dt_match$asian)
bal.tab(dt_match$pacific)

sum_e <- summary(dt_match$euro)
sum_m <- summary(dt_match$māori)
sum_p <- summary(dt_match$pacific)
sum_a <- summary(dt_match$asian)


plot(sum_e)
plot(sum_m)
plot(sum_p)
plot(sum_a)

love.plot(dt_match$euro, binary = "std", thresholds = c(m = .1))
love.plot(dt_match$māori, binary = "std", thresholds = c(m = .1))
love.plot(dt_match$pacific, binary = "std", thresholds = c(m = .1))
love.plot(dt_match$asian, binary = "std", thresholds = c(m = .1))


# prepare data
dt_ref_e <- subset(dt_ref, t0_eth_cat == "euro")
dt_ref_e$weights <- dt_match$euro$weights

# prepare data
dt_ref_m <- subset(dt_ref, t0_eth_cat == "māori")
dt_ref_m$weights <- dt_match$māori$weights
# prepare data
dt_ref_p <- subset(dt_ref, t0_eth_cat == "pacific")
dt_ref_p$weights <- dt_match$pacific$weights
# prepare data
dt_ref_a <- subset(dt_ref, t0_eth_cat == "asian")
dt_ref_a$weights <- dt_match$asian$weights

# combine
dt_ref_all <- rbind(dt_ref_e, dt_ref_m, dt_ref_p, dt_ref_a)


mod_ref_meaning   <- gcomp_sim(
  df = dt_ref_all,  # note change
  Y = "t2_meaning_z",
  X = X_pc,
  baseline_vars = baseline_vars_reflective_cont,
  treat_1 = "high",
  treat_0 = "medium",
  estimand = "ATE",
  scale = "RD",
  type = "RD",
  nsims = 200,
  cores = 8,
  family = gaussian,
  weights = TRUE,
  continuous_X = FALSE,
  splines = FALSE,
  new_name = "t2_meaning_z (composite)"
)


# marginal effect in everyone
mod_ref_meaning



### SUBGROUP
df = dt_ref_all
Y = "t2_meaning_z"
X = "t1_perfectionism_coarsen"
baseline_vars = baseline_vars_reflective_cont
treat_0 = "medium"
treat_1 = "high"
estimand = "ATT"
scale = "RD"
nsims = 200
family = "gaussian"
continuous_X = FALSE
splines = FALSE
cores = parallel::detectCores()

formula_str <- paste(Y, "~", X , "*", "(", paste(baseline_vars, collapse = "+"), ")")



## Subgroup Euro
df = dt_ref_all
Y = "t2_meaning_z"
X = "t1_perfectionism_coarsen"
baseline_vars = baseline_vars_reflective_cont
treat_0 = "medium"
treat_1 = "high"
estimand = "ATE"
scale = "RD"
nsims = 200
family = "gaussian"
continuous_X = FALSE
splines = FALSE
cores = parallel::detectCores()

formula_str
# fit model
fit_all_all  <- glm(
  as.formula(formula_str),
  weights = weights,
  # weights = if (!is.null(weight_var)) weight_var else NULL,
  family = family,
  data = df)

# simulate coefficients
sim_model_all <- sim(fit_all_all, n = nsims)


# simulate effect as modified in europeans
sim_estimand_all_e <- sim_ame(sim_model_all,
                              var = X,
                              cl = cores,
                              subset = t0_eth_cat == "euro",
                              verbose = FALSE)

sim_estimand_all_e <- transform(sim_estimand_all_e, RD = `E[Y(low)]` - `E[Y(high)]`)
sim_estimand_all_e


# simulate effect as modified in māori
sim_estimand_all_m <- sim_ame(sim_model_all,
                              var = X,
                              cl = cores,
                              subset = t0_eth_cat == "māori",
                              verbose = FALSE)

sim_estimand_all_m <- transform(sim_estimand_all_m, RD = `E[Y(low)]` - `E[Y(high)]`)



summary(sim_estimand_all_e)
summary(sim_estimand_all_m)

names(sim_estimand_all_e) <- paste(names(sim_estimand_all_e), "e", sep = "_")

names(sim_estimand_all_m) <- paste(names(sim_estimand_all_m), "m", sep = "_")

sim_estimand_all_m
sim_estimand_all_e

est_all <- cbind(sim_estimand_all_m, sim_estimand_all_e)
est_all <- transform(est_all, `RD_m - RD_e` = RD_m - RD_e)

summary(est_all)




## only regression


# fit model
fit_all_r <- glm(
  as.formula(formula_str),
  #  weights = weights,  # remove weights
  family = family,
  data = df)

# simulate coefficients
sim_model_r <- sim(fit_all_r, n = nsims)


# simulate effect as modified in europeans
sim_estimand_r_e <- sim_ame(sim_model_r,
                            var = X,
                            cl = cores,
                            subset = t0_eth_cat == "euro",
                            verbose = FALSE)

sim_estimand_r_e <- transform(sim_estimand_r_e, RD = `E[Y(low)]` - `E[Y(high)]`)
sim_estimand_r_e


# simulate effect as modified in māori
sim_estimand_r_m <- sim_ame(sim_model_r,
                            var = X,
                            cl = cores,
                            subset = t0_eth_cat == "māori",
                            verbose = FALSE)

sim_estimand_r_m <- transform(sim_estimand_r_m, RD = `E[Y(low)]` - `E[Y(high)]`)



summary(sim_estimand_r_e)
summary(sim_estimand_r_m)

names(sim_estimand_r_e) <- paste(names(sim_estimand_r_e), "e", sep = "_")

names(sim_estimand_r_m) <- paste(names(sim_estimand_r_m), "m", sep = "_")

sim_estimand_r_m
sim_estimand_r_e

est_r <- cbind(sim_estimand_r_e, sim_estimand_r_m)
est_r <- transform(est_r, `RD_m - RD_e` = RD_m - RD_e)

summary(est_all)
summary(est_r)


# only propensity score

# fit model
fit_all_p <- glm(
  t2_meaning_z  ~ t1_perfectionism_coarsen * t0_eth_cat,
  weights = weights,  # remove weights
  family = family,
  data = df)

# simulate coefficients
sim_model_p <- sim(fit_all_p, n = nsims)


# simulate effect as modified in europeans
sim_estimand_p_e <- sim_ame(sim_model_p,
                            var = X,
                            cl = cores,
                            subset = t0_eth_cat == "euro",
                            verbose = FALSE)

sim_estimand_p_e <- transform(sim_estimand_p_e, RD = `E[Y(low)]` - `E[Y(high)]`)
sim_estimand_p_e


# simulate effect as modified in māori
sim_estimand_p_m <- sim_ame(sim_model,
                            var = X,
                            cl = cores,
                            subset = t0_eth_cat == "māori",
                            verbose = FALSE)

sim_estimand_p_m <- transform(sim_estimand_p_m, RD = `E[Y(low)]` - `E[Y(high)]`)



summary(sim_estimand_p_e)
summary(sim_estimand_p_m)

names(sim_estimand_p_e) <- paste(names(sim_estimand_p_e), "e", sep = "_")

names(sim_estimand_p_m) <- paste(names(sim_estimand_p_m), "m", sep = "_")


est_p <- cbind(sim_estimand_p_e, sim_estimand_p_m)
est_p <- transform(est, `RD_m - RD_e` = RD_m - RD_e)

summary(est_all)
summary(est_r)
summary(est_p)



## DO THE SAME WITH LOW AND MEDIUM

# simulate coefficients
sim_model_all <- sim(fit_all_all, n = nsims)


# simulate effect as modified in europeans
sim_estimand_all_e <- sim_ame(sim_model_all,
                              var = X,
                              cl = cores,
                              subset = t0_eth_cat == "euro",
                              verbose = FALSE)

sim_estimand_all_e_lo <- transform(sim_estimand_all_e, RD = `E[Y(low)]` - `E[Y(medium)]`)



# simulate effect as modified in māori
sim_estimand_all_m <- sim_ame(sim_model_all,
                              var = X,
                              cl = cores,
                              subset = t0_eth_cat == "māori",
                              verbose = FALSE)

sim_estimand_all_m_lo <- transform(sim_estimand_all_m, RD = `E[Y(low)]` - `E[Y(medium)]`)



summary(sim_estimand_all_e_lo)
summary(sim_estimand_all_m_lo)

names(sim_estimand_all_e_lo) <- paste(names(sim_estimand_all_e_lo), "e", sep = "_")

names(sim_estimand_all_m_lo) <- paste(names(sim_estimand_all_m_lo), "m", sep = "_")


est_all_lo <- cbind(sim_estimand_all_e_lo, sim_estimand_all_m_lo)
est_all_lo <- transform(est_all_lo, `RD_m - RD_e` = RD_m - RD_e)

summary(est_all)
summary(est_all_med)

# reveals a strong sensitivity
summary(est_all_lo)





## DO THE SAME WITH MEDIUM AND HIGH

# simulate coefficients
sim_model_all <- sim(fit_all_all, n = nsims)


# simulate effect as modified in europeans
sim_estimand_all_e <- sim_ame(sim_model_all,
                              var = X,
                              cl = cores,
                              subset = t0_eth_cat == "euro",
                              verbose = FALSE)

sim_estimand_all_e_med <- transform(sim_estimand_all_e, RD = `E[Y(medium)]` - `E[Y(high)]`)



# simulate effect as modified in māori
sim_estimand_all_m <- sim_ame(sim_model_all,
                              var = X,
                              cl = cores,
                              subset = t0_eth_cat == "māori",
                              verbose = FALSE)

sim_estimand_all_m_med <- transform(sim_estimand_all_m, RD = `E[Y(medium)]` - `E[Y(high)]`)



summary(sim_estimand_all_e_med)
summary(sim_estimand_all_m_med)

names(sim_estimand_all_e_med) <- paste(names(sim_estimand_all_e_med), "e", sep = "_")

names(sim_estimand_all_m_med) <- paste(names(sim_estimand_all_m_med), "m", sep = "_")


est_all_med <- cbind(sim_estimand_all_m_med, sim_estimand_all_e_med)
est_all_med <- transform(est_all_med, `RD_m - RD_e` = RD_m - RD_e)

summary(est_all)
summary(est_all_med)




## multi-level model

str(nzavs_synth)

dt_ml <- nzavs_synth |>
  mutate(time = as.numeric(wave)-1)|>
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
         perfectionism,# for comparison
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

formula_str <- paste(Y_ml, "~",   "time", "*",  "(", X_ml , "*", "(", paste(baseline_vars_ml, collapse = "+"), ")", "+", "(1|id)", ")")

formula_str

library(lme4)
as.formula(formula_str)

model_ml <- lmer(as.formula(formula_str), data = dt_ml)


tab_ml <- parameters::model_parameters(model_ml, effects = "fixed")

plot(tab_ml)


library(ggeffects)


graph_ml <- plot(
  ggeffects::ggpredict(model_ml, terms = c("time", "perfectionism_z","eth_cat"))
)

graph_ml

# create a wide data set
create_wide_data <-
  function(dat_long,
           baseline_vars,
           exposure_var,
           outcome_vars,
           exclude_vars = c()) {
    require(tidyverse)
    # Add the 'time' column to the data
    data_with_time <- dat_long %>%
      mutate(time = as.numeric(wave) - 1) %>%
      arrange(id, time)
    
    # Filter the data based on the time condition
    data_filtered <- data_with_time %>%
      filter(time >= 0)
    
    # Create the wide data frame
    wide_data <- data_filtered %>%
      dplyr::select(-exclude_vars)  %>%  # Exclude specified variables
      pivot_wider(
        id_cols = id,
        names_from = time,
        values_from = -c(id, time),
        names_glue = "t{time}_{.value}",
        names_prefix = "t"
      )
    
    # Define a custom function to filter columns based on conditions
    custom_col_filter <- function(col_name) {
      if (startsWith(col_name, "t0_")) {
        return(col_name %in% c(
          paste0("t0_", baseline_vars),
          paste0("t0_", exposure_var),
          paste0("t0_", outcome_vars)
        ))
      } else if (startsWith(col_name, "t1_")) {
        return(col_name %in% paste0("t1_", exposure_var))
      } else if (grepl("^t[2-9][0-9]*_", col_name)) {
        return(col_name %in% paste0("t2_", outcome_vars))
      } else {
        return(FALSE)
      }
    }
    
    # Apply the custom function to select the desired columns
    wide_data_filtered <- wide_data %>%
      dplyr::select(id, which(sapply(
        colnames(wide_data), custom_col_filter
      ))) %>%
      dplyr::relocate(starts_with("t0_"), .before = starts_with("t1_"))  %>%
      arrange(id)
    
    # Extract unique time values from column names
    time_values <-
      gsub("^t([0-9]+)_.+$", "\\1", colnames(wide_data_filtered))
    time_values <- time_values[grepl("^[0-9]+$", time_values)]
    time_values <- unique(as.numeric(time_values))
    time_values <- time_values[order(time_values)]
    
    # Relocate columns iteratively
    for (i in 2:(length(time_values) - 1)) {
      wide_data_filtered <- wide_data_filtered %>%
        dplyr::relocate(starts_with(paste0("t", time_values[i + 1], "_")), .after = starts_with(paste0("t", time_values[i], "_")))
    }
    
    # Reorder t0_ columns
    t0_column_order <-
      c(
        paste0("t0_", baseline_vars),
        paste0("t0_", exposure_var),
        paste0("t0_", outcome_vars)
      )
    wide_data_ordered <- wide_data_filtered %>%
      select(id, t0_column_order, everything()) %>%
      select(-id)
    
    return(wide_data_ordered)
  }



# general function (work in progress)
match_mi_general <- function(data, X, baseline_vars, estimand, method,  subgroup = NULL, focal = NULL) {
  if (!requireNamespace("WeightIt", quietly = TRUE)) {
    stop("Package 'WeightIt' is required but not installed. Please install it using 'install.packages(\"WeightIt\")'.")
  }
  
  if (!requireNamespace("MatchThem", quietly = TRUE)) {
    stop("Package 'MatchThem' is required but not installed. Please install it using 'install.packages(\"MatchThem\")'.")
  }
  
  data_class <- class(data)
  
  if (!data_class %in% c("mids", "data.frame")) {
    stop("Input data must be either 'mids' or 'data.frame' object")
  }
  
  formula_str <- as.formula(paste(X, "~", paste(baseline_vars, collapse = "+")))
  
  weight_function <- if (data_class == "mids") weightthem else weightit
  
  perform_matching <- function(data_subset) {
    weight_function(
      formula = formula_str,
      data = data_subset,
      estimand = estimand,
      stabilize = TRUE,
      method = method,
      focal = focal,
      # SL.library = c("SL.glm.interaction")
    )
  }
  
  if (is.null(subgroup)) {
    dt_match <- perform_matching(data)
  } else {
    levels_list <- unique(data[[subgroup]])
    
    dt_match_list <- lapply(levels_list, function(level) {
      data_subset <- data[data[[subgroup]] == level, ]
      perform_matching(data_subset)
    })
    
    names(dt_match_list) <- levels_list
    dt_match <- dt_match_list
  }
  
  return(dt_match)
}