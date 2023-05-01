# for week 8


# we can do this contrasts for subgroups. Here lets compare Māori with NZ Europeans.

# find names
levels_list <- unique(df[["t0_eth_cat"]])
levels_list


# simulate effect as modified in māori
sim_estimand_r_m <- sim_ame(sim_model_r2,
                            var = X_pc,
                            cl = cores,
                            subset = t0_eth_cat == "māori",
                            verbose = FALSE)

# simulate effect as modified in nz euro
sim_estimand_r_e <- sim_ame(sim_model_r2,
                            var = X_pc,
                            cl = cores,
                            subset = t0_eth_cat == "euro",
                            verbose = FALSE)

# create risk differences
sim_estimand_r_m <- transform(sim_estimand_r_m, RD = `E[Y(low)]` - `E[Y(high)]`)
sim_estimand_r_e <- transform(sim_estimand_r_e, RD = `E[Y(low)]` - `E[Y(high)]`)

# view
summary(sim_estimand_r_e)
summary(sim_estimand_r_m)


# consider in one table
names(sim_estimand_r_e) <- paste(names(sim_estimand_r_e), "e", sep = "_")

names(sim_estimand_r_m) <- paste(names(sim_estimand_r_m), "m", sep = "_")

# view
sim_estimand_r_m
sim_estimand_r_e

# combine tables
est_r <- cbind(sim_estimand_r_e, sim_estimand_r_m)

# create the difference in contrasts
est_r <- transform(est_r, `RD_m - RD_e` = RD_m - RD_e)

summary(est_r). # we do not find a difference in differences.



## However we can do better by using propensity score matching.  Let's consider this next.


# again make sure that our data object is a dataframe

dt_ref <- data.frame( dt_ref )



##  we find there are the baseline variables, however this time without the exposure at baseline (we are modelling the exposure)

baseline_vars_reflective_propensity = dt_ref |>
  dplyr::select(starts_with("t0"), -t0_eth_cat) |> colnames()

# view
baseline_vars_reflective_propensity

# I have created a function for matching. having done this before I have found "energy" matching works best. however this method is slow.

dt_match <- match_mi_general(
  data = dt_ref,
  X = X_pc,
  baseline_vars = baseline_vars_reflective_propensity,
  subgroup = "t0_eth_cat",
  estimand = "ATE",
  #focal = "high",
  method = "energy"
)

# save the matched data it has taken us a while
saveRDS(dt_match, here::here("data", "dt_match"))




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
