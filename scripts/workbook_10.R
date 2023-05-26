## Workbook 10
# Before running this source code, make sure to update to the current version of R, and to update all existing packages.

# functions
source("https://raw.githubusercontent.com/go-bayes/templates/main/functions/funs.R")


# experimental functions (more functions)
source(
  "https://raw.githubusercontent.com/go-bayes/templates/main/functions/experimental_funs.R"
)


#  If you haven't already, you should have created a folder called "data", in your Rstudio project. If not, download this file, add it to your the folder called "data" in your Rstudio project. # "https://www.dropbox.com/s/vwqijg4ha17hbs1/nzavs_dat_synth_t10_t12?dl=0"

# A function we will use for our tables.
tab_ate_subgroup_rd <- function(x,
                                new_name,
                                delta = 1,
                                sd = 1) {
  # Check if required packages are installed
  required_packages <- c("EValue", "dplyr")
  new_packages <-
    required_packages[!(required_packages %in% installed.packages()[, "Package"])]
  if (length(new_packages))
    stop("Missing packages: ", paste(new_packages, collapse = ", "))

  require(EValue)
  require(dplyr)

  # check if input data is a dataframe
  if (!is.data.frame(x))
    stop("Input x must be a dataframe")

  # Check if required columns are in the dataframe
  required_cols <- c("estimate", "lower_ci", "upper_ci")
  missing_cols <- required_cols[!(required_cols %in% colnames(x))]
  if (length(missing_cols) > 0)
    stop("Missing columns in dataframe: ",
         paste(missing_cols, collapse = ", "))

  # Check if lower_ci and upper_ci do not contain NA values
  if (any(is.na(x$lower_ci), is.na(x$upper_ci)))
    stop("Columns 'lower_ci' and 'upper_ci' should not contain NA values")

  x <- x %>%
    dplyr::mutate(across(where(is.numeric), round, digits = 3)) %>%
    dplyr::rename("E[Y(1)]-E[Y(0)]" = estimate)

  x$standard_error <- abs(x$lower_ci - x$upper_ci) / 3.92

  evalues_list <- lapply(seq_len(nrow(x)), function(i) {
    row_evalue <- EValue::evalues.OLS(
      x[i, "E[Y(1)]-E[Y(0)]"],
      se = x[i, "standard_error"],
      sd = sd,
      delta = delta,
      true = 0
    )
    # If E_value is NA, set it to 1
    if (is.na(row_evalue[2, "lower"])) {
      row_evalue[2, "lower"] <- 1
    }
    if (is.na(row_evalue[2, "upper"])) {
      row_evalue[2, "upper"] <- 1
    }
    data.frame(round(as.data.frame(row_evalue)[2,], 3)) # exclude the NA column
  })

  evalues_df <- do.call(rbind, evalues_list)
  colnames(evalues_df) <- c("E_Value", "E_Val_bound")

  tab_p <- cbind(x, evalues_df)

  tab <-
    tab_p |> select(c(
      "E[Y(1)]-E[Y(0)]",
      "lower_ci",
      "upper_ci",
      "E_Value",
      "E_Val_bound"
    ))

  return(tab)
}
