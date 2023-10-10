#' Coverage - Unit and Time
#'
#' The coverage package and associated function provides you with a visual summary of your time and unit coverage.
#' @param fit A fitted object. (Optional if dqta is supplied.)
#' @param data Data to be investigated. If none is supplied, attempts to use same data as "\code{fit}", first by considering its model matrix, and - if variables are missing (such as timevar and unitvar) - by looking for the source data in the global environment. (Optional)
#' @param timevar Time variable.
#' @param timevar.type Type of time variable. One of "numeric" and "date".
#' @param date_breaks In case of timevar.type = "date", specify the breaks for dates on x-axis.
#' @param unitvar Unit variable.
#' @param variable.names Variables to be checked for coverage. If none is supplied, defaults to variables used in \code{fit}, or if fit not provided, all variables in data. (Optional)
#' @param facet.var Variable to facet plot by
#' @param special.NA Variable that if missing will indicate "special" missingness. Can be used to distinguish observations with missing data from time-unit combinations which did not exist or were not considered. (Optional)
#' @param data.frequency Integer specifying the increments between observations. Defaults to 1.
#' @param ... Additional arguments passed to ggplot2's \code{theme} function (Optional)
#' @keywords coverage lm missingness missing NA
#' @export
#' @examples
#' library(WDI)
#' wdi.sample <- WDI(indicator=c("GDPPC" = "NY.GDP.PCAP.KD",
#'                              "services_gdp" = "BG.GSR.NFSV.GD.ZS",
#'                              "agriculture_gdp" = "NV.AGR.TOTL.ZS",
#'                              "telephones" = "IT.TEL.TOTL.P3"),
#'                              start=1970, end=2012,
#'                              country="all")
#'
#' lm.fit <- lm(GDPPC ~ ., data = wdi.sample)
#'
#' coverage(lm.fit)
#'
#' # One may also specify variables explicitly:
#' coverage(timevar = "year",
#'          unitvar = "country",
#'          variable.names = c("GDPPC",
#'                   "services_gdp",
#'                   "agriculture_gdp",
#'                   "telephones"),
#'          data = wdi.sample)
#'


coverage <- function(fit = NULL,
                     data = NULL,
                     timevar,
                     unitvar,
                     timevar.type = "numeric",
                     date_breaks = NULL,
                     variable.names,
                     facet.var = NULL,
                     special.NA = NULL,
                     data.frequency = 1,
                     ...) {

  # Check if data or fit provided
  if (is.null(data)) {
    if (is.null(fit)) {
      stop("Coverage requires you to supply either data or a fitted object.")
    }
  }

  # If not specified, get variables to consider
  if (is.null(variable.names)) {

    # from fitted object:
    if (!is.null(fit)) {
    variable.names <- all.vars(formula(fit))

    } else {
    # or just use all variables in data
    variable.names <- colnames(data)

    }
  }

  if (!is.null(special.NA)) {
    variable.names <- unique(c(variable.names, special.NA))
  }

  # Add timevar and unitvar to list of variables:
  variable.names <- unique(c(variable.names, unitvar, timevar, facet.var))

  #  If not specified, get data to consider from fitted object:
  if (is.null(data)) {

    # Check if all variables in model matrix & special.NA is not selected:
    # (Note: special.NA would otherwise be equal to NA, and thus pointless)
    if (sum(variable.names %in% all.vars(formula(fit))) == length(variable.names) & is.null(special.NA)) {
      if ("Zelig" %in% class(fit)) {
        data <- na.omit(fit$originaldata)
      } else {
        data <- fit$model
      }
    } else { # Else attempt to find source data in parent environment:
      if ("Zelig" %in% class(fit)) {
        data <- as.data.frame(eval(fit$model.call[[3]], envir = parent.env(environment())))
      } else {
        data <- as.data.frame(eval(fit$call[[3]], envir = parent.env(environment())))
      }
    }
  }

  # Currently does not work with tibbles, because of the way subsetting is done
  data <- as.data.frame(data)

  # Check that timevar.type is of an accepted type
  if (timevar.type == "numeric") {
    if (!is.numeric(data[, timevar])) {
      stop("Numeric timevar.type provided, but timevar is not numeric.")
    }
  } else if (timevar.type == "date") {
    if (class(data[, timevar]) != "Date") {
      stop("Date timevar.type provided, but timevar is not numeric.")
    }
  } else {
    stop("timevar.type must be either 'numeric' or 'date'.")
  }

  # Subsetting data frame:
  data <- data[order(data[, timevar]), variable.names]

  # Assessing missingness:
  data$missing <- !complete.cases(data)

  # Check if any complete cases:
  if (sum(!data$missing) == 0) {
    stop("For these variables, your data has no complete cases")
  }

  # Generating a summary data frame detailing the unit and time combinations present in the data (and their number, if data is 3-dimensional):
  if (!is.null(facet.var)) {
    coverage_df <- as.data.frame(table(data[!data$missing, unitvar], data[!data$missing, timevar], data[!data$missing, facet.var]))
    colnames(coverage_df) <- c("Unit", "Time", "Facet", "N")
  } else {
    coverage_df <- as.data.frame(table(data[!data$missing, unitvar], data[!data$missing, timevar]))
    colnames(coverage_df) <- c("Unit", "Time", "N")
  }

  coverage_df[, "Unit"] <- as.character(coverage_df$Unit)

  # Adding special N column if specified:
  if(is.null(special.NA) == FALSE){
    special.NA.df <- unique(data[,c(unitvar, timevar, special.NA)])
    colnames(special.NA.df) <- c("Unit", "Time", "special.NA")
    coverage_df <- merge(coverage_df, special.NA.df, all.x = TRUE, by = c("Time", "Unit"))
    coverage_df$N[is.na(coverage_df$special.NA)] <- NA
  }

  ### Generating and return visual (default):
  library(ggplot2, quietly = TRUE)

  # Parameter tweaked to make things a bit more pretty
  base_size <- 9

  # Eases alphabetic sort
  coverage_df$Unit <- factor(coverage_df$Unit, levels = rev(sort(unique(coverage_df$Unit))))

  if (timevar.type == "numeric") {
    coverage_df$Time <- as.factor(coverage_df$Time)
  } else {
    coverage_df$Time <- as.Date(coverage_df$Time)
  }

  p <- ggplot(coverage_df, aes(Time, Unit)) +
    geom_tile(aes(fill = N), colour = "white") +
    scale_fill_gradient(low = "white", high = "steelblue", na.value = "lightgrey") +
    theme_grey(base_size = base_size) + labs(x = "", y = "")

  if (timevar.type == "numeric") {
    p <- p +
      scale_x_discrete(
        expand = c(0, 0),
        breaks = pretty(as.numeric(as.character(coverage_df$Time)), n = 20)
      )
  } else {
    p <- p +
      scale_x_date(date_breaks = date_breaks)
  }

  p <- p +
    scale_y_discrete(expand = c(0, 0))

  if (!is.null(facet.var)) {
    p <- p + facet_wrap("Facet")
  }

  p <- p +
    theme(legend.position = "none",
        axis.text.x = element_text(size = base_size * 0.8, angle = 330, hjust = 0, colour = "grey50"),
        axis.text.y = element_text(size = 45 / (sqrt(length(unique(coverage_df[, 1]))))),
        plot.margin = unit(c(5, 15, 5, 5), "pt")) +
    theme(...)

  return(print(p))
}

