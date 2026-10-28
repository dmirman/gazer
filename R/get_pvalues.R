#' Get p-values for a multilevel regression model
#'
#' Takes a model fit with lmer and (optionally) an estimation method, returns a
#' data frame containing fixed effect coefficient estimates, SE, t-values, and
#' p-values. Can use normal approximation, Kenward-Roger approximation for
#' degrees of freedom, or both. If Kenward-Roger approximation is used, result
#' also contains approximated degrees of freedom.
#'
#' @param model a merMod model object fit by lmer
#' @param method p-value estimation method, defaults to "normal", can also be
#'   "KR" or "all"
#' @return data frame containing fixed effect estimates, SE, t-values, (degrees
#'   of freedom if appropriate) and p-values.
#' @export
#' @examples
#' require(lme4)
#' model <- lmer(Reaction ~ Days + (Days | Subject), sleepstudy)
#' get_pvalues(model)
#' if (requireNamespace("pbkrtest", quietly = TRUE)) get_pvalues(model, "KR")
get_pvalues <- function(model, method = "normal") {
  # Extract fixed effect coefficients from model
  coefs <- data.frame(coef(summary(model)))

  # Check/warn if bad method name used
  supported <- c("normal", "KR", "all")
  if (!is.element(method, supported)) {
    warning("Invalid p-value estimation method. Use 'normal', 'KR'",
            "or 'all'. Defaulting to normal approximation.")
    method <- "normal"
  }

  # Apply normal approx if asked for
  if (method %in% c("normal", "all")) {
    coefs$p.normal <- get_pvalues_normal(coefs$t.value)
    coefs$p.normal.star <- get_p_stars(coefs$p.normal)
  }

  # Apply KR if asked for
  if (method %in% c("KR", "all")) {
    #run a minimal KRmodcomp
    if (!requireNamespace("lme4", quietly = TRUE)) {
      stop("get_pvalues(method = \"KR\") needs the lme4 package: install.packages(\"lme4\").", call. = FALSE)
    }
    if (!requireNamespace("pbkrtest", quietly = TRUE)) {
      stop("get_pvalues(method = \"KR\") needs the pbkrtest package: install.packages(\"pbkrtest\").", call. = FALSE)
    }
    restrictionMatrix <- cbind(t(rep(0, length(lme4::fixef(model))-1)), 1)
    kr <- pbkrtest::KRmodcomp(model, restrictionMatrix)
    #get ddf
    coefs$df.KR <- pbkrtest::getKR(kr, "ddf")
    coefs$p.KR <- get_pvalues_kr(coefs$t.value, coefs$df.KR)
    coefs$p.KR.star <- get_p_stars(coefs$p.KR)
  }

  coefs
}

get_pvalues_normal <- function(ts) {
  2 * (1 - pnorm(abs(ts)))
}

get_pvalues_kr <- function(ts, dfs) {
  2 * (1 - pt(abs(ts), dfs))
}

get_p_stars <- function(ps) {
  symnum(ps, na = FALSE, cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1),
         symbols = c("***", "**", "*", ".", " "), legend = FALSE)
}
