#' \%~\%
#'
#' Model update operator. It functions in place of the model update function.
#' In the context of %~% and update.formula, the  . means ‘what was previously
#' in this part of the formula’.
#'
#' @return Updated formula.
#'
#' @param original_formula the original formula to use.
#'
#' @param addition Additions / removals / changes to the formula.
#'
#' @examples
#' ## Add Sepal.Width to model2.
#' model <- setosa ~ Sepal.Length
#' ## Add Sepal.Width to the existing model.
#' model2 <- model %~% ~ . + Sepal.Width
#' ## Note the brackets are necessary in case of LHS substitution, otherwise
#' ## the whole formula is consumed without evaluation.
#' model3 <- model %~% (Sepal.Width ~ .)
#' @export
#'

"%~%" <- function(original_formula, addition) {
  update(original_formula, addition)
}

#' strip_glm
#'
#' Strip the glm model, leaving only the necessary bits for doing a predict.
#'
#' @param cm The glm model to strip.
#'
#' @return Stripped glm model.
#'
#' @examples
#' data(iris)
#' iris$setosa <- ifelse(iris$Species == 'setosa', 1, 0)
#' sglm <- strip_glm(glm(setosa ~ Sepal.Length + Sepal.Width, data = iris))
#'
#' @export
#'
strip_glm <- function(cm) {
  cm$y <- c()
  cm$model <- c()

  cm$residuals <- c()
  cm$fitted.values <- c()
  cm$effects <- c()
  cm$qr$qr <- c()
  cm$linear.predictors <- c()
  cm$weights <- c()
  cm$prior.weights <- c()
  cm$data <- c()


  cm$family$variance <- c()
  cm$family$dev.resids <- c()
  cm$family$aic <- c()
  cm$family$validmu <- c()
  cm$family$simulate <- c()
  attr(cm$terms,".Environment") <- c()
  attr(cm$formula,".Environment") <- c()

  cm
}

#' robust_lm
#'
#' Estimates an lm model but uses robust standard errors by default. Note that
#' for functions for which a robust_lm method is not defined (which is, in
#' effect all of them with the exception of broom's tidy), the fallback class is
#' lm, which will yield standard (non-robust) standard errors.
#'
#' @param ... Arguments to pass to lm.
#'
#' @examples
#' data(iris)
#' iris$setosa <- ifelse(iris$Species == 'setosa', 1, 0)
#' lm_model <- robust_lm(setosa ~ Sepal.Length + Sepal.Width, data = iris)
#'
#' @export
#'
robust_lm <- function(...) {
  model <- lm(...)

  ctest <- lmtest::coeftest(
    model, vcov = sandwich::vcovHC(model, type = 'HC')
  )
  model$robSE <- ctest[, 2]
  model$robP <- ctest[, 4]

  class(model) <- c('robust_lm', 'lm')
  model
}

#' @importFrom broom tidy
tidy.robust_lm <- function(
  x,
  conf.int = FALSE,
  conf.level = 0.95,
  exponentiate = FALSE,
  quick = FALSE,
  ...
) {
  require_package('broom')

  if (quick) {
    co <- stats::coef(x)
    ret <- data.frame(term = names(co), estimate = unname(co))
    return(process_lm(ret, x, conf.int = FALSE, exponentiate = exponentiate))
  }

  s <- summary(x)
  ret <- broom:::tidy.summary.lm(s)
  process <- broom:::process_lm(
    ret,
    x,
    conf.int = conf.int,
    conf.level = conf.level,
    exponentiate = exponentiate
  )
  process$std.error <- x$robSE
  process$p.value <- x$robP
  process
}
