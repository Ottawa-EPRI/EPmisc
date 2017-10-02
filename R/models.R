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
#' Estimate an lm model but uses robust standard errors by default.
#'
#' @param robust.se Whether to use robust standard errors.
#'
#' @examples
#' data(iris)
#' iris$setosa <- ifelse(iris$Species == 'setosa', 1, 0)
#' lm_model <- robust_lm(setosa ~ Sepal.Length + Sepal.Width, data = iris)
#'
#' @export
#'
robust_lm <- function(..., robust.se = TRUE) {
  # Kill if sandwich or lmtest are not installed.
  if (!requireNamespace('sandwich', quietly = TRUE)|
      !requireNamespace('lmtest', quietly = TRUE)) {
    stop(
      "Sandwich and lmtest are needed to use robust_lm. Please install them.",
      call. = FALSE
    )
  }

  model <- lm(...)

  if (robust.se) {
    ctest <- lmtest::coeftest(
      model, vcov = sandwich::vcovHC(model, type = 'HC')
    )
    model$robSE <- ctest[, 2]
    model$robP <- ctest[, 4]
  }

  model
}
