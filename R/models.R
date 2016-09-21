#' Strip glm model.
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
#' sglm <- stripGlm(glm(setosa ~ Sepal.Length + Sepal.Width, data = iris))
#'
#' @export
#'
stripGlm = function(cm) {
  cm$y = c()
  cm$model = c()

  cm$residuals = c()
  cm$fitted.values = c()
  cm$effects = c()
  cm$qr$qr = c()
  cm$linear.predictors = c()
  cm$weights = c()
  cm$prior.weights = c()
  cm$data = c()


  cm$family$variance = c()
  cm$family$dev.resids = c()
  cm$family$aic = c()
  cm$family$validmu = c()
  cm$family$simulate = c()
  attr(cm$terms,".Environment") = c()
  attr(cm$formula,".Environment") = c()

  cm
}

#' lm model with robust standard errors.
#'
#' Estimate an lm model but uses robust standard errors by default.
#'
#' @param robust.se Whether to use robust standard errors.
#'
#' @examples
#' data(iris)
#' iris$setosa <- ifelse(iris$Species == 'setosa', 1, 0)
#' lmModel <- lm2(setosa ~ Sepal.Length + Sepal.Width, data = iris)
#'
#' @export
#'
lm2 <- function(..., robust.se = TRUE) {
  model <- lm(...)

  if (robust.se) {
    ctest <- sandwich::coeftest(model, vcov = vcovHC(model, type = 'HC'))
    model$robSE <- ctest[, 2]
    model$robP <- ctest[, 4]
  }

  model
}
