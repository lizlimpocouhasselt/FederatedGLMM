#---------------------------------------------------------------
# FAMILY OBJECT FOR POISSON DISTRIBUTION ALLOWING FOR NON-INTEGER VALUES
# (adapted version of the poisson built-in function)
#---------------------------------------------------------------


soft_poisson <- function (link = "log") 
{
  linktemp <- substitute(link)
  if (!is.character(linktemp)) 
    linktemp <- deparse(linktemp)
  okLinks <- c("log", "identity", "sqrt")
  family <- "soft_poisson"
  if (linktemp %in% okLinks) 
    stats <- make.link(linktemp)
  else if (is.character(link)) {
    stats <- make.link(link)
    linktemp <- link
  }
  else {
    if (inherits(link, "link-glm")) {
      stats <- link
      if (!is.null(stats$name)) 
        linktemp <- stats$name
    }
    else {
      stop(gettextf("link \"%s\" not available for %s family; available links are %s", 
                    linktemp, family, paste(sQuote(okLinks), collapse = ", ")), 
           domain = NA)
    }
  }
  variance <- function(mu) mu
  validmu <- function(mu) all(is.finite(mu)) && all(mu > 0)
  # dev.resids <- function(y, mu, wt) {
  #   r <- mu * wt
  #   p <- which(y > 0)
  #   r[p] <- (wt * (y * log(y/mu) - (y - mu)))[p]
  #   2 * r
  # }
  dev.resids <- function(y, mu, wt) {
    -2 * wt * (y * log(mu) - mu)
  }
  
  # aic <- function(y, n, mu, wt, dev) -2 * sum(dpois(y, mu, 
  #                                                   log = TRUE) * wt)
  # aic <- function(y, n, mu, wt, dev) -2 * sum((y*log(mu) - mu - log(factorial(y))) * wt)
  aic <- function(y, n, mu, wt, dev) -2 * sum(y*log(mu)* wt - mu * wt)
  initialize <- expression({
    # if (any(y < 0)) stop("negative values not allowed for the 'Poisson' family")
    n <- rep.int(1, nobs)
    mustart <- pmax(y, 0.1) + 0.1 #added pmax
  })
  simfun <- function(object, nsim) {
    wts <- object$prior.weights
    if (any(wts != 1)) 
      warning("ignoring prior weights")
    ftd <- fitted(object)
    rpois(nsim * length(ftd), ftd)
  }
  structure(list(family = family, link = linktemp, linkfun = stats$linkfun, 
                 linkinv = stats$linkinv, variance = variance, dev.resids = dev.resids, 
                 aic = aic, mu.eta = stats$mu.eta, initialize = initialize, 
                 validmu = validmu, valideta = stats$valideta, simulate = simfun, 
                 dispersion = 1), class = "family")
}