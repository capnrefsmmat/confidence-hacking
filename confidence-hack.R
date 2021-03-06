
library(assertthat)

## Take a data frame and return a vector of c(p value, CI.lower, CI.upper,
## effect size) from the analysis of the given formula. Take only the first
## n rows of dataset. Formula terms should be columns in the dataset.
##
## The p value, CI, and effect size are calculated for the effect of the
## first term in the formula, so any interactions or covariates should be
## added as later terms.
##
## Intercepts are assumed to be included in all fits.
analyze <- function(dataset, formula, n) {
    assert_that(n <= nrow(dataset))

    fit <- lm(formula, data=dataset[1:n,])

    # get p value and coefficient of first term that's not the intercept
    p <- summary(fit)$coefficients[2, "Pr(>|t|)"]
    effect.size <- coef(fit)[2]

    CIs <- confint(fit)

    return(c(p, CIs[2,1], CIs[2,2], effect.size))
}

## Run analyze for several different formulas and return the result with the
## best p value.
p.hack <- function(dataset, formulas, n) {
    assert_that(is.list(formulas))

    results <- matrix(nrow=length(formulas), ncol=4)

    for (i in seq_along(formulas)) {
        results[i,] <- analyze(dataset, formulas[[i]], n)
    }

    best.result <- which.min(results[,1])
    return(results[best.result,])
}

## Run hack.fn for several increasing sample sizes, stopping when condition.fn
## reports the result is significant enough.
n.hack <- function(dataset, formulas, ns, hack.fn, condition.fn) {
    for (n in ns) {
        result <- hack.fn(dataset[1:n,], formulas, n)

        if (condition.fn(result)) { break }
    }

    return(result)
}
