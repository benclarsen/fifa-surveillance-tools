


cases_A <- 41; exp_A <- 857 + 3195. # Women 
cases_B <- 31;  exp_B <- 693 + 2753   # Men

## Exact rate CI per group
pt_A <- poisson.test(cases_A, T = exp_A, r = 1)
pt_B <- poisson.test(cases_B, T = exp_B, r = 1)

rate_A <- cases_A / exp_A
rate_B <- cases_B / exp_B

cat("Rate A:", rate_A, "Exact 95% CI:", pt_A$conf.int, "\n")
cat("Rate B:", rate_B, "Exact 95% CI:", pt_B$conf.int, "\n")







## Exact IRR (rate ratio) + CI
## poisson.test(x, T) with length 2 treats it as a two-sample rate comparison
pt_rr <- poisson.test(c(cases_B, cases_A), T = c(exp_B, exp_A), r = 1,
                      alternative = "two.sided")

cat("\nIRR (B vs A):", (rate_B / rate_A), "\n")
cat("Exact 95% CI:", pt_rr$conf.int, "\n")
cat("p-value:", pt_rr$p.value, "\n")





irr_poisson_exact <- function(cases_A, exp_A,
                              cases_B, exp_B,
                              conf.level = 0.95,
                              alternative = c("two.sided", "less", "greater"),
                              scale_rate = 1,
                              labels = c("A", "B")) {
  
  alternative <- match.arg(alternative)
  
  # Basic checks
  if (any(c(cases_A, cases_B) < 0) || any(c(exp_A, exp_B) <= 0)) {
    stop("Cases must be >= 0 and exposure must be > 0.")
  }
  if (length(labels) != 2) stop("labels must be length 2, e.g., c('A','B').")
  
  # Rates (optionally scaled)
  rate_A <- cases_A / exp_A * scale_rate
  rate_B <- cases_B / exp_B * scale_rate
  
  # Exact test for rate ratio (IRR): group B vs A
  # poisson.test with x=c(x1,x2), T=c(T1,T2) performs an exact comparison of rates
  # The reported estimate is rate1/rate2, so with (B, A) it matches IRR = rate_B / rate_A
  pt_rr <- poisson.test(
    x = c(cases_B, cases_A),
    T = c(exp_B, exp_A),
    r = 1,
    alternative = alternative,
    conf.level = conf.level
  )
  
  # Exact CIs for each group rate (optional but often useful)
  pt_A <- poisson.test(cases_A, T = exp_A, conf.level = conf.level)
  pt_B <- poisson.test(cases_B, T = exp_B, conf.level = conf.level)
  
  out <- list(
    input = list(
      cases_A = cases_A, exp_A = exp_A,
      cases_B = cases_B, exp_B = exp_B,
      labels = labels,
      scale_rate = scale_rate
    ),
    rates = setNames(
      c(rate_A, rate_B),
      labels
    ),
    rate_CI_exact = setNames(
      list(
        A = pt_A$conf.int * scale_rate,
        B = pt_B$conf.int * scale_rate
      ),
      labels
    ),
    IRR = unname((cases_B / exp_B) / (cases_A / exp_A)),
    IRR_CI_exact = unname(pt_rr$conf.int),
    p_value = pt_rr$p.value,
    alternative = alternative,
    conf_level = conf.level,
    poisson_test_object = pt_rr
  )
  
  return(out)
}

res <- irr_poisson_exact(

  cases_B = 41, exp_B = (857 + 2941), # Men
  cases_A = 31,  exp_A = (693 + 2570),   # Women
  
  scale_rate = 1000,                # rates per 1000 exposure units
  conf.level = 0.95,
  alternative = "two.sided",
  labels = c("Women", "Men")
)

res$rates
res$rate_CI_exact
res$IRR
res$IRR_CI_exact
res$p_value

24/3797.8*1000

