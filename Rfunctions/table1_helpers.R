##### p-value function
pvalue <- function(x, ...) {
  y <- unlist(x)
  g <- factor(rep(1:length(x), times = sapply(x, length)))
  
  if (is.numeric(y)) {
    # Check for normality using Shapiro-Wilk test on each group
    groups <- split(y, g)
    normal <- all(sapply(groups, function(v) {
      length(v) >= 3 && shapiro.test(v)$p.value > 0.05
    }))
    
    if (length(levels(g)) > 2) {
      # More than 2 groups → ANOVA or Kruskal-Wallis
      if (normal) {
        fit <- aov(y ~ g)
        p <- summary(fit)[[1]][["Pr(>F)"]][1]
        test_used <- "ANOVA"
      } else {
        p <- kruskal.test(y ~ g)$p.value
        test_used <- "Kruskal-Wallis"
      }
    } else {
      # Two groups → t-test or Wilcoxon
      if (normal) {
        p <- t.test(y ~ g)$p.value
        test_used <- "t-test"
      } else {
        p <- wilcox.test(y ~ g)$p.value
        test_used <- "Wilcoxon"
      }
    }
    
  } else {
    # For categorical data: decide between chi-squared and Fisher's exact
    tbl <- table(y, g)
    
    # Handle empty or degenerate tables gracefully
    if (all(tbl == 0) || nrow(tbl) < 2 || ncol(tbl) < 2) {
      p <- NA
      test_used <- "Invalid table"
    } else {
      chi <- suppressWarnings(chisq.test(tbl, correct = FALSE))
      expected <- chi$expected
      
      if (all(expected >= 5)) {
        p <- chi$p.value
        test_used <- "Chi-squared"
      } else {
        p <- fisher.test(tbl, simulate.p.value=TRUE)$p.value
        test_used <- "Fisher"
      }
    }
  }
  
  # Return both formatted p-value and the test used
  c("", paste0(sub("<", "&lt;", format.pval(p, digits = 3, eps = 0.001)),
               " (", test_used, ")"))
}



#### Render continuous
my.render.cont <- function(x) {
  with(stats.apply.rounding(stats.default(x), digits=3), {
    c("", 
      "Mean ± SD" = sprintf("%s ± %s", MEAN, SD),
      "Median [IQR]" = sprintf("%s [%s, %s]", MEDIAN, Q1, Q3))
  })
}
