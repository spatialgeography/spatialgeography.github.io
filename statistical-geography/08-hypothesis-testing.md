## Sampling and Hypothesis Testing

::: {.callout-warning}
#### 📘 Syllabus Coverage
| Syllabus | Topic Details |
|----------|---------------|
| **UGC NET** | Sampling, Sampling Procedure and Hypothesis Testing (chi square, t-test, ANOVA) |
:::


[![](https://via.placeholder.com/600x300?text=Course+Module+Video)](#)

[Get the Presentation &nearr;](#) &nbsp; | &nbsp; [Watch the Video &nearr;](#)

::: {.callout-note}
### Key Concepts
* **Sampling**: Selecting a subset from a population. Probability (Random, Stratified, Systematic, Cluster) vs. Non-probability (Purposive, Snowball, Quota).
* **Hypothesis Testing**: Formulating Null ($H_0$) and Alternative ($H_1$) hypotheses. Type I error (reject true $H_0$) and Type II error (accept false $H_0$). Set significance level ($lpha$, usually 0.05).
* **Standard Error (SE):** Considered as the measure of the **sampling error**. It indicates how much the sample mean is likely to differ from the actual population mean.
* **Parametric Tests** (assume normal distribution):
  - *t-test*: Compares means of two groups.
  - *ANOVA (Analysis of Variance)*: Compares means of three or more groups.
* **Non-Parametric Tests** (distribution-free):
  - *Chi-Square ($\chi^2$) Test*: Tests association between categorical variables (observed vs. expected frequencies). A value of **0** indicates that the Null hypothesis is accepted.
:::



### Statistical Tests — Detailed (NET Notes — Pulakesh Pradhan)

#### Parametric Tests — Assumptions
- Based on a known distribution (usually normal)
- Population parameters are known or estimated
- Data measured on interval or ratio scale



#### 1. T-Test (Student's t-Test)

- **Developed by William Gosset (1908)*
- Also called **Student's t-test** or **Welch's t-test*
- **Small sample test** — when sample size < 30
- Degree of freedom: v = n − 1

**Formula:*
> t = Deviation from population parameter / Standard error of sample statistic

##### Uses
- Test of significance of regression coefficient
- In multiple regression with individual variables
- When population variance is **unknown*
- When population parameters follow normal distribution



#### 2. Z-Test

- **Given by Fisher*
- **Large sample test** — sample size n > 30
- Used when population variance is **known*
- Based on standard normal distribution
- Critical value for 5% significance (two-tailed): **1.96*

##### Uses
- To determine whether two population means are different
- When correlation coefficient of population is zero → use Z-test
- Types: (1) One sample (2) Two sample (3) Location test (4) Maximum likelihood test



#### 3. F-Test (Variance Ratio Test)

- **Given by Fisher*
- Used to compare two independent estimates of population variance
- Small sample test
- Null hypothesis: H₀: S₁² = S₂²

**Formula:*
> F = Larger estimate of variance / Smaller estimate of variance

##### Degrees of Freedom
- v1 = n1 − 1 and v2 = n2 − 1

##### Key Points
- F-test value is **never negative*
- Used for testing overall significance in regression models
- Value lies from **zero to infinity*



#### 4. Chi-Square Test — Detailed

- **Introduced by Karl Pearson*
- **Non-parametric test*
- Chi-square values lie between **0 and infinity*

##### Conditions for Use
- Total frequency and sample size must be large
- Samples must be independent
- Expected frequency should not be small — if < 5, use **pooling technique*



#### 5. ANOVA — Detailed Assumptions
1. Independence of samples
2. Normal population
3. Same population variance
4. Based on qualitative data



### Summary Table of Tests

| Test | Type | Sample Size | Key Use |
|:---|:---|:---|:---|
| **T-test** | Parametric | n < 30 | Means; unknown variance |
| **Z-test** | Parametric | n > 30 | Means; known variance |
| **F-test** | Parametric | Small | Variance comparison |
| **Chi-square** | Non-parametric | Large | Goodness of fit |
| **ANOVA** | Parametric | — | Multiple group comparison |
