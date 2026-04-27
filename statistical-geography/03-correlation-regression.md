## Correlation and Regression Analysis

::: {.callout-warning}
#### 📘 Syllabus Coverage
| Syllabus | Topic Details |
|----------|---------------|
| **NEP-2020** | Applied in geographical data analysis |
| **UGC NET** | Correlation and Regression Analysis |
:::


[![](https://via.placeholder.com/600x300?text=Course+Module+Video)](#)

[Get the Presentation &nearr;](#) &nbsp; | &nbsp; [Watch the Video &nearr;](#)

::: {.callout-note}
### Key Concepts
* **Correlation**: Measures the strength and direction of the linear relationship between two variables (x and y).
  - *Pearson's Product Moment (r)*: For interval/ratio data. Ranges from -1 to +1.
  - *Spearman's Rank Correlation (Rho)*: For ordinal (ranked) data.
* **Regression**: Predictive modeling. Investigates the dependence of a dependent variable (Y) on one or more independent variables (X).
  - *Linear Regression*: $Y = a + bX$. 'a' is intercept, 'b' is slope (regression coefficient).
  - *Residuals*: The difference between observed and predicted values. Mapping residuals (Spatial Regression) reveals spatial anomalies.
:::

---

### Correlation Analysis — Detailed (NET Notes — Pulakesh Pradhan)

#### Karl Pearson's Correlation Coefficient (r)

> Method for measuring intensity or magnitude of **linear relationship** between two variables

> Also called **Product Moment Correlation Coefficient**

**Formula:**
> r = Σ(x − x̄)(y − ȳ) / √[Σ(x − x̄)² × Σ(y − ȳ)²]

| Value of r | Interpretation |
|:---|:---|
| r = +1 | Perfect positive correlation |
| r = −1 | Perfect negative correlation |
| r = 0 | No linear correlation |
| 0 < r < 1 | Positive correlation |
| −1 < r < 0 | Negative correlation |

#### Spearman's Rank Correlation
> Used when data is in ranks (ordinal scale)

> rs = 1 − [6ΣD² / n(n² − 1)]

---

### Regression Analysis — Detailed (NET Notes)

> *"Regression is stepping back or returning to the average value"*

- Mathematical measure of **average relationship** between two or more variables in terms of original units of data

#### Simple Linear Regression
> Y = a + bX
> Y = dependent variable; X = independent variable
> b = regression coefficient (slope)
> a = intercept

#### Types
- **Simple regression** — one dependent, one independent variable
- **Multiple regression** — one dependent, multiple independent variables
