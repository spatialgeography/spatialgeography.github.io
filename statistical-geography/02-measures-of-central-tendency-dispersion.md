## Measures of Central Tendency and Dispersion

::: {.callout-warning}
#### 📘 Syllabus Coverage
| Syllabus | Topic Details |
|----------|---------------|
| **NEP-2020** | Basic statistical analysis required for practical geography |
| **UGC NET** | Applications of Measures of Central Tendency, Dispersion and Inequalities |
:::


[![](https://via.placeholder.com/600x300?text=Course+Module+Video)](#)

[Get the Presentation &nearr;](#) &nbsp; | &nbsp; [Watch the Video &nearr;](#)

::: {.callout-note}
### Key Concepts
* **Central Tendency**: Identifying the 'center' of a dataset.
  - *Mean*: Arithmetic average. Sensitive to outliers. Spatial mean (mean center) for spatial data.
  - *Median*: The **score in the middle** of a distribution. Robust to outliers.
  - *Mode*: Most frequent value.
* **Z-score:** Also known as the Standard Score, it is used for data transformation and comparison. The formula is **$Z = (X - \bar{X}) / \sigma$**, where $X$ is the value, $\bar{X}$ is the mean, and $\sigma$ is the standard deviation.
* **Dispersion**: How spread out the data is around the center.
  - *Range*, *Quartile Deviation*.
  - *Standard Deviation (SD)*: Average distance from the mean. Standard distance in spatial analysis.
  - *Coefficient of Variation (CV)*: (SD/Mean)*100. Useful for comparing variability of datasets with different units.
* **Inequalities Measurement**: Lorenz Curve (graphical), Gini Coefficient (numerical, 0 to 1). Gini's coefficient is a widely used technique to show **Income Inequality**.
* **Shape of Distribution:** **Skewness and Kurtosis** are used for determining the **Shape** of a frequency distribution.
* **Normal Distribution:** In a normal distribution, the percentage of the area between the mean $\pm 1 \sigma$ (Standard Deviation) is **68.27%**.
:::

---

### Measures of Central Tendency — Detailed (NET Notes — Pulakesh Pradhan)

> Types: Arithmetic Mean, Median, Mode, Geometric Mean, Harmonic Mean

---

#### 1. Arithmetic Mean (x̄)

> Sum of all observations divided by the number of observations

**Direct Method:**
> x̄ = Σfx / Σf

**Step Deviation Method:**
> x̄ = A + h(Σfd / N)
> A = assumed mean, h = class interval

::: {.callout-tip}
##### Merits
- Easy to calculate
- Based on all observations
- Suitable for further mathematical treatment
- Most stable average (least affected by sampling)

##### Demerits
- Cannot be used for open-end classes
- Cannot be located graphically
- Very much affected by extreme observations
- Not suitable for qualitative data
:::

---

#### Weighted Arithmetic Mean

> Used when all items are not of equal importance
> Xw = (w1x1 + w2x2 + w3x3 + ...) / (w1 + w2 + w3 + ...)

---

#### 2. Median

> L.R. Connor: *"The median is that value of the variable which divides the group into two equal parts"*

- A **positional average**, not based on all items (unlike arithmetic mean)

**Ungrouped Data:**
> Median = Arithmetic mean of two middle terms

**Grouped Data:**
> Median = L + [(N/2 − cf) / f] × h

---

#### 3. Mode

> Value which occurs most frequently; point of highest concentration

**Formula:**
> Mode = L + h × (f1 − f0) / [(f1 − f0) − (f2 − f1)]

**Relation:**
> Mode = Mean − 3(Mean − Median)
> Mode = 3 Median − 2 Mean

---

#### 4. Geometric Mean (GM)

> nth root of the product of n observations

**Formula:**
> GM = (x1 × x2 × x3 × ... × xn)^(1/n)
> GM = Antilog [1/n × Σ log x]

::: {.callout-tip}
##### Merits
- Rigidly defined
- Based on all observations
- Suitable for further mathematical treatment
- Has bias for smaller observations (unlike AM)

##### Demerits
- Not easy to calculate
- If any observation = 0 → GM = 0
- If any observation is negative → GM becomes imaginary

##### Uses
- Useful for averaging ratios, percentages and rates of increase
- Used in construction of Index Numbers
- Useful for river data analysis
:::

---

#### 5. Harmonic Mean (HM)

> Reciprocal of the arithmetic mean of reciprocals of the given observations

**Formula:**
> HM = 1 / [1/n × Σ(1/x)] = 2ab / (a + b)

::: {.callout-tip}
##### Merits
- Rigidly defined
- Based on all observations
- Gives greater weight to smaller observations
- Not affected by sampling fluctuations

##### Demerits
- Not easy to understand or calculate
- Cannot be obtained if any observation = 0
- May not be representative unless smaller items need higher weightage

##### Uses
- Specifically useful for averaging rates and ratios where **time factor is variable** and **distance is constant**
:::

---

#### Relation Among AM, GM, HM

> AM ≥ GM ≥ HM (always)

| Measure | Two terms (a, b) | Three terms (a, b, c) |
|:---|:---|:---|
| **AM** | (a+b)/2 | (a+b+c)/3 |
| **GM** | √(ab) | ∛(abc) |
| **HM** | 2ab/(a+b) | 3abc/(ab+bc+ca) |

---

### Measures of Dispersion — Detailed (NET Notes)

> Dispersion = measures of variation of items in a distribution

#### Characteristics of a Good Measure of Dispersion
- Rigidly defined
- Easy to calculate and understand
- Based on all observations
- Amenable to further mathematical treatment
- Not much affected by sampling fluctuations
- Not much affected by extreme observations

#### Types
1. Range
2. Quartile Deviation (Semi-Inter-Quartile Range)
3. Mean Deviation
4. Standard Deviation
5. Lorenz Curve

---

#### 1. Range

> R = X(max) − X(min)

> Coefficient of Range = [X(max) − X(min)] / [X(max) + X(min)]

::: {.callout-note}
##### Merits
- Based on entire data

##### Demerits
- Varies widely from sample to sample
- Not used for open-end classes
- Very sensitive to sample size

##### Uses
- Stock market functions
- Industrial statistical quality control
- Day-to-day life measurements
- Meteorological department
:::

---

#### 2. Quartile Deviation (QD)

> Inter-Quartile Range = Q3 − Q1

> QD = (Q3 − Q1) / 2

> Coefficient of QD = (Q3 − Q1) / (Q3 + Q1)

::: {.callout-note}
##### Merits
- Easy to understand and calculate
- Uses 50% of data
- Not affected by extreme observations
- **Only** measure of dispersion that can be used with **open-end classes**

##### Demerits
- Ignores starting 25% and ending 25% of data
- Affected by sampling fluctuations
- Not suitable for further mathematical treatment
:::

---

#### 3. Mean Deviation (MD)

> MD = (1/N) × Σ|x − A|

> Coefficient of MD = MD / Average (about which it is calculated)

::: {.callout-note}
##### Merits
- Based on all observations
- More accurate than Range or QD

##### Demerits
- Ignores signs of deviations (mathematically unsound)
- Not satisfactory for skewed distributions
- Rarely used in social studies
- Cannot be computed for open-end classes

##### Uses
- Used in economics and statistics for simplicity
- Useful for computing distribution of personal wealth
- National Bureau of Economic Research (USA) uses it
:::

---

#### 4. Standard Deviation (σ)

> First suggested by **Karl Pearson (1893)**

> Positive square root of the arithmetic mean of squared deviations from the arithmetic mean

> σ = √[1/N × Σ(x − x̄)²]

> C.V. = 100 × σ / x̄

::: {.callout-tip}
##### Merits
- Most important and widely used measure of dispersion
- Based on all observations
- Removes drawback of ignoring signs (uses squared deviations)
- Suitable for further mathematical treatment
- Least affected by sampling fluctuations

##### Demerits
- Not easy to understand for non-mathematical persons
- Gives greater importance to extreme values
:::

##### Interpretation Methods
1. Empirical rule — for bell-shaped (normal) distribution
2. Chebyshev's Theorem
3. Z-score / standard scores

##### Coefficient of Variation (CV)

> CV = (σ / x̄) × 100

- Relative measure of dispersion
- *"Coefficient of variation is the percentage variation in mean, standard deviation being considered as the total variation in the mean"* — Karl Pearson
- Used to compare variability between two different groups

---

#### 5. Lorenz Curve

- Graphic method of studying **dispersion in a distribution**
- Both size of items (values of variable) and frequencies are cumulated
- Provides **relative** idea of dispersion compared with the line of equal distribution
- Cannot immediately show what % of persons corresponds to a given % of items

##### Gini Coefficient
- Based on the coefficient of mean difference
- **G = Δ1 / 2X̄**
- Varies from **0 to 1**
- G = 0 → perfect equality
- G increases with increasing inequality
- Gini = ratio of area of concentration to total area of lower triangle below line of equal distribution

---

### Normal Distribution — Detailed (NET Notes)

> Also known as **Bell Curve**

> In normal distribution: **Mean = Median = Mode**

::: {.callout-note}
#### Properties
- Symmetric about the mean
- Bell-shaped
- Total area under curve = 1
- 68% data within ±1σ
- 95% data within ±2σ
- 99.7% data within ±3σ (Empirical Rule)
:::
