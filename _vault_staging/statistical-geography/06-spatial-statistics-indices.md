## Spatial Statistics and Morphometry

::: {.callout-warning}
#### 📘 Syllabus Coverage
| Syllabus | Topic Details |
|----------|---------------|
| **NEP-2020** | Nearest Neighbour, Slope Analysis (Wentworth), Agricultural indices |
| **UGC NET** | Morphometric Analysis, Measurement of Indices |
:::


[![](https://via.placeholder.com/600x300?text=Course+Module+Video)](#)

[Get the Presentation &nearr;](#) &nbsp; | &nbsp; [Watch the Video &nearr;](#)

::: {.callout-note}
### Key Concepts
* **Nearest Neighbour Analysis (NNA)**: Measures spatial arrangement of points. Nearest Neighbour Index (R). R=0 (clustered), R=1 (random), R=2.15 (perfectly uniform/regular).
* **Slope Analysis**: Wentworth's method (based on contour crossings per grid cell) and Smith's method (relative relief combined with drainage density).
* **Drainage Morphometry**:
  - *Stream Ordering*: Strahler (only same orders combine to increase) vs. Horton.
  - *Bifurcation Ratio*: Ratio of number of streams of a given order to the number of next higher order.
  - *Drainage Density*: Total stream length / basin area.
* **Indices**: Agricultural efficiency (Kendall's ranking, Bhatia's weighted output). Handling data of different scales requires normalization (Z-scores) to create composite indices.
:::



### Nearest Neighbour Analysis — Detailed (NET Notes — Pulakesh Pradhan)

> Measures the pattern of point distribution — whether clustered, random or regular

**Formula:*
> Rn = 2D̄ × √(n/A)
> D̄ = mean nearest neighbour distance
> n = number of points
> A = total area

| Rn Value | Pattern |
|:---|:---|
| Rn = 0 | Maximum clustering (all points coincide) |
| Rn = 1 | Random distribution |
| Rn = 2.149 | Maximum dispersion / perfect hexagonal |
