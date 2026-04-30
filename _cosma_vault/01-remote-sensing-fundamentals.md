---
title: 01-remote-sensing-fundamentals
id: 20260430145958
---
## Fundamentals of Remote Sensing and EMR

::: {.callout-warning}
#### 📘 Syllabus Coverage
| Syllabus | Topic Details |
|----------|---------------|
| **NEP-2020** | Unit I — Concept of Remote Sensing, EMR Spectrum, Resolution |
| **UGC NET** | Remote sensing applications |
:::


[![](https://via.placeholder.com/600x300?text=Course+Module+Video)](#)

[Get the Presentation &nearr;](#) &nbsp; | &nbsp; [Watch the Video &nearr;](#)

::: {.callout-note}
### Key Concepts
* **Remote Sensing**: Acquiring information about an object/phenomenon without making physical contact.
* **EMR (Electromagnetic Radiation)**: Energy waves interacting with targets. Key regions: Visible (0.4-0.7 µm), Near Infrared (NIR, 0.7-1.3 µm), Shortwave Infrared (SWIR), Thermal IR, Microwave. **Vegetation stress** is best detected using the **SWIR** region.
* **NDVI (Normalized Difference Vegetation Index)**: Calculated as **(NIR - Red) / (NIR + Red)**.
* **GIS Components**: It is important to note that **GPS** is NOT considered a core component of a Geographic Information System (GIS), which typically consists of hardware, software, data, people, and methods.
* **Interaction with Atmosphere**: Scattering (Rayleigh, Mie, Non-selective) and Absorption (Ozone, Carbon dioxide, Water vapour). Atmospheric windows.
* **Spectral Signatures**: Unique reflectance/emittance curve of a material (e.g., healthy vegetation reflects highly in NIR, absorbs in Red).
* **Sensor Resolutions**:
  - *Spatial*: Size of the smallest detectable feature (pixel size).
  - *Spectral*: Number and width of spectral bands.
  - *Temporal*: Revisit time of the satellite.
  - *Radiometric*: Sensitivity to differences in signal strength (bit depth).
* **Key Satellite Sensors and Platforms:*
  - *Landsat-8 OLI, Sentinel-2 MSI, MODIS, ASTER* are major multispectral sensors.
  - **LISS-3:** The sensor has a spatial resolution of **23.5 meters**. The spectral range of the 10-bit SWIR band of the LISS-3 sensor on Resourcesat-2 is **1.55 – 1.70 µm**.
  - **AWiFS (Advanced Wide Field Sensor):** The spatial resolution of this sensor is **56 meters**.
  - **CartoDEM:** This digital elevation model has its origin in **India**.
  - **INSAT (Indian National Satellite):** Series of multipurpose geostationary satellites used for **Meteorology**, telecommunications, and broadcasting.
  - **EOS-01:** An Indian SAR-based (Synthetic Aperture Radar) earth imaging satellite launched in 2020.
* **Focal Plane**: The area in the camera body where the film (or sensor) is held flat during an exposure. It lies at the focal length distance behind the lens.
* **GPS (Global Positioning System) — Three Segments**:
  - **Space Segment**: Constellation of satellites orbiting the Earth (24+ satellites in 6 orbital planes).
  - **Control Segment**: Ground stations that track GPS satellites and upload updated navigational data.
  - **User Segment**: GPS receivers used by individuals/organizations to determine position.
* **Mid-Infrared (MIR) Waveband**: Can be used to **discriminate among clouds, snow, and ice** — snow and ice have high absorption in MIR while clouds reflect differently, enabling differentiation.
* **Aerial Photo Interpretation — Key Elements**:
  - **Tone/Colour**: Railway lines appear as dark thin lines; concrete highways appear in a bright tone.
  - **Texture**: Helps identify areas with vegetative cover, crops, pastures, etc.
  - **Shape**: Aids in visualising relief dimensions.
  - **Pattern**: Drainage network patterns give definite indications of underlying rock type, structure, and soil characteristics.
  - **Shadow, Size, Site, Association**: Other fundamental elements used in visual interpretation.
* **Image Processing & ML:*
  - **Random Forest:** A widely used and effective machine learning algorithm based on the idea of **bagging**.
  - **FCC Representation:** In a standard FCC of the winter season, **laterite duricrusts** are typically represented in **Brown**.
:::
