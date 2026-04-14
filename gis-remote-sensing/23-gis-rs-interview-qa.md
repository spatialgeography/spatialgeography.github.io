### GIS & Remote Sensing: Short Q&A for Interviews

This section provides concise answers to technical concepts frequently asked during academic and professional interviews in the field of Geoinformatics.

---

#### 1. What is the fundamental difference between GIS and Remote Sensing?
**Answer**: Remote Sensing is the technology of acquiring information about Earth's surface from a distance (satellites/drones), while GIS (Geographic Information System) is the tool used for storing, analyzing, and visualizing that spatial data. RS provides the **data**, and GIS provides the **analysis framework**.

#### 2. Explain 'Passive' vs. 'Active' Remote Sensing.
**Answer**: 
- **Passive RS**: Sensors detect natural energy (sunlight) reflected or emitted from the Earth's surface (e.g., Landsat). 
- **Active RS**: Sensors provide their own energy source, emit a pulse, and measure the return (e.g., RADAR, LiDAR).

#### 3. What is the 'Spectral Signature'?
**Answer**: It is the unique pattern of reflectance or emittance of an object across different wavelengths of the electromagnetic spectrum. Just like a human fingerprint, it allows us to identify materials (e.g., distinguishing healthy vegetation from stressed vegetation).

#### 4. Define 'Raster' and 'Vector' Data Models.
**Answer**: 
- **Raster**: Represents data as a grid of cells or pixels (best for continuous data like elevation or satellite imagery).
- **Vector**: Represents data using points, lines, and polygons (best for discrete data like boundaries, roads, or wells).

#### 5. What are the four types of Resolution in Remote Sensing?
**Answer**: 
1. **Spatial**: The smallest area on the ground represented by a pixel.
2. **Spectral**: The number and width of wavelength bands the sensor can detect.
3. **Radiometric**: The ability of a sensor to distinguish small differences in energy (bit depth).
4. **Temporal**: The revisit time (how often the satellite images the same spot).

#### 6. What is 'Georeferencing'?
**Answer**: The process of assigning real-world geographic coordinates (like Latitude/Longitude) to a digital image or map so that it can be correctly aligned with other spatial data.

#### 7. What is the difference between 'GCP' and 'DGPS'?
**Answer**: 
- **GCP (Ground Control Point)**: A physical point on the ground with known coordinates used for georeferencing.
- **DGPS (Differential GPS)**: An enhancement to standard GPS that provides sub-meter accuracy by using a fixed base station to correct errors in mobile receivers.

#### 8. What is 'Topology' in GIS?
**Answer**: It is a set of rules and mathematical relationships that define how geometric features (points, lines, polygons) share common space, ensuring data integrity (e.g., no overlapping polygons or disconnected road segments).

#### 9. Explain 'NDVI'.
**Answer**: The **Normalized Difference Vegetation Index** is a formula `(NIR - Red) / (NIR + Red)` used to measure the "greenness" or health of vegetation by comparing high reflectance in Near-Infrared and high absorption in Red light.

#### 10. What is a 'Buffer Analysis'?
**Answer**: A spatial analysis tool used to create a zone of a specific distance around a feature (e.g., identifying all houses within 500 meters of a proposed highway).

---

> [!TIP]
> **Interview Strategy**: When answering, always try to give a real-world example (e.g., "NDVI is used in crop monitoring to detect drought stress before it's visible to the naked eye").
