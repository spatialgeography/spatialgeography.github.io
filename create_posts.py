
import os
import re
from datetime import datetime, timedelta

# Raw text from the user
raw_text = """
NASA’s PACE Hyperspectral Data Now Accessible in Google Earth Engine
NASA's Plankton, Aerosol, Cloud, ocean Ecosystem (PACE) satellite, launched on February 8, 2024, represents a big leap in Earth observation technologyNASA's Plankton, Aerosol, Cloud, ocean Ecosystem (PACE) satellite, launched on February 8, 2024, represents a big leap in Earth observation technology
by Pulakesh Pradhan • 7 min read

Google AlphaEarth: All-in-One Optical, Radar, and LiDAR Embeddings at 10m Resolution
Google's recently launched Satellite Embedding dataset represents a paradigm shift in how we approach geospatial analysis and Earth observation. PowerGoogle's recently launched Satellite Embedding dataset represents a paradigm shift in how we approach geospatial analysis and Earth observation. Power
by Pulakesh Pradhan • 7 min read

GEE to Python/R: Instantly convert JavaScript to Python (.ipynb) or R (rgee) with GEE AI Assistant
The Google Earth Engine (GEE) platform has transformed geospatial analysis by offering access to planetary-scale satellite imagery and a powerful clouThe Google Earth Engine (GEE) platform has transformed geospatial analysis by offering access to planetary-scale satellite imagery and a powerful clou
by Pulakesh Pradhan • 3 min read

Advanced GIS & Remote Sensing for College Students—Simplified by Google Earth Engine AI Assistant
No Coding? No Problem! Unlock Powerful Geospatial Analysis with the AI-Powered GEE Assistant AI : A New Era in Geospatial Education Remote sensing andNo Coding? No Problem! Unlock Powerful Geospatial Analysis with the AI-Powered GEE Assistant AI : A New Era in Geospatial Education Remote sensing and
by Pulakesh Pradhan • 4 min read

Introducing 'Google Earth Engine AI Assistant' - Your AI-Powered Coding Partner
Easily analyze and visualize Google Earth Engine (GEE) datasets by just typing messages — no coding knowledge required. Geospatial analysis is integraEasily analyze and visualize Google Earth Engine (GEE) datasets by just typing messages — no coding knowledge required. Geospatial analysis is integra
by Pulakesh Pradhan • 4 min read

Automating InVEST Data Preparation with Google Earth Engine
The InVEST (Integrated Valuation of Ecosystem Services and Tradeoffs) Carbon Storage and Sequestration model is a cornerstone tool for conservation plThe InVEST (Integrated Valuation of Ecosystem Services and Tradeoffs) Carbon Storage and Sequestration model is a cornerstone tool for conservation pl
by Pulakesh Pradhan • 4 min read

Interactive Spectral Reflectance Curve Generator
The Sentinel-2 Based Interactive Spectral Reflectance Curve Generator is a web-based geospatial application built on Google Earth Engine. It allows usThe Sentinel-2 Based Interactive Spectral Reflectance Curve Generator is a web-based geospatial application built on Google Earth Engine. It allows us
by Pulakesh Pradhan • 2 min read

Pixel to Policy: Administrative Boundary wise Spatial Data Analysis in Google Earth Engine
Mapping climate, environmental, biophysical, geophysical, and land use/land cover (LULC) variables to administrative boundaries — rather than using raMapping climate, environmental, biophysical, geophysical, and land use/land cover (LULC) variables to administrative boundaries — rather than using ra
by Pulakesh Pradhan • 4 min read

Google Earth Engine in JavaScript, Python and R
Google Earth Engine (GEE) has transformed the way we access and analyze enormous volumes of geospatial data. This cloud-based platform provides users Google Earth Engine (GEE) has transformed the way we access and analyze enormous volumes of geospatial data. This cloud-based platform provides users 
by Pulakesh Pradhan • 3 min read

Glimpse of the 10-Day Online Google Earth Engine (GEE) Training Sessions🗺️
Successfully Concluded! - 10-Day Online Google Earth Engine (GEE) Training. Delighted to share a few glimpses from the recently concluded 10-day GooglSuccessfully Concluded! - 10-Day Online Google Earth Engine (GEE) Training. Delighted to share a few glimpses from the recently concluded 10-day Googl
by Pulakesh Pradhan • 1 min read

Advanced Time Series Analysis with Google Earth Engine and Python Libraries
This guide shows how to extract time series data from Google Earth Engine (GEE) and analyze it with Python. I cover time series plotting, seasonal decThis guide shows how to extract time series data from Google Earth Engine (GEE) and analyze it with Python. I cover time series plotting, seasonal dec
by Pulakesh Pradhan • 3 min read

Large Scale Climatic Data Analysis using xee and xarray with Google Earth Engine
In this article, we’ll explore how to leverage GEE along with Python libraries like xarray and xee to perform large-scale climatic data analysis. ClimIn this article, we’ll explore how to leverage GEE along with Python libraries like xarray and xee to perform large-scale climatic data analysis. Clim
by Pulakesh Pradhan • 2 min read

Basic Understanding of Sentinel-1 SAR Data
What is SAR Data? Synthetic Aperture Radar (SAR) is a type of active data collection where a sensor produces its own energy and then records the amounWhat is SAR Data? Synthetic Aperture Radar (SAR) is a type of active data collection where a sensor produces its own energy and then records the amoun
by Pulakesh Pradhan • 4 min read

Sentinel-1 SAR Data Pre-processing in Google Earth Engine (GEE)
In the realm of geospatial data analysis, preprocessing of Synthetic Aperture Radar (SAR) data is a crucial step. This article provides a brief overviIn the realm of geospatial data analysis, preprocessing of Synthetic Aperture Radar (SAR) data is a crucial step. This article provides a brief overvi
by Pulakesh Pradhan • 3 min read

Import IMD Gridded Rainfall Data in Google Earth Engine using Google Colab (imdlib) & Code Editor
I will demonstrate the steps to download the Indian Meteorological Department (IMD) Gridded Rainfall data as a GeoTIFF (.tif) file using imdlib (pythoI will demonstrate the steps to download the Indian Meteorological Department (IMD) Gridded Rainfall data as a GeoTIFF (.tif) file using imdlib (pytho
by Pulakesh Pradhan • 3 min read

Image Statistics with Google Earth Engine: A Step-by-Step Guide
(Mean, Median, Mode, SD, CV, P25, P75 etc.) As remote sensing and geospatial analysis become increasingly important in various fields, tools like Goog(Mean, Median, Mode, SD, CV, P25, P75 etc.) As remote sensing and geospatial analysis become increasingly important in various fields, tools like Goog
by Pulakesh Pradhan • 3 min read

Server-Side vs Client-Side Objects in Google Earth Engine
Google Earth Engine (GEE) is a cloud-based platform that provides a massive catalogue of satellite imagery and geospatial datasets, along with powerfuGoogle Earth Engine (GEE) is a cloud-based platform that provides a massive catalogue of satellite imagery and geospatial datasets, along with powerfu
by Pulakesh Pradhan • 2 min read

Google Earth Engine: A Paradigm Shift for Geography Research
In the ever-evolving landscape of geographic information systems (GIS) and remote sensing, Google Earth Engine (GEE) has emerged as a powerful tool thIn the ever-evolving landscape of geographic information systems (GIS) and remote sensing, Google Earth Engine (GEE) has emerged as a powerful tool th
by Pulakesh Pradhan • 3 min read

Module Programming Concept in Google Earth Engine Code Editor
Google Earth Engine (GEE) introduced a module system to simplify the organization, reuse, and sharing of code. In this article, I will build a completGoogle Earth Engine (GEE) introduced a module system to simplify the organization, reuse, and sharing of code. In this article, I will build a complet
by Pulakesh Pradhan • 3 min read

How to Use Functions in Google Earth Engine: A Step-by-Step Guide for Beginners
Google Earth Engine (GEE) is a powerful tool for planetary-scale environmental data analysis, and functions play a central role in writing scripts witGoogle Earth Engine (GEE) is a powerful tool for planetary-scale environmental data analysis, and functions play a central role in writing scripts wit
by Pulakesh Pradhan • 3 min read

IMD Temperature Data Interpolation in Google Earth Engine
To perform spatial interpolation using kriging for temperature data in Google Earth Engine (GEE), let’s break down this script, step by step. This appTo perform spatial interpolation using kriging for temperature data in Google Earth Engine (GEE), let’s break down this script, step by step. This app
by Pulakesh Pradhan • 3 min read

Code Editor vs. Python: Choosing the Best Environment for Google Earth Engine (GEE)
When working with Google Earth Engine (GEE), developers have two primary environments to choose from: the Code Editor (JavaScript) and the Python enviWhen working with Google Earth Engine (GEE), developers have two primary environments to choose from: the Code Editor (JavaScript) and the Python envi
by Pulakesh Pradhan • 4 min read

Upload OpenStreetMap (OSM) data into Google Earth Engine (GEE)
In this article, I will explore how to integrate OpenStreetMap (OSM) data with Google Earth Engine (GEE) to visualize geospatial features—for example In this article, I will explore how to integrate OpenStreetMap (OSM) data with Google Earth Engine (GEE) to visualize geospatial features—for example 
by Pulakesh Pradhan • 2 min read
"""

# Base directory for blog posts
base_dir = "f:/Agent/Tools/spatial_website/blog"

# Helper to clean filenames
def clean_filename(title):
    return re.sub(r'[^a-zA-Z0-9]', '-', title).lower().strip('-')

# Split entries by double newlines matching the pattern
entries = re.split(r'\n\n(?=[A-Z])', raw_text.strip())

date_counter = datetime(2025, 1, 10) # Start from a recent date and go back

for entry in entries:
    lines = entry.strip().split('\n')
    if len(lines) < 3:
        continue
    
    title = lines[0].strip()
    
    # Description might be multi-line or duplicated in the input. 
    # The input seems to be copy-pasted with duplication. We'll take the first part.
    desc_raw = " ".join(lines[1:-1]).strip()
    # Simple fix for duplicated text in the prompt
    half_len = len(desc_raw) // 2
    if desc_raw[:half_len] == desc_raw[half_len:]: 
        description = desc_raw[:half_len]
    else:
        # Just take a reasonable chunk if it's not perfectly duplicated
        description = desc_raw.split("  ")[0] 

    meta_line = lines[-1].strip()
    # Parse read time if needed, but we can just put it in the body.
    
    # Generate slug and path
    slug = clean_filename(title)
    
    # Use different dates for sorting
    pub_date = date_counter.strftime("%Y-%m-%d")
    date_counter -= timedelta(days=2) # Space them out
    
    folder_path = os.path.join(base_dir, f"{pub_date}_{slug}")
    os.makedirs(folder_path, exist_ok=True)
    
    file_path = os.path.join(folder_path, "index.qmd")
    
    content = f"""---
title: "{title}"
date: {pub_date}
description: "{description.replace('"', "'")}"
author: "Pulakesh Pradhan"
categories: [Google Earth Engine, Geospatial, Tutorial]
image: https://github.com/jadeynryan/orcas/blob/master/inst/img/leaflet_screenshot.png?raw=true
---

**{meta_line}**

{description}

### Read the full article
*Content placeholder - originally from LinkedIn*
"""
    
    with open(file_path, "w", encoding="utf-8") as f:
        f.write(content)
    
    print(f"Created: {file_path}")
