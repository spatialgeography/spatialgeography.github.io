"""
Script to restructure the remaining geography subject directories with proper
NEP-2020 and UGC NET syllabus attribution.
For each subject:
  1. Creates a clean 00-syllabus.md with both NEP and UGC NET sections
  2. Creates numbered topic MD files with syllabus tags
  3. Rewrites the QMD index with organized sections
"""
import os
import glob

BASE = r"e:\spatialgeography\geography"

SUBJECTS = []

# ============================================================
# 11. GIS AND REMOTE SENSING
# ============================================================
SUBJECTS.append({
    'dir': 'gis-remote-sensing',
    'qmd': 'gis-remote-sensing.qmd',
    'title': 'GIS & Remote Sensing',
    'subtitle': 'Geospatial technologies for mapping, analysis, and Earth observation.',
    'has_interview': False,
    'nep_syllabus': """#### Core I Paper VIII — Remote Sensing and GIS
**(4 Credit, Theory: 45hrs, Practical: 30hrs)**

**Unit I:**
- Sources and characteristics of spatial data: Maps vs RS images
- Concept of Remote Sensing, Meaning and significance of EMR Spectrum
- Elements of a RS image: Pixel, Digital Number (DN), Band, Resolution
- Visual Interpretation of Aerial Photograph and Satellite imagery

**Unit II:**
- Abstraction and representation of Spatial Data
- Data Models in GIS (Vector Data Model: Types/Components)
- Attribute data Management
- Query of Spatial and non-spatial Data
- Processing and Analysis of Vector Data (Geoprocessing, Overlay Analysis)

**Unit III:**
- Working with Continuous spatial Data (Raster Data processing)
- Various Gridded Data Sources and application, DEM
- Interpretation of Remote Sensing images: Visual and Digital interpretation
- Supervised and Unsupervised classification, LULC classification, NDVI
- Accuracy assessment
- Facility Information System using spatial data, Land Use planning, disaster management""",
    'ugc_syllabus': """#### Unit X — Remote Sensing and GIS

- Remote sensing applications
- Digital mapping
- Geographic Information System (GIS)
- Thematic maps""",
    'common': [
        ('01-remote-sensing-fundamentals.md', 'Fundamentals of Remote Sensing and EMR',
         'Unit I — Concept of Remote Sensing, EMR Spectrum, Resolution',
         'Remote sensing applications',
         """* **Remote Sensing**: Acquiring information about an object/phenomenon without making physical contact.
* **EMR (Electromagnetic Radiation)**: Energy waves interacting with targets. Key regions: Visible (0.4-0.7 µm), Near Infrared (NIR, 0.7-1.3 µm), Shortwave Infrared (SWIR), Thermal IR, Microwave.
* **Interaction with Atmosphere**: Scattering (Rayleigh, Mie, Non-selective) and Absorption (Ozone, Carbon dioxide, Water vapour). Atmospheric windows.
* **Spectral Signatures**: Unique reflectance/emittance curve of a material (e.g., healthy vegetation reflects highly in NIR, absorbs in Red).
* **Sensor Resolutions**:
  - *Spatial*: Size of the smallest detectable feature (pixel size).
  - *Spectral*: Number and width of spectral bands.
  - *Temporal*: Revisit time of the satellite.
  - *Radiometric*: Sensitivity to differences in signal strength (bit depth)."""),

        ('02-gis-data-models.md', 'GIS Fundamentals and Spatial Data Models',
         'Unit II — Data Models in GIS, Vector Data Model',
         'Geographic Information System (GIS)',
         """* **GIS (Geographic Information System)**: A computer system for capturing, storing, querying, analyzing, and displaying geospatial data.
* **Components of GIS**: Hardware, Software, Data, People, Procedures.
* **Vector Data Model**: Represents features as discrete points, lines, and polygons using coordinates. Best for boundaries, roads, land parcels.
  - *Topology*: Mathematical rules defining spatial relationships (adjacency, connectivity, containment).
* **Raster Data Model**: Represents continuous space as a grid of cells (pixels). Better for elevation (DEM), temperature, imagery.
* **Attribute Data**: Non-spatial tabular data linked to spatial features (Relational Database Management System - RDBMS)."""),

        ('03-image-interpretation.md', 'Image Interpretation (Visual and Digital)',
         'Unit I & III — Visual Interpretation, Digital interpretation',
         'Remote sensing applications',
         """* **Visual Image Interpretation**: Identifying features using human cognitive processes.
  - *Elements*: Tone/Color, Size, Shape, Texture, Pattern, Shadow, Site, Association.
* **Digital Image Processing (DIP)**: Computer-based manipulation of digital numbers (DNs).
  - *Pre-processing*: Radiometric correction (haze, sun angle), Geometric correction (rectification).
  - *Enhancement*: Contrast stretching, spatial filtering, band rationing (e.g., NDVI).
* **Image Classification**: Assigning pixels to thematic classes (Land Use/Land Cover).
  - *Supervised*: Analyst defines training sites (spectral signatures), algorithm classifies rest of image (Maximum Likelihood, Random Forest).
  - *Unsupervised*: Algorithm automatically groups pixels into clusters based on statistical similarity (K-Means, ISODATA); analyst assigns labels later."""),

        ('04-spatial-analysis.md', 'Spatial Analysis and Geoprocessing',
         'Unit II — Query, Geoprocessing, Overlay Analysis',
         'Geographic Information System (GIS)',
         """* **Attribute Query (SQL)**: Selecting features based on their attribute values (e.g., "Population > 1,000,000").
* **Spatial Query**: Selecting features based on spatial relationships (intersect, within, adjacent).
* **Vector Overlay Operations**: Combining two or more layers to create a new layer (Union, Intersect, Identity).
* **Proximity Analysis**: Buffering (creating zones of a specified distance around features).
* **Geoprocessing**: Executing operations (Clip, Dissolve, Merge) to manipulate data and solve spatial problems.
* **Raster Analysis**: Map Algebra (local, focal, zonal, global operations), suitability modeling."""),

        ('05-applications-of-rs-gis.md', 'Applications of GIS and Remote Sensing',
         'Unit III — LULC, NDVI, Facility Information System, Land Use planning, disaster management',
         'Remote sensing applications, Digital mapping, Thematic maps',
         """* **Vegetation Indices**: NDVI (Normalized Difference Vegetation Index) = (NIR - Red) / (NIR + Red). Evaluates vegetation health and biomass.
* **Land Use / Land Cover (LULC)**: Mapping and monitoring changes over time (urban sprawl, deforestation).
* **Disaster Management**: Flood inundation mapping, forest fire burn scar analysis, earthquake damage assessment, landslide susceptibility zonation.
* **Urban and Regional Planning**: Facility site selection (suitability analysis), transportation network routing (Network Analysis), land use zoning.
* **Digital Elevation Models (DEM)**: Extracting slope, aspect, stream networks, and watershed boundaries."""),
    ],
    'nep_only': [],
    'ugc_only': []
})

# ============================================================
# 12. GEOGRAPHY OF INDIA (Physical & Socio-Economic)
# ============================================================
SUBJECTS.append({
    'dir': 'geography-of-india',
    'qmd': 'indian-geography.qmd',
    'title': 'Geography of India',
    'subtitle': 'Comprehensive study of India\'s physical environment, resources, economy, and society.',
    'has_interview': False,
    'nep_syllabus': """#### Core I Paper XIV — Geography of India
**(4 Credit, Theory: 45hrs, Practical: 30hrs)**

**Unit I:**
- Physiography divisions of India
- Himalayan and Peninsular rivers, watershed and interlinking rivers
- India's Climatic classification (Koeppen and Trewartha), Mechanism of Indian monsoon
- Soils: distribution and characteristics of major soil groups

**Unit II:**
- Agricultural regionalization (Agro climatic zones), regional disparities
- Govt. Schemes related to Agriculture; Green/White/Blue revolution and implications
- Joint Forest Management, Social forestry
- Conventional and non-conventional sources of energy, Energy Crisis
- Biosphere reserves and National Parks

**Unit III:**
- Industries: Types and classifications, location factors (jute, Tea, Paper, Fertilizer, IT)
- Industrial policies, SEZs
- Transport system: Roadways, Railways, Ports, Waterways, Airways, Pipeline
- Growing importance of ports in trade

**Unit IV:**
- Experience of regional planning: Five year plans, integrated rural dev, Panchayati Raj
- Command area, Watershed management, backward area planning
- Geographical basis of Indian federalism, State reorganization, Emergence of new states
- Regional consciousness, International boundary and related issues""",
    'ugc_syllabus': """#### Unit IX — Geography of India

- Major Physiographic Regions and their Characteristics
- Drainage System (Himalayan and Peninsular)
- Climate: Seasonal Weather Characteristics, Climatic Divisions, Indian Monsoon mechanism
- Types and Distribution of Natural Resources: Soil, Vegetation, Water, Mineral and Marine
- Natural Disasters in India (Earthquake, Drought, Flood, Cyclone, Tsunami)
- Population Characteristics: Growth, Composition, Determinants, Policies
- Agriculture: Major Food Crops, Regions, Variations, Agro-Climatic Zones, Green Revolution, Food Security
- Industrial Development: Industrial Regions, Industrial Policies
- Transport Networks, Internal and External Trade, Regional Development Planning
- Globalisation and its impact on Indian Economy""",
    'common': [
        ('01-physiography-of-india.md', 'Physiographic Divisions of India',
         'Unit I — Physiography divisions of India',
         'Major Physiographic Regions and their Characteristics',
         """* **The Great Himalayas**: Young fold mountains. Trans-Himalaya, Greater (Himadri), Lesser (Himachal), Outer (Shiwaliks), Northeast hills (Purvanchal). Influence on climate and rivers.
* **The Northern Plains**: Formed by Indus, Ganga, Brahmaputra systems. Bhabar, Terai, Bangar, Khadar (soil zones). Extremely fertile and agriculturally productive.
* **The Peninsular Plateau**: Oldest stable landmass (Gondwana). Central Highlands (Malwa, Chotanagpur) and Deccan Plateau. Flanked by Western and Eastern Ghats.
* **The Great Indian Desert (Thar)**: Arid region, Luni river, shifting dunes (barchans).
* **The Coastal Plains**: Western Coast (narrow, submerged - Konkan, Malabar) vs. Eastern Coast (broad, emergent - Coromandel, Northern Circars).
* **The Islands**: Andaman & Nicobar (volcanic/tectonic origin) and Lakshadweep (coral origin)."""),

        ('02-drainage-systems.md', 'Drainage Systems of India',
         'Unit I — Himalayan and Peninsular rivers, watershed and interlinking',
         'Drainage System (Himalayan and Peninsular)',
         """* **Himalayan Rivers**: Antecedent, perennial (snow and rain-fed), youth stage (gorges, rapids), shifting courses (e.g., Kosi). Examples: Indus, Ganga, Brahmaputra systems.
* **Peninsular Rivers**: Consequent, seasonal (rain-fed), mature stage (broad valleys, graded profiles). Flow largely west to east (Mahanadi, Godavari, Krishna, Cauvery) forming deltas, except Narmada and Tapi (flowing west in rift valleys forming estuaries).
* **Water Divides**: The Great Water Divide separating Arabian Sea drainage (23%) from Bay of Bengal drainage (77%).
* **Interlinking of Rivers (NRLP)**: Proposal to transfer water from surplus (Himalayan/Brahmaputra) to deficit basins (Peninsular). Ecological and geopolitical concerns.
* **Watershed Management**: Integrated conservation of soil, water, and biomass within a hydrological unit to prevent degradation."""),

        ('03-indian-climate-and-monsoon.md', 'Climate and Mechanism of Indian Monsoon',
         'Unit I — Climatic classification, Mechanism of Indian monsoon',
         'Climate: Seasonal Characteristics, Climatic Divisions, Indian Monsoon',
         """* **Characteristics**: Tropical Monsoon climate. Four seasons: Winter, Pre-Monsoon (Summer), Southwest Monsoon, Post-Monsoon (Retreating).
* **Monsoon Mechanism**:
  - *Thermal Concept (Halley)*: Differential heating of land and sea.
  - *Dynamic Concept (Flohn)*: Shifting of the ITCZ northward over the Ganga plain.
  - *Jet Stream Theory (Yin)*: Sub-Tropical Westerly Jet moving north of Himalayas allowing Tropical Easterly Jet to establish, triggering the onset.
  - *Oceanic Factors*: El Niño (negative impact), Indian Ocean Dipole (Positive IOD enhances rainfall).
* **Climatic Classifications**: Koeppen (Amw, Aw, BShw, Cwg), Trewartha.
* **Western Disturbances**: Cyclonic storms from the Mediterranean bringing winter rain to NW India (crucial for Rabi crops)."""),

        ('04-soils-and-vegetation.md', 'Soils and Natural Vegetation',
         'Unit I — Soils: distribution and characteristics; Unit II — Biosphere reserves',
         'Types and Distribution of Natural Resources: Soil, Vegetation',
         """* **Major Soil Types (ICAR)**:
  - *Alluvial*: Most widespread, fertile (Khadar/Bangar), transported by rivers.
  - *Black (Regur)*: Deccan trap region, clayey, high moisture retention, ideal for cotton.
  - *Red & Yellow*: Crystalline rocks, iron-rich, poor in humus/NPK.
  - *Laterite*: Formed by leaching in high temp/rainfall areas (Western Ghats), poor fertility, used for brick-making.
  - *Arid, Forest, Saline/Alkaline, Peaty soils*.
* **Natural Vegetation**:
  - *Tropical Evergreen* (Western Ghats, NE), *Tropical Deciduous* (Monsoon forests, most widespread - Sal, Teak), *Thorn Forests* (NW), *Montane Forests* (Himalayas - altitudinal zonation), *Mangroves* (Sundarbans).
* **Conservation**: Social forestry, Joint Forest Management (JFM), Biosphere Reserves, National Parks."""),

        ('05-indian-agriculture.md', 'Agriculture and Green Revolution',
         'Unit II — Agro climatic zones, Green/White/Blue revolution',
         'Agriculture: Major Food Crops, Regions, Variations, Agro-Climatic Zones, Green Revolution',
         """* **Characteristics**: Subsistence farming transitioning to commercial, monsoon-dependent, small landholdings.
* **Agro-Climatic Zones (Planning Commission)**: 15 zones based on soil, climate, and water availability for regionalized planning.
* **Green Revolution (1960s)**: High Yielding Varieties (HYV) of wheat and rice, assured irrigation, chemical fertilizers. Concentrated in Punjab, Haryana, Western UP. Led to food security but caused ecological issues (groundwater depletion, soil salinity, monoculture).
* **Other Revolutions**: White (Dairy - Operation Flood), Blue (Aquaculture), Yellow (Oilseeds).
* **Govt. Schemes**: PM-KISAN, PMFBY (Crop Insurance), Soil Health Card.
* **Regional Disparities**: Stark contrast between high productivity regions (NW/South) and low productivity regions (Eastern India)."""),

        ('06-industries-and-development.md', 'Industrial Development in India',
         'Unit III — Industries: location factors, Industrial policies, SEZs',
         'Industrial Development: Industrial Regions, Industrial Policies',
         """* **Location Factors**: Raw materials (iron/steel in Chotanagpur), market/labor/ports (textiles in Mumbai/Ahmedabad), footloose (IT in Bengaluru/Hyderabad).
* **Major Industrial Regions**: Hooghly, Mumbai-Pune, Ahmedabad-Vadodara, Madurai-Coimbatore-Bengaluru, Chotanagpur, NCR, Visakhapatnam-Guntur, Kollam-Thiruvananthapuram.
* **Industrial Policies**:
  - *1956*: State-led command economy, focus on heavy industries.
  - *1991 (New Economic Policy)*: Liberalization, Privatization, Globalization (LPG). De-licensing, welcoming FDI.
* **Special Economic Zones (SEZs)**: Export hubs with liberal laws. Aimed at mimicking China's manufacturing success, mixed results in India."""),

        ('07-transport-trade-planning.md', 'Transport, Trade, and Regional Planning',
         'Unit III — Transport system; Unit IV — Regional planning, Panchayati Raj',
         'Transport Networks, Internal and External Trade, Regional Development Planning',
         """* **Transport System**:
  - *Roads*: Golden Quadrilateral, North-South/East-West Corridors, Bharatmala.
  - *Railways*: Largest network under single management, Dedicated Freight Corridors (DFC).
  - *Waterways/Ports*: Sagarmala project, 12 major ports (Kandla, JNPT, Chennai, etc.). Inland Waterways (NW-1, NW-2).
* **Regional Planning**: Addressing spatial inequality. Target area planning (DPAP, Hill Area, Tribal Sub-Plan). Decentralized planning via 73rd/74th Constitutional Amendments (Panchayati Raj).
* **Federalism and Reorganization**: Linguistic reorganization of states (1956), later demands based on regional underdevelopment (Balkanization debates vs. administrative convenience). Inter-state river disputes (Cauvery, Krishna)."""),
    ],
    'nep_only': [],
    'ugc_only': [
        ('08-natural-disasters.md', 'Natural Disasters in India',
         'Natural Disasters in India (Earthquake, Drought, Flood, Cyclone, Tsunami)',
         """* **Earthquakes**: Himalayan belt, North-East, and Gujarat (High seismic zones IV and V due to tectonic collision).
* **Floods**: Brahmaputra, Ganga, Kosi (Bihar), Mahanadi basins. Caused by heavy monsoon, poor drainage, deforestation in upper catchments.
* **Droughts**: Chronically drought-prone areas in Rajasthan, Rayalaseema, Vidarbha, Bundelkhand. Erratic monsoon rainfall.
* **Cyclones**: Bay of Bengal highly prone (Odisha, Andhra, West Bengal coasts) during pre and post-monsoon seasons. Storm surges cause massive destruction.
* **Tsunami**: Vulnerability of eastern coast and A&N islands (2004 Indian Ocean Tsunami).
* **Management**: NDMA, SDMA framework, early warning systems, shifting focus from relief to mitigation and preparedness."""),

        ('09-population-dynamics.md', 'Population Dynamics and Policies',
         'Population Characteristics: Growth, Composition, Determinants, Policies',
         """* **Distribution**: High density in Gangetic plains and Kerala; sparse in Himalayas, Northeast, and deserts.
* **Growth Phases**: Stagnant (1901-21), Steady (1921-51), Rapid Explosive (1951-81), High but Decreasing (1981-present).
* **Demographic Dividend**: Large working-age population (15-59 years) — an economic opportunity if skilled and employed.
* **Composition**: Improving literacy, rural to urban shifts, adverse but stabilizing sex ratio.
* **Population Policy (2000)**: Focus shifting from target-based coercive approaches to reproductive health, female education, and voluntary family planning. Goal to stabilize population around 2045."""),
        
        ('10-globalisation-impact.md', 'Impact of Globalisation on Indian Economy',
         'Globalisation and its impact on Indian Economy',
         """* **Economic Transformation**: Shift from import substitution to outward-looking, export-oriented economy post-1991.
* **Service Sector Boom**: IT and BPO industries leveraging English-speaking workforce and time zone difference. Shifted GDP composition heavily towards tertiary sector.
* **Agricultural Distress**: Exposure to global price volatilities, reduction in subsidies, WTO compliance challenges.
* **Spatial Polarization**: 'India vs. Bharat' — heavy concentration of FDI and growth in southern/western metro corridors, leaving eastern/central regions behind.
* **Urbanisation**: Growth of peri-urban areas, mega-cities, and consumerist culture."""),
    ],
})

# ============================================================
# 13. HUMAN GEOGRAPHY
# ============================================================
SUBJECTS.append({
    'dir': 'human-geography',
    'qmd': 'human-geography.qmd',
    'title': 'Human Geography',
    'subtitle': 'The study of human societies, cultures, settlements, and their interaction with the environment.',
    'has_interview': False,
    'nep_syllabus': """#### Core I Paper II — Human Geography
**(4 Credit, Theory: 45hrs, Practical: 30hrs)**

**Unit I:**
- Meaning, Nature and Scope of Human Geography; Its Contemporary Relevance
- Major Themes/Concepts: Location, Place, Region, Movement, Accessibility, Agglomeration, Mental Map, Space, Landscape, Diffusion, Distribution
- Man-nature inter-relationship (Determinism, Possibilism and Neo-determinism)
- Development of human geography (Germans, French, Americans)

**Unit II:**
- World Distribution of racial, religious, linguistic and ethnic groups
- Evolution of Culture and Cultural realms of the world
- World Population Growth, Population Problems
- Global cultural diversities - diffusion of culture

**Unit III:**
- Types and Patterns of Rural Settlements
- Concept and Classification of Urban Settlements
- World Urbanization with special reference to developing countries
- Salient Features of cultural globalization
- Regional diversity and disparity (Gender, Ethnicity and Income)""",
    'ugc_syllabus': """#### Unit V & VII — Settlement, Social, and Cultural Geography

- Settlement Geography: Site and situation of settlements
- Types, size, and spacing of settlements
- Internal morphology of rural and urban settlements
- Urban fringe, City-region concepts, Settlement systems
- Patterns of world distribution, growth, and density of population
- Spatial distribution of social groups in India (Tribe, Caste, Religion and Language)
- Cultural Hearths and Cultural Realms
- Man and environment: determinism and possibilism""",
    'common': [
        ('01-nature-and-scope.md', 'Nature, Scope, and Concepts of Human Geography',
         'Unit I — Meaning, Nature, Scope, Contemporary Relevance; Major Themes',
         'Foundations/Nature of the discipline',
         """* **Definition**: The study of spatial organization of human societies and their interactions with the physical environment. (Vidal de la Blache, Jean Brunhes).
* **Six Major Themes**:
  - *Location*: Absolute (coordinates) and Relative.
  - *Place*: Physical and human characteristics of a location. Sense of place, Topophilia (Yi-Fu Tuan).
  - *Human-Environment Interaction*: Adaptation and modification.
  - *Movement*: Spatial interaction, migration, trade, diffusion.
  - *Region*: Formal, functional, and vernacular.
  - *Landscape*: The visible human imprint on earth (Cultural Landscape - Sauer).
* **Space vs. Place**: Space is absolute and abstract; Place is space infused with human meaning, memory, and experience."""),

        ('02-man-environment-relationship.md', 'Man-Environment Inter-relationship',
         'Unit I — Determinism, Possibilism and Neo-determinism',
         'Man and environment: determinism and possibilism',
         """* **Environmental Determinism**: The physical environment strictly limits and dictates human activities and culture (Ratzel, Semple, Huntington).
* **Possibilism**: The physical environment offers a range of possibilities, but humans (with technology and culture) are the active agents of choice (Vidal de la Blache, Febvre).
* **Neo-determinism / Stop-and-Go Determinism**: Geography is not absolute dictates nor absolute freedom. Environment sets limits, but within those limits humans can decide, provided they respect ecological laws (Griffith Taylor). 
* **Probabilism**: Physical environment makes some human responses more probable than others (Spate)."""),

        ('03-race-religion-language.md', 'World Social and Cultural Groups',
         'Unit II — World Distribution of racial, religious, linguistic and ethnic groups',
         'Spatial distribution of social groups (Tribe, Caste, Religion, Language)',
         """* **Race vs. Ethnicity**: Race is a socially constructed biological categorization based on physical traits (skin color, facial form). Ethnicity is based on shared cultural heritage, ancestry, language, or history.
* **Major Races (Traditional)**: Caucasoid, Mongoloid, Negroid, Australoid. Modern geography rejects biological racism, focusing on spatial distribution and social inequality.
* **Religions**:
  - *Universalizing*: Seek converts, widespread (Christianity, Islam, Buddhism).
  - *Ethnic*: Tied to a specific culture/location (Hinduism, Judaism).
* **Languages**: Indo-European (largest), Sino-Tibetan, Niger-Congo, Afro-Asiatic. Language as the core carrier of culture and identity."""),

        ('04-rural-settlements.md', 'Rural Settlements: Types and Patterns',
         'Unit III — Types and Patterns of Rural Settlements',
         'Site/situation; Types, size, spacing; Internal morphology of rural settlements',
         """* **Settlement**: Any form of human habitation from a single dwelling to a megalopolis.
* **Site vs. Situation**: Site is the exact physical ground (e.g., hilltop, riverbank). Situation is the relative location concerning surrounding features.
* **Types of Rural Settlements**:
  - *Clustered (Nucleated)*: Houses built close together, sharing common land/amenities (common in fertile plains).
  - *Semi-clustered*: Nucleated core with dispersed hamlets around it.
  - *Hamleted*: Physically fragmented into several units bearing a common name (Panna, Para, Dhani).
  - *Dispersed (Scattered)*: Isolated farms/dwellings separated by fields/pastures (mountain/arid regions).
* **Patterns**: Determined by site lines — Linear (along roads/rivers), Rectangular, Circular (around lakes), Star-like (crossroads)."""),

        ('05-urban-settlements.md', 'Urban Settlements and Urbanization',
         'Unit III — Concept/Classification of Urban Settlements; World Urbanization',
         'Urbanization process; Settlement systems',
         """* **Urban Criteria**: Population size, density, and occupational structure (majority non-agricultural). Varies by country (India: 5000 pop, 400 density, 75% male non-agri workforce).
* **Classification**: Towns, Cities, Million-plus cities, Conurbations (Geddes), Megalopolis (Gottmann).
* **Functional Classification**: Administrative, Industrial, Transport, Commercial, Mining, Garrison/Cantonment, Educational, Religious/Cultural cities.
* **World Urbanization trends**: Developed world is highly urbanized (>75%) but growing slowly. Developing world (Global South) is rapidly urbanizing — fueling the growth of mega-cities and peri-urban sprawl."""),
    ],
    'nep_only': [
        ('06-cultural-globalization-diversity.md', 'Cultural Globalization and Inequalities',
         'Unit III — Salient Features of cultural globalization, Regional diversity and disparity (Gender, Ethnicity, Income)',
         """* **Cultural Globalization**: The rapid transmission of ideas, meanings, and values across world space, driven by internet, media, and global trade.
* **Impacts**: Homogenization (Westernization/McDonaldization), Hybridization (Glocalization - adapting global ideas locally), and Polarization (backlash, rise of ethno-nationalism).
* **Regional Disparities**:
  - *Gender*: Spatial variations in female literacy, workforce participation, and sex ratios.
  - *Income*: Spatial concentration of wealth (core vs. periphery).
  - *Intersectionality*: How geography, gender, caste, and class overlap marginalized specific groups."""),
    ],
    'ugc_only': [
        ('07-urban-morphology.md', 'Internal Morphology of Cities and Urban Models',
         'Internal morphology of urban settlements; Urban fringe, City-region concepts',
         """* **Urban Morphology**: The physical layout, structure, and land-use patterns of a city. CBD (Central Business District), residential zones, industrial zones.
* **Classical Models of Urban Structure**:
  - *Concentric Zone Model (Burgess, 1925)*: City grows in rings from CBD outward (Transition zone, working-class, commuter zone). Based on Chicago.
  - *Sector Model (Hoyt, 1939)*: Growth happens in wedge-shaped sectors radiating from CBD along transport routes.
  - *Multiple Nuclei Model (Harris & Ullman, 1945)*: A city has multiple centers/nodes around which different activities cluster.
* **Urban Fringe**: The transition zone where urban and rural land uses mix. Zone of rapid change, land speculation, and infrastructure deficit.
* **City-Region**: The city and its surrounding hinterland that are functionally integrated (commuting, trade, services)."""),
        
        ('08-central-place-theory.md', 'Settlement Hierarchy and Central Place Theory',
         'Christaller’s Central Place Theory, August Losch’s Market Centre Theory, Primate city, Rank-Size rule',
         """* **Central Place Theory (Christaller, 1933)**: Explains the size, number, and distribution of human settlements.
  - *Concepts*: Central place (provides goods/services), Threshold (minimum market needed to support a service), Range (maximum distance people will travel for a good).
  - *Geometry*: Hexagonal hinterlands. Hierarchies based on K-values: K=3 (Marketing principle), K=4 (Transport principle), K=7 (Administrative principle).
* **Lösch's Modification**: More flexible, allowing for specialized production centers rather than strict hierarchies.
* **Rank-Size Rule (Zipf)**: In a national settlement system, the $n^{th}$ largest city's population is $1/n$ of the largest city's population ($P_n = P_1/n$). Indicates an integrated, balanced urban system.
* **Primate City (Jefferson)**: A city disproportionately larger (> twice) than the second largest, dominating the country politically/economically (e.g., Paris, Bangkok, London). Indicates centralization/underdevelopment."""),
    ],
})

# Execute processing
def write_syllabus(dir_path, nep_text, ugc_text):
    content = f"## Official Syllabus\n\n### NEP-2020 Syllabus\n\n::: {{.callout-note}}\n{nep_text}\n:::\n\n---\n\n### UGC NET Syllabus\n\n::: {{.callout-tip}}\n{ugc_text}\n:::\n"
    filepath = os.path.join(dir_path, '00-syllabus.md')
    with open(filepath, 'w', encoding='utf-8') as f:
        f.write(content)

def write_topic(dir_path, filename, title, coverage_md, content):
    md = f"## {title}\n\n::: {{.callout-warning}}\n#### 📘 Syllabus Coverage\n| Syllabus | Topic Details |\n|----------|---------------|\n{coverage_md}:::\n\n\n[![](https://via.placeholder.com/600x300?text=Course+Module+Video)](#)\n\n[Get the Presentation &nearr;](#) &nbsp; | &nbsp; [Watch the Video &nearr;](#)\n\n::: {{.callout-note}}\n### Key Concepts\n{content}\n:::\n"
    filepath = os.path.join(dir_path, filename)
    with open(filepath, 'w', encoding='utf-8') as f:
        f.write(md)

def write_qmd_index(qmd_path, subject):
    title, subtitle, dir_name = subject['title'], subject['subtitle'], subject['dir']
    lines = ['---', f'title: "{title}"', f'subtitle: "{subtitle}"', 'author: "Geography Team"', 'toc: true', 'toc-depth: 3', '---', '', f'{{{{< include {dir_name}/00-syllabus.md >}}}}', '', '---', '', f'Welcome to the **{title}** module of Geography OpenCourseWare.', '', '---', '']
    
    if subject['common']:
        lines += ['## Part A: Common Topics (NEP-2020 & UGC NET)', '', 'These topics are covered in both the NEP-2020 undergraduate syllabus and the UGC NET syllabus.', '']
        lines += [f'{{{{< include {dir_name}/{filename} >}}}}\n' for filename, *_ in subject['common']]
        lines += ['---', '']
    
    if subject['nep_only']:
        lines += ['## Part B: NEP-2020 Specific Topics', '', 'These topics are part of the NEP-2020 undergraduate programme only.', '']
        lines += [f'{{{{< include {dir_name}/{filename} >}}}}\n' for filename, *_ in subject['nep_only']]
        lines += ['---', '']
    
    if subject['ugc_only']:
        lines += ['## Part C: UGC NET Specific Topics', '', 'These topics are part of the UGC NET syllabus only.', '']
        lines += [f'{{{{< include {dir_name}/{filename} >}}}}\n' for filename, *_ in subject['ugc_only']]
        lines += ['---', '']
    
    with open(qmd_path, 'w', encoding='utf-8') as f:
        f.write('\n'.join(lines))

if __name__ == '__main__':
    for subject in SUBJECTS:
        dir_path, qmd_path = os.path.join(BASE, subject['dir']), os.path.join(BASE, subject['qmd'])
        
        # Clean directory
        for f in glob.glob(os.path.join(dir_path, '*.md')): os.remove(f)
        
        write_syllabus(dir_path, subject['nep_syllabus'], subject['ugc_syllabus'])
        
        for f, t, n, u, c in subject['common']: 
            cov = f"| **NEP-2020** | {n} |\n| **UGC NET** | {u} |\n"
            write_topic(dir_path, f, t, cov, c)
            
        for f, t, n, c in subject['nep_only']: 
            cov = f"| **NEP-2020** | {n} |\n"
            write_topic(dir_path, f, t, cov, c)
            
        for f, t, u, c in subject['ugc_only']: 
            cov = f"| **UGC NET** | {u} |\n"
            write_topic(dir_path, f, t, cov, c)
            
        write_qmd_index(qmd_path, subject)
        print(f"Processed: {subject['title']}")
