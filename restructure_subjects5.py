"""
Script to restructure the remaining 6 NEP-specific geography subject directories.
"""
import os
import glob

BASE = r"e:\spatialgeography\geography"

SUBJECTS = []

# ============================================================
# 14. CARTOGRAPHY AND GEO-SPATIAL TECHNIQUES
# ============================================================
SUBJECTS.append({
    'dir': 'cartography',
    'qmd': 'cartography.qmd',
    'title': 'Cartography and Geo-Spatial Techniques',
    'subtitle': 'The science and art of map-making and spatial data representation.',
    'has_interview': False,
    'nep_syllabus': """#### Core I Paper IV — Cartography and Geo-Spatial Techniques
**(4 Credit, Theory: 45hrs, Practical: 30hrs)**

**Unit I:**
- Scientific basis of Cartography, needs of map making, characteristics of maps
- Geographical Coordinates (Latitude and Longitude), Graticules
- Types of Scales (Plain, and Diagonal Scale)

**Unit II:**
- Meaning, Uses and types of Map Projection
- Transformation of area, Distance and Direction, Choice of map projection
- Interpretation of Bedding plane, Strike, Dip, structure & stratigraphy of Geological map
- Methods of determination of slope (Wentworth's method and Smith)

**Unit III:**
- History of Geographical Information System, Components of GIS
- Dimensions of GIS data (field/object) and logical (raster/vector)
- Data sources, data types (raster/vector/attribute)
- History of Remote Sensing, Types of Platform, Sensor characteristics
- Aerial photographs and Visual interpretation of Satellite images
- Global Positioning System (GPS) and Global Navigation Satellite System (GNSS)
- Applications of GIS and RS""",
    'ugc_syllabus': """#### Unit X — Cartography
- Types of Maps, Techniques of Map Making
- Data Representation on Maps
- Thematic maps: Choropleth, Isarithmic, Dasymetric, Chorochromatic, Flow Maps""",
    'common': [
        ('01-basics-of-cartography.md', 'Basics of Cartography and Scale',
         'Unit I — Needs, characteristics of maps, Coordinates, Scales',
         'Types of Maps, Techniques of Map Making',
         """* **Cartography**: Science and art of making maps. Evolution from manual drafting to digital cartography.
* **Geographical Coordinates**: Latitude (parallels) and Longitude (meridians) forming a graticule to pinpoint absolute locations.
* **Map Scale**: The ratio between distance on the map and corresponding distance on the ground.
  - *Representative Fraction (RF)*: 1:50,000. Independent of units.
  - *Graphical/Linear Scale*: A drawn line divided into segments. Remains true even if the map is enlarged/reduced.
  - *Statement Scale*: "1 cm represents 1 km"."""),

        ('02-map-projections.md', 'Map Projections',
         'Unit II — Meaning, Uses, types, Choice of projection',
         'Techniques of Map Making',
         """* **Map Projection**: Mathematical method of transferring the graticule of a sphere onto a flat surface.
* **Types by Developable Surface**:
  - *Cylindrical*: Best for equatorial regions (e.g., Mercator).
  - *Conical*: Best for mid-latitudes.
  - *Zenithal/Azimuthal*: Best for polar regions.
* **Types by Property Preserved**:
  - *Orthomorphic/Conformal*: Preserves shape/local angles.
  - *Homolographic/Equal Area*: Preserves area (crucial for thematic mapping like population density).
  - *Equidistant*: Preserves scale along certain lines.
* **Choice**: Depends on purpose (navigation vs. statistical display) and Latitudinal extent of the mapped area."""),

        ('03-topographical-geological-maps.md', 'Topographical and Geological Maps',
         'Unit II — Interpretation of Bedding plane, Strike, Dip, structure',
         'Types of Maps',
         """* **Topographical Maps**: Detailed, accurate graphic representation of features that appear on the Earth's surface (relief, drainage, settlements, roads). Produced by Survey of India (SoI).
* **Geological Maps**: Show the distribution of different rock types or geologic strata exposed at the surface.
  - *Strike*: The direction of the line formed by the intersection of a rock surface with a horizontal plane.
  - *Dip*: The acute angle that a rock surface makes with a horizontal plane (magnitude and direction).
* **Slope Analysis**: Techniques like Wentworth's or Smith's method to derive quantitative slope data from contour lines on topomaps."""),

        ('04-introduction-to-gis-rs.md', 'Introduction to GIS and Remote Sensing',
         'Unit III — GIS components, data types, RS platforms, GPS',
         'Geographic Information System (GIS), Remote Sensing',
         """* **GIS Components**: Hardware, Software, Data, People, Methods.
* **Data Models**: Vector (points, lines, polygons for discrete objects) and Raster (grid cells for continuous fields like elevation).
* **Remote Sensing**: Acquiring information from a distance. Types of platforms (ground, aerial, space-borne) and sensor characteristics (spatial, spectral, temporal, radiometric resolution).
* **GNSS/GPS**: Global Navigation Satellite System providing autonomous geo-spatial positioning. Uses triangulation from a constellation of satellites. Segments: Space, Control, and User."""),
    ],
    'nep_only': [],
    'ugc_only': []
})

# ============================================================
# 15. FIELD SURVEY
# ============================================================
SUBJECTS.append({
    'dir': 'field-survey',
    'qmd': 'field-survey.qmd',
    'title': 'Field Survey: Tools and Techniques',
    'subtitle': 'Practical methods for primary data collection and physical surveying in Geography.',
    'has_interview': False,
    'nep_syllabus': """#### Core I Paper XXI — Field Survey: Tools and Technique
**(4 Credit, Theory: 45hrs, Practical: 30hrs)**

**Unit I:**
- Field survey: rationales, approaches, basic principles, Classification

**Unit II:**
- Physical survey: Plain Table survey (instruments, steps, methods, limitations)
- Dumpy level survey (components, steps, methods)
- Prismatic Compass survey (components, steps, methods)

**Unit III:**
- Socio-economic survey: Objectives, Sources of data, methods of data collection
- Quantitative survey: basic concepts of sampling, questionnaire preparation, finding errors
- Qualitative survey: Key Informant Interview (KII), Focus Group Discussion (FGD), Participatory Rapid Appraisal (PRA)
- Qualitative/quantitative data analysis, content analysis, ethics of primary data collection""",
    'ugc_syllabus': """#### UGC NET Overlaps (Unit IX/X)
- Sources of Geographic Information and Data (primary/spatial)
- Sampling, Sampling Procedure
- Techniques for studying spatial patterns""",
    'common': [
        ('01-principles-of-field-survey.md', 'Principles of Field Survey',
         'Unit I — Rationales, approaches, basic principles',
         'Sources of Geographic Information',
         """* **Rationale**: Geography is an empirical science; field surveys bridge the gap between theoretical knowledge and ground reality. Allows for primary data collection where secondary data is unavailable or unreliable.
* **Approaches**: Inductive (field observation to general laws) vs. Deductive (testing hypotheses in the field).
* **Classification**:
  - *Physical/Topographical Surveying*: Measuring earth's surface features (Plane table, Dumpy level).
  - *Socio-Economic Surveying*: Gathering demographic, economic, and social data from populations."""),

        ('02-physical-survey-instruments.md', 'Physical Survey Instruments and Methods',
         'Unit II — Plain Table, Dumpy level, Prismatic Compass',
         'Techniques of spatial data gathering',
         """* **Plane Table Survey**: Graphical method where plotting and surveying are done simultaneously in the field. Instruments: Plane table, alidade, plumbing fork, compass, spirit level. Methods: Radiation, Intersection, Traversing, Resection.
* **Prismatic Compass Survey**: Used for measuring magnetic bearings of lines. Closed and open traverse. Useful for filling in details in forested or urban areas where line of sight is obstructed.
* **Dumpy Level Survey**: Used for determining relative elevations (leveling) and contouring. Establishes a horizontal line of sight to measure height differences."""),

        ('03-socio-economic-survey.md', 'Socio-Economic Survey Methods',
         'Unit III — Objectives, Quantitative vs Qualitative, Sampling, Questionnaires',
         'Sampling, Sampling Procedure',
         """* **Quantitative Survey**: Deals with numbers and statistics.
  - *Sampling*: Simple random, stratified, systematic. Reducing sampling error.
  - *Tools*: Structured questionnaires, census, schedules.
* **Qualitative Survey**: Deals with words and meanings, exploring complex social phenomena.
  - *KII (Key Informant Interview)*: In-depth interviews with experts or community leaders.
  - *FGD (Focus Group Discussion)*: Guided group discussion to understand collective perspectives.
  - *PRA (Participatory Rapid Appraisal)*: Involving local communities in analyzing their own conditions (e.g., social mapping, resource mapping, seasonality calendars).
* **Ethics**: Informed consent, anonymity, avoiding harm, respectful engagement with communities."""),
    ],
    'nep_only': [],
    'ugc_only': []
})

# ============================================================
# 16. RESEARCH METHODOLOGY
# ============================================================
SUBJECTS.append({
    'dir': 'research-methodology',
    'qmd': 'research-methodology.qmd',
    'title': 'Research Methodology',
    'subtitle': 'Fundamentals of research design, data collection, and academic writing in Geography.',
    'has_interview': False,
    'nep_syllabus': """#### Core I Paper XVI — Research Methodology
**(4 Credit, Theory: 45hrs, Practical: 30hrs)**

**Unit I:**
- Research in Geography: Meaning/significance, Types of research, Research Design
- Defining research problems, literature review, identifying research gap, questions and objectives
- Hypothesis building, Framework of research writing, ethics

**Unit II:**
- Materials and methods; Primary and secondary data collection
- Differentiating Questionnaire and Schedule
- Techniques of writing dissertation: drafts, abstract, keywords, citations, references
- Plagiarism: classification and prevention

**Unit III:**
- Fieldwork in Geographical studies: role and significance, Selection of study area
- Pre-field academic preparations, Ethics of fieldwork
- Field techniques: Observation, Interview, Landscape survey (transects, quadrants)
- Post-field tabulation, qualitative/quantitative data analysis
- Emergency handling in fieldwork""",
    'ugc_syllabus': """#### UGC NET Overlaps
- Sampling, Hypothesis Testing
- Methods of Data Collection""",
    'common': [
        ('01-research-design.md', 'Research Design and Problem Formulation',
         'Unit I — Meaning, Types, Design, Problem formulation, Literature review',
         'Conceptual methodologies',
         """* **Meaning of Research**: Systematic investigation to establish novel facts, solve new or existing problems, or prove new ideas.
* **Types**: Pure/Fundamental (theory building), Applied (problem-solving), Explanatory, Descriptive, Exploratory.
* **Research Design**: The blueprint of the study.
* **Formulation**:
  1. *Literature Review*: Existing knowledge base.
  2. *Research Gap*: What hasn't been studied yet.
  3. *Problem Statement*: Clear articulation of the issue.
  4. *Objectives & Questions*: Specific goals of the study.
  5. *Hypothesis*: Testable prediction of the relationship between variables."""),

        ('02-data-collection-methods.md', 'Data Collection and Fieldwork',
         'Unit II & III — Primary/Secondary data, Questionnaire vs Schedule, Observation, Transects',
         'Sources of Geographic Information and Data',
         """* **Primary Data**: First-hand data collected by the researcher.
  - *Observation*: Participant (researcher joins the group) vs. Non-participant.
  - *Questionnaire*: Form filled by the respondent.
  - *Schedule*: Form filled by the enumerator while asking questions (better for illiterate populations).
  - *Landscape Survey*: Using transects (lines cut across a landscape to sample data) and quadrats (sample plots).
* **Secondary Data**: Pre-existing data (Census, government reports, previous research).
* **Pre-field Preparation**: Formulating objectives, creating tools, logistical planning, ethical clearance.
* **Post-field Processing**: Data cleaning, tabulation, statistical analysis, and interpretation."""),

        ('03-academic-writing-and-ethics.md', 'Academic Writing and Research Ethics',
         'Unit II — Writing dissertation, citations, references, Plagiarism',
         'Research ethics',
         """* **Structure of a Dissertation**: Title, Abstract, Introduction, Literature Review, Methodology, Results/Analysis, Discussion, Conclusion, References/Bibliography, Appendices.
* **Citations and Referencing**: Acknowledging sources (APA, MLA, Chicago styles) to give credit and allow readers to trace sources.
* **Fieldwork Ethics**: Informed consent, protecting anonymity, respecting local cultures.
* **Plagiarism**: Presenting someone else's work or ideas as your own.
  - *Types*: Direct copying, mosaic plagiarism (patchwriting), self-plagiarism.
  - *Prevention*: Proper citation, paraphrasing, using plagiarism detection software (e.g., Turnitin)."""),
    ],
    'nep_only': [],
    'ugc_only': []
})

# ============================================================
# 17. HUMAN DEVELOPMENT AND SUSTAINABLE DEVELOPMENT
# ============================================================
SUBJECTS.append({
    'dir': 'human-development',
    'qmd': 'human-development.qmd',
    'title': 'Human Development and Sustainable Development',
    'subtitle': 'Indicators of development, global sustainability goals, and geographies of health.',
    'has_interview': False,
    'nep_syllabus': """#### Core I Paper XIX — Human Development and Sustainable Development
**(4 Credit, Theory: 45hrs, Practical: 30hrs)**

**Unit I:**
- Development: Concept and Indicators, Physical Quality of Life Index (PQLI), HDI
- Goals of Development: Economic Growth, Distributive Justice, Ecological Sustainability
- Sustainable development: Concept and Approaches

**Unit II:**
- Goals to Sustainability: UN's 17 SDGs
- Localizing SDGs
- Population dynamics and SDGs
- Global health and SDGs

**Unit III:**
- Concepts of Health and Wellbeing; Factors affecting Health and Diseases
- Concepts of morbidity, WHO Classification
- Communicable and non-communicable diseases; Epidemiological Transition Theory
- Evolution of health policies in India; Health Inequalities (Regional/Social)""",
    'ugc_syllabus': """#### UGC NET Overlaps
- Concept of development and indicators of development
- Environment and Human Health, Diseases Ecology
- Health Care Planning and Policies in India""",
    'common': [
        ('01-development-indicators.md', 'Indicators of Human Development',
         'Unit I — Concept, Indicators, PQLI, HDI',
         'Concept of development and indicators of development',
         """* **Economic Growth vs. Development**: Growth is quantitative (increase in GDP), Development is qualitative (improvement in living standards, structural change).
* **Physical Quality of Life Index (PQLI)**: Introduced by Morris (1979). Averages three indicators: Basic literacy rate, Infant mortality rate, Life expectancy at age one.
* **Human Development Index (HDI)**: Created by Mahbub ul Haq and Amartya Sen for the UNDP (1990). Measures:
  1. *Long and healthy life* (Life expectancy at birth)
  2. *Knowledge* (Mean years of schooling and Expected years of schooling)
  3. *Decent standard of living* (GNI per capita).
* **Other Indices**: Inequality-adjusted HDI (IHDI), Gender Development Index (GDI), Multidimensional Poverty Index (MPI)."""),

        ('02-sustainable-development-goals.md', 'Sustainable Development and the UN SDGs',
         'Unit I & II — Sustainable development, UN 17 SDGs, Localizing SDGs',
         'Regional imbalances, Planning paradigms',
         """* **Sustainable Development**: "Development that meets the needs of the present without compromising the ability of future generations to meet their own needs" (Brundtland Commission, 1987).
* **The Three Pillars**: Economic viability, environmental protection, and social equity.
* **Agenda 2030 (The 17 SDGs)**: Adopted by UN in 2015. 
  - Key goals: No Poverty (1), Zero Hunger (2), Good Health (3), Quality Education (4), Gender Equality (5), Clean Water (6), Affordable Energy (7), Climate Action (13), Life on Land/Water (14,15).
* **Localizing SDGs**: Adapting global goals to local/national contexts (e.g., NITI Aayog's SDG India Index). Incorporating SDGs into Panchayat planning."""),

        ('03-health-geography-epidemiology.md', 'Geographies of Health and Epidemiology',
         'Unit III — Health and Wellbeing, Morbidity, Epidemiological Transition, Health Inequalities',
         'Environment and Human Health, Diseases Ecology, Health Care Planning',
         """* **Health**: A state of complete physical, mental, and social well-being, not merely the absence of disease (WHO).
* **Morbidity vs. Mortality**: Morbidity refers to the state of being diseased/unhealthy; Mortality is death.
* **Disease Types**: Communicable (infectious, vector-borne) vs. Non-communicable (lifestyle diseases - diabetes, cardiovascular).
* **Epidemiological Transition Theory (Omran)**: Society shifts from high mortality driven by infectious diseases/famine to lower mortality driven by chronic/degenerative diseases as they modernize.
* **Health Inequalities in India**: Massive rural-urban divides in healthcare access. States like Kerala outperforming northern states (BIMARU). Influence of caste and class on health outcomes."""),
    ],
    'nep_only': [],
    'ugc_only': []
})

# ============================================================
# 18. NATURAL HAZARDS AND DISASTER MANAGEMENT
# ============================================================
SUBJECTS.append({
    'dir': 'natural-hazards',
    'qmd': 'natural-hazards.qmd',
    'title': 'Natural Hazards and Disaster Management',
    'subtitle': 'Understanding extreme events, vulnerability, and resilience strategies.',
    'has_interview': False,
    'nep_syllabus': """#### Core I Paper XVII — Natural Hazards and Disaster Management
**(4 Credit, Theory: 45hrs, Practical: 30hrs)**

**Unit I:**
- Understanding Climate Change: Definition, variability, risks
- Disasters, Hazards, Risk, Vulnerability (Concept and Classification)
- Global warming, Greenhouse effect, sea level rise, Adaptation strategy

**Unit II:**
- Physical, social and economic vulnerability
- Disaster Management Cycle, Community Based Disaster Management
- Role of NDMA and SDMA in Mitigation Strategy

**Unit III:**
- Types of Disasters (Natural and Man-made)
- Floods, Droughts, Cyclones, Thunderstorms, Tornado, Earthquakes, Landslides, oil spills, chemical attacks""",
    'ugc_syllabus': """#### UGC NET Overlaps
- Natural Disasters in India (Earthquake, Drought, Flood, Cyclone, Tsunami)
- Geopolitics of Climate Change
- Meteorological Hazards and Disasters""",
    'common': [
        ('01-hazards-vulnerability-risk.md', 'Concepts of Hazards, Vulnerability, and Risk',
         'Unit I & II — Hazards, Risk, Vulnerability (Physical, social, economic)',
         'Natural Disasters Fundamentals',
         """* **Hazard**: A dangerous phenomenon, substance, human activity, or condition that may cause loss of life, injury, property damage, or environmental degradation (The potential threat).
* **Vulnerability**: The characteristics and circumstances of a community, system, or asset that make it susceptible to the damaging effects of a hazard. Types: Physical, Social, Economic, Environmental.
* **Risk**: The probability of harmful consequences. Equation: $\\text{Risk} = \\text{Hazard} \\times \\text{Vulnerability} / \\text{Capacity}$.
* **Disaster**: A serious disruption of the functioning of a community involving widespread losses which exceed the ability of the affected community to cope using its own resources. (A hazard becomes a disaster when it hits a vulnerable population)."""),

        ('02-climate-change-impacts.md', 'Climate Change and Global Warming',
         'Unit I — Climate change, variability, Global warming, sea level rise',
         'Geopolitics of Climate Change',
         """* **Greenhouse Effect**: Natural process warming the Earth's surface. Enhanced greenhouse effect due to anthropogenic emissions ($CO_2, CH_4$, CFCs, $N_2O$).
* **Indicators of Climate Change**: Rising global average temperatures, melting ice caps and glaciers, ocean acidification, increasing frequency of extreme weather events.
* **Sea Level Rise**: Caused by thermal expansion and melting ice. Threatens low-lying coastal areas and island nations.
* **Adaptation vs. Mitigation**:
  - *Mitigation*: Reducing emissions (renewable energy, carbon sinks).
  - *Adaptation*: Adjusting to the effects of climate change (building sea walls, drought-resistant crops)."""),

        ('03-types-of-disasters.md', 'Types of Disasters: Natural and Man-Made',
         'Unit III — Floods, Droughts, Cyclones, Earthquakes, etc.',
         'Natural Disasters in India; Meteorological Hazards',
         """* **Geological Disasters**: Earthquakes (seismic waves along fault lines), Landslides/Avalanches, Volcanoes, Tsunamis (seismically generated sea waves).
* **Hydrometeorological Disasters**: Tropical Cyclones (intense low-pressure systems over warm oceans), Floods (riverine, flash, urban), Droughts (meteorological, hydrological, agricultural), Heat/Cold Waves.
* **Man-Made (Anthropogenic) Disasters**:
  - *Industrial/Chemical*: Bhopal Gas Tragedy (1984), oil spills (Exxon Valdez).
  - *Biological*: Epidemics, biological warfare.
  - *Nuclear*: Chernobyl, Fukushima."""),

        ('04-disaster-management-framework.md', 'Disaster Management and Frameworks',
         'Unit II — Disaster Management Cycle, NDMA, SDMA',
         'Disaster management applications',
         """* **Disaster Management Cycle**:
  1. *Pre-Disaster*: Prevention, Mitigation (structural and non-structural), Preparedness.
  2. *During Disaster*: Early warning, Response (rescue/relief).
  3. *Post-Disaster*: Recovery, Rehabilitation, Reconstruction (Build Back Better).
* **Institutional Framework in India**: Disaster Management Act 2005 shifted approach from relief-centric to proactive mitigation.
  - *NDMA* (National Disaster Management Authority) headed by PM.
  - *SDMA* (State level) headed by CM.
  - *DDMA* (District level) headed by District Magistrate.
* **Global Frameworks**: Yokohama Strategy (1994), Hyogo Framework (2005), Sendai Framework for Disaster Risk Reduction (2015-2030)."""),
    ],
    'nep_only': [],
    'ugc_only': []
})

# ============================================================
# 19. NATURAL RESOURCE MANAGEMENT
# ============================================================
SUBJECTS.append({
    'dir': 'natural-resource-management',
    'qmd': 'natural-resource-management.qmd',
    'title': 'Natural Resource Management',
    'subtitle': 'Conservation, valuation, and sustainable management of Earth\'s resources.',
    'has_interview': False,
    'nep_syllabus': """#### Core I Paper XVIII — Natural Resource Management and Geo informatics
**(4 Credit, Theory: 45hrs, Practical: 30hrs)**

**Unit I:**
- Natural Resource Base (land, water, forest, biodiversity, minerals), classification
- Resource depletion, pollution, Resource Curse
- Historical evolution of NRM Measures: policy and practices
- Valuation methods of Natural resources

**Unit II:**
- Management under different property regimes (Public, Private, Commons, Open Access)
- Tragedy of commons, Ostrom's idea on institutional designs
- Measures: Watershed Management, Forest Management, JFM - Forest Rights Act, CAMPA
- Wetland Management, Grassland Management, Ecosystem services (Millennium Ecosystem Assessment)

**Unit III:**
- Application of Geoinformatics in mapping and monitoring NR
- Watershed delineation, soil erosion estimation, forest monitoring, ground water management
- Conjunctive use of tank-groundwater, Adaptive crop planning""",
    'ugc_syllabus': """#### UGC NET Overlaps
- Natural Resources (classification, distribution and associated problems), Natural Resources Management
- Conservation and management of ecosystems
- Remote sensing applications in resource management""",
    'common': [
        ('01-natural-resource-base.md', 'Natural Resource Base and Classification',
         'Unit I — Resource Base, classification, Resource Curse, Valuation',
         'Natural Resources (classification, distribution)',
         """* **Resource Types**: Land, Water, Forest, Biodiversity, Minerals. Classified by renewability (flow/fund vs. stock), origin (biotic/abiotic).
* **Resource Depletion**: Exhaustion of non-renewables and overuse of renewables beyond their regenerative capacity.
* **Resource Curse (Paradox of Plenty)**: Phenomenon where countries with abundant natural resources (like oil or diamonds) tend to have less economic growth, less democracy, and worse development outcomes than countries with fewer natural resources.
* **Valuation of Natural Resources**: Assigning economic value to ecosystems to prevent their destruction. 
  - *Use values* (direct/indirect) vs. *Non-use values* (existence, bequest).
  - Methods: Contingent Valuation, Travel Cost Method, Hedonic Pricing."""),

        ('02-property-regimes-and-commons.md', 'Property Regimes and The Tragedy of the Commons',
         'Unit II — Property regimes, Tragedy of commons, Ostrom\'s idea',
         'Resource Management and Governance',
         """* **Property Regimes**:
  - *Private Property*: Owned by individuals/corporations.
  - *State/Public Property*: Owned by government.
  - *Common Property*: Owned and managed collectively by a defined group.
  - *Open Access*: No rules or ownership; free-for-all.
* **Tragedy of the Commons (Garrett Hardin, 1968)**: In open-access systems, individuals acting in rational self-interest will deplete the shared resource, ruining it for everyone.
* **Elinor Ostrom's Response**: Proved that communities can and do successfully manage Common Pool Resources (CPRs) without top-down state regulation or privatization, provided there are clear boundaries, local rule-making, and conflict resolution mechanisms (Nobel Prize in Economics, 2009)."""),

        ('03-resource-management-strategies.md', 'Ecosystem Services and Management Strategies',
         'Unit II — Ecosystem services, Watershed/Forest/Wetland management, JFM, FRA',
         'Conservation and management of ecosystems',
         """* **Ecosystem Services (Millennium Ecosystem Assessment)**:
  1. *Provisioning* (food, water, timber)
  2. *Regulating* (climate regulation, water purification)
  3. *Cultural* (recreation, spiritual)
  4. *Supporting* (nutrient cycling, soil formation).
* **Watershed Management**: Treating the watershed as a unified hydrological and ecological unit for soil and moisture conservation.
* **Joint Forest Management (JFM)**: Partnership between state forest departments and local communities to protect and manage degraded forests.
* **Forest Rights Act (FRA, 2006)**: Recognizes pre-existing rights of forest-dwelling tribal communities and other traditional forest dwellers over forest land and resources.
* **CAMPA (Compensatory Afforestation Fund)**: Funds collected for diverting forest land for non-forest purposes, used for afforestation."""),

        ('04-geoinformatics-in-nrm.md', 'Geoinformatics in Natural Resource Management',
         'Unit III — Application of Geoinformatics in monitoring NR',
         'Remote sensing applications',
         """* **Mapping and Monitoring**: Satellite imagery enables continuous monitoring of remote and vast natural resources.
* **Applications**:
  - *Forestry*: Deforestation tracking, forest fire detection, biomass estimation using NDVI.
  - *Water*: Watershed delineation (using DEMs), water body area extraction, groundwater potential mapping.
  - *Soil*: Erosion modeling (Universal Soil Loss Equation - USLE) using slope, land cover, and rainfall data in GIS.
  - *Agriculture*: Adaptive crop planning, precision agriculture.
* **Decision Support**: Geoinformatics integrates multiple data layers to help policymakers make informed conservation decisions."""),
    ],
    'nep_only': [],
    'ugc_only': []
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
        
        # Clean directory if exists
        if os.path.exists(dir_path):
            for f in glob.glob(os.path.join(dir_path, '*.md')): os.remove(f)
        else:
            os.makedirs(dir_path)
            
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
