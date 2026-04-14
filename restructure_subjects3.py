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
# 8. SOCIAL AND CULTURAL GEOGRAPHY
# ============================================================
SUBJECTS.append({
    'dir': 'social-and-cultural',
    'qmd': 'social-and-cultural.qmd',
    'title': 'Social & Cultural Geography',
    'subtitle': 'Spatial dimensions of culture, identity, religion, language, and social structures.',
    'has_interview': False,
    'nep_syllabus': """#### Core I Paper XXIII — Social and Cultural Geography
**(4 Credit, Theory: 45hrs, Practical: 30hrs)**

**Unit I:**
- Definition and Nature of Social Geography; Social Geography in the Realm of Social Sciences
- Concept of Social Structure and its importance in Indian Social Realities
- Understanding early Cultural differentiation and Social Formation in India (Mahajanapadas, Mughal/British Provinces)

**Unit II:**
- Concepts of Social Justice and Wellbeing; Social Inclusion and Exclusion
- Geographies of Gated Communities and Slums
- Social Empowerment: Major Schemes of Social Empowerment in India
- Affordable Housing Schemes for Low and Moderate-income Resident in India (PMAY, BPGY)

**Unit III:**
- Meaning and definition of Culture and Cultural traits
- Types of Culture: Folk and Popular Culture, Cultural Realms, Cultural Hearth
- Diffusion of Innovations: Spatial Dimensions
- Concept, Types and Conservation of Geo-heritage""",
    'ugc_syllabus': """#### Unit VII — Political, Social, and Cultural Geography

**II. Social Geography**
- Nature and scope of social geography
- Social structure and processes 
- Social Well-being and Quality of Life, Social Exclusion
- Spatial distribution of social groups in India (Tribe, Caste, Religion and Language)
- Environment and Human Health, Diseases Ecology
- Nutritional Status (classification and spatial/seasonal patterns in India)
- Health Care Planning and Policies in India, Medical Tourism

**III. Cultural Geography**
- Nature and scope of cultural geography
- Concept of Culture, Cultural Complexes, Areas and Region, Cultural Heritage, Cultural Ecology, Cultural Convergence
- Theories of tribal groups
- Dwelling places as cultural expressions""",
    'common': [
        ('01-social-geography-nature-scope.md', 'Nature and Scope of Social Geography',
         'Unit I — Definition and Nature of Social Geography',
         'Nature and scope of social geography',
         """* **Social Geography**: Study of spatial patterns of social phenomena, social structures, and social groups in interaction with their environment.
* **Nature**: Evolved from welfare geography and radical geography. Focuses on social justice, inequality, deprivation, and social well-being.
* **Scope**: Includes studies of caste, tribe, religion, language, gender, poverty, health, education, housing, and social segregation.
* **Approaches**: Positivist (spatial science), Humanist (lived experience of social groups), Radical (structural inequalities), Feminist (gendered spaces)."""),

        ('02-social-structure-indian-context.md', 'Social Structure and Indian Realities',
         'Unit I — Concept of Social Structure; Indian Social Realities',
         'Social structure and processes; Spatial distribution of social groups in India (Tribe, Caste, Religion, Language)',
         """* **Social Structure**: The patterned social arrangements in society that are emergent from and determinant of the actions of individuals (e.g., class, caste).
* **Caste System (Jati/Varna)**: Hierarchical social stratification unique to India. Influences residential segregation (Dalit bastis), occupational structure, and land ownership.
* **Tribal Groups (Adivasis)**: Indigenous populations with distinct cultures, mostly concentrated in Central India (Gonds, Santhals, Bhils) and Northeast India.
* **Linguistic Diversity**: Indo-Aryan (North), Dravidian (South), Austroasiatic (Central), Tibeto-Burman (Northeast). Language families form major cultural fault lines.
* **Religious Composition**: Hindu majority with significant Muslim, Christian, Sikh, Buddhist, and Jain communities. Spatial concentration patterns."""),

        ('03-social-justice-wellbeing.md', 'Social Justice, Wellbeing, and Exclusion',
         'Unit II — Concepts of Social Justice and Wellbeing; Social Inclusion and Exclusion',
         'Social Well-being and Quality of Life, Social Exclusion',
         """* **Social Wellbeing**: Beyond economic growth (GDP) to include quality of life metrics (HDI, PQLI) — health, education, housing, environment.
* **Social Justice (David Harvey)**: Equitable distribution of society's resources and opportunities across space. Territorial social justice.
* **Social Exclusion**: Process by which individuals or groups are systematically marginalized from rights, resources, and opportunities normally available to members of society (e.g., untouchability, gender discrimination).
* **Spatial Segregation**: Physical separation of groups (residential segregation by race, class, or caste)."""),

        ('04-cultural-geography-nature-scope.md', 'Cultural Geography and Core Concepts',
         'Unit III — Meaning and definition of Culture and Cultural traits',
         'Nature and scope of cultural geography; Concept of Culture, Cultural Complexes',
         """* **Cultural Geography**: Study of cultural products and norms and their variations across spaces and places. (Pioneered by Carl Sauer - Berkeley School).
* **Culture**: Total way of life of a group — material (tools, housing) and non-material (beliefs, language, religion).
* **Cultural Trait**: Smallest distinguishing item of culture (e.g., wearing a turban, using chopsticks).
* **Cultural Complex**: A related set of cultural traits (e.g., cattle keeping complex in East Africa).
* **Cultural Ecology**: Study of human adaptations to social and physical environments."""),

        ('05-cultural-regions-and-hearths.md', 'Cultural Hearths, Realms, and Diffusion',
         'Unit III — Cultural Realms of the World, Cultural Hearth; Diffusion of Innovations',
         'Concept of culture-areas and cultural regions; Cultural Convergence',
         """* **Cultural Hearth**: Area where a culture or cultural trait originates (e.g., Mesopotamia, Indus Valley, Mesoamerica).
* **Cultural Realm**: Large geographical region where culture traits maintain homogeneity (e.g., Islamic Realm, Slavic Realm, Latin American Realm) — Broek and Webb's classification.
* **Cultural Diffusion**: Spread of cultural traits from a hearth.
  - *Expansion Diffusion*: Contagious (snowballing), Hierarchical (top-down), Stimulus (idea spreads but modified).
  - *Relocation Diffusion*: Physical movement of people carrying the culture.
* **Cultural Convergence**: Cultures becoming more alike due to globalization and technology."""),

        ('06-heritage-and-dwellings.md', 'Heritage and Cultural Landscapes',
         'Unit III — Concept, Types and Conservation of Geo-heritage',
         'Cultural Heritage; Dwelling places as cultural expressions',
         """* **Cultural Landscape**: The visible imprint of human activity on the landscape (Sauer's morphology of landscape).
* **Dwelling Places**: Folk housing styles reflecting local climate, materials, and cultural beliefs (e.g., Igloos, Yurts, Stilt houses, Courtyard houses in India).
* **Geo-heritage**: Sites of geological and geomorphological significance that provide scientific, educational, and cultural value (e.g., Lonar Crater, columnar basalts).
* **Conservation**: UNESCO World Heritage sites, role of local communities in protecting heritage as part of identity."""),

        ('07-urban-social-geography.md', 'Geographies of Slums and Gated Communities',
         'Unit II — Geographies of Gated Communities and Slums',
         'Social structure and processes (Urban context)',
         """* **Slums**: Heavily populated urban informal settlements characterized by substandard housing, squalor, and lack of basic services. Often a result of rural-urban migration and poverty.
* **Gated Communities**: Residential areas with restricted access, walls/fences, and privatized public spaces. 
* **Spatial Polarization**: Increasing divide between wealthy (gated enclaves) and poor (slums) within the same city — fragmenting the urban fabric and reducing social cohesion."""),
    ],
    'nep_only': [
        ('08-social-formations-india.md', 'Historical Social Formations in India',
         'Unit I — Early Cultural differentiation (Mahajanapadas, Mughal, British)',
         """* **Mahajanapadas (c. 600 BCE)**: Sixteen ancient kingdoms/republics in the Indo-Gangetic plain — early state formation, urbanization, and distinct regional cultures (Magadha, Kosala, Vatsa).
* **Mughal Provinces (Subahs)**: Administrative reorganization of space that left deep cultural, architectural, and linguistic (Urdu) impacts.
* **British Provinces**: Reorganization of Indian geography for colonial extraction and administration (Presidencies of Bengal, Bombay, Madras) — created new urban centers and transport networks, disrupting traditional spatial economies."""),

        ('09-social-empowerment-housing.md', 'Social Empowerment and Housing Schemes',
         'Unit II — Major Schemes of Social Empowerment; Affordable Housing Schemes',
         """* **Social Empowerment**: Policies aiming to uplift marginalized groups (SC/ST/OBC/Women) through education, employment (reservations/affirmative action), and financial inclusion.
* **Affordable Housing**: Essential for wellbeing and dignity.
  - *Pradhan Mantri Awas Yojana (PMAY)*: 'Housing for All' — urban and rural components.
  - *Biju Pucca Ghar Yojana (BPGY)*: Odisha state-specific scheme to convert kachha houses to pucca houses for rural poor.
* **Geographical Impact**: Transforming rural and urban landscapes, altering socio-economic mobility."""),
    ],
    'ugc_only': [
        ('10-geography-of-health.md', 'Environment, Human Health, and Disease Ecology',
         'Environment and Human Health, Diseases Ecology',
         """* **Medical/Health Geography**: Spatial aspects of health and healthcare delivery.
* **Disease Ecology**: Interaction between humans, pathogens, and the environment (Triangle of Human Ecology: Habitat, Population, Behaviour).
* **Endemic vs. Epidemic vs. Pandemic**: Spatial scale of disease outbreaks.
* **Environmental Links**: Water-borne diseases (cholera), vector-borne diseases (malaria, dengue - linked to climate and standing water), air-pollution related diseases (respiratory issues in cities).
* **Epidemiological Transition**: Shift from infectious/communicable diseases to chronic/degenerative diseases as a society develops."""),

        ('11-nutrition-and-healthcare.md', 'Nutritional Status and Health Care Planning',
         'Nutritional Status; Health Care Planning and Policies in India, Medical Tourism',
         """* **Nutritional Status**: Spatial and seasonal distribution of malnutrition, stunting, and wasting in India. Links to agricultural productivity, poverty, and dietary habits.
* **Health Care Planning**: Distribution of health infrastructure (PHCs, CHCs, district hospitals). Urban bias in healthcare provision. Rural-urban/inter-state disparities in doctor-patient ratios.
* **National Health Mission (NHM)**: Policy intervention to address health inequities.
* **Medical Tourism**: Travel for medical treatment. India as a hub due to quality private healthcare, skilled doctors, and lower costs compared to Western countries. Impact on local healthcare systems (brain drain to private sector)."""),
        
        ('12-theories-of-tribal-groups.md', 'Theories of Tribal Groups',
         'Theories of tribal groups',
         """* **Anthropological/Geographical Approaches**: Study of indigenous populations.
* **Isolation vs. Integration vs. Assimilation**:
  - *Isolation (Elwin)*: 'National Park' policy to preserve tribal culture from outside exploitation.
  - *Assimilation (Ghurye)*: Viewed tribals as 'backward Hindus' who should integrate into the mainstream.
  - *Integration (Nehru)*: 'Panchsheel' policy — integration without destroying their distinctive culture and land rights.
* **Spatial Challenges**: Displacement by development projects, forest rights conflicts, cultural erosion."""),
    ],
})

# ============================================================
# 9. REGIONAL DEVELOPMENT AND PLANNING
# ============================================================
SUBJECTS.append({
    'dir': 'regional-development',
    'qmd': 'regional-development.qmd',
    'title': 'Regional Development',
    'subtitle': 'Theories, strategies, and spatial patterns of regional planning and development.',
    'has_interview': False,
    'nep_syllabus': """#### Core I Paper XI — Regional Planning and Development
**(4 Credit, Theory: 45hrs, Practical: 30hrs)**

**Unit I:**
- Concept of Region, Types of regions: Formal, Functional and Planning Region
- Need for Regional Planning, Characteristics of an Ideal Planning Region
- Delineation of Formal and Functional regions
- Planning Regions; Approaches and Methods

**Unit II:**
- Theories and Models for Regional Planning
- Growth Pole Model of Perroux; Myrdal, Hirschman, Rostow
- Export Base Model, Core-Periphery Model
- Modified Growth Foci approach of R.P. Mishra

**Unit III:**
- Regional Disparity and Imbalances in India
- Strategies for balanced regional development in India through Policies and Programmes in FYPs
- Concept and characteristics of city master plan, NCR Planning
- Decentralised planning in India: District Plan and Multi Level Planning
- Formulation and function of Niti Ayog; Development planning skills
- Welfare program of different sectors; Special Component plan (Tribal Sub Plan and Weaker Section)
- Participatory planning""",
    'ugc_syllabus': """#### Unit VIII — Regional Planning

- Regional concept in geography, Concept of planning region
- Types of regions and methods of regionalisation (Formal and Functional)
- Regional hierarchy
- Conceptual and theoretical framework of regional planning
- Theories of Regional Development (Albert O. Hirschman, Gunnar Myrdal, John Friedman)
- Dependency theory of Underdevelopment, Global Economic Blocks
- Concept of development and indicators of development
- Regional imbalances, World Regional Disparities
- Regional planning in India
- Regional Development and Social Movements in India""",
    'common': [
        ('01-concept-and-types-of-regions.md', 'Concept and Types of Regions',
         'Unit I — Concept of Region, Types of regions: Formal, Functional and Planning',
         'Regional concept in geography; Types of regions and methods of regionalisation',
         """* **Concept of Region**: An area of the Earth's surface with specific characteristics that distinguish it from surrounding areas. A mental construct created for spatial study.
* **Formal Region (Uniform/Homogeneous Region)**: An area defined by one or more common attributes (e.g., climate type, language, crop region). Example: the Corn Belt.
* **Functional Region (Nodal Region)**: An area organized around a node or focal point, connected by flows (transport, communication, trade). Example: a metropolitan area and its commuter zone.
* **Planning Region**: An area delineated for administrative or planning purposes. Must be large enough to take investment decisions, have some resource homogeneity, and possess an administrative center.
* **Regional Hierarchy**: Regions exist at different scales (macro, meso, micro) forming a nested hierarchy."""),

        ('02-delineation-of-regions.md', 'Delineation of Regions',
         'Unit I — Delineation of Formal and Functional regions; Approaches and Methods',
         'Types of regions and methods of regionalisation',
         """* **Delineation of Formal Regions**: Finding boundaries where homogeneity ends.
  - *Methods*: Superimposition of index maps, Statistical methods (Weighted Index, Principal Component Analysis/Cluster Analysis).
* **Delineation of Functional Regions**: Defining the sphere of influence of a node.
  - *Methods*: Flow analysis (traffic, commuting, telephone calls), Gravitational analysis (Reilly's Law of Retail Gravitation, breaking point equation).
* **Characteristics of an Ideal Planning Region**: Contiguous, viable resource base, social-cultural cohesion, administrative convenience, nodal center."""),

        ('03-development-indicators-disparities.md', 'Development Concepts and Regional Disparities',
         'Unit III — Regional Disparity and Imbalances in India',
         'Concept and indicators of development; Regional imbalances, World Regional Disparities',
         """* **Concept of Development**: Structural transformation of economy, society, and institutions leading to improved quality of life (not just economic growth).
* **Indicators of Development**: 
  - *Economic*: Per capita income, GDP, sectoral composition.
  - *Social*: Literacy rate, life expectancy, IMR, poverty ratio.
  - *Composite*: Human Development Index (HDI), Multidimensional Poverty Index (MPI), Gender Inequality Index (GII).
* **Regional Imbalances**: Unequal distribution of resources, industrialization, and infrastructure across space. Caused by historical advantages, physical geography, and cumulative causation.
* **Measurement**: Coefficient of Variation, Lorenz Curve, Gini Coefficient, Principal Component Analysis of socio-economic variables."""),

        ('04-theories-of-regional-development-1.md', 'Theories of Regional Development: Perroux, Myrdal, Hirschman',
         'Unit II — Growth Pole Model (Perroux); Myrdal, Hirschman',
         'Theories of Regional Development (Albert O. Hirschman, Gunnar Myrdal)',
         """* **Growth Pole Theory (François Perroux, 1955)**: Economic growth does not appear everywhere at once; it appears at points/poles with variable intensities. Based on propulsive industries and agglomeration economics (abstract economic space, not geographic space). Expanded to geographic space by Boudeville (Growth Centers).
* **Cumulative Causation Theory (Gunnar Myrdal, 1957)**: Capitalist system naturally increases regional inequalities.
  - *Backwash Effects*: Negative impacts on the periphery (brain drain, capital flight) leading to underdevelopment.
  - *Spread Effects*: Positive impacts radiating from center to periphery. (Usually, backwash > spread).
* **Unbalanced Growth Theory (Albert Hirschman, 1958)**: Investments should be concentrated in key sectors/regions.
  - *Polarization Effects* (similar to backwash) vs. *Trickle-down Effects* (similar to spread)."""),

        ('05-theories-of-regional-development-2.md', 'Theories of Regional Development: Friedman, Rostow',
         'Unit II — Rostow, Core-Periphery Model',
         'Theories of Regional Development (John Friedman)',
         """* **Core-Periphery Model (John Friedmann, 1966)**: Four stages of spatial organization during economic development.
  1. Pre-industrial (independent local centers)
  2. Transitional (single strong core, exploited periphery)
  3. Industrial (development of sub-cores, reducing inequality)
  4. Post-industrial (functionally interdependent polycentric system)
* **Stages of Economic Growth (W.W. Rostow, 1960)**: Linear, historical model of national development (Traditional society → Pre-conditions for take-off → Take-off → Drive to maturity → Age of high mass consumption).
* **Modified Growth Foci Approach (R.P. Misra)**: Adapted Growth Pole theory for India (five-tier hierarchy: Growth Poles, Growth Centers, Growth Points, Service Centers, Central Villages) to bridge the gap between urban centers and rural hinterlands."""),

        ('06-regional-planning-india.md', 'Regional Planning in India',
         'Unit III — Strategies in FYPs, Decentralised planning, Niti Ayog',
         'Regional planning in India',
         """* **Planning Commission and FYPs**: Five-Year Plans aimed at balanced regional development. Evolution from centralized sectoral planning to spatial/regional considerations.
* **NITI Aayog**: Replaced Planning Commission. Focus on 'cooperative federalism', bottom-up approach, think-tank role.
* **Multi-Level Planning**: Central → State → District → Block → Gram Panchayat (73rd/74th Constitutional Amendments).
* **District Planning**: Constitution of District Planning Committees (DPCs) to integrate rural and urban plans.
* **Target Area/Group Planning**: Addressed specific geographic disadvantages.
  - *Area*: Drought Prone Area Prog (DPAP), Hill Area Development Prog (HADP), Command Area Development (CAD).
  - *Groups*: Tribal Sub Plan (TSP), Special Component Plan for Scheduled Castes."""),
    ],
    'nep_only': [
        ('07-urban-planning-ncr.md', 'Urban Planning and NCR',
         'Unit III — Concept of city master plan, NCR Planning',
         """* **City Master Plan / Development Plan**: Long-term (20-25 years) statutory document outlining land use zoning, transport networks, housing, and infrastructure for an urban area.
* **National Capital Region (NCR) Planning**: Concept of regional planning to manage the explosive growth of Delhi by developing surrounding counter-magnet cities in adjoining states (Haryana, UP, Rajasthan). Aims to decentralize population and economic activity.
* **Participatory Planning**: Involving local communities, NGOs, and stakeholders in the planning process to ensure plans reflect real needs (e.g., Kerala model of decentralized planning)."""),
    ],
    'ugc_only': [
        ('08-dependency-world-systems.md', 'Dependency Theory and Global Economic Blocks',
         'Dependency theory of Underdevelopment, Global Economic Blocks',
         """* **Dependency Theory (Frank, Amin)**: Underdevelopment of the Global South is a direct result of its historical and ongoing exploitation by the developed Global North (development of underdevelopment). Extracting raw materials and providing a market for manufactured goods.
* **World Systems Theory (Immanuel Wallerstein)**: Global capitalist system divided into Core, Semi-periphery, and Periphery. Exploitative trade relations.
* **Global Economic Blocks**: EU, NAFTA, ASEAN, MERCOSUR. Their role in shaping global regional development, creating core regions of prosperity while sometimes marginalizing non-members."""),

        ('09-social-movements-regional-development.md', 'Regional Development and Social Movements in India',
         'Regional Development and Social Movements in India',
         """* **Statehood Demands**: Movements for separate states based on regional deprivation and cultural identity (Telangana, Gorkhaland, Bodoland, Vidarbha).
* **Environmental/Developmental Movements**: Resistance against displacement by large projects (dams, mining) pointing to unequal distribution of development costs and benefits (Narmada Bachao Andolan, anti-POSCO).
* **Agrarian Movements**: Farmers' protests regarding pricing, subsidies, and land rights.
* **Impact on Planning**: Forces the state to rethink top-down development paradigms and acknowledge regional aspirations and ecological limits."""),
    ],
})

# ============================================================
# 10. CARTOGRAPHY AND STATISTICAL METHODS
# ============================================================
SUBJECTS.append({
    'dir': 'statistical-geography',
    'qmd': 'statistical-geography.qmd',
    'title': 'Statistical Geography',
    'subtitle': 'Quantitative methods, spatial statistics, and data analysis for geographic research.',
    'has_interview': False, # Merging Statistical and Cartography into Statistical/Quantitative
    'nep_syllabus': """#### Core I Paper IV — Cartography and Geo-Spatial Techniques
*(Note: Quantitative topics are distributed in practical sections of various NEP papers)*

**Cartography & Data:**
- Scientific basis of Cartography, needs of map making, characteristics of maps
- Geographical Coordinates, Graticules, Types of Scales (Plain, Diagonal)
- Types of Map Projection, Transformation of area, Distance and Direction
- Drawing of Choropleth and isopleth maps
- Traffic flow diagram and Drawing of Isochrones, Isotims, Isodapanes
- Slope Analysis (Wentworth's method and Smith)
- Determination of agricultural efficiency (Kendall and Bhatia method)
- Delineation of crop combination regions (Weaver and Doi's method)
- Nearest Neighbour Analysis, Rank Size Rule""",
    'ugc_syllabus': """#### Unit IX/X — Statistical Methods and Cartography

**I. Statistical Methods**
- Sources of Geographic Information and Data (spatial and non-spatial)
- Applications of Measures of Central Tendency, Dispersion and Inequalities
- Sampling, Sampling Procedure and Hypothesis Testing (chi square, t-test, ANOVA)
- Time Series Analysis, Correlation and Regression Analysis
- Measurement of Indices, Making Indicators Scale Free, Computation of Composite Index
- Principal Component Analysis and Cluster Analysis
- Morphometric Analysis: Stream ordering, Bifurcation ratio, Drainage density, Slope Analysis

**II. Cartography**
- Types of Maps, Techniques of Map Making
- Data Representation on Maps (Pie diagrams, Bar diagrams, Line Graph)
- Thematic maps: Choropleth, Isarithmic, Dasymetric, Chorochromatic, Flow Maps""",
    'common': [
        ('01-geographic-data-sources.md', 'Geographic Data Sources and Types',
         'Geo-spatial techniques data handling',
         'Sources of Geographic Information and Data (spatial and non-spatial)',
         """* **Spatial Data**: Information tied to a specific location on Earth's surface (coordinates, addresses, administrative boundaries).
* **Non-Spatial (Attribute) Data**: Characteristics or qualities of the spatial features (population count, soil type name, temperature reading).
* **Data Sources**:
  - *Primary*: Field survey, interviews, observations, GPS data collection.
  - *Secondary*: Census reports, meteorological data, satellite imagery, topographical maps, statistical abstracts.
* **Measurement Scales**: Nominal (categories), Ordinal (rank), Interval (no true zero, e.g., Temp in C), Ratio (true zero, e.g., population, distance)."""),

        ('02-measures-of-central-tendency-dispersion.md', 'Measures of Central Tendency and Dispersion',
         'Basic statistical analysis required for practical geography',
         'Applications of Measures of Central Tendency, Dispersion and Inequalities',
         """* **Central Tendency**: Identifying the 'center' of a dataset.
  - *Mean*: Arithmetic average. Sensitive to outliers. Spatial mean (mean center) for spatial data.
  - *Median*: Middle value. Robust to outliers.
  - *Mode*: Most frequent value.
* **Dispersion**: How spread out the data is around the center.
  - *Range*, *Quartile Deviation*.
  - *Standard Deviation (SD)*: Average distance from the mean. Standard distance in spatial analysis.
  - *Coefficient of Variation (CV)*: (SD/Mean)*100. Useful for comparing variability of datasets with different units.
* **Inequalities Measurement**: Lorenz Curve (graphical), Gini Coefficient (numerical, 0 to 1)."""),

        ('03-correlation-regression.md', 'Correlation and Regression Analysis',
         'Applied in geographical data analysis',
         'Correlation and Regression Analysis',
         """* **Correlation**: Measures the strength and direction of the linear relationship between two variables (x and y).
  - *Pearson's Product Moment (r)*: For interval/ratio data. Ranges from -1 to +1.
  - *Spearman's Rank Correlation (Rho)*: For ordinal (ranked) data.
* **Regression**: Predictive modeling. Investigates the dependence of a dependent variable (Y) on one or more independent variables (X).
  - *Linear Regression*: $Y = a + bX$. 'a' is intercept, 'b' is slope (regression coefficient).
  - *Residuals*: The difference between observed and predicted values. Mapping residuals (Spatial Regression) reveals spatial anomalies."""),

        ('04-map-projections-and-scales.md', 'Map Projections and Scales',
         'Types of Scales, Map Projection, Transformation',
         'Techniques of Map Making',
         """* **Map Scale**: Ratio between map distance and ground distance. Types: Representative Fraction (RF), Statement, Graphical/Linear, Diagonal.
* **Map Projection**: Mathematical transformation of the 3D Earth surface to a 2D plane. Always involves distortion.
* **Preserved Properties**:
  - *Conformal (Orthomorphic)*: Preserves exact local angles/shapes (e.g., Mercator). Used for navigation.
  - *Equal-Area (Equivalent)*: Preserves area proportions (e.g., Mollweide, Peters). Used for thematic density distributions.
  - *Equidistant*: Preserves correct distance from a center point.
  - *Azimuthal*: Preserves correct direction from a center point.
* **Developable Surfaces**: Cylinder, Cone, Plane (Zenithal)."""),

        ('05-thematic-mapping-techniques.md', 'Thematic Mapping Techniques',
         'Drawing of Choropleth, Isopleth, Traffic flow diagrams',
         'Data Representation: Choropleth, Isarithmic, Dasymetric, Chorochromatic, Flow Maps',
         """* **Choropleth Map**: Uses shading/colors within predefined administrative boundaries (states, districts) to represent derived data (densities, rates, percentages). Not suitable for absolute numbers.
* **Isopleth (Isarithmic) Map**: Lines connecting points of equal value (e.g., isotherms, contours, isobars). Assumes continuous data mapped over an isotropic surface.
* **Chorochromatic Map**: Color-patch map showing qualitative distribution without numerical value (e.g., soil types, land use zones).
* **Dasymetric Map**: Advanced choropleth that ignores administrative boundaries in favor of actual geographical boundaries of the phenomenon (e.g., mapping population density excluding lakes and forests).
* **Flow Map**: Lines of varying thickness show direction and volume of movement (trade, traffic, migration).
* **Diagrammatic Maps**: Pie diagrams, bar graphs superimposed on maps to show multiple variables."""),

        ('06-spatial-statistics-indices.md', 'Spatial Statistics and Morphometry',
         'Nearest Neighbour, Slope Analysis (Wentworth), Agricultural indices',
         'Morphometric Analysis, Measurement of Indices',
         """* **Nearest Neighbour Analysis (NNA)**: Measures spatial arrangement of points. Nearest Neighbour Index (R). R=0 (clustered), R=1 (random), R=2.15 (perfectly uniform/regular).
* **Slope Analysis**: Wentworth's method (based on contour crossings per grid cell) and Smith's method (relative relief combined with drainage density).
* **Drainage Morphometry**:
  - *Stream Ordering*: Strahler (only same orders combine to increase) vs. Horton.
  - *Bifurcation Ratio*: Ratio of number of streams of a given order to the number of next higher order.
  - *Drainage Density*: Total stream length / basin area.
* **Indices**: Agricultural efficiency (Kendall's ranking, Bhatia's weighted output). Handling data of different scales requires normalization (Z-scores) to create composite indices."""),
    ],
    'nep_only': [
        ('07-agricultural-delineation-methods.md', 'Agricultural Delineation Methods',
         'Delineation of crop combination regions (Weaver and Doi\'s method)',
         """* **Crop Combination Analysis**: Identifying the dominant crop assemblages in a region to understand agricultural regionalization.
* **Weaver's Method (1954)**: Calculates variance ($\Sigma d^2 / n$) between actual crop percentages and theoretical distributions (monoculture = 100%, 2-crop = 50% each, etc.). The combination with the lowest variance is chosen.
* **Doi's Modification (1959)**: Simplified Weaver's method by using $\Sigma d^2$ without dividing by 'n' and providing a ready-to-use critical value table, making manual calculation much faster while yielding similar results."""),
    ],
    'ugc_only': [
        ('08-hypothesis-testing.md', 'Sampling and Hypothesis Testing',
         'Sampling, Sampling Procedure and Hypothesis Testing (chi square, t-test, ANOVA)',
         """* **Sampling**: Selecting a subset from a population. Probability (Random, Stratified, Systematic, Cluster) vs. Non-probability (Purposive, Snowball, Quota).
* **Hypothesis Testing**: Formulating Null ($H_0$) and Alternative ($H_1$) hypotheses. Type I error (reject true $H_0$) and Type II error (accept false $H_0$). Set significance level ($\alpha$, usually 0.05).
* **Parametric Tests** (assume normal distribution):
  - *t-test*: Compares means of two groups.
  - *ANOVA (Analysis of Variance)*: Compares means of three or more groups.
* **Non-Parametric Tests** (distribution-free):
  - *Chi-Square ($\chi^2$) Test*: Tests association between categorical variables (observed vs. expected frequencies)."""),

        ('09-advanced-multivariate-techniques.md', 'Advanced Multivariate Analysis',
         'Principal Component Analysis and Cluster Analysis',
         """* **Principal Component Analysis (PCA)**: A dimensionality reduction technique. Correlated variables are transformed into a smaller number of uncorrelated variables called Principal Components. PC1 accounts for the most variance. Used to create composite indices (e.g., developmental index from 15 variables).
* **Cluster Analysis**: Grouping a set of objects such that objects in the same group (cluster) are more similar to each other than to those in other groups. Used in regionalization (e.g., grouping districts based on socio-economic similarity).
* **Time Series Analysis**: Analyzing sequence of data points over time to extract meaningful statistics. Components: Trend (long term), Cyclical, Seasonal, Irregular/Random variations. Used for climate data or population forecasting."""),
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
