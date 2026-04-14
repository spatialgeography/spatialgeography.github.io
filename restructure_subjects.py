"""
Script to restructure all geography subject directories with proper
NEP-2020 and UGC NET syllabus attribution.
For each subject:
  1. Creates a clean 00-syllabus.md with both NEP and UGC NET sections
  2. Creates numbered topic MD files with syllabus tags
  3. Rewrites the QMD index with organized sections
"""
import os
import glob
import shutil

BASE = r"e:\spatialgeography\geography"

# ============================================================
# SUBJECT DEFINITIONS
# Each subject has:
#   - 'dir': directory name
#   - 'qmd': QMD filename
#   - 'title': display title
#   - 'subtitle': subtitle for QMD
#   - 'nep_syllabus': NEP-2020 syllabus text (for 00-syllabus.md)
#   - 'ugc_syllabus': UGC NET syllabus text (for 00-syllabus.md)
#   - 'common': list of (filename, title, nep_ref, ugc_ref, content)
#   - 'nep_only': list of (filename, title, nep_ref, content)
#   - 'ugc_only': list of (filename, title, ugc_ref, content)
# ============================================================

SUBJECTS = []

# ============================================================
# 1. GEOMORPHOLOGY
# ============================================================
SUBJECTS.append({
    'dir': 'geomorphology',
    'qmd': 'geomorphology.qmd',
    'title': 'Geomorphology',
    'subtitle': 'Study of landforms, their processes, and the forces that shape Earth\'s surface.',
    'has_interview': True,
    'nep_syllabus': """#### Core I Paper IX — Geomorphology
**(4 Credit, Theory: 45hrs, Practical: 30hrs)**

**Unit I — Applied Geomorphology**

- Applied Geomorphology: Nature, Scope and Significance
- Principles of Geomorphology
- Modern Techniques in Geomorphology: Profile, Hypsometry, Altimetry and Clinographic
- Drainage Basin: Network Characteristics, Morphology, Phases of Development
- Major Landforms: Fluvial, Aeolian, Glacial, Karst and Coastal

**Unit II — Environmental Geomorphology**

- Environmental Geomorphology: Meaning and Application
- Natural Hazards and Environmental Management
- Geomorphic Hazards: Volcanic, Earthquakes, Landslide and Floods
- Anthropogenic Activities and Effects on Erosion and Sedimentation
- Urban Geomorphology and Application in Urban Planning
- Concept of Economic Geomorphology

**Unit III — Case Studies in Applied Geomorphology**

- Geomorphic Application in Soil Studies
- Geomorphology and Disaster Management
- Geomorphology in Engineering Construction (Dams, Roads, Tunnels)
- Coastal Geomorphology and Management
- Land Degradation and Restoration
- Sustainable Geomorphological Practices""",
    'ugc_syllabus': """#### Unit I — Geomorphology

- Fundamental concepts of geomorphology
- Factors controlling landform development
- Endogenetic and exogenetic forces
- Denudation processes: weathering and erosion
- Geosynclines
- Mountain building
- Continental drift and plate tectonics
- Concept of the Geomorphic Cycle
- Landforms associated with: Fluvial, Glacial, Arid, Coastal, Karst cycles
- Slope forms and processes
- Environmental and Applied Geomorphology""",
    'common': [
        ('01-fundamental-concepts-of-geomorphology.md', 'Fundamental Concepts of Geomorphology',
         'Unit I — Principles of Geomorphology; Nature, Scope and Significance',
         'Fundamental concepts of geomorphology',
         """* **Definition**: Study of Earth's surface forms (landforms) and the processes that shape them.
* **Nature and Scope**: Integrates geology, physics, chemistry, and biology to understand landscape evolution.
* **Approaches**: Descriptive (morphographic), Genetic (process-based), Quantitative (morphometric).
* **Principles**: Uniformitarianism (Hutton), Catastrophism (Cuvier), Dynamic Equilibrium (Hack).
* **Geomorphic Agents**: Rivers, glaciers, wind, waves, groundwater — their erosional and depositional work.
* **Significance**: Applications in engineering, urban planning, hazard assessment, resource exploration."""),

        ('02-factors-controlling-landform-development.md', 'Factors Controlling Landform Development',
         'Unit I — Principles affecting landform development',
         'Factors controlling landform development',
         """* **Structure**: Rock type, geological structure (folds, faults, joints) determine landform resistance and form.
* **Process**: Weathering, erosion, mass wasting, deposition — the agents of landscape change.
* **Stage/Time**: Davis's cycle of erosion — youth, maturity, old age stages of landscape evolution.
* **Climate**: Temperature and precipitation regimes control dominant geomorphic processes (humid, arid, glacial, periglacial).
* **Tectonic Activity**: Uplift, subsidence, faulting — endogenetic forces driving landscape rejuvenation.
* **Sea Level Change**: Eustatic and isostatic changes affecting coastal and fluvial landforms."""),

        ('03-endogenetic-and-exogenetic-forces.md', 'Endogenetic and Exogenetic Forces',
         'Unit II — Environmental Geomorphology: Geomorphic Hazards',
         'Endogenetic and exogenetic forces',
         """* **Endogenetic Forces**: Internal forces originating within the Earth.
  - *Diastrophism*: Epeirogenic (vertical) and Orogenic (horizontal) movements
  - *Volcanism*: Intrusive and extrusive volcanic activity
  - *Earthquakes*: Seismic activity along plate boundaries and fault lines
* **Exogenetic Forces**: External forces acting on Earth's surface.
  - *Weathering*: Physical, chemical, and biological breakdown of rocks
  - *Erosion*: Removal and transport of weathered material by running water, wind, glaciers, waves
  - *Mass Wasting*: Gravity-driven downslope movement (landslides, rockfalls, creep, mudflows)
  - *Deposition*: Accumulation of transported material"""),

        ('04-denudation-processes.md', 'Denudation Processes: Weathering and Erosion',
         'Unit I — Major Landforms formation processes',
         'Denudation processes: weathering and erosion',
         """* **Weathering**: In-situ disintegration and decomposition of rocks.
  - *Physical/Mechanical*: Frost action, thermal expansion, exfoliation, salt crystallization
  - *Chemical*: Oxidation, carbonation, hydrolysis, solution, hydration
  - *Biological*: Root wedging, burrowing organisms, lichen action
* **Erosion**: Removal and transport of weathered material.
  - *Fluvial Erosion*: Hydraulic action, abrasion, attrition, corrosion
  - *Glacial Erosion*: Plucking, abrasion, freeze-thaw
  - *Aeolian Erosion*: Deflation, abrasion by wind-borne particles
  - *Marine Erosion*: Wave action, hydraulic pressure, abrasion
* **Mass Wasting**: Rapid vs. slow movements — landslides, mudflows, soil creep, solifluction."""),

        ('05-continental-drift-and-plate-tectonics.md', 'Continental Drift and Plate Tectonics',
         'Related concepts in tectonic geomorphology',
         'Continental drift and plate tectonics',
         """* **Continental Drift Theory** (Wegener, 1912): Pangaea → present continents. Evidences: jigsaw fit, fossil distribution, geological similarities, paleoclimate.
* **Plate Tectonics**: Lithosphere divided into rigid plates moving on asthenosphere.
  - *Divergent Boundaries*: Mid-ocean ridges, rift valleys (Mid-Atlantic Ridge, East African Rift)
  - *Convergent Boundaries*: Subduction zones, mountain building (Himalayas, Andes)
  - *Transform Boundaries*: Lateral sliding (San Andreas Fault)
* **Driving Mechanisms**: Convection currents, slab pull, ridge push.
* **Evidence**: Seafloor spreading (Hess), magnetic striping, earthquake and volcanic distribution.
* **Wilson Cycle**: Opening and closing of ocean basins over geological time."""),

        ('06-geomorphic-cycle.md', 'Concept of the Geomorphic Cycle',
         'Related to landscape evolution concepts',
         'Concept of the Geomorphic Cycle',
         """* **Davis's Geographical Cycle (1899)**: Normal cycle of erosion — Structure, Process, Stage (Youth, Maturity, Old Age). Peneplain as the final stage.
* **Penck's Morphological System**: Endogenetic vs. exogenetic processes — Aufsteigende (waxing), Gleichformige (uniform), Absteigende (waning) development.
* **King's Pediplain Cycle**: Parallel retreat of slopes → pediplain formation (scarp retreat model). Relevant to arid and semi-arid landscapes.
* **Hack's Dynamic Equilibrium**: Landscape in continuous adjustment — no final stage; balance between erosion and uplift.
* **Rejuvenation**: Renewed erosion due to uplift or sea-level fall — knickpoints, incised meanders, river terraces."""),

        ('07-landforms-fluvial.md', 'Fluvial Landforms',
         'Unit I — Major Landforms: Fluvial',
         'Landforms associated with Fluvial cycles',
         """* **Erosional Landforms**: V-shaped valleys, gorges, canyons, waterfalls, rapids, potholes, river terraces, incised/entrenched meanders.
* **Depositional Landforms**: Alluvial fans, flood plains, natural levees, deltas (arcuate, bird's foot, cuspate), meanders, oxbow lakes.
* **Drainage Patterns**: Dendritic, trellis, rectangular, radial, annular, parallel, centripetal.
* **River Morphometry**: Stream ordering (Strahler, Horton), bifurcation ratio, drainage density, stream frequency.
* **Channel Morphology**: Braided, meandering, straight — factors controlling channel patterns.
* **Base Level**: Ultimate (sea level) and temporary/local base levels."""),

        ('08-landforms-glacial.md', 'Glacial and Periglacial Landforms',
         'Unit I — Major Landforms: Glacial',
         'Landforms associated with Glacial cycles',
         """* **Erosional Landforms**: Cirques (corries), arêtes, horns, U-shaped valleys (troughs), hanging valleys, fjords, roches moutonnées, striations.
* **Depositional Landforms**: Moraines (lateral, medial, terminal, ground), drumlins, eskers, outwash plains, kames, erratics.
* **Types of Glaciers**: Continental/ice sheets, valley/alpine glaciers, piedmont glaciers, ice caps.
* **Periglacial Landforms**: Permafrost features — thermokarst, patterned ground, pingos, ice wedges, solifluction lobes.
* **Glacial Cycles**: Pleistocene glaciations — Gunz, Mindel, Riss, Würm (Alpine); evidence of glacial-interglacial cycles."""),

        ('09-slope-forms-and-processes.md', 'Slope Forms and Processes',
         'Unit III — Land Degradation; slope analysis',
         'Slope forms and processes',
         """* **Slope Elements**: Waxing slope (convexity), free face (cliff), constant slope (debris/talus), waning slope (concavity) — King's four-element slope model.
* **Slope Processes**: Weathering, mass wasting (creep, flows, slides, falls), surface wash, rill and gully erosion.
* **Slope Evolution Theories**:
  - *Davis*: Slope decline model — gradual flattening over time
  - *Penck*: Slope replacement model — parallel retreat with slope segments replacing each other
  - *King*: Parallel retreat model — backwearing of slopes maintaining angle
* **Factors Controlling Slopes**: Lithology, structure, climate, vegetation, human activity.
* **Slope Stability Analysis**: Factor of safety, shear strength vs. shear stress. Engineering applications."""),

        ('10-coastal-landforms.md', 'Coastal Geomorphology',
         'Unit III — Coastal Geomorphology and Management',
         'Landforms associated with Coastal cycles',
         """* **Coastal Processes**: Waves (constructive/destructive), tides, longshore drift, marine erosion and deposition.
* **Erosional Landforms**: Cliffs, wave-cut platforms, sea caves, arches, stacks, stumps, headlands and bays.
* **Depositional Landforms**: Beaches, spits, bars, tombolos, barrier islands, lagoons, sand dunes.
* **Coral Reefs**: Fringing, barrier, and atoll reefs — Darwin's subsidence theory.
* **Shoreline Classification**: Emergent (uplifted) vs. submergent (drowned) coastlines; Johnson's classification.
* **Coastal Management**: Beach nourishment, sea walls, groynes, managed retreat. Coastal zone regulations."""),

        ('11-karst-landforms.md', 'Karst Landforms',
         'Unit I — Major Landforms: Karst',
         'Landforms associated with Karst cycles',
         """* **Karst Processes**: Solution (carbonation) of limestone and dolomite by acidic water. Chemical weathering dominance.
* **Surface Karst Features**: Sinkholes (dolines), uvalas, poljes, karren (lapies), disappearing streams, dry valleys.
* **Subsurface Karst Features**: Caves, caverns, stalactites, stalagmites, columns, underground rivers.
* **Karst Cycle**: Youth (sinkholes, caves forming) → Maturity (caves connecting, large caverns) → Old Age (collapse, karst plain).
* **Factors Controlling Karst**: Rock solubility, joint/bedding patterns, climate (precipitation, temperature), vegetation cover.
* **Tower Karst and Cockpit Karst**: Tropical karst landforms — mogotes, cockpits (Jamaica, South China)."""),

        ('12-aeolian-landforms.md', 'Aeolian Landforms',
         'Unit I — Major Landforms: Aeolian',
         'Landforms associated with Arid cycles',
         """* **Aeolian Processes**: Deflation, abrasion, attrition by wind. Saltation, suspension, surface creep of particles.
* **Erosional Landforms**: Yardangs, ventifacts, desert pavements, deflation hollows, pedestal rocks, mushroom rocks (zeugen).
* **Depositional Landforms**: Sand dunes — barchan, seif (longitudinal), transverse, star dunes, parabolic dunes.
* **Desert Landforms**: Playas, mesas, buttes, bajadas, pediments, inselbergs, wadis.
* **Loess Deposits**: Wind-deposited fine silt — extensive in China, central Europe, and North America.
* **Factors**: Wind velocity, sand supply, vegetation cover, moisture content."""),
    ],
    'nep_only': [
        ('13-applied-geomorphology.md', 'Applied Geomorphology',
         'Unit I — Applied Geomorphology; Unit III — Case Studies',
         """* **Nature and Scope**: Application of geomorphic knowledge to solve practical problems in engineering, planning, and resource management.
* **Geomorphic Application in Soil Studies**: Relationship between landforms and soil types; soil mapping using geomorphic analysis.
* **Engineering Geomorphology**: Site selection for dams, roads, tunnels, bridges — terrain analysis for construction.
* **Urban Geomorphology**: Understanding pre-existing terrain for urban planning; slope stability, flood risk, foundation conditions.
* **Economic Geomorphology**: Mineral exploration, groundwater prospecting, sand and gravel extraction.
* **Coastal Management**: Shoreline protection, beach nourishment, harbor siting."""),

        ('14-environmental-geomorphology.md', 'Environmental Geomorphology',
         'Unit II — Environmental Geomorphology',
         """* **Definition**: Study of human impact on geomorphic processes and landforms, and natural hazards affecting human activities.
* **Anthropogenic Impacts**: Effects of construction, mining, deforestation, agriculture on erosion, sedimentation, and landscape change.
* **Land Degradation and Restoration**: Causes (overgrazing, deforestation, urbanization), assessment, and remediation strategies.
* **Sustainable Geomorphological Practices**: Balancing development with landscape conservation.
* **Geomorphic Hazards**: Volcanic eruptions, earthquakes, landslides, floods — hazard mapping and risk assessment.
* **Natural Hazards and Environmental Management**: Disaster preparedness, mitigation strategies, early warning systems."""),

        ('15-drainage-basin-morphometry.md', 'Drainage Basin Morphometry',
         'Unit I — Drainage Basin: Network Characteristics, Morphology; Unit IV — Practical',
         """* **Drainage Basin**: Fundamental geomorphic unit — area drained by a river and its tributaries.
* **Stream Ordering**: Horton's and Strahler's methods of stream classification.
* **Morphometric Parameters**:
  - *Linear Aspects*: Stream order, stream number, stream length, mean stream length ratio, bifurcation ratio
  - *Areal Aspects*: Drainage density, stream frequency, drainage texture, form factor, circularity ratio, elongation ratio
  - *Relief Aspects*: Basin relief, relief ratio, ruggedness number
* **Hypsometric Analysis**: Hypsometric curve and integral — indicates stage of basin erosion.
* **Clinographic Curve**: Relationship between slope angle and area.
* **Altimetric Frequency Graph**: Frequency distribution of elevations within a basin.
* **Applications**: Flood prediction, watershed management, groundwater exploration, soil conservation."""),

        ('16-modern-techniques-in-geomorphology.md', 'Modern Techniques in Geomorphology',
         'Unit I — Modern Techniques; Unit IV — Practical (RS, GIS, DEM)',
         """* **Remote Sensing**: Satellite imagery and aerial photographs for landform identification and mapping.
* **GIS (Geographic Information Systems)**: Spatial analysis, terrain modeling, overlay analysis for geomorphic studies.
* **Digital Elevation Models (DEM)**: Generation, analysis, and visualization of terrain — SRTM, ASTER, LiDAR.
* **GPS and GNSS**: Precise positioning for field surveys, monitoring ground displacement.
* **Profile Analysis**: Longitudinal and transverse profiles of rivers and slopes.
* **Hypsometry and Altimetry**: Quantitative terrain analysis using elevation data.
* **Geomorphic Mapping**: Systematic mapping of landforms, processes, and deposits using standardized legends."""),
    ],
    'ugc_only': [
        ('17-geosynclines-and-mountain-building.md', 'Geosynclines and Mountain Building',
         'Geosynclines; Mountain building',
         """* **Geosyncline Concept**: Large, elongated trough of subsidence filled with thick sedimentary deposits. Types: Miogeosyncline, Eugeosyncline.
* **Geosynclinal Theory**: Sediment accumulation → compression → folding → mountain building (Dana, Hall, Haug).
* **Orogenesis (Mountain Building)**: Compressive forces causing crustal shortening, folding, and thrusting.
  - *Fold Mountains*: Himalayas, Alps, Andes, Rockies — formed at convergent plate boundaries
  - *Block Mountains*: Horsts and grabens — formed by faulting (Vosges, Black Forest)
  - *Volcanic Mountains*: Built by volcanic material accumulation (Mt. Fuji, Mt. Kilimanjaro)
* **Modern Interpretation**: Plate tectonics replaces classical geosyncline theory — subduction, collision, and accretion."""),
    ],
})

# ============================================================
# 2. CLIMATOLOGY
# ============================================================
SUBJECTS.append({
    'dir': 'climatology',
    'qmd': 'climatology.qmd',
    'title': 'Climatology',
    'subtitle': 'The science of climate systems, atmospheric processes, and global weather patterns.',
    'has_interview': False,
    'nep_syllabus': """#### Core I Paper XIII — Climatology and Oceanography (Climatology Section)
**(4 Credit, Theory: 45hrs, Practical: 30hrs)**

*Note: The NEP-2020 syllabus combines Climatology and Oceanography as a single paper. The climatology-specific topics are listed here.*

- Composition and structure of the atmosphere
- Insolation and heat budget of the Earth
- Distribution of temperature
- Atmospheric pressure and general circulation of winds
- Monsoons and jet streams
- Stability and instability of the atmosphere
- Air masses and fronts
- Temperate and tropical cyclones
- Types and distribution of precipitation
- Climate classification (Köppen, Thornthwaite)
- Hydrological cycle
- Global warming""",
    'ugc_syllabus': """#### Unit II — Climatology

- Composition and structure of the atmosphere
- Insolation and heat budget of the Earth
- Distribution of temperature
- Atmospheric pressure and general circulation of winds
- Monsoons and jet streams
- Stability and instability of the atmosphere
- Air masses and fronts
- Temperate and tropical cyclones
- Types and distribution of precipitation
- Classification of world climates: Köppen's scheme, Thornthwaite's scheme
- Hydrological cycle
- Global warming

#### Climatology (Paper II / Advanced Topics)

- ENSO Events (El Niño, La Niña and Southern Oscillations)
- Meteorological Hazards and Disasters (Cyclones, Thunderstorms, Tornadoes, Hailstorms, Heat and Cold Waves, Drought and Cloudburst, GLOF)
- Climate Change: Evidences and Causes of Climatic Change in the Past, Human Impact on Global Climate""",
    'common': [
        ('01-atmosphere-composition-structure.md', 'Composition and Structure of the Atmosphere',
         'Climatology section — Composition and structure',
         'Unit II — Composition and structure of the atmosphere',
         """* **Composition**: Nitrogen (78%), Oxygen (21%), Argon (0.93%), CO₂ (0.04%), trace gases (ozone, methane, water vapour).
* **Variable Components**: Water vapour, dust particles, aerosols, pollutants — vary by location and time.
* **Layers of Atmosphere**:
  - *Troposphere*: 0–12 km, weather phenomena, temperature decreases with altitude (lapse rate ~6.5°C/km)
  - *Stratosphere*: 12–50 km, ozone layer, temperature increases with altitude
  - *Mesosphere*: 50–80 km, coldest layer, meteors burn up
  - *Thermosphere/Ionosphere*: 80–700 km, very high temperatures, aurora
  - *Exosphere*: >700 km, transition to outer space"""),

        ('02-insolation-and-heat-budget.md', 'Insolation and Heat Budget of the Earth',
         'Climatology section — Insolation and heat budget',
         'Unit II — Insolation and heat budget of the earth',
         """* **Insolation**: Incoming solar radiation — short-wave energy from the Sun.
* **Factors Affecting Insolation**: Angle of incidence, duration of daylight, distance from Sun, atmospheric transparency.
* **Albedo**: Reflectivity of surfaces — snow (80-90%), water (6-10%), forest (10-20%).
* **Heat Budget**: Balance between incoming solar radiation and outgoing terrestrial radiation.
  - Earth receives and re-radiates equal amounts of energy (equilibrium)
  - Absorption by atmosphere (greenhouse gases), land, and oceans
  - Radiation, conduction, and convection as heat transfer mechanisms
* **Latitudinal Heat Balance**: Surplus energy in tropics, deficit at poles → drives atmospheric and oceanic circulation."""),

        ('03-temperature-distribution.md', 'Distribution of Temperature',
         'Climatology section — Distribution of temperature',
         'Unit II — Distribution of temperature',
         """* **Factors Controlling Temperature**: Latitude, altitude, distance from sea (continentality), ocean currents, prevailing winds, cloud cover, aspect.
* **Isotherms**: Lines connecting places of equal temperature; shift poleward in summer, equatorward in winter.
* **Temperature Inversion**: Reversal of normal lapse rate — radiation, advection, and subsidence inversions.
* **Global Temperature Patterns**: Thermal equator, annual and diurnal temperature ranges.
* **Temperature Anomaly**: Difference between actual temperature and expected temperature for that latitude.
* **Effect of Land and Water**: Continental (large range) vs. Maritime (small range) climates."""),

        ('04-atmospheric-pressure-and-circulation.md', 'Atmospheric Pressure and General Circulation',
         'Climatology section — Atmospheric pressure and general circulation of winds',
         'Unit II — Atmospheric pressure and general circulation of winds',
         """* **Atmospheric Pressure**: Force exerted by air column per unit area. Decreases with altitude.
* **Pressure Belts**: Equatorial low, subtropical highs, subpolar lows, polar highs — shift with seasons.
* **General Circulation of the Atmosphere**: Three-cell model — Hadley cell, Ferrel cell, Polar cell.
* **Wind Systems**: Trade winds, westerlies, polar easterlies — deflected by Coriolis force.
* **Planetary and Local Winds**: Sea/land breezes, mountain/valley winds, chinook, foehn, mistral, sirocco.
* **Jet Streams**: High-altitude, fast-flowing air currents — subtropical and polar jet streams. Role in Indian monsoon.
* **Upper Atmospheric Circulation**: Rossby waves, polar vortex, tropopause dynamics."""),

        ('05-monsoons-and-jet-streams.md', 'Monsoons and Jet Streams',
         'Climatology section — Monsoons and jet streams',
         'Unit II — Monsoons and jet streams',
         """* **Monsoon Concept**: Seasonal reversal of wind direction — wet summer, dry winter.
* **Indian Monsoon Mechanism**: Differential heating, ITCZ shift, Tibetan Plateau heating, Somali jet, Tropical Easterly Jet.
* **Southwest Monsoon**: Arabian Sea and Bay of Bengal branches — onset, advance, and withdrawal.
* **Northeast Monsoon**: Winter monsoon — rainfall in Tamil Nadu and southeastern coast.
* **Jet Streams**: Subtropical jet stream (STJ) and Tropical Easterly Jet (TEJ) — role in monsoon onset and withdrawal.
* **Monsoon Variability**: El Niño, La Niña, Indian Ocean Dipole (IOD) influences on monsoon rainfall."""),

        ('06-air-masses-and-fronts.md', 'Air Masses and Fronts',
         'Climatology section — Air masses and fronts',
         'Unit II — Air-masses and fronts',
         """* **Air Masses**: Large body of air with uniform temperature and humidity. Source regions: polar, tropical, continental, maritime.
* **Classification**: cP (continental polar), cT (continental tropical), mP (maritime polar), mT (maritime tropical), cA (continental Arctic).
* **Fronts**: Boundary between two contrasting air masses.
  - *Cold Front*: Cold air advances under warm air — steep slope, heavy rainfall, thunderstorms
  - *Warm Front*: Warm air advances over cold air — gentle slope, prolonged light precipitation
  - *Occluded Front*: Cold front overtakes warm front — complex weather
  - *Stationary Front*: No significant movement — prolonged cloudy/rainy weather
* **Frontogenesis and Frontolysis**: Formation and dissipation of fronts."""),

        ('07-cyclones.md', 'Temperate and Tropical Cyclones',
         'Climatology section — Temperate and tropical cyclones',
         'Unit II — Temperate and tropical cyclones',
         """* **Tropical Cyclones**: Intense low-pressure systems formed over warm tropical oceans (>26°C).
  - Structure: Eye, eye wall, rain bands. Coriolis force required (>5° latitude)
  - Classification: Tropical depression, tropical storm, hurricane/typhoon/cyclone (based on wind speed)
  - Stages: Formation, intensification, maturity, decay
  - Indian Ocean cyclones: Bay of Bengal and Arabian Sea tracks
* **Temperate (Extra-tropical) Cyclones**: Form along polar front in mid-latitudes.
  - Polar Front Theory (Bjerknes): Cyclogenesis along polar front
  - Structure: Warm and cold sectors, frontal systems
  - Associated weather: Sequence of weather changes as cyclone passes
* **Anticyclones**: High pressure systems — clear skies, stable weather, subsidence."""),

        ('08-precipitation.md', 'Types and Distribution of Precipitation',
         'Climatology section — Types and distribution of precipitation',
         'Unit II — Types and distribution of precipitation',
         """* **Forms of Precipitation**: Rain, snow, sleet, hail, drizzle, freezing rain.
* **Types by Mechanism**:
  - *Convectional*: Intense heating → rising air → cumulonimbus → heavy rain (equatorial regions)
  - *Orographic*: Air forced over mountains → cooling → precipitation on windward side, rain shadow on leeward
  - *Frontal/Cyclonic*: Warm air rises over cold air along fronts → widespread precipitation
  - *Convergence*: Air convergence in ITCZ and low-pressure areas
* **World Distribution**: Equatorial belt (highest), deserts (lowest), mid-latitude west coasts, monsoon regions.
* **Rain Gauge and Measurement**: Standard rain gauges, radar, satellite-based estimation."""),

        ('09-climate-classification.md', 'Classification of World Climates',
         'Climatology section — Köppen, Thornthwaite',
         'Unit II — Köppen\'s scheme, Thornthwaite\'s scheme',
         """* **Köppen's Classification (1884, revised 1918)**: Based on temperature and precipitation boundaries.
  - *A*: Tropical climates (Af, Am, Aw)
  - *B*: Dry climates (BW, BS)
  - *C*: Warm temperate (Cfa, Cfb, Cs, Cw)
  - *D*: Cold/Continental (Dfa, Dfb, Dfc, Dwd)
  - *E*: Polar (ET tundra, EF ice cap)
  - *H*: Highland climate (added later)
* **Thornthwaite's Classification (1948)**: Based on potential evapotranspiration (PE) and moisture index.
  - Moisture Index = (Surplus - Deficit) / PE × 100
  - Humidity provinces: Perhumid, Humid, Moist subhumid, Dry subhumid, Semiarid, Arid
  - Thermal efficiency index based on PE
* **Trewartha's Modification**: Refinement of Köppen system for better representation."""),

        ('10-hydrological-cycle.md', 'Hydrological Cycle',
         'Climatology section — Hydrological cycle',
         'Unit II — Hydrological cycle',
         """* **Definition**: Continuous circulation of water through the Earth system — atmosphere, hydrosphere, lithosphere, biosphere.
* **Processes**: Evaporation, transpiration (evapotranspiration), condensation, precipitation, infiltration, percolation, runoff, groundwater flow.
* **Components**: Oceans (97.5%), ice caps/glaciers (1.76%), groundwater (0.76%), surface water (0.02%), atmosphere (0.001%).
* **Water Balance Equation**: P = E + R + ΔS (Precipitation = Evaporation + Runoff + Change in Storage).
* **Human Impacts**: Urbanization (increased runoff), deforestation, irrigation (altered water balance), dam construction.
* **Global Water Budget**: Oceanic evaporation exceeds oceanic precipitation; reverse on land — atmospheric transport connects the two."""),

        ('11-global-warming.md', 'Global Warming and Climate Change',
         'Climatology section — Global warming',
         'Unit II — Global warming; Climate Change: Evidences and Causes',
         """* **Enhanced Greenhouse Effect**: Increasing CO₂, CH₄, N₂O concentrations amplifying natural greenhouse warming.
* **Evidence**: Rising global temperatures, glacial retreat, sea-level rise, ocean acidification, extreme weather events.
* **IPCC Reports**: Scientific assessment of climate change causes, impacts, and mitigation options.
* **Past Climate Changes**: Milankovitch cycles, volcanic activity, solar variability, paleoclimatic records (ice cores, tree rings).
* **Human Impact**: Fossil fuel combustion, deforestation, agriculture, industrial processes.
* **Mitigation and Adaptation**: Paris Agreement, carbon trading, renewable energy, climate-resilient infrastructure."""),
    ],
    'nep_only': [
        ('12-stability-instability-atmosphere.md', 'Stability and Instability of the Atmosphere',
         'Climatology section — Stability and instability of the atmosphere',
         """* **Atmospheric Stability**: Condition where displaced air parcel returns to original position — inhibits vertical motion.
* **Atmospheric Instability**: Displaced air parcel continues to rise — promotes cloud formation and precipitation.
* **Lapse Rates**:
  - *Environmental Lapse Rate (ELR)*: Actual rate of temperature decrease in atmosphere (~6.5°C/km average)
  - *Dry Adiabatic Lapse Rate (DALR)*: Rate for unsaturated rising air (~10°C/km)
  - *Saturated Adiabatic Lapse Rate (SALR)*: Rate for saturated rising air (~5-6°C/km, varies)
* **Stability Conditions**: ELR < SALR (absolutely stable), ELR > DALR (absolutely unstable), SALR < ELR < DALR (conditionally unstable).
* **Temperature Inversion**: Stable layer inhibiting vertical mixing — traps pollutants, fog formation."""),
    ],
    'ugc_only': [
        ('13-enso-events.md', 'ENSO Events',
         'ENSO Events (El Niño, La Niña and Southern Oscillations)',
         """* **El Niño**: Anomalous warming of eastern Pacific Ocean surface waters — weakens trade winds, suppresses upwelling.
* **La Niña**: Anomalous cooling of eastern Pacific — strengthens trade winds, enhances upwelling.
* **Southern Oscillation**: See-saw pattern of atmospheric pressure between eastern and western Pacific — Walker Circulation.
* **ENSO Index**: Southern Oscillation Index (SOI) — pressure difference between Tahiti and Darwin.
* **Global Impacts**: El Niño → drought in India/Australia, floods in South America; La Niña → enhanced monsoon, floods in Australia.
* **Teleconnections**: ENSO influences weather patterns globally — Indian monsoon, African rainfall, Atlantic hurricanes."""),

        ('14-meteorological-hazards.md', 'Meteorological Hazards and Disasters',
         'Meteorological Hazards and Disasters (Cyclones, Thunderstorms, Tornadoes, Hailstorms, Heat/Cold Waves, Drought, Cloudburst, GLOF)',
         """* **Cyclones**: Tropical and extra-tropical — wind damage, storm surge, flooding. Warning systems and preparedness.
* **Thunderstorms**: Convective storms — lightning, heavy rain, hail, gusty winds. Nor'westers in eastern India.
* **Tornadoes**: Violently rotating columns of air — Fujita scale, damage patterns. Rare in India but occur.
* **Hailstorms**: Large ice pellets — damage to crops, property, livestock.
* **Heat and Cold Waves**: Extreme temperature events — health impacts, agricultural losses, urban vulnerability.
* **Drought**: Meteorological, hydrological, agricultural — causes, monitoring (SPI, PDSI), management.
* **Cloudburst**: Sudden, extremely heavy rainfall — flash floods, landslides. Common in Himalayan regions.
* **GLOF (Glacial Lake Outburst Flood)**: Catastrophic release of water from glacial lakes — Himalayan risk."""),

        ('15-applied-climatology.md', 'Applied Climatology',
         'Climate applications in various sectors',
         """* **Agricultural Climatology**: Crop-weather relationships, agroclimatic zones, growing season, frost-free period.
* **Urban Climatology**: Urban heat island, air quality, urban wind patterns, building energy requirements.
* **Aviation Climatology**: Weather impacts on flight operations — visibility, turbulence, icing, crosswinds.
* **Bioclimatology**: Climate-health relationships — thermal comfort indices, disease ecology, seasonal patterns.
* **Renewable Energy**: Solar and wind resource assessment — site selection for solar/wind farms.
* **Climate Data and Monitoring**: Weather stations, satellites, radar, automated weather stations, climate databases."""),
    ],
})

# ============================================================
# 3. OCEANOGRAPHY
# ============================================================
SUBJECTS.append({
    'dir': 'oceanography',
    'qmd': 'oceanography.qmd',
    'title': 'Oceanography',
    'subtitle': 'Physical and chemical properties of the world\'s oceans and their geographic significance.',
    'has_interview': False,
    'nep_syllabus': """#### Core I Paper XIII — Climatology and Oceanography (Oceanography Section)

- Origin of ocean basins
- Bottom relief of Indian, Atlantic, and Pacific Oceans
- Ocean deposits
- Coral reefs
- Temperature and salinity of the oceans
- Density of seawater
- Tides and ocean currents
- Sea-level changes""",
    'ugc_syllabus': """#### Unit III — Oceanography

- Origin of ocean basins
- Bottom relief of Indian, Atlantic, and Pacific Oceans
- Ocean deposits
- Coral reefs
- Temperature and salinity of the oceans
- Density of seawater
- Tides and ocean currents
- Sea-level changes

#### Oceanography (Paper II / Advanced Topics)

- Relief of Oceans
- Composition: Temperature, Density and Salinity
- Circulation: Warm and Cold Currents, Waves, Tides
- Sea Level Changes
- Hazards: Tsunami and Cyclone""",
    'common': [
        ('01-origin-of-ocean-basins.md', 'Origin of Ocean Basins',
         'Oceanography section — Origin of ocean basins',
         'Unit III — Origin of ocean basins',
         """* **Continental Drift and Seafloor Spreading**: Wegener's theory and Hess's seafloor spreading hypothesis explain ocean basin formation.
* **Plate Tectonics**: Ocean basins form at divergent boundaries (mid-ocean ridges) and are consumed at convergent boundaries (subduction zones).
* **Wilson Cycle**: Complete lifecycle of ocean basins — embryonic (rift valley), juvenile (Red Sea), mature (Atlantic), declining (Pacific), terminal (Mediterranean), suture (Himalayas).
* **Age of Ocean Floor**: Youngest at mid-ocean ridges, oldest near continental margins — confirmed by magnetic anomalies.
* **Passive vs. Active Continental Margins**: Atlantic-type (stable, wide shelves) vs. Pacific-type (subduction, narrow shelves, trenches)."""),

        ('02-ocean-floor-relief.md', 'Bottom Relief of the Oceans',
         'Oceanography section — Bottom relief of Indian, Atlantic, and Pacific Oceans',
         'Unit III / Relief of Oceans',
         """* **Continental Shelf**: Shallow, gently sloping extension of continental landmass — up to 200m depth. Rich in marine resources.
* **Continental Slope**: Steep descent from shelf edge to deep ocean floor. Submarine canyons.
* **Continental Rise**: Gentle slope at base of continental slope — sediment accumulation from turbidity currents.
* **Abyssal Plains**: Extensive flat areas of deep ocean floor — covered with fine sediments.
* **Mid-Ocean Ridges**: Underwater mountain chains at divergent boundaries — Mid-Atlantic Ridge, East Pacific Rise, Indian Ocean Ridge system.
* **Deep Sea Trenches**: Deepest parts of ocean — Mariana Trench (11,034m), Tonga Trench, Java Trench. Formed at subduction zones.
* **Seamounts and Guyots**: Submarine volcanic mountains — guyots are flat-topped (eroded by waves).
* **Specific Ocean Features**: Atlantic (S-shaped ridge), Pacific (Ring of Fire, trenches), Indian (triple junction, Ninety East Ridge)."""),

        ('03-temperature-salinity-density.md', 'Temperature, Salinity, and Density of Oceans',
         'Oceanography section — Temperature, salinity, density',
         'Unit III / Composition: Temperature, Density and Salinity',
         """* **Ocean Temperature**: Decreases with depth — three layers: mixed layer, thermocline, deep water. Average surface temperature ~17°C.
* **Factors Affecting Temperature**: Latitude, ocean currents, depth, season, proximity to landmass.
* **Salinity**: Average 35‰ (parts per thousand). Highest in subtropics (high evaporation), lowest near equator and poles.
* **Factors Affecting Salinity**: Evaporation, precipitation, river discharge, ice formation/melting, ocean currents.
* **Density of Seawater**: Function of temperature, salinity, and pressure. Cold, saline water is densest.
* **Thermohaline Circulation**: Global conveyor belt driven by density differences — meridional overturning circulation.
* **Heat and Salt Budget**: Ocean as a heat reservoir — role in climate regulation."""),

        ('04-waves-tides-currents.md', 'Waves, Tides, and Ocean Currents',
         'Oceanography section — Tides and ocean currents',
         'Unit III / Circulation: Warm and Cold Currents, Waves, Tides',
         """* **Ocean Waves**: Generated by wind — wave height, wavelength, period, fetch. Constructive vs. destructive waves.
* **Tides**: Periodic rise and fall of sea level caused by gravitational pull of Moon and Sun.
  - Spring tides (syzygy), neap tides (quadrature)
  - Diurnal, semi-diurnal, and mixed tidal patterns
  - Tidal bore and tidal range
* **Ocean Currents**: Large-scale movement of seawater driven by wind, temperature, salinity, and Coriolis force.
  - *Warm Currents*: Gulf Stream, Kuroshio, North Atlantic Drift, Brazil Current
  - *Cold Currents*: Labrador, Benguela, Peru (Humboldt), California Current
  - *Gyres*: Circular current systems in major ocean basins — North Atlantic, South Pacific, etc.
* **Upwelling and Downwelling**: Vertical water movements — upwelling brings nutrients to surface (productive fishing grounds)."""),

        ('05-sea-level-changes.md', 'Sea-Level Changes',
         'Oceanography section — Sea-level changes',
         'Unit III / Sea Level Changes',
         """* **Eustatic Changes**: Global sea-level changes due to change in ocean water volume.
  - Glacial periods → sea-level fall (water locked in ice sheets)
  - Interglacial periods → sea-level rise (ice melting)
* **Isostatic Changes**: Local changes due to crustal uplift or subsidence.
  - Post-glacial rebound (Scandinavia, Canada)
  - Tectonic subsidence
* **Current Sea-Level Rise**: ~3.6 mm/year — thermal expansion of oceans + melting of glaciers and ice sheets.
* **Impacts**: Coastal flooding, erosion, saltwater intrusion, loss of wetlands, island submergence.
* **Emergent and Submergent Coastlines**: Raised beaches, marine terraces (emergent); rias, fjords, drowned valleys (submergent).
* **Coral Reefs as Indicators**: Reef growth tracks sea-level changes — Darwin's subsidence theory."""),

        ('06-coral-reefs.md', 'Coral Reefs and Ocean Deposits',
         'Oceanography section — Coral reefs, Ocean deposits',
         'Unit III — Coral reefs, Ocean deposits',
         """* **Coral Reefs**: Biogenic formations by coral polyps — require warm (>20°C), shallow, clear, saline water.
  - *Fringing Reefs*: Grow directly from shore — most common type
  - *Barrier Reefs*: Separated from shore by lagoon — Great Barrier Reef
  - *Atolls*: Ring-shaped reefs enclosing lagoon — formed by subsidence of volcanic island (Darwin's theory)
* **Coral Bleaching**: Expulsion of symbiotic algae due to stress (warming, pollution) — widespread global threat.
* **Ocean Deposits**: Materials deposited on the ocean floor.
  - *Terrigenous*: Land-derived sediments (gravel, sand, silt, clay) — near coasts
  - *Pelagic*: Deep-sea deposits — biogenic oozes (foraminiferal/calcareous, radiolarian/siliceous), red clay
  - *Authigenic*: Formed in-situ — manganese nodules, phosphorite deposits"""),
    ],
    'nep_only': [],
    'ugc_only': [
        ('07-oceanic-hazards.md', 'Oceanic Hazards: Tsunami and Cyclone',
         'Hazards: Tsunami and Cyclone',
         """* **Tsunami**: Series of ocean waves caused by underwater earthquakes, volcanic eruptions, or landslides.
  - Characteristics: Long wavelength, short amplitude in deep water; amplified near coast
  - 2004 Indian Ocean Tsunami: Magnitude 9.1 earthquake — devastated coastal regions of 14 countries
  - Warning Systems: Pacific Tsunami Warning Centre, Indian Ocean Tsunami Warning System
* **Cyclones (Marine Perspective)**: Storm surge, coastal flooding, wave damage.
  - Storm Surge: Abnormal rise in sea level during cyclone — most destructive component
  - Coastal Vulnerability: Low-lying areas, deltaic regions, island nations
* **Rogue Waves**: Unusually large, unexpected waves — hazard to shipping.
* **Marine Pollution**: Oil spills, plastic pollution, chemical contamination — impact on marine ecosystems.
* **Law of the Sea (UNCLOS)**: Maritime zones — territorial sea, contiguous zone, EEZ, continental shelf, high seas."""),
    ],
})

# ============================================================
# 4. GEOGRAPHICAL THOUGHT
# ============================================================
SUBJECTS.append({
    'dir': 'geographical-thought',
    'qmd': 'geographical-thought.qmd',
    'title': 'Geographical Thought',
    'subtitle': 'Evolution of geographic ideas from ancient civilizations to contemporary paradigms.',
    'has_interview': False,
    'nep_syllabus': """#### Core I Paper X — Evolution of Geographical Thought
**(4 Credit, Theory: 45hrs, Practical: 30hrs)**

**Unit I** — Geographical concepts of ancient and medieval period: Contributions of Greek, Roman, Indian and Arab scholars

**Unit II** — Modern period: Contributions of Alexander Von Humboldt, Carl Ritter, Ratzel, Vidal De La Blache and Mackinder

**Unit III** — Dichotomy and dualism in Geography: Environmental Determinism and Possibilism, Systematic and Regional Geography, Ideographic and Nomothetic, Physical and Human Geography; Recent Trends: Quantitative Revolution, Behaviouralism and Radicalism""",
    'ugc_syllabus': """#### Unit IV — History of Geographic Thought

- Geographic knowledge during the ancient and medieval period
- Foundations of Modern Geography: Contributions of German, French, British, American schools
- Conceptual and methodological developments during the 20th century
- Changing paradigms in geography
- Man and environment: determinism and possibilism
- Area differentiation and spatial organization
- Quantitative revolution
- Impact of: Positivism, Humanism, Radicalism, Behavioralism

#### Geographic Thought (Paper II / Advanced Topics)

- Contributions of Greek, Roman, Arab, Chinese and Indian Scholars
- Contributions of Geographers (Varenius, Kant, Humboldt, Ritter, Schaefer & Hartshorne)
- Impact of Darwinian Theory on Geographical Thought
- Contemporary trends in Indian Geography
- Major Geographic Traditions (Earth Science, man-environment, area studies, spatial analysis)
- Dualisms in Geographic Studies
- Paradigm Shift, Perspectives (Positivism, Behaviouralism, Humanism, Structuralism, Feminism, Postmodernism)""",
    'common': [
        ('01-ancient-medieval-geography.md', 'Ancient and Medieval Geographic Thought',
         'Unit I — Greek, Roman, Indian and Arab scholars',
         'Ancient and medieval period; Greek, Roman, Arab, Chinese and Indian Scholars',
         """* **Greek Contributions**: Eratosthenes (circumference of Earth, "Father of Geography"), Ptolemy (Geographia, map projections), Herodotus (regional descriptions), Aristotle (spherical Earth), Strabo (Geographica — 17 volumes).
* **Roman Contributions**: Pliny's Natural History, practical geography for administration and military.
* **Indian Contributions**: Ancient astronomical and geographical knowledge — Aryabhata, Varahamihira. Concept of Saptadvipa (seven continents).
* **Arab Contributions**: Al-Idrisi (Tabula Rogeriana), Ibn Battuta (extensive travel accounts), Al-Biruni (geodesy, Indian geography), Al-Masudi (geographical encyclopaedia).
* **Chinese Contributions**: Zhang Heng (seismoscope), Zheng He (maritime expeditions), cartographic traditions.
* **Medieval Period**: Geographic knowledge preserved and advanced in the Islamic world while Europe entered the Dark Ages."""),

        ('02-modern-geography-foundations.md', 'Foundations of Modern Geography',
         'Unit II — Humboldt, Ritter, Ratzel, Vidal de la Blache, Mackinder',
         'Foundations of Modern Geography; Varenius, Kant, Humboldt, Ritter, Schaefer, Hartshorne',
         """* **Bernhardus Varenius (1622-1650)**: Distinguished between General Geography and Special (Regional) Geography. Geographia Generalis (1650).
* **Immanuel Kant (1724-1804)**: Geography as a spatial science — classification of knowledge. Physische Geographie.
* **Alexander von Humboldt (1769-1859)**: "Father of Modern Geography." Cosmos (5 vols). Systematic, scientific study of nature. Isotherms, plant geography, altitudinal zonation.
* **Carl Ritter (1779-1859)**: Comparative geography, teleological approach — Die Erdkunde (19 vols). Regional/systematic synthesis.
* **Friedrich Ratzel (1844-1904)**: Anthropogeographic, political geography, Lebensraum concept, environmental determinism.
* **Vidal de la Blache (1845-1918)**: "Father of French Geography." Possibilism, genres de vie (ways of life), pays (locality).
* **Halford Mackinder (1861-1947)**: Heartland Theory (1904), "The Geographical Pivot of History." Geography as the bridge between natural and social sciences."""),

        ('03-dualisms-and-dichotomies.md', 'Dualisms and Dichotomies in Geography',
         'Unit III — Dichotomy and dualism in Geography',
         'Dualisms in Geographic Studies',
         """* **Physical vs. Human Geography**: Study of natural environment vs. human activities — quest for integration.
* **Systematic vs. Regional Geography**: Topical study of phenomena (Humboldt) vs. regional synthesis (Ritter) — Hartshorne vs. Schaefer debate.
* **Determinism vs. Possibilism**: Environment controls human activity (Ratzel, Semple) vs. humans choose from possibilities (Vidal, Febvre).
* **Ideographic vs. Nomothetic**: Study of unique cases/regions vs. searching for general laws/patterns — Kantian distinction.
* **Qualitative vs. Quantitative**: Descriptive approaches vs. statistical/mathematical methods — quantitative revolution.
* **General vs. Special Geography**: Varenius's distinction between universal principles and regional descriptions.
* **Resolution**: Modern geography seeks integration — human-environment systems, mixed methods."""),

        ('04-paradigm-shifts-perspectives.md', 'Paradigm Shifts and Perspectives in Geography',
         'Unit III — Quantitative Revolution, Behaviouralism, Radicalism',
         'Paradigm Shift; Positivism, Behaviouralism, Humanism, Structuralism, Feminism, Postmodernism',
         """* **Quantitative Revolution (1950s-60s)**: Shift from descriptive to statistical/mathematical methods. Schaefer, Bunge, Berry, Haggett. Spatial science, models, hypothesis testing.
* **Positivism**: Logical positivism — objective, empirical, value-free science. Scientific method in geography.
* **Behaviouralism (1960s-70s)**: Focus on perception, cognition, and decision-making in spatial behaviour. Mental maps, cognitive environments.
* **Humanism (1970s)**: Reaction against positivism — focus on lived experience, place, meaning. Yi-Fu Tuan, Edward Relph. Phenomenology, existentialism.
* **Radicalism/Marxism (1970s)**: Social justice, inequality, class structure, political economy. David Harvey, Richard Peet. Spatial analysis of capitalism.
* **Structuralism**: Underlying structures shaping spatial patterns — Lévi-Strauss, Althusser influence.
* **Feminism**: Gender relations in space, gendered experiences, feminist epistemology.
* **Postmodernism**: Rejection of grand theories, multiple truths, deconstruction, fragmentation. Soja, Dear."""),
    ],
    'nep_only': [],
    'ugc_only': [
        ('05-geographic-schools.md', 'National Schools of Geography',
         'Contributions of German, French, British, American schools',
         """* **German School**: Humboldt, Ritter, Ratzel, Richthofen, Hettner, Christaller. Systematic and theoretical approach, environmental determinism, Länderkunde.
* **French School**: Vidal de la Blache, Jean Brunhes, De Martonne. Possibilism, pays, genres de vie, regional monographs.
* **British School**: Mackinder, Herbertson, Stamp, Wooldridge. Applied geography, land use surveys, regional geography.
* **American School**: Davis, Semple, Huntington, Sauer, Hartshorne, Berry. Geomorphic cycle, cultural landscape, areal differentiation, spatial analysis.
* **Contemporary Indian Geography**: Contributions in cartography, thematic and methodological developments. Major geographers and institutions."""),

        ('06-geographic-traditions.md', 'Major Geographic Traditions',
         'Major Geographic Traditions (Earth Science, man-environment, area studies, spatial analysis)',
         """* **Earth Science Tradition**: Physical geography — geomorphology, climatology, biogeography. Study of Earth systems and processes.
* **Man-Environment Tradition**: Ecological tradition — human-environment interaction, environmental perception, resource management.
* **Area Studies (Regional) Tradition**: Regional geography — areal differentiation, regional synthesis, chorological approach (Hettner, Hartshorne).
* **Spatial Analysis Tradition**: Spatial patterns, distributions, and interactions — quantitative methods, location theory, spatial modelling.
* **Pattison's Four Traditions (1964)**: Framework for understanding geography's multifaceted nature.
* **Integration**: Modern geography increasingly integrates all traditions through geospatial technology, systems thinking, and interdisciplinary approaches."""),

        ('07-darwinian-impact.md', 'Impact of Darwinian Theory on Geographical Thought',
         'Impact of Darwinian Theory on Geographical Thought',
         """* **Evolutionary Theory and Geography**: Darwin's Origin of Species (1859) profoundly influenced geographic thinking.
* **Environmental Determinism**: Ratzel's application of Darwinian ideas — natural selection in human-environment relations. Semple's "Influences of Geographic Environment."
* **Social Darwinism**: Misapplication of evolutionary theory to justify racial and cultural hierarchies in geography.
* **Biogeography**: Darwin's observations (Galápagos finches) → island biogeography, species distribution patterns, zoogeographic regions.
* **Landscape Evolution**: Davis's geomorphic cycle influenced by evolutionary stage concept — youth, maturity, old age.
* **Reaction and Critique**: Possibilism (Febvre) and probabilism as reactions against rigid deterministic interpretations of Darwinism."""),
    ],
})

# ============================================================
# 5. POPULATION GEOGRAPHY
# ============================================================
SUBJECTS.append({
    'dir': 'population-geography',
    'qmd': 'population-geography.qmd',
    'title': 'Population Geography',
    'subtitle': 'Spatial distribution, density, growth, composition, and migration of human populations.',
    'has_interview': False,
    'nep_syllabus': """#### Core I Paper III — Population Geography
**(4 Credit, Theory: 45hrs, Practical: 30hrs)**

*The detailed unit-wise NEP syllabus for Population Geography covers:*

- Nature, scope, and recent trends in population geography
- Patterns of world distribution, growth, and density of population
- Population policy issues
- Patterns and processes of migration
- Demographic transition
- Population-resource regions""",
    'ugc_syllabus': """#### Unit V — Population Geography

- Nature, scope, and recent trends
- Patterns of world distribution, growth, and density of population
- Population policy issues
- Patterns and processes of migration
- Demographic transition
- Population-resource regions

#### Population Geography (Paper II / Advanced Topics)

- Sources of population data (census, sample surveys, vital statistics, data reliability and errors)
- World Population Distribution (measures, patterns and determinants), World Population Growth
- Demographic Transition, Theories of Population Growth (Malthus, Sadler, Ricardo)
- Fertility and Mortality Analysis (indices, determinants and world patterns)
- Migration (types, causes, consequences and models)
- Population Composition and Characteristics (age, sex, rural-urban, occupational structure, educational levels)
- Population Policies in Developed and Developing Countries""",
    'common': [
        ('01-population-distribution-density.md', 'World Population Distribution, Growth, and Density',
         'Patterns of world distribution, growth, and density',
         'World Population Distribution (measures, patterns and determinants), Growth',
         """* **Distribution Patterns**: Ecumene (inhabited areas) vs. non-ecumene. Concentration in Asia (~60%), Europe, eastern North America.
* **Determinants of Distribution**: Climate, terrain, soil fertility, water availability, mineral resources, industrial development, historical factors.
* **Measures of Density**: Arithmetic density, physiological density, agricultural density.
* **Population Growth**: Prehistoric to modern — exponential growth since Industrial Revolution. World population milestones.
* **Growth Rate**: Crude birth rate, crude death rate, natural increase rate, doubling time.
* **Regional Variations**: High growth in Sub-Saharan Africa, South Asia; low/negative growth in Europe, Japan."""),

        ('02-demographic-transition.md', 'Demographic Transition',
         'Demographic transition',
         'Demographic Transition, Theories of Population Growth (Malthus, Sadler, Ricardo)',
         """* **Demographic Transition Model (DTM)**: Describes transition from high birth/death rates to low birth/death rates.
  - *Stage 1*: High stationary — high birth and death rates, slow growth (pre-industrial)
  - *Stage 2*: Early expanding — declining death rates, high birth rates, rapid growth
  - *Stage 3*: Late expanding — declining birth rates, continued growth at slower pace
  - *Stage 4*: Low stationary — low birth and death rates, slow growth/stable population
  - *Stage 5*: Declining — birth rate below death rate, population decline (Germany, Japan)
* **Malthusian Theory**: Population grows geometrically, food supply arithmetically — positive and preventive checks.
* **Sadler's Theory**: Reproductive power inversely proportional to population density.
* **Ricardo's Theory**: Law of diminishing returns in agriculture limits food production.
* **Neo-Malthusian and Anti-Malthusian Views**: Modern debates on population and resources."""),

        ('03-migration.md', 'Migration: Types, Causes, and Consequences',
         'Patterns and processes of migration',
         'Migration (types, causes, consequences and models)',
         """* **Types of Migration**: Internal (rural-urban, inter-state) vs. International; Voluntary vs. Forced; Temporary vs. Permanent; Seasonal.
* **Causes**: Push factors (poverty, drought, conflict, unemployment) and Pull factors (employment, education, amenities, political stability).
* **Ravenstein's Laws of Migration**: Most migrants travel short distances; counter-current; rural to urban; females migrate internally, males externally.
* **Lee's Theory**: Push-pull model with intervening obstacles and personal factors.
* **Gravity Model**: Migration proportional to population sizes, inversely proportional to distance.
* **Consequences**: Economic (remittances, brain drain/gain), social (cultural exchange, family disruption), demographic (age-sex structure), political (refugee crises).
* **Refugee and Displacement**: UNHCR data, internally displaced persons (IDPs), climate refugees."""),

        ('04-population-policy.md', 'Population Policies',
         'Population policy issues',
         'Population Policies in Developed and Developing Countries',
         """* **Pro-natalist Policies**: Encourage higher birth rates — France (family allowances), Japan (childcare subsidies), Russia.
* **Anti-natalist Policies**: Discourage high birth rates — China (One-Child Policy, now Three-Child), India (family planning programme).
* **India's Population Policy**: National Population Policy 2000 — reducing TFR to 2.1, improving maternal health, contraception access.
* **Population Control Methods**: Family planning (contraception), education (especially female), economic development, legislative measures.
* **Ethical Considerations**: Coercive vs. voluntary approaches, gender equity, reproductive rights.
* **Population and Development**: Relationship between population growth, economic development, and sustainable resource use."""),
    ],
    'nep_only': [
        ('05-population-resource-regions.md', 'Population-Resource Regions',
         'Population-resource regions',
         """* **Concept**: Relationship between population size, resources availability, and technology level determines carrying capacity.
* **Ackerman's Classification**: Technology-deficient, population-resource balance, technology-surplus regions.
* **Over-population**: Population exceeds carrying capacity — resource pressure, environmental degradation (parts of South Asia, Sub-Saharan Africa).
* **Under-population**: Resources exceed population needs — labour shortage, untapped potential (Australia, Canada, parts of Russia).
* **Optimum Population**: Ideal balance between population and resources for maximum welfare.
* **Resource Frontiers**: Newly developed regions with low population but high resource potential."""),
    ],
    'ugc_only': [
        ('06-population-data-sources.md', 'Sources of Population Data',
         'Sources: census, sample surveys, vital statistics, data reliability and errors',
         """* **Census**: Complete enumeration of population — decennial in India. De facto vs. de jure enumeration.
* **Sample Surveys**: National Sample Survey (NSS/NSSO), Demographic and Health Surveys (DHS) — representative sampling.
* **Vital Statistics Registration**: Registration of births, deaths, marriages — Civil Registration System (CRS) in India, Sample Registration System (SRS).
* **Data Reliability and Errors**: Coverage errors, content errors, sampling errors, non-sampling errors. Age-sex misreporting, digit preference.
* **International Sources**: UN Population Division, World Bank, WHO — standardized datasets for comparison.
* **Census of India**: History, methodology, enumeration process, data dissemination. Census 2011 highlights."""),

        ('07-fertility-mortality-analysis.md', 'Fertility and Mortality Analysis',
         'Fertility and Mortality Analysis (indices, determinants and world patterns)',
         """* **Fertility Measures**: Crude Birth Rate (CBR), General Fertility Rate (GFR), Age-Specific Fertility Rate (ASFR), Total Fertility Rate (TFR), Gross/Net Reproduction Rate (GRR/NRR).
* **Determinants of Fertility**: Age at marriage, education (especially female), contraceptive use, economic development, cultural and religious factors, government policies.
* **Mortality Measures**: Crude Death Rate (CDR), Age-Specific Death Rate (ASDR), Infant Mortality Rate (IMR), Maternal Mortality Rate (MMR), Life Expectancy at Birth.
* **Determinants of Mortality**: Healthcare access, nutrition, sanitation, epidemiological transition, living conditions.
* **World Patterns**: High fertility in Sub-Saharan Africa, low in Europe/East Asia. Mortality declining globally but disparities persist.
* **Epidemiological Transition (Omran)**: Shift from infectious to degenerative/chronic diseases as causes of death."""),

        ('08-population-composition.md', 'Population Composition and Characteristics',
         'Age, sex, rural-urban, occupational structure, educational levels',
         """* **Age-Sex Structure**: Population pyramids — expansive (young population), constrictive (ageing), stationary (stable).
* **Sex Ratio**: Males per females (or vice versa) — biological, social, and migration factors. India's adverse sex ratio.
* **Rural-Urban Composition**: Urbanization levels, rural-urban migration, peri-urban growth.
* **Occupational Structure**: Primary, secondary, tertiary, quaternary sectors — shift from primary to service economy.
* **Educational Levels**: Literacy rates, gross enrolment ratios, educational attainment — gender and regional disparities.
* **Dependency Ratio**: Ratio of dependent (0-14, 65+) to working-age population — demographic dividend vs. ageing challenge.
* **Ethnic, Linguistic, and Religious Composition**: Diversity indices, spatial patterns, geopolitical implications."""),
    ],
})

# ============================================================
# HELPER FUNCTIONS
# ============================================================

def clean_directory(dir_path, keep_files=None):
    """Remove all .md files in directory except those in keep_files list."""
    if keep_files is None:
        keep_files = []
    for f in glob.glob(os.path.join(dir_path, '*.md')):
        basename = os.path.basename(f)
        if basename not in keep_files:
            os.remove(f)
            print(f"  Removed: {basename}")

def write_syllabus(dir_path, nep_text, ugc_text):
    """Write the 00-syllabus.md with both NEP and UGC NET sections."""
    content = f"""## Official Syllabus

### NEP-2020 Syllabus

::: {{.callout-note}}
{nep_text}
:::

---

### UGC NET Syllabus

::: {{.callout-tip}}
{ugc_text}
:::
"""
    filepath = os.path.join(dir_path, '00-syllabus.md')
    with open(filepath, 'w', encoding='utf-8') as f:
        f.write(content)
    print(f"  Created: 00-syllabus.md")

def write_common_topic(dir_path, filename, title, nep_ref, ugc_ref, content):
    """Write a common topic MD file with both syllabus references."""
    md = f"""## {title}

::: {{.callout-warning}}
#### 📘 Syllabus Coverage
| Syllabus | Topic Details |
|----------|---------------|
| **NEP-2020** | {nep_ref} |
| **UGC NET** | {ugc_ref} |
:::


[![](https://via.placeholder.com/600x300?text=Course+Module+Video)](#)

[Get the Presentation &nearr;](#) &nbsp; | &nbsp; [Watch the Video &nearr;](#)

::: {{.callout-note}}
### Key Concepts
{content}
:::

"""
    filepath = os.path.join(dir_path, filename)
    with open(filepath, 'w', encoding='utf-8') as f:
        f.write(md)

def write_nep_topic(dir_path, filename, title, nep_ref, content):
    """Write a NEP-only topic MD file."""
    md = f"""## {title}

::: {{.callout-warning}}
#### 📘 Syllabus Coverage
| Syllabus | Topic Details |
|----------|---------------|
| **NEP-2020** | {nep_ref} |
:::


[![](https://via.placeholder.com/600x300?text=Course+Module+Video)](#)

[Get the Presentation &nearr;](#) &nbsp; | &nbsp; [Watch the Video &nearr;](#)

::: {{.callout-note}}
### Key Concepts
{content}
:::

"""
    filepath = os.path.join(dir_path, filename)
    with open(filepath, 'w', encoding='utf-8') as f:
        f.write(md)

def write_ugc_topic(dir_path, filename, title, ugc_ref, content):
    """Write a UGC NET-only topic MD file."""
    md = f"""## {title}

::: {{.callout-warning}}
#### 📘 Syllabus Coverage
| Syllabus | Topic Details |
|----------|---------------|
| **UGC NET** | {ugc_ref} |
:::


[![](https://via.placeholder.com/600x300?text=Course+Module+Video)](#)

[Get the Presentation &nearr;](#) &nbsp; | &nbsp; [Watch the Video &nearr;](#)

::: {{.callout-note}}
### Key Concepts
{content}
:::

"""
    filepath = os.path.join(dir_path, filename)
    with open(filepath, 'w', encoding='utf-8') as f:
        f.write(md)

def write_qmd_index(qmd_path, subject):
    """Write the restructured QMD index file."""
    title = subject['title']
    subtitle = subject['subtitle']
    dir_name = subject['dir']
    
    lines = [
        '---',
        f'title: "{title}"',
        f'subtitle: "{subtitle}"',
        'author: "Geography Team"',
        'toc: true',
        'toc-depth: 3',
        '---',
        '',
        f'{{{{< include {dir_name}/00-syllabus.md >}}}}',
        '',
        '---',
        '',
        f'Welcome to the **{title}** module of Geography OpenCourseWare.',
        '',
        '---',
        '',
    ]
    
    # Common topics
    if subject['common']:
        lines.append('## Part A: Common Topics (NEP-2020 & UGC NET)')
        lines.append('')
        lines.append('These topics are covered in both the NEP-2020 undergraduate syllabus and the UGC NET syllabus.')
        lines.append('')
        for filename, title_t, _, _, _ in subject['common']:
            lines.append(f'{{{{< include {dir_name}/{filename} >}}}}')
            lines.append('')
        lines.append('---')
        lines.append('')
    
    # NEP-only topics
    if subject['nep_only']:
        lines.append('## Part B: NEP-2020 Specific Topics')
        lines.append('')
        lines.append('These topics are part of the NEP-2020 undergraduate programme only.')
        lines.append('')
        for filename, title_t, _, _ in subject['nep_only']:
            lines.append(f'{{{{< include {dir_name}/{filename} >}}}}')
            lines.append('')
        lines.append('---')
        lines.append('')
    
    # UGC NET-only topics
    if subject['ugc_only']:
        lines.append('## Part C: UGC NET Specific Topics')
        lines.append('')
        lines.append('These topics are part of the UGC NET syllabus only.')
        lines.append('')
        for filename, title_t, _, _ in subject['ugc_only']:
            lines.append(f'{{{{< include {dir_name}/{filename} >}}}}')
            lines.append('')
        lines.append('---')
        lines.append('')
    
    # Interview QA section if exists
    if subject.get('has_interview'):
        lines.append('## Interview Questions & Answers')
        # Check if interview file exists
        interview_file = os.path.join(BASE, dir_name, f'{len(subject["common"]) + len(subject["nep_only"]) + len(subject["ugc_only"]) + 1:02d}-interview-qa.md')
        # We'll add this after processing
    
    content = '\n'.join(lines)
    with open(qmd_path, 'w', encoding='utf-8') as f:
        f.write(content)
    print(f"  Created: {os.path.basename(qmd_path)}")

def process_subject(subject):
    """Process a single subject — clean, create files, write index."""
    dir_path = os.path.join(BASE, subject['dir'])
    qmd_path = os.path.join(BASE, subject['qmd'])
    
    print(f"\n{'='*60}")
    print(f"Processing: {subject['title']}")
    print(f"{'='*60}")
    
    # 1. Clean old files
    print("\nCleaning old files...")
    clean_directory(dir_path)
    
    # 2. Write syllabus
    print("\nWriting syllabus...")
    write_syllabus(dir_path, subject['nep_syllabus'], subject['ugc_syllabus'])
    
    # 3. Write common topics
    print("\nWriting common topics...")
    for filename, title, nep_ref, ugc_ref, content in subject['common']:
        write_common_topic(dir_path, filename, title, nep_ref, ugc_ref, content)
        print(f"  Created: {filename}")
    
    # 4. Write NEP-only topics
    if subject['nep_only']:
        print("\nWriting NEP-only topics...")
        for filename, title, nep_ref, content in subject['nep_only']:
            write_nep_topic(dir_path, filename, title, nep_ref, content)
            print(f"  Created: {filename}")
    
    # 5. Write UGC-only topics
    if subject['ugc_only']:
        print("\nWriting UGC NET-only topics...")
        for filename, title, ugc_ref, content in subject['ugc_only']:
            write_ugc_topic(dir_path, filename, title, ugc_ref, content)
            print(f"  Created: {filename}")
    
    # 6. Write QMD index
    print("\nWriting QMD index...")
    write_qmd_index(qmd_path, subject)


# ============================================================
# MAIN EXECUTION
# ============================================================
if __name__ == '__main__':
    print("=" * 60)
    print("GEOGRAPHY COURSE RESTRUCTURING SCRIPT")
    print("=" * 60)
    
    for subject in SUBJECTS:
        process_subject(subject)
    
    print("\n" + "=" * 60)
    print(f"COMPLETED: Processed {len(SUBJECTS)} subjects")
    print("=" * 60)
