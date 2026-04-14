"""
Script to restructure the remaining 2 geography subjects: Urban Geography and Geography of Odisha.
"""
import os
import glob

BASE = r"e:\spatialgeography\geography"

SUBJECTS = []

# ============================================================
# 20. URBAN GEOGRAPHY
# ============================================================
SUBJECTS.append({
    'dir': 'urban-geography',
    'qmd': 'urban-geography.qmd',
    'title': 'Urban Geography',
    'subtitle': 'The study of cities, urban processes, morphology, and spatial patterns of urbanization.',
    'has_interview': False,
    'nep_syllabus': """#### Core I Paper XV — Urban Geography
**(4 Credit, Theory: 45hrs, Practical: 30hrs)**

**Unit I:**
- Concept of Urban, Urbanization and Urbanism; Nature and scope of urban geography
- Classification of towns and cities: Census vs. statutory towns, functional classification
- Compact city, virtual city, network city, world class city, global city, smart city

**Unit II:**
- Origin and evolution of urban settlements; Hierarchy of urban settlements; urban region
- Slum: a unique character of Indian cities, ghettoization and gentrification
- Urban public places; Peri-urban: concept, challenges and opportunities
- Unequal cities; urban planning, policy and governance

**Unit III:**
- Models of Internal Structure of the City (Burgess, Hoyt, Harris and Ullman)
- Central Place theory of Christaller
- Trends and patterns of urbanization in World and India
- Urbanization: problems and opportunities; Recent urban changes""",
    'ugc_syllabus': """#### Unit V — Settlement Geography (Urban overlaps)
- Types, size, and spacing of settlements
- Internal morphology of urban settlements
- Ecological processes of urban growth, Urban fringe, City-region concepts
- Settlement systems, Primate city, Rank-Size rule, Central Place Theory, Market Centre Theory""",
    'common': [
        ('01-concepts-of-urban-geography.md', 'Concepts of Urban, Urbanization, and Urbanism',
         'Unit I — Concept of Urban, Urbanization and Urbanism',
         'Settlement systems',
         """* **Urban**: An area characterized by high population density, built environment, and non-agricultural occupations.
* **Urbanization**: The demographic process (structural shift) whereby a growing percentage of a population lives in urban areas. Linked to industrialization and economic development.
* **Urbanism**: A way of life, attitudes, values, and patterns of behavior characteristic of city dwellers (Louis Wirth).
* **Scope of Urban Geography**: Studies the spatial aspects of urban development, the internal structure of cities, and the relationships between cities conceptually and functionally."""),

        ('02-classification-of-towns.md', 'Classification of Urban Settlements',
         'Unit I — Census vs statutory towns, functional classification',
         'Types, size, and spacing of settlements',
         """* **Statutory Town**: Places with a municipality, corporation, cantonment board or notified town area committee.
* **Census Town**: Places satisfying 3 criteria: Pop > 5,000; > 75% male main working population in non-agricultural pursuits; Density > 400 persons/sq.km.
* **Functional Classification**: Categorizing cities by their dominant economic activity (e.g., Administrative - New Delhi, Industrial - Jamshedpur, Religious - Varanasi). Ashish Bose and Ashok Mitra classifications in India.
* **Emerging Concepts**: Smart City (technology-driven efficiency), Global City (Sassen - nodes of global finance/trade like NY, London, Tokyo), Compact City (high density, transit-oriented)."""),

        ('03-urban-morphology-models.md', 'Models of Internal Urban Structure',
         'Unit III — Burgess, Hoyt, Harris and Ullman',
         'Internal morphology of urban settlements',
         """* **Concentric Zone Model (Burgess, 1925)**: City grows outward in concentric rings from the CBD (Central Business District). Based on Chicago. Driven by land value and social mobility.
* **Sector Model (Hoyt, 1939)**: City grows in wedge-shaped sectors radiating from the CBD along transport corridors. High-class residential seeks best amenities/highest ground.
* **Multiple Nuclei Model (Harris & Ullman, 1945)**: Rejects single CBD. City develops around several distinct nuclei (nodes) like ports, universities, or industrial parks, driven by agglomeration and repelling forces."""),

        ('04-central-place-theory.md', 'Central Place Theory and Settlement Hierarchy',
         'Unit II & III — Hierarchy of urban settlements, Christaller',
         'Christaller’s Central Place Theory, Market Centre Theory',
         """* **Christaller's Central Place Theory (1933)**: Explains the size, spacing, and number of settlements.
  - *Assumptions*: Isotropic plain, uniform transport cost, rational consumers.
  - *Concepts*: Threshold (minimum population to support a service), Range (maximum distance people will travel).
  - *Hierarchy*: Hexagonal trade areas. K=3 (Marketing), K=4 (Transport), K=7 (Administrative).
* **August Lösch (1940)**: Modified Christaller by building the model from the bottom up (starting from the lowest order goods) to maximize consumer welfare and create complex economic landscapes."""),

        ('05-urban-fringe-and-slums.md', 'Urban Fringe, Ghettoization, and Slums',
         'Unit II — Slum, ghettoization, gentrification, Peri-urban',
         'Urban fringe, Ecological processes of urban growth',
         """* **Peri-Urban / Urban Fringe**: The transition zone where urban and rural land uses mix. Highly dynamic, characterized by land speculation, shifting agriculture, and lacking urban amenities.
* **Slums**: Informal settlements lacking basic services, characterized by overcrowding and poverty. Result of rapid rural-urban migration exceeding city capacity.
* **Ghettoization**: Spatial isolation of an ethnic, religious, or socio-economic minority group (intentional or systemic).
* **Gentrification**: Renewal/rebuilding of deteriorated urban neighborhoods by influx of more affluent residents, often displacing poorer original inhabitants."""),
    ],
    'nep_only': [
        ('06-trends-of-urbanization.md', 'Trends of Urbanization in India',
         'Unit III — Trends and patterns of urbanization in World and India',
         """* **Indian Urbanization**: Currently around 35% (census/estimates). Characterized as "over-urbanization" or "urbanization without industrialization" where pushing forces (rural poverty) are stronger than pulling forces (urban jobs).
* **Top-Heavy Structure**: Growth is highly concentrated in Class I cities and metropolises, causing regional imbalances and straining infrastructure.
* **Urban Policy and Governance**: AMRUT, Smart Cities Mission, Swachh Bharat Abhiyan (Urban). Governance involves balancing economic growth with social equity and environmental sustainability in "unequal cities"."""),
    ],
    'ugc_only': [
        ('07-rank-size-and-primate-city.md', 'Rank-Size Rule and Primate City Concept',
         'Rank-Size rule, Primate city',
         """* **Rank-Size Rule (G.K. Zipf, 1949)**: The population of a city is inversely proportional to its rank in the urban hierarchy ($P_r = P_1 / r$). Represents a balanced, integrated urban system.
  - E.g., The USA roughly follows this rule.
* **Primate City (Mark Jefferson, 1939)**: The largest city is "supereminent" and disproportionately larger than the second largest city (often >2x). It dominates the country politically and economically.
  - E.g., Paris, London, Mexico City, Bangkok.
  - Often indicates centralization, colonial legacy, or an underdeveloped national settlement network."""),
    ],
})

# ============================================================
# 21. GEOGRAPHY OF ODISHA
# ============================================================
SUBJECTS.append({
    'dir': 'geography-of-odisha',
    'qmd': 'geography-of-odisha.qmd',
    'title': 'Geography of Odisha',
    'subtitle': 'Detailed study of the physical and socio-cultural geography of the state of Odisha.',
    'has_interview': False,
    'nep_syllabus': """#### Core I Paper XII — Geography of Odisha
**(4 Credit, Theory: 45hrs, Practical: 30hrs)**

**Unit I:**
- Geological Structure: Distribution of Major Rock Systems; Physiographic Divisions
- Factors Influencing Climate of Odisha; Climatic Regions
- Major Soil Types; Natural Vegetation

**Unit II:**
- Growth, Distribution and Density of Population
- Population Composition: Linguistic, Rural-Urban; Distribution of Cities and Towns
- Regional Variation in Folk Housing Types; Dynamics of Migration

**Unit III:**
- Geographical Factors Behind Odisha's History and Culture
- Evolution of Odia Language and Regional Variations
- Diversity of Tribes and Social Formation; Regional Variation of Food Preferences
- Folk and Popular Odia Cultures; Vernacular Cultural Regions

**Unit IV:**
- Major Farming Types; Industrial Belts of Odisha
- Cottage and Handicraft Industries; Fisheries: Fresh and Marine
- Aspirational districts and other major welfare schemes
- Natural Disaster and Odisha (Cyclone, Flood): Risk and Vulnerability
- Paradigm Shifts in Disaster Management: Success Stories of Odisha""",
    'ugc_syllabus': """#### UGC NET Overlaps
- Regional Geography/Geography of India context at state level
- Natural Disasters in India (Cyclone, Flood)
- Spatial distribution of social groups (Tribe, Language)""",
    'common': [
        ('01-physiography-and-climate.md', 'Physical Geography of Odisha',
         'Unit I — Physiographic Divisions, Climate, Major Soils, Vegetation',
         'Geography of India (State application)',
         """* **Physiographic Divisions**:
  1. *Coastal Plains (Utkal Plains)*: Formed by the deltas of Subarnarekha, Budhabalanga, Baitarani, Brahmani, Mahanadi, and Rushikulya ("Gift of Six Rivers").
  2. *Middle Mountainous Region*: Eastern Ghats (Similipal, Deomali).
  3. *Central Plateaus*: Panposh, Keonjhar, Nabarangpur.
  4. *Western Rolling Uplands*: Sundargarh, Bolangir.
* **Climate**: Tropical Savanna/Monsoon type. Strongly influenced by the Bay of Bengal, SW Monsoon, and retreating monsoon cyclones.
* **Natural Vegetation**: Tropical moist deciduous (Sal, Teak) and dry deciduous. Mangroves (Bhitarkanika)."""),

        ('02-demography-and-tribes.md', 'Demography, Tribes, and Social Geography',
         'Unit II & III — Population Distribution, Diversity of Tribes, Social Formation',
         'Population/Social Geography (State application)',
         """* **Population Distribution**: High density in the coastal plains and river valleys; sparse density in the inland hilly and forested districts (Kandhamal, Malkangiri).
* **Tribal Diversity**: Odisha has 62 tribal communities, the highest in India (including 13 PVTGs - Particularly Vulnerable Tribal Groups like Dongria Kondh, Bonda). Concentrated in the Mayurbhanj, Keonjhar, Sundargarh, Koraput, and Rayagada belts.
* **Socio-Cultural Geography**: Evolution of Odia language. Impact of geography (hills vs. plains) on food habits, folk arts (Pattachitra), and housing types."""),

        ('03-economy-and-industries.md', 'Economy: Agriculture and Industries',
         'Unit IV — Farming Types, Industrial Belts, Fisheries',
         'Economic Geography (State application)',
         """* **Agriculture**: Primarily agrarian economy, rain-fed (mostly paddy). Coastal plains are the "rice bowl". Shifting cultivation (Podu) among tribes.
* **Mineral Wealth and Industries**: Highly rich in Iron Ore, Bauxite, Coal, Chromite.
  - *Major Industrial Belts*: Rourkela (Steel), Angul-Talcher (Coal, Power, Aluminum), Kalinganagar (Steel hub), Jharsuguda (Aluminum).
* **Primary Activities**: Significant marine fisheries along the long coastline and freshwater fisheries (Chilika Lake). Cottage industries (handlooms - Sambalpuri, Pipli appliqué work)."""),

        ('04-disaster-management-odisha.md', 'Disasters and Disaster Management in Odisha',
         'Unit IV — Cyclone, Flood Risk; Success Stories of Odisha',
         'Natural Disasters in India (Cyclone, Flood)',
         """* **Vulnerability**: Highly prone to severe tropical cyclones from the Bay of Bengal (e.g., 1999 Super Cyclone, Phailin, Fani) and frequent floods in the Mahanadi-Brahmani-Baitarani delta.
* **Paradigm Shift in Management**:
  - The 1999 Super Cyclone caused ~10,000 deaths, acting as a wake-up call leading to the creation of OSDMA (Odisha State Disaster Management Authority), the first of its kind in India.
  - Focus shifted from post-disaster relief to pre-disaster preparedness (Early Warning Systems, Cyclone Shelters, community-level ODRAF teams).
  - *Success Story*: Cyclone Phailin (2013) saw the largest human evacuation in Indian history (>1 million people) resulting in minimal loss of life (global recognition by the UN)."""),
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
