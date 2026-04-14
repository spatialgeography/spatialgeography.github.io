"""
Script to restructure remaining geography subject directories with proper
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
# 6. ECONOMIC GEOGRAPHY
# ============================================================
SUBJECTS.append({
    'dir': 'economic-geography',
    'qmd': 'economic-geography.qmd',
    'title': 'Economic Geography',
    'subtitle': 'Spatial patterns of economic activities, production, and resource distribution.',
    'has_interview': False,
    'nep_syllabus': """#### Core I Paper VII — Economic Geography
**(4 Credit, Theory: 45hrs, Practical: 30hrs)**

**Unit I:**
- Meaning and scope of economic geography; classification of economic activities
- Factors affecting location of Economic Activity (agriculture, manufacturing, services)
- Classification of world agricultural system of Whittlesey
- Von Thunen theory of location of agricultural activity

**Unit II:**
- Secondary Activities: Manufacturing (Cotton Textile, Iron and Steel, Petrochemical)
- Major Industrial Regions of the world
- Special Economic Zones and Technology Parks
- Tertiary Activities: Transport (Land, Air, Water, Pipelines), Trade (National/International)
- Weber and Smith's Industrial location Theory

**Unit III:**
- Resource: Concept, Classification, Functional Theory of Resources
- Distribution, Utilization, Problems and Management of World's Resources (Land, Water, Forest, Energy)
- Mineral resources (Iron, Bauxite)
- Resource Development Regions of India
- Resource depletion, conservation and Sustainable use of resources""",
    'ugc_syllabus': """#### Unit VI — Geography of Economic Activities and Regional Development

**I. Economic Geography**
- Factors affecting spatial organisation of economic activities (primary, secondary, tertiary and quarternary)
- Natural Resources (classification, distribution and associated problems), Natural Resources Management
- World Energy Crises in Developed and Developing Countries

**II. Agricultural Geography**
- Land capability classification and Land Use Planning
- Cropping Pattern: Methods of delineating crop combination regions (Weaver, Doi and Rafiullah), Crop diversification
- Von Thunen's Model of Land Use Planning
- Measurement and Determinants of Agricultural Productivity, Regional variations
- Agricultural Systems of the World

**III. Industrial Geography**
- Classification of Industries, Factors of Industrial Location
- Theories of Industrial Location (A. Weber, E. M. Hoover, August Losch, A. Pred and D. M. Smith)
- World Industrial Regions, Impact of Globalisation on manufacturing sector
- Tourism Industry, ICT and Knowledge Production Industries""",
    'common': [
        ('01-economic-activities-and-sectors.md', 'Classification of Economic Activities',
         'Unit I — Meaning and scope; classification of economic activities',
         'Factors affecting spatial organisation of economic activities (primary, secondary, tertiary and quarternary)',
         """* **Meaning and Scope**: Study of the spatial variation of human economic activities on the Earth's surface — production, exchange, and consumption of wealth.
* **Primary Activities**: Harvesting or extracting natural resources directly from the Earth (agriculture, mining, forestry, fishing).
* **Secondary Activities**: Processing raw materials, manufacturing, and construction (adding value to primary products).
* **Tertiary Activities**: Provision of services (retail, banking, transport, education, healthcare).
* **Quaternary Activities**: Information processing and knowledge-based services (research, IT, financial planning).
* **Quinary Activities**: High-level decision-making roles (executives, government officials, research scientists).
* **Sectoral Shift**: Transition of economies from primary to secondary to tertiary dominance as they develop (Clark-Fisher model)."""),

        ('02-factors-of-location.md', 'Factors Affecting Location of Economic Activities',
         'Unit I — Factors affecting location of Economic Activity',
         'Factors affecting spatial organisation of economic activities; Factors of Industrial Location',
         """* **Physical Factors**: Climate, terrain, soil fertility, availability of water, raw materials, power/energy sources.
* **Economic Factors**: Transport and communication costs, market access, labour (cost, skill, availability), capital access, agglomeration economies.
* **Political/Institutional Factors**: Government policies, taxes, subsidies, environmental regulations, political stability, zoning laws.
* **Social/Cultural Factors**: Entrepreneurship, cultural preferences, behavioural factors in decision-making.
* **Footloose Industries**: Industries not strongly tied to specific raw materials or markets (e.g., software, light electronics) — located based on other factors like skilled labour or amenities."""),

        ('03-agricultural-systems.md', 'Agricultural Systems of the World',
         'Unit I — Classification of world agricultural system of Whittlesey',
         'Agricultural Systems of the World',
         """* **Whittlesey's Classification (1936)**: Based on crop/livestock combination, intensity of land use, processing methods, mechanization, and destination of products.
* **13 Agricultural Regions**:
  1. Nomadic Herding 
  2. Livestock Ranching
  3. Shifting Cultivation
  4. Rudimentary Sedentary Tillage
  5. Intensive Subsistence (rice dominant)
  6. Intensive Subsistence (without rice)
  7. Commercial Plantation
  8. Mediterranean Agriculture
  9. Commercial Grain Farming
  10. Commercial Livestock and Crop Farming (Mixed)
  11. Subsistence Crop and Livestock Farming
  12. Commercial Dairy Farming
  13. Specialized Horticulture
* **Modern Changes**: Impact of Green Revolution, globalization, corporate farming, contract farming."""),

        ('04-von-thunen-model.md', 'Von Thunen\'s Model of Agricultural Location',
         'Unit I — Von Thunen theory of location of agricultural activity',
         'Von Thunen\'s Model of Land Use Planning',
         """* **Johann Heinrich von Thünen (1826)**: *The Isolated State* — first quantitative model of spatial economic organization.
* **Assumptions**: Isolated state, isotropic plain, single central market, uniform transport costs, profit-maximizing farmers.
* **Concept**: Economic rent (locational rent) determines land use. Products with high transport costs or perishability are produced closest to the market.
* **Concentric Rings**:
  1. Market gardening and dairying (perishable, high value)
  2. Forestry/Firewood (heavy/bulky to transport)
  3. Intensive crop farming without fallow
  4. Crop farming with fallow and pasture
  5. Three-field system
  6. Livestock ranching (animals can walk to market)
* **Relevance Today**: Modified by modern transport (refrigeration, highways) and global markets, but core concept of *distance decay* remains valid."""),

        ('05-industrial-location-theories.md', 'Theories of Industrial Location',
         'Unit II — Weber and Smith\'s Industrial location Theory',
         'Theories of Industrial Location (A. Weber, E. M. Hoover, August Losch, A. Pred and D. M. Smith)',
         """* **Alfred Weber's Least Cost Theory (1909)**: Location minimizes transport and labour costs. Uses locational triangle (two raw materials, one market). 
  - *Material Index* = Weight of localized raw materials / Weight of finished product. MI > 1 = material-oriented; MI < 1 = market-oriented.
  - *Isodapanes*: Lines of equal total transport cost.
* **August Lösch's Profit Maximization Theory (1954)**: Location where profit is maximized (total revenue minus total cost). Spatial demand cone; hexagonal market areas.
* **David Smith's Spatial Margins of Profitability (1971)**: Incorporates spatial variations in both costs and revenues to find the area where profit is possible.
* **Edgar Hoover**: Emphasized transport cost structure (terminal vs. line-haul costs).
* **Allan Pred**: Behavioural approach — bounded rationality, imperfect information in locational decision-making."""),

        ('06-manufacturing-industries.md', 'Manufacturing Industries',
         'Unit II — Manufacturing (Cotton Textile, Iron and Steel, Petrochemical)',
         'Classification of Industries; World Industrial Regions',
         """* **Classification of Industries**: By size (cottage, small-scale, large-scale), raw material (agro-based, mineral-based), output (basic/heavy, consumer/light), ownership (public, private, joint).
* **Iron and Steel Industry**: Basic/key industry. Location historically tied to coal (Ruhr, Appalachians), later to iron ore, and currently shifting to coastal locations or markets (mini-steel plants using scrap).
* **Textile Industry**: Historically labour-intensive, raw material oriented, but highly mobile today due to cheap labour search (shift from developed to developing global South).
* **Petrochemical Industry**: Market or port-oriented; forms base for plastics, fertilizers, synthetic fibres.
* **World Industrial Regions**: Great Lakes/Northeastern USA, Western Europe (Ruhr, Midlands), Eastern Europe (Donbas), East Asia (Japan, coastal China, South Korea)."""),

        ('07-transport-and-trade.md', 'Transport and Trade',
         'Unit II — Transport (Land, Air, Water, Pipelines), Trade',
         'Factors affecting spatial organisation (tertiary)',
         """* **Modes of Transport**:
  - *Roads*: Door-to-door, best for short distances.
  - *Railways*: Heavy/bulky goods over long distances inland.
  - *Waterways*: Cheapest mode for bulky goods over long distances; major ocean routes (North Atlantic, Suez, Panama).
  - *Airways*: Fastest, highest cost, best for low-volume/high-value/perishable goods.
  - *Pipelines*: Continuous flow of liquids/gases (oil, natural gas, water).
* **Trade**: Exchange of goods/services. Based on comparative advantage (Ricardo).
* **Global Trade Patterns**: Shift from North-North dominance to increasing South-South and intra-regional trade. Role of WTO, regional trade blocs (EU, NAFTA/USMCA, ASEAN, MERCOSUR)."""),

        ('08-resources-concept-classification.md', 'Natural Resources: Concept and Classification',
         'Unit III — Resource: Concept, Classification, Functional Theory',
         'Natural Resources (classification, distribution and associated problems)',
         """* **Zimmermann's Functional Theory (1933)**: "Resources are not, they become." A neutral stuff becomes a resource when human knowledge and technology find a use for it and it is economically viable.
* **Classification**:
  - *Renewability*: Renewable (flow/fund - water, solar, forest) vs. Non-renewable (stock - minerals, fossil fuels).
  - *Origin*: Biotic (forests, wildlife, fossil fuels) vs. Abiotic (land, water, minerals).
  - *Development Stage*: Potential, actual/developed, reserve, stock.
  - *Distribution*: Ubiquitous (air) vs. Localized (minerals).
* **Resource Problems**: Uneven distribution leading to geopolitical conflicts, overexploitation, environmental degradation, tragedy of the commons."""),

        ('09-energy-resources.md', 'Energy Resources and Crisis',
         'Unit III — Energy (Coal, petroleum and non-conventional)',
         'World Energy Crises in Developed and Developing Countries',
         """* **Conventional/Non-Renewable Energy**:
  - *Coal*: Four types (anthracite, bituminous, lignite, peat). Major producers: China, USA, India, Australia.
  - *Petroleum*: Middle East, USA, Russia. Geopolitical significance (OPEC).
  - *Natural Gas*: Cleanest fossil fuel.
* **Non-Conventional/Renewable Energy**: Solar, wind, hydroelectric, geothermal, biomass, tidal. Crucial for sustainable development.
* **World Energy Crisis**: Situation where energy demand outweighs supply or is threatened by geopolitical instability, infrastructure failures, or environmental constraints.
* **Developed vs. Developing Contexts**: Developed nations focus on transition to renewables and energy security; developing nations face challenges of energy access/poverty while managing rapid demand growth."""),
    ],
    'nep_only': [
        ('10-sez-technology-parks.md', 'Special Economic Zones and Technology Parks',
         'Unit II — Special Economic Zones and Technology Parks',
         """* **Special Economic Zones (SEZs)**: Designated geographical areas within a country with liberal economic laws (tax exemptions, relaxed labour laws, single-window clearance) intended to boost foreign direct investment (FDI), exports, and employment.
  - Example: Shenzhen (China) as a pioneer; Indian SEZ Act 2005.
* **Technology Parks / Technopoles**: Centers of high-tech manufacturing and information-based quaternary industries.
  - Agglomeration economies, proximity to major universities/research centers, high-quality amenities to attract skilled workers.
  - Examples: Silicon Valley (USA), Bengaluru IT corridor (India), Cambridge Science Park (UK).
* **Impacts**: Regional development engines, but can create enclaves, widen regional disparities, and cause land acquisition conflicts."""),

        ('11-resource-development-conservation.md', 'Resource Management and Conservation',
         'Unit III — Resource Development Regions of India; Resource depletion, conservation and Sustainable use',
         """* **Resource Depletion**: Rapid exhaustion of non-renewable resources (Hubbert peak theory) and degradation of renewables (overfishing, deforestation, groundwater depletion).
* **Conservation**: Rational and efficient use of resources to ensure availability for future generations without ecological damage.
* **Sustainable Development**: Meeting present needs without compromising future generations (Brundtland Report 1987).
* **Strategies**: 3Rs (Reduce, Reuse, Recycle), substitution (finding alternatives), improving technological efficiency, afforestation, watershed management.
* **Resource Development Regions of India**: Delineated based on homogeneity of physical and economic resources (e.g., Damodar Valley, Western Ghats) for targeted planning."""),
    ],
    'ugc_only': [
        ('12-agriculture-measurement-methods.md', 'Agricultural Measurement and Modeling Methods',
         'Cropping Pattern: Methods of delineating crop combination regions (Weaver, Doi and Rafiullah), Crop diversification',
         """* **Crop Combination Regions**: Area where two or more crops are grown in association.
  - *Weaver's Method (1954)*: Standard deviation approach comparing actual crop percentages with a theoretical distribution (monoculture 100%, 2-crop 50-50%, etc.). Minimum variance determines the combination.
  - *Doi's Method (1959)*: Modified Weaver's formula using $\Sigma d^2$ without dividing by $n$; uses a critical value table.
  - *Rafiullah's Method (1956)*: Maximum positive deviation method ($\Sigma D^2 / N^2$) — reduces the number of crops in the final combination compared to Weaver.
* **Crop Diversification**: Growing a variety of crops rather than monoculture — insurance against weather/market failure. Measured by Bhatia's or Gibbs-Martin index.
* **Agricultural Productivity**: Output per unit of input (land, labour, capital). Measured by Kendall's Ranking Coefficient, Enyedi's Index, Bhatia's Method."""),

        ('13-contemporary-industry-trends.md', 'Contemporary Trends in Industrial Geography',
         'Impact of Globalisation on manufacturing sector; Tourism Industry, ICT and Knowledge Production',
         """* **Impact of Globalisation**: Deindustrialization in advanced economies (Rust Belt), relocation of manufacturing to NICs (Newly Industrialized Countries) seeking cheap labour and lax environmental rules. Emergence of Global Value Chains (GVCs).
* **Tourism Industry**: Largest single employer globally. Forms: ecotourism, heritage, medical, adventure. Impacts on local economies and environments.
* **ICT and Knowledge Production**: The "New Economy". Highly footloose, cluster in technopoles. Teleworking and the "death of distance".
* **Flexible Specialization (Post-Fordism)**: Shift from mass production (Fordism) to batch production, just-in-time inventory, and product customization.
* **Agglomeration vs. Dispersion**: Centripetal forces (knowledge spillovers, input sharing) vs. Centrifugal forces (congestion, high rent)."""),
    ],
})

# ============================================================
# 7. POLITICAL GEOGRAPHY
# ============================================================
SUBJECTS.append({
    'dir': 'political-geography',
    'qmd': 'political-geography.qmd',
    'title': 'Political Geography',
    'subtitle': 'Spatial dimensions of political power, borders, states, and geopolitics.',
    'has_interview': False,
    'nep_syllabus': """#### Core I Paper XXII — Political Geography and Globalisation
**(4 Credit, Theory: 45hrs, Practical: 30hrs)**

**Unit I**
- Political geography: concept, nature, scope and evolution
- Concept of state, nation and nation-state
- Attributes of state: frontiers, boundaries, buffer zone, population, territory, sovereignty
- Geopolitics: concept, and theories (Heartland and Rimland)

**Unit II**
- Political geography and systems of government: Federalism, Local self-government administration
- Creation of new states
- Political geography of resource conflicts: inter-state river water sharing disputes
- Conflicts on forest rights and mineral resources
- Politics of Displacement: Issues of relief, compensation and rehabilitation (Dams, SEZs)

**Unit III**
- Geo-Economic and Political Blocks: UNO, SAARC, ASEAN, NATO, EU, OPEC, BRICS
- India as a global power with special reference to G20
- India and its relationship with neighbouring countries: Pakistan, China, Nepal, Bhutan, Bangladesh, Myanmar and Sri Lanka""",
    'ugc_syllabus': """#### Unit VII — Political Geography

- Boundaries and Frontiers (with special reference to India)
- Heartland and Rimland Theories
- Trends and Developments in Political Geography
- Geography of Federalism, Electoral Reforms in India, Determinants of Electoral Behaviour
- Geopolitics of Climate Change, Geopolitics of World Resources, Geo-politics of India Ocean
- Regional Organisations of Cooperation (SAARC, ASEAN, OPEC, EU)
- Neopolitics of World Natural Resources""",
    'common': [
        ('01-concept-nature-scope.md', 'Concept, Nature, and Scope of Political Geography',
         'Unit I — Political geography: concept, nature, scope and evolution',
         'Trends and Developments in Political Geography',
         """* **Definitions**: The study of the spatial distribution of political processes and spatial patterns produced by political decisions. (e.g., R. Hartshorne, N.J.G. Pounds).
* **Evolution**: 
  - *Early phase*: Environmental determinism (Ratzel, Semple), Geopolitics (Kjellén, Haushofer).
  - *Mid-20th Century*: Functional approach (Hartshorne), Areal differentiation.
  - *Modern/Critical phase*: Systems analysis, critical geopolitics, electoral geography, political ecology.
* **Scope**: Ranges from local (electoral redistricting) to national (state formation, boundaries) to global (geopolitics, international organizations).
* **Approaches**: Morphological (studying structure), Functional (how a state functions), Behavioural, Political Economy approach."""),

        ('02-state-nation-nationstate.md', 'State, Nation, and Nation-State',
         'Unit I — Concept of state, nation and nation-state; Attributes of state',
         'Trends and Developments in Political Geography',
         """* **State**: A politically organized territory with a permanent population, defined territory, and a government with sovereignty (e.g., India, France).
* **Nation**: A group of people with a shared cultural identity, history, language, or religion, and a strong sense of unity (e.g., the Kurds, the Palestinians).
* **Nation-State**: When the geographical boundaries of a state coincide exactly with the cultural boundaries of a nation (e.g., Japan, Iceland) — an ideal rarely achieved perfectly.
* **Stateless Nation**: A nation without a sovereign state of its own (e.g., Kurds, Tibetans).
* **Attributes of a State**:
  - *Territory*: Land, territorial waters, airspace.
  - *Population*: Citizens and residents.
  - *Government*: Administrative structure.
  - *Sovereignty*: Supreme authority over its territory, free from external control."""),

        ('03-boundaries-frontiers.md', 'Boundaries and Frontiers',
         'Unit I — Frontiers, boundaries, buffer zone',
         'Boundaries and Frontiers (with special reference to India)',
         """* **Frontiers**: Geographic zones or belts that separate states. Margin of settlement, outward-oriented, dynamic, transitional (historical concept, now largely replaced by boundaries).
* **Boundaries**: Precise linear dividing lines defined by treaties and demarcated on the ground. Inward-oriented.
* **Boundary Classification (Genetic)**:
  - *Antecedent*: Drawn before the area was highly populated (e.g., 49th parallel US-Canada).
  - *Subsequent*: Drawn after settlement, reflecting cultural patterns (e.g., India-Pakistan borders).
  - *Superimposed*: Forced upon a landscape by outside powers, ignoring cultural realities (e.g., colonial borders in Africa).
  - *Relict*: Ceased to function but its imprint remains on the landscape (e.g., Great Wall of China, Berlin Wall).
* **Morphological Classification**: Geometric (straight lines), Physiographic (rivers, mountains — e.g., Rio Grande).
* **Buffer Zone/State**: Weak, independent state located between two major, rival powers (e.g., historical Nepal between British India and China/Tibet)."""),

        ('04-geopolitical-theories.md', 'Geopolitical Theories: Heartland and Rimland',
         'Unit I — Geopolitics: concept, and theories (Heartland and Rimland)',
         'Heartland and Rimland Theories',
         """* **Geopolitics**: Term coined by Rudolf Kjellén. The study of the effects of Earth's geography on politics and international relations.
* **Heartland Theory (Halford Mackinder, 1904/1919)**:
  - Divided world into the "World-Island" (Eurasia+Africa) and peripheral islands.
  - The "Heartland" (pivot area) is the vast interior of Eurasia, inaccessible to sea power, dominated by land power (railways).
  - *Dictum*: "Who rules East Europe commands the Heartland; Who rules the Heartland commands the World-Island; Who rules the World-Island commands the World."
* **Rimland Theory (Nicholas Spykman, 1942)**:
  - Refuted Mackinder. Argued the coastal fringes (Rimland) of Eurasia are more important than the Heartland due to their demographic weight, resources, and access to both sea and land.
  - *Dictum*: "Who controls the Rimland rules Eurasia; who rules Eurasia controls the destinies of the world."
  - Heavily influenced US Cold War "Containment" policy.
* **Seapower Theory (A.T. Mahan)**: Emphasized control of the seas and strategic chokepoints."""),

        ('05-geography-of-federalism.md', 'Systems of Government and Federalism',
         'Unit II — Federalism, Local self-government administration, Creation of new states',
         'Geography of Federalism',
         """* **Unitary State**: Power centralized in a single national government (e.g., France, UK, China). Works best in compact states with homogeneous populations.
* **Federal State**: Power divided vertically between a central government and sub-national units (states/provinces) (e.g., USA, India, Australia). Accommodates regional diversity and large territories.
* **Geography of Indian Federalism**: Asymmetrical federalism. Reorganization of states initially on linguistic lines (States Reorganisation Act, 1956), later addressing regional inequalities (creation of Uttarakhand, Jharkhand, Chhattisgarh, Telangana).
* **Local Self-Government**: Decentralization to grassroots level. In India: 73rd (Panchayati Raj) and 74th (Municipalities) Amendments creating a three-tier system.
* **Centripetal vs. Centrifugal Forces**: Hartshorne's concept. Centripetal forces bind a state together (nationalism, institutions, transport network); Centrifugal forces pull it apart (ethnic conflict, regionalism, physical barriers)."""),

        ('06-regional-organisations.md', 'Regional Organisations of Cooperation',
         'Unit III — Geo-Economic and Political Blocks: UNO, SAARC, ASEAN, NATO, EU, OPEC, BRICS',
         'Regional Organisations of Cooperation (SAARC, ASEAN, OPEC, EU)',
         """* **Supranationalism**: Voluntary association of three or more states for mutual benefit.
* **Economic Blocs**:
  - *EU (European Union)*: Deepest integration — free movement of goods, services, capital, people. Common currency (Eurozone).
  - *ASEAN (Association of Southeast Asian Nations)*: Economic and security cooperation in SE Asia.
  - *OPEC (Org. of the Petroleum Exporting Countries)*: Cartel coordinating oil policies and prices.
* **Strategic/Political Blocs**:
  - *NATO (North Atlantic Treaty Organization)*: Collective defense alliance.
  - *UNO (United Nations)*: Global governance, peacekeeping, international law.
* **Regional/Emerging Blocs**:
  - *SAARC (South Asian Association for Regional Cooperation)*: Hindered by India-Pakistan tensions.
  - *BRICS (Brazil, Russia, India, China, South Africa)*: Emerging economies challenging Western-dominated financial architecture."""),

        ('07-India-neighbour-relations.md', 'India and its Neighbouring Countries',
         'Unit III — India and its relationship with neighbouring countries',
         'Boundaries and Frontiers (with special reference to India)',
         """* **India-Pakistan**: Radcliffe Line, LOC (Line of Control), Siachen Glacier. Issues: Kashmir dispute, cross-border terrorism, Indus Water Treaty.
* **India-China**: McMahon Line, LAC (Line of Actual Control). Issues: Border disputes in Aksai Chin and Arunachal Pradesh, Tibet issue, String of Pearls strategy.
* **India-Bangladesh**: Longest land border, Radcliffe Line. Resolved issues: Land Boundary Agreement (enclaves), maritime boundary. Ongoing: Teesta river water sharing, illegal migration.
* **India-Nepal**: Open border (1950 Treaty), Kalapani dispute.
* **India-Myanmar**: Free Movement Regime, insurgency in Northeast India.
* **India-Sri Lanka**: Palk Strait, Katchatheevu island, Tamil ethnic issue, Chinese influence in ports (Hambantota).
* **Geopolitics of the Indian Ocean**: India's "Necklace of Diamonds" vs. China's "String of Pearls". Importance of SLOCs (Sea Lines of Communication)."""),
    ],
    'nep_only': [
        ('08-resource-conflicts.md', 'Political Geography of Resource Conflicts',
         'Unit II — Inter-state river water sharing disputes, forest rights, Politics of Displacement',
         """* **Inter-State River Water Disputes**: Triggered by increasing demand, climate change, and regional politics.
  - *Cauvery*: Karnataka, Tamil Nadu, Kerala, Puducherry.
  - *Krishna*: Maharashtra, Karnataka, Telangana, Andhra Pradesh.
  - *Mahanadi*: Chhattisgarh, Odisha.
  - Mechanisms: Inter-State Water Disputes Act (1956), Tribunals.
* **Conflicts on Forest and Mineral Resources**: State vs. local communities/tribals. Niyamgiri hills (Odisha), coal block allocations. Forest Rights Act (FRA, 2006) implementation.
* **Politics of Displacement**: Development-induced displacement (Dams, SEZs, mining).
  - *Issues*: Inadequate compensation, loss of livelihood, cultural disruption, poor rehabilitation.
  - Examples: Narmada Bachao Andolan (Sardar Sarovar Dam), POSCO/Vedanta in Odisha. Land Acquisition Act (LARR, 2013)."""),
    ],
    'ugc_only': [
        ('09-electoral-geography.md', 'Electoral Geography',
         'Electoral Reforms in India, Determinants of Electoral Behaviour',
         """* **Electoral Geography**: Spatial analysis of election results, voting behaviour, and the geographical effects of electoral boundaries.
* **Gerrymandering**: Manipulating electoral district boundaries to give an unfair political advantage to a particular party (Packing and Cracking techniques).
* **Determinants of Electoral Behaviour in India**: Geography, caste, religion, class, regionalism, language, local issues vs. national issues, incumbency factor.
* **Electoral Reforms in India**:
  - Introduction of EVMs and VVPATs.
  - Lowering voting age (61st Amendment).
  - NOTA (None of the Above) option.
  - Delimitation commissions (freezing of seats to 1971 census populations to not penalize states controlling population).
  - Model Code of Conduct, expenditure limits, tackling criminalization."""),

        ('10-neopolitics-of-resources.md', 'Geopolitics of Climate Change and Resources',
         'Geopolitics of Climate Change, Geopolitics of World Resources, Neopolitics of World Natural Resources',
         """* **Geopolitics of Climate Change**: Differential impacts and responsibilities. Developed vs. Developing nations (CBDR - Common But Differentiated Responsibilities). Arctic geopolitics (melting ice opening sea routes and resource access). Climate refugees.
* **Geopolitics of World Resources (Neopolitics)**: Shift from territorial conquest to securing control over strategic resources (oil, gas, rare earth elements, water).
  - "Resource Curse" in Africa and the Middle East.
  - China's Belt and Road Initiative (BRI) and securing critical minerals in Africa/South America.
  - Energy security and pipeline geopolitics (e.g., Nord Stream, TAPI).
* **Hydropolitics**: Conflicts over international river basins (Nile, Mekong, Jordan, Indus)."""),
    ],
})

def clean_directory(dir_path, keep_files=None):
    if keep_files is None:
        keep_files = []
    for f in glob.glob(os.path.join(dir_path, '*.md')):
        basename = os.path.basename(f)
        if basename not in keep_files:
            os.remove(f)

def write_syllabus(dir_path, nep_text, ugc_text):
    content = f"## Official Syllabus\n\n### NEP-2020 Syllabus\n\n::: {{.callout-note}}\n{nep_text}\n:::\n\n---\n\n### UGC NET Syllabus\n\n::: {{.callout-tip}}\n{ugc_text}\n:::\n"
    filepath = os.path.join(dir_path, '00-syllabus.md')
    with open(filepath, 'w', encoding='utf-8') as f:
        f.write(content)

def write_common_topic(dir_path, filename, title, nep_ref, ugc_ref, content):
    md = f"## {title}\n\n::: {{.callout-warning}}\n#### 📘 Syllabus Coverage\n| Syllabus | Topic Details |\n|----------|---------------|\n| **NEP-2020** | {nep_ref} |\n| **UGC NET** | {ugc_ref} |\n:::\n\n\n[![](https://via.placeholder.com/600x300?text=Course+Module+Video)](#)\n\n[Get the Presentation &nearr;](#) &nbsp; | &nbsp; [Watch the Video &nearr;](#)\n\n::: {{.callout-note}}\n### Key Concepts\n{content}\n:::\n"
    filepath = os.path.join(dir_path, filename)
    with open(filepath, 'w', encoding='utf-8') as f:
        f.write(md)

def write_nep_topic(dir_path, filename, title, nep_ref, content):
    md = f"## {title}\n\n::: {{.callout-warning}}\n#### 📘 Syllabus Coverage\n| Syllabus | Topic Details |\n|----------|---------------|\n| **NEP-2020** | {nep_ref} |\n:::\n\n\n[![](https://via.placeholder.com/600x300?text=Course+Module+Video)](#)\n\n[Get the Presentation &nearr;](#) &nbsp; | &nbsp; [Watch the Video &nearr;](#)\n\n::: {{.callout-note}}\n### Key Concepts\n{content}\n:::\n"
    filepath = os.path.join(dir_path, filename)
    with open(filepath, 'w', encoding='utf-8') as f:
        f.write(md)

def write_ugc_topic(dir_path, filename, title, ugc_ref, content):
    md = f"## {title}\n\n::: {{.callout-warning}}\n#### 📘 Syllabus Coverage\n| Syllabus | Topic Details |\n|----------|---------------|\n| **UGC NET** | {ugc_ref} |\n:::\n\n\n[![](https://via.placeholder.com/600x300?text=Course+Module+Video)](#)\n\n[Get the Presentation &nearr;](#) &nbsp; | &nbsp; [Watch the Video &nearr;](#)\n\n::: {{.callout-note}}\n### Key Concepts\n{content}\n:::\n"
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
        clean_directory(dir_path)
        write_syllabus(dir_path, subject['nep_syllabus'], subject['ugc_syllabus'])
        for f, t, n, u, c in subject['common']: write_common_topic(dir_path, f, t, n, u, c)
        for f, t, n, c in subject['nep_only']: write_nep_topic(dir_path, f, t, n, c)
        for f, t, u, c in subject['ugc_only']: write_ugc_topic(dir_path, f, t, u, c)
        write_qmd_index(qmd_path, subject)
        print(f"Processed: {subject['title']}")
