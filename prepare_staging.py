import os
import shutil

allowed_dirs = [
    'agricultural-geography',
    'attachments',
    'cartography',
    'climatology',
    'economic-geography',
    'environmental',
    'field-survey',
    'geographical-thought',
    'geography-of-india',
    'geography-of-odisha',
    'geomorphology',
    'geospatial-ai',
    'gis-remote-sensing',
    'human-development',
    'human-geography',
    'indian-physical',
    'indian-socioeco',
    'industrial-geography',
    'natural-hazards',
    'natural-resource-management',
    'oceanography',
    'physical-geography',
    'political-geography',
    'population-geography',
    'regional-development',
    'research-methodology',
    'settlement-geography',
    'social-and-cultural',
    'statistical-geography',
    'trade-and-transport',
    'urban-geography'
]

staging_dir = '_vault_staging'
if os.path.exists(staging_dir):
    shutil.rmtree(staging_dir)
os.makedirs(staging_dir)

for d in allowed_dirs:
    src = d
    dst = os.path.join(staging_dir, d)
    if os.path.exists(src):
        shutil.copytree(src, dst)
