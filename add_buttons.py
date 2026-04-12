import os
import glob
import re

button_html = '''
<a href="#" class="btn btn-primary btn-sm" role="button" target="_blank" style="margin-right: 10px;"><i class="bi bi-file-slides"></i> Get Presentation</a>
<a href="#" class="btn btn-danger btn-sm" role="button" target="_blank"><i class="bi bi-youtube"></i> Watch Video</a>
<br><br>
'''

skip_files = ['index.qmd', 'ugc-net-pyq.qmd', 'climatology.qmd', 'geomorphology.qmd', 'about.qmd']
qmd_files = glob.glob('*.qmd')

for file in qmd_files:
    if file in skip_files:
        continue
        
    with open(file, 'r', encoding='utf-8') as f:
        content = f.read()
        
    def replacer(match):
        heading = match.group(1)
        name = heading.strip()
        if name in ['Course Content', 'UGC NET Syllabus Alignment', 'Overview']:
            return match.group(0) # don't modify
        return f'## {name}\n\n{button_html.strip()}\n'
        
    new_content = re.sub(r'^##\s+(.*)$', replacer, content, flags=re.MULTILINE)
    
    if new_content != content:
        with open(file, 'w', encoding='utf-8') as f:
            f.write(new_content)
        print(f'Updated {file}')
