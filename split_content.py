import os
import glob
import re

os.makedirs('_files', exist_ok=True)

skip_files = ['index.qmd', 'ugc-net-pyq.qmd', 'about.qmd']
qmd_files = glob.glob('*.qmd')

for file in qmd_files:
    if file in skip_files:
        continue
        
    with open(file, 'r', encoding='utf-8') as f:
        content = f.read()
        
    # Find the boundary between the syllabus and the rest of the course content.
    # The course content starts with the first "## " that is NOT "## UGC NET Syllabus"
    
    lines = content.split('\n')
    split_idx = -1
    for i, line in enumerate(lines):
        if line.startswith('## ') and 'UGC NET Syllabus' not in line:
            split_idx = i
            break
            
    if split_idx != -1:
        # Check if we already have an include statement to avoid double processing
        if '{{< include _files/' in "\n".join(lines[split_idx:]):
            continue
            
        header_part = '\n'.join(lines[:split_idx])
        content_part = '\n'.join(lines[split_idx:])
        
        md_filename = file.replace('.qmd', '.md')
        md_filepath = os.path.join('_files', md_filename)
        
        # Write the content part to the new .md file
        with open(md_filepath, 'w', encoding='utf-8') as f:
            f.write(content_part)
            
        # Write the header part + include directive back to the .qmd file
        new_qmd_content = header_part + f'\n\n{{{{< include _files/{md_filename} >}}}}\n'
        
        with open(file, 'w', encoding='utf-8') as f:
            f.write(new_qmd_content)
            
        print(f'Extracted content from {file} to {md_filepath}')
