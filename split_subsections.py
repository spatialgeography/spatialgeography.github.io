import os
import glob
import re

qmd_files = glob.glob('*.qmd')
skip_files = ['index.qmd', 'ugc-net-pyq.qmd', 'about.qmd']

def slugify(text):
    text = text.lower()
    text = re.sub(r'[^a-z0-9]+', '-', text).strip('-')
    return text

for file in qmd_files:
    if file in skip_files:
        continue
        
    basename = file.replace('.qmd', '')
    folder_name = f'{basename}_files'
    content_file = os.path.join(folder_name, '_content.md')
    
    if not os.path.exists(content_file):
        continue
        
    with open(content_file, 'r', encoding='utf-8') as f:
        content = f.read()
        
    sections = re.split(r'(^##\s+.*?$)', content, flags=re.MULTILINE)
    
    includes = []
    
    i = 1 # Sections start at 1 because 0 is pre-first-header (empty)
    while i < len(sections):
        heading_line = sections[i]
        heading_text = heading_line.replace('##', '').strip()
        slug = slugify(heading_text)
        
        body = ""
        if i + 1 < len(sections):
            body = sections[i+1]
            
        full_section_content = heading_line + "\n" + body
        full_section_content = full_section_content.strip() + "\n\n"
        
        target_md = f"_{slug}.md"
        target_path = os.path.join(folder_name, target_md)
        
        with open(target_path, 'w', encoding='utf-8') as sf:
            sf.write(full_section_content)
            
        includes.append(f"{{{{< include {folder_name}/{target_md} >}}}}")
        i += 2
        
    with open(file, 'r', encoding='utf-8') as f:
        qmd_content = f.read()
        
    old_include = f'{{{{< include {folder_name}/_content.md >}}}}'
    new_includes = "\n\n".join(includes)
    
    if old_include in qmd_content:
        new_qmd_content = qmd_content.replace(old_include, new_includes)
        with open(file, 'w', encoding='utf-8') as f:
            f.write(new_qmd_content)
            
    os.remove(content_file)
    print(f'Successfully split content sections for {file}')
