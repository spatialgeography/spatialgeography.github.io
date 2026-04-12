import os
import glob
import re

button_html_regex = r'<a href="#" class="btn.*?</br>\n*'

media_html = '''
[![](https://via.placeholder.com/600x300?text=Course+Module+Video)](#)

[Get the Presentation &nearr;](#) &nbsp; | &nbsp; [Watch the Video &nearr;](#)

'''

skip_files = ['index.qmd', 'ugc-net-pyq.qmd', 'about.qmd']
qmd_files = glob.glob('*.qmd')

for file in qmd_files:
    if file in skip_files:
        continue
        
    with open(file, 'r', encoding='utf-8') as f:
        content = f.read()
        
    # First, let's clean up any previously injected bootstrap buttons
    content = re.sub(r'<a href="#" class="btn btn-primary btn-sm".*?<br><br>\n?', '', content, flags=re.DOTALL)
        
    # Now inject the new format below each relevant heading
    # To do this correctly without duplicating, we should check if the new format is already there.
    def replacer(match):
        heading = match.group(0)
        name = match.group(1).strip()
        if name in ['Course Content', 'UGC NET Syllabus Alignment', 'Overview', 'Conclusion & Next Steps', 'Conclusion']:
            return heading # don't modify these specific structural headings
            
        # The new format goes *after* the introductory paragraph if we can figure it out, 
        # or simply right below the heading. The image from the user shows the text FIRST, 
        # then the video, then the links!
        # So we want to find: "## Heading \n\n Some text... \n\n ::: {.callout... " 
        # and inject before the callout or at the end.
        # But doing this safely with regex is hard. Let's just put it right below the heading for now, 
        # or immediately before a callout.
        return heading
        
    # Actually, let's just insert it before the first "::: {" block that occurs after a heading.
    # We can split by headings
    sections = re.split(r'(^##\s+.*?$)', content, flags=re.MULTILINE)
    new_sections = []
    
    for i, section in enumerate(sections):
        if section.startswith('## '):
            new_sections.append(section)
            continue
            
        # This is the content block following a heading
        # If the previous block was a valid heading that we want to inject into:
        if i > 0 and sections[i-1].startswith('## '):
            heading_name = sections[i-1].replace('## ', '').strip()
            if heading_name not in ['Course Content', 'UGC NET Syllabus Alignment', 'Overview', 'Conclusion & Next Steps', 'Conclusion']:
                # Ensure we don't duplicate
                if 'Get the Presentation &nearr;' not in section and 'Watch the Video &nearr;' not in section:
                    # Let's insert the media block right before the first ":::" block if it exists,
                    # or at the end of the paragraph if there's no callout.
                    if ':::' in section:
                        section = section.replace(':::', media_html + ':::', 1)
                    else:
                        section = section + '\n' + media_html
        new_sections.append(section)
        
    new_content = "".join(new_sections)
    
    if new_content != content:
        with open(file, 'w', encoding='utf-8') as f:
            f.write(new_content)
        print(f'Updated {file}')
