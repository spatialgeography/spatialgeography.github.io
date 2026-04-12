import os
import re

def main():
    root = r"e:\spatialgeography\geography"
    
    # We are looking for directories that contain .md files starting with _
    # Excluding .git, .quarto, _site, etc.
    exclude_dirs = {'.git', '.quarto', '_site', '.obsidian', 'site_libs'}
    
    mapping = {} # Store { (dir, old_name): new_name }
    
    for item in os.listdir(root):
        dir_path = os.path.join(root, item)
        if os.path.isdir(dir_path) and item not in exclude_dirs:
            for filename in os.listdir(dir_path):
                if filename.endswith(".md") and filename.startswith("_"):
                    old_path = os.path.join(dir_path, filename)
                    new_filename = filename[1:] # Remove leading _
                    new_path = os.path.join(dir_path, new_filename)
                    
                    print(f"Renaming {item}/{filename} -> {item}/{new_filename}")
                    try:
                        os.rename(old_path, new_path)
                        # Store the mapping relative to the root for path replacement
                        # Format in .qmd is usually "dir/filename.md"
                        old_rel = f"{item}/{filename}"
                        new_rel = f"{item}/{new_filename}"
                        mapping[old_rel] = new_rel
                    except Exception as e:
                        print(f"Error renaming {filename}: {e}")

    # Now update paths in .qmd files
    for item in os.listdir(root):
        if item.endswith(".qmd"):
            filepath = os.path.join(root, item)
            with open(filepath, 'r', encoding='utf-8') as f:
                content = f.read()
            
            updated_content = content
            for old, new in mapping.items():
                # Replace the full path in include tags
                # Use escaped slashes just in case, or literal strings
                updated_content = updated_content.replace(old, new)
            
            if updated_content != content:
                print(f"Updating references in {item}")
                with open(filepath, 'w', encoding='utf-8') as f:
                    f.write(updated_content)

if __name__ == "__main__":
    main()
