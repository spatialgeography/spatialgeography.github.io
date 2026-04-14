import datetime
import re
import os

def update_quarto_yml():
    file_path = '_quarto.yml'
    if not os.path.exists(file_path):
        return

    with open(file_path, 'r', encoding='utf-8') as f:
        content = f.read()

    current_date = datetime.datetime.now().strftime("%B %d, %Y")
    new_content = re.sub(r'Last Updated: .*', f'Last Updated: {current_date}', content)

    with open(file_path, 'w', encoding='utf-8') as f:
        f.write(new_content)
    print(f"Updated footer date to: {current_date}")

if __name__ == "__main__":
    update_quarto_yml()
