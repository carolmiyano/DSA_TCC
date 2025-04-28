from bs4 import BeautifulSoup
import os
import csv
import re

# Function to extract data from each HTML file
def extract_data_from_html(html_file):
    with open(html_file, 'r', encoding='utf-8') as file:
        soup = BeautifulSoup(file, 'html.parser')
        
        # Find the table containing the data
        table = soup.find('table', {'width': '90%', 'border': '1'})
        
        # Initialize variables to store extracted data
        classifications = []
        agencies = []
        company_name = None
        company_type = None  # New field for company type
        
        # Extract company name
        company_name_header = soup.find('td', {'colspan': '2'})
        if company_name_header:
            raw_company_name = company_name_header.text.strip()
            match = re.search(r'\b[A-Za-zÀ-ÿ]+.*', raw_company_name)  # Improved regex
            company_name = match.group(0).strip() if match else raw_company_name

        if not company_name:
            company_name = '-'
        
        # Extract company type (Reinsurer or Insurance Company in Portuguese)
        company_type_header = soup.find('td', string=re.compile(r'Informações de Resseguradoras|Informações de Seguradoras', re.IGNORECASE))
        if company_type_header:
            if "Resseguradoras" in company_type_header.text:
                company_type = "Resseguradora"
            elif "Seguradoras" in company_type_header.text:
                company_type = "Seguradora"
        
        if not company_type:
            company_type = '-'
        
        # Extract data from table rows
        if table:
            rows = table.find_all('tr')
            for row in rows[1:]:  # Skip the header row
                cols = row.find_all('td')
                if len(cols) == 2:
                    classifications.append(cols[0].text.strip())
                    agencies.append(cols[1].text.strip())
        
        if not classifications:
            classifications = ['-']
        if not agencies:
            agencies = ['-']
        
        return company_name, company_type, classifications, agencies

# Function to process all HTML files in the directory
def process_html_files_in_directory():
    current_directory = os.path.dirname(os.path.abspath(__file__))
    txt_directory = os.path.join(current_directory, 'txt')
    csv_directory = current_directory
    
    os.makedirs(txt_directory, exist_ok=True)
    
    html_files = [os.path.join(txt_directory, filename) for filename in os.listdir(txt_directory) if filename.endswith('.txt')]
    
    csv_file = os.path.join(csv_directory, 'extracted_data.csv')
    with open(csv_file, 'w', newline='', encoding='utf-8') as csvfile:
        writer = csv.writer(csvfile)
        writer.writerow(['Company Code', 'Company Name', 'Company Type', 'Classificação de Risco', 'Agência Classificadora'])
        
        for html_file in html_files:
            company_code_match = re.search(r'_(\d+)\.txt$', os.path.basename(html_file))
            company_code = company_code_match.group(1) if company_code_match else '-'
            company_name, company_type, classifications, agencies = extract_data_from_html(html_file)
            
            for classification, agency in zip(classifications, agencies):
                writer.writerow([company_code, company_name, company_type, classification, agency])
    
    print(f"Data extracted from {len(html_files)} files and saved to {csv_file}")

if __name__ == "__main__":
    process_html_files_in_directory()
