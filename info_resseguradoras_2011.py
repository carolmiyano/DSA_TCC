import os
import requests

# List of entcodigo values to query
id_list = [
    '04669',
    '40207',
    # '40215',
    # '41416',
    # '41432',
    # '41491',
    # '41629',
    # '42307',
    # '42463',
    # '42684',
    # '42790',
    # '42871',
    # '43290',
    # '43419',
    # '44814',
    # '44920',
    # '45004',
    # '45420',
    # '45896',
    # '45951',
    # '46507',
    # '47406',
    # '47660',
    # '47708',
    # '47724',
    # '47741',
    # '47848',
    # '48178',
    # '48186',
    # '48259',
    # '48275',
    # '48747',
    # '48879',
    # '49000',
    # '49131',
    # '49760',
    # '50075',
    # '50091',
    # '50211',
    # '50806',
    # '50938',
    # '51063',
    # '51438',
    # '51497',
    # '51748',
    # '51934',
    # '52329',
    # '52469',
    # '52540',
    # '52736',
    # '52795',
    # '52850',
    # '52876',
    # '52957',
    # '52965',
    # '53261',
    # '53651',
    # '54038',
    # '54381',
    # '54666',
    # '54810',
    # '54933',
    # '54950',
    # '54984',
    # '55000',
    # '55093',
    # '55425',
    # '55514',
    # '56090',
    # '56120',
    # '56235',
    # '56448',
    # '56502',
    # '56766',
    # '56898',
    # '56928',
    # '57088',
    # '57398',
    # '57401',
    # '57665',
    # '57703',
    # '57720',
    # '57959',
    # '58378',
    # '58661',
    # '58742',
    # '58815',
    # '59137',
]

# URL endpoint
base_url = "https://www2.susep.gov.br/menuatendimento/info_resseguradoras_2011.asp"

# Function to fetch data for a given entcodigo and save to a file
def fetch_and_save_data(entcodigo):
    url = f"{base_url}?entcodigo={entcodigo}&codativo=True"
    response = requests.get(url)
    
    if response.status_code == 200:
        # Constructing filename based on entc'odigo
        filename = f"txt/data_{entcodigo}.txt"
        with open(filename, 'w', encoding='utf-8') as file:
            file.write(response.text)
        print(f"Data for entcodigo {entcodigo} saved to {filename}")
    else:
        print(f"Failed to fetch data for entcodigo {entcodigo}. Status code: {response.status_code}")

# Step to delete all files in the txt folder
def clear_txt_folder():
    current_directory = os.path.dirname(os.path.abspath(__file__))
    txt_directory = os.path.join(current_directory, 'txt')

    os.makedirs(txt_directory, exist_ok=True)

    txt_files = [os.path.join(txt_directory, filename) for filename in os.listdir(txt_directory) if filename.endswith('.txt')]

    for file in txt_files:
        os.remove(file)

# Execution starts here
clear_txt_folder()
# Loop through each entcodigo in the list and fetch data
for entcodigo in id_list:
    fetch_and_save_data(entcodigo)
