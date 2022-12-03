import requests
from bs4 import BeautifulSoup
import sys

# Define the URL of the website to scrape
url = "https://www.foxsports.com/" + sys.argv[1] + "/" + sys.argv[2] + "-player-stats"

# Make a GET request to the website
response = requests.get(url)

# Parse the HTML response using BeautifulSoup
soup = BeautifulSoup(response.text, "html.parser")

# Output the parsed HTML to a file
with open("./data/res.html", "w") as f:
    f.write(str(soup))