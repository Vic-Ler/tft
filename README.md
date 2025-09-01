# 🎮 Personal TFT Dashboard

As a fan of **TFT (Teamfight Tactics)**, I wanted to explore Riot's API data and create a **personal dashboard** in R as a hobby project.  
This dashboard was built purely out of curiosity and is **not meant to compete** with professional dashboards like [tactics.tools](https://tactics.tools/player/euw/). 
It's more of a personal playground for experimenting with data visualization and API integration.

---

## 📁 Repository Structure

- **`get_data`**  
  Script to fetch data from Riot's API using a personal API key.  
  - Retrieves the **Player PUUID**, which is then used to fetch summoner, account, and match data.  
  - Due to API rate limits, only the **most recent matches** are fetched.  

- **`report_script`**  
  Generates the dashboard layout and visualizations.  
  - Uses **reactable**, **plotly**, and **bslib** for interactive tables and charts.  
  - Focused on creating a **clean and visually appealing dashboard**.  

- **`styles.scss`**  
  Adds custom styles to enhance the dashboard’s **colors, shapes, and overall appearance**.  

---

## 🛠️ Technologies Used

- **R**  
- **Riot API**  
- **Data/Icons Sources:**  
  - [ddragon.leagueoflegends.com](https://ddragon.leagueoflegends.com)  
  - [raw.communitydragon.org](https://raw.communitydragon.org)  
  - [MetaTFT CDN](https://cdn.metatft.com/file/metatft)  

## ⚠️ Limits & Notes

- The dashboard **works best on desktop** and is **not fully optimized for mobile layouts**.  
- The data fetched is **correct**, but some statistics (like win rates) can be **less meaningful** if only a small number of matches are available.
- **Lib folder is needed** to display the dashboard correctly. It is usually generated automatically upon rendering the HTML file. 
