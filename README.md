# Air Quality Monitoring & Forecasting System (Occitanie)

**Operational forecasting tool leveraging SARIMA-GARCH models and an RShiny interface.**

## Project Overview

This project aims to monitor and forecast air quality in the Occitanie region (France). It focuses on three major pollutants: **Ozone ($O_3$)**, **Nitrogen Dioxide ($NO_2$)**, and **Fine Particles ($PM_{2.5}$)** across three station typologies (Urban, Peri-urban, and Rural).

The system uses a hybrid **SARIMA-GARCH** approach to model:
1.  **Linear Dynamics & Seasonality:** Handled by SARIMA.
2.  **Volatility & Uncertainty:** Handled by GARCH, providing dynamic confidence intervals (80% and 95%) for risk assessment.

The final output is an interactive **RShiny Dashboard** that translates these statistical forecasts into operational alerts.

---

## Quick Start: Running the Dashboard

**You do not need to download raw data to run the application.**

The repository comes with pre-computed forecast files (`data_shiny_*.csv`). These files contain the predictions generated from our latest model run (up to **January 6, 2026**).

To launch the dashboard:
1. Open `app.R` in RStudio.
2. Click **"Run App"**.
3. The interface will display the forecasts, map, and alerts based on the pre-saved data.

---

## Reproducing Results & Retraining Models

If you wish to explore the exploratory data analysis (EDA), re-train the models, or update the forecasts with **new data**, you must download the raw dataset.

### 1. Data Source
The data is sourced from **Atmo Occitanie** (Open Data).
* **Source URL:** [Picto-Occitanie Catalogue](https://catalogue.picto-occitanie.fr/geonetwork/srv/fre/catalog.search#/metadata/49d986b4-c998-4e7a-8431-0989ad9d99e0)
* **Dataset to choose:** Select the **"Données horaires sur 1 an glissant"** (Hourly data, rolling 1-year).

### 2. Setup Instructions
1.  Download the CSV file from the link above.
2.  Save the file in the root directory of this project.
3.  **Renaming:** Ensure the filename matches the one called in the scripts (e.g., `mesure_horaire_....csv`) or update the `read.csv()` line in the modeling scripts.

### 3. Adaptability
The scripts are designed to be **dynamic**.
* The current predictions included in the repo stop at **January 6, 2026**.
* However, if you download a fresher dataset today, the code will automatically detect the new date range, re-train the models, and generate updated forecasts for the next 48 hours relative to your new data.

---

## Repository Structure

* `app.R`: The RShiny user interface code.
* `data_shiny_*.csv`: Pre-calculated predictions used by the app (lightweight).
* `Script_EDA.R`: Exploratory Data Analysis. **(Requires raw data)**
* `Script_Models.R`: SARIMA-GARCH fitting and Rolling Forecast generation. **(Requires raw data)**
* `README.md`: Project documentation.

## Requirements

Make sure you have the following R libraries installed:

```r
install.packages(c(
  "shiny", "leaflet", "dplyr", "DT", "bslib", "lubridate", "jsonlite", 
  "forecast", "rugarch", "tseries", "ggplot2", 
  "Metrics", "zoo", "future.apply", "progressr", "htmltools"
))
