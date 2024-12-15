# olfactometer_app

## Description

This Shiny app is designed to record and analyze insect behavior in olfactometer bioassays. It supports four-arm, two-arm, and six-arm olfactometers. The app allows users to start, stop, reset, and add new trials, as well as download data for further analysis.

## Installation

To run this app locally, you need to have R and RStudio installed on your computer. You also need to install the required packages:

```R
install.packages(c("shiny", "dplyr", "lubridate", "shinyjs"))
```

Clone the repository and set the working directory to the location of the app:

```sh
git clone https://github.com/Dr-Joe-Roberts/olfactometer_app.git
cd olfactometer_app
```

Run the app:

```R
shiny::runApp()
```

## Usage

### Four-Arm Olfactometer

1. Enter the trial number and the odour source for each arm.
2. Click the "Start Recording" button to begin the trial.
3. Press the corresponding key (1, 2, 3, 4, or 5) on your keyboard to indicate that the insect has moved to a different arm.
4. Click the "Stop Recording" button to stop the trial.
5. Click the "Add New Trial" button to save the current trial data and start a new trial.
6. Click the "Reset Trial" button to clear the current trial data without saving.
7. Click the "Download Data as CSV" button to download the summarised data from all trials as a CSV file.

### Two-Arm Olfactometer

1. Enter the trial number, the number of individuals released, and the odour source for each arm.
2. Click the "Start Recording" button to begin the trial.
3. Press the corresponding key (1 or 2) on your keyboard to indicate that the insect has moved to a different arm.
4. Click the "Stop Recording" button to stop the trial.
5. Click the "Add New Trial" button to save the current trial data and start a new trial.
6. Click the "Reset Trial" button to clear the current trial data without saving.
7. Click the "Download Data as CSV" button to download the summarised data from all trials as a CSV file.

### Six-Arm Olfactometer

1. Enter the trial number, the number of individuals released, and the odour source for each arm.
2. Click the "Start Recording" button to begin the trial.
3. Press the corresponding key (1, 2, 3, 4, 5, or 6) on your keyboard to indicate that the insect has moved to a different arm.
4. Click the "Stop Recording" button to stop the trial.
5. Click the "Add New Trial" button to save the current trial data and start a new trial.
6. Click the "Reset Trial" button to clear the current trial data without saving.
7. Click the "Download Data as CSV" button to download the summarised data from all trials as a CSV file.

### Treatment Randomisation

1. Enter the treatments for each arm in the "Treatment Randomisation" tab.
2. Click the "Randomise Treatments" button to randomise the positions of the treatments.
3. The randomised positions will be automatically updated in the data collection tabs for the corresponding olfactometer type.

## Best Practices for Conducting Insect Olfactometer Bioassays

1. **Olfactometer Design**: Ensure the design suits the research question and species; use chemically inert materials; pilot test configurations.
2. **Environment**: Report and control temperature and humidity; use consistent lighting; minimise external stimuli.
3. **Stimuli Presentation**: Avoid mechanical damage to biological materials; consider solvent properties for synthetic chemicals.
4. **Cleaning Procedures**: Thoroughly clean between replicates; use fragrance-free detergents; appropriately clean or replace activated charcoal filters.
5. **Physiological State**: Report and standardise the physiological state; account for time-of-day effects; use blocking in design and analysis.
6. **Releasing Organisms**: Decide between individual or group releases based on interaction and social behaviours; treat each group release as a single replicate.

