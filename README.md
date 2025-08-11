# Cross-Organizational Process Mining on German State Parliament Data
In this repository we provide all implementations and data collected and generated to perform cross-organizational process mining on data from German state parliaments - specifically, from their documentation services (Parlamentsdokumentation). Note, that some of the documentation services provide new raw data on a daily basis - the code in this repository can then be used to generate new event logs from the new data. If you use our code or data for academic purposes please cite our paper:
```
*toDo*: @article{}
```
You can access the corresponding research paper here: *toDo*

## Requirements
See the requirements.txt file 
We ran our code only with Python 3, so we can not guarantee that it works with Python 2.

## Directory Structre
- `Exploration-Types/` – Code and outputs to explore the different types of processes contained in the data.
- `LegislativeCapacityInGermanysParliaments/` – Code and data from Fortunato, David; Appeldorn, Niels H. (2021) - see Acknowledgments below
- `OriginalData/` – The original data, new data can be obtained from the parliaments websites.
- `eventLogs/` – The generated event logs.
- `lawmaking_analysis/` – Code, pre-processed data, and results for the analysis of the lawmaking processes.
- `performancePlots/` – Generated plots as image files.
- `performance-measures.ipynb` – Code to generate results for performance measures.
- `xes-creator-per-folder-all-types.ipynb` – Code to generate one event log in the .xes format per folder including all process types.
- `xes-creator-per-folder-and-type.ipynb` – Code to generate one event log in the .xes format per folder for each process types separately.
- `README.md` – Project overview and instructions.

## Usage/ Reproducing Results
1. Install dependencies
2. Generate event logs using 

## Acknowledgments

Parts of this repository (folder `LegislativeCapacityInGermanysParliaments/`) contain data and code from:

Fortunato, David; Appeldorn, Niels H. (2021). *Replication Data for: Legislative Capacity in Germany's Parliaments*. Harvard Dataverse, V2. https://doi.org/10.7910/DVN/BA8G7H  

This material is made available under the [CC0 1.0 Universal](https://creativecommons.org/publicdomain/zero/1.0/) license.

Part of the code in this repository (in folder `lawmaking_analysis`) is reused from hvoelzer. (2025). hvoelzer/outcomeanalysis: promise (promise). Zenodo. https://doi.org/10.5281/zenodo.15703293 - licensed under **CC BY 4.0**.



