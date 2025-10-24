# Cross-Organizational Process Mining on German State Parliament Data
In this repository we provide all implementations and data collected and generated to perform cross-organizational process mining on data from German state parliaments - specifically, from their documentation services (Parlamentsdokumentation). Note, that some of the documentation services provide new raw data on a daily basis - the code in this repository can then be used to generate new event logs from the new data. If you use our code or data for academic purposes please cite our paper:
```
@misc{hillmann2025crossorganizationalanalysisparliamentaryprocesses,
      title={Cross-Organizational Analysis of Parliamentary Processes: A Case Study}, 
      author={Paul-Julius Hillmann and Stephan A. Fahrenkrog-Petersen and Jan Mendling},
      year={2025},
      eprint={2508.10381},
      archivePrefix={arXiv},
      primaryClass={cs.DB},
      url={https://arxiv.org/abs/2508.10381}, 
}
```
You can access the pre-print of the corresponding research paper here: *https://doi.org/10.48550/arXiv.2508.10381*

Once the article has been officially published the citation will be here:
```
*toDo*: @article{}
```

## If you just want to download the Event Logs
All event logs are in the folder `eventLogs/`. If you cannot unzip the files, this might be because GitHub did not let you download the proper zip file. **To download the proper zip files through GitHub, you have to click on the file and then on raw.**
For further information what exactly the differences are between the original and preprocessed event logs, please check out our paper.

## Requirements
See the requirements.txt file 
We ran our code only with Python 3, so we can not guarantee that it works with Python 2.

## Directory Structre
- `Exploration-Types/` – Code and outputs to explore the different types of processes contained in the data.
- `LegislativeCapacityInGermanysParliaments/` – Code and data from Fortunato, David; Appeldorn, Niels H. (2021) - see Acknowledgments below
- `OriginalData/` – The original data, new data can be obtained from the parliaments websites.
- `eventLogs/` – The generated event logs. **To download the proper zip files through GitHub, you have to click on the file and then on raw.**
- `lawmaking_analysis/` – Code, pre-processed data, and results for the analysis of the lawmaking processes.
- `performancePlots/` – Generated plots as image files.
- `performance-measures.ipynb` – Code to generate results for performance measures.
- `xes-creator-per-folder-all-types.ipynb` – Code to generate one event log in the .xes format per folder including all process types.
- `xes-creator-per-folder-and-type.ipynb` – Code to generate one event log in the .xes format per folder for each process types separately.
- `README.md` – Project overview and instructions.

## Usage/ Reproducing Results
1. Install dependencies
2. Create a folder `all-data-xes` and extract the event logs to it or generate new event logs using `xes-creator-per-folder-all-types.ipynb` (for each parliament you need to change the folderPath and outputFilename variables in the first cell of the code). 
3. Explore the process types contained in the event logs using `Exploration-Types/type-explorer.ipynb`.
4. Create and exlpore performance measure results using `performance-measures.ipynb`. Change the variable `performanceMeasure` to explore the different performance measures.
5. If you want to filter differently than for our analysis, you can use any filtering function or tool (like from PM4Py) to generate differently filtered event logs. If you want to reproduce our analysis the filtered event logs are all in the `lawmaking_analysis` folder.
6. Use `lawmaking_analysis/preprocessing_comparisons.ipynb` for Baden-Württemberg and Brandenburg to make the activities more comparable. 
7. Enrich and explore the data with context information using `lawmaking_analysis/enrich_data_with_context.ipynb` - again you need to change variables in the first cell in order to process the different data sets. If you want to refetch all of the pdf information (it takes a long time since all of the pdf files need to be fetched) you need to remove `df_earlier` in the code. If you are missing any .csv files, make sure you ran the `performance-measures.ipynb` for all necessary performance measures.
8. Use `lawmaking_analysis/my_case_log_builder.ipynb` to build case logs. Make sure you change the input and output filename variables.
9. Use `lawmaking_analysis/difference_explorer.ipynb` to explore basic differrences between the datasets.
10. Use `lawmaking_analysis/outcome_explorer.ipynb` to explore the lawmaking processes outcome distributions (here: delayed vs in time)
11. Use `lawmaking_analysis/hypothesis_inducer.ipynb` and `lawmaking_analysis/hypothesis_tester.ipynb` to induce and test rules. Changing the HIDE variable in the inducer file, you can hide different attributes of the data to ignore it for the rule induction. Note: If you get an error for testing the manually derived rules: You might have to change the order of activities since in the generation process it can be one way or another.
12. Use `lawmaking_analysis/logistic_regression.ipynb` to perform the logistic regressions and check for the coefficients of the data attributes.

## Acknowledgments

Parts of this repository (folder `LegislativeCapacityInGermanysParliaments/`) contain data and code from:

Fortunato, David; Appeldorn, Niels H. (2021). *Replication Data for: Legislative Capacity in Germany's Parliaments*. Harvard Dataverse, V2. https://doi.org/10.7910/DVN/BA8G7H  

This material is made available under the [CC0 1.0 Universal](https://creativecommons.org/publicdomain/zero/1.0/) license.

Part of the code in this repository (in folder `lawmaking_analysis`) is reused from hvoelzer. (2025). hvoelzer/outcomeanalysis: promise (promise). Zenodo. https://doi.org/10.5281/zenodo.15703293 - licensed under **CC BY 4.0**.



