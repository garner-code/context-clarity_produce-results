## Produce results for 'Contextual clarity impacts learning transfer but spares task switching'

K. Garner (2025).


This repo is designed to allow computational reproducibility of the results reported in the paper 'Contextual clarity impacts learning transfer but spares task switching'. 

The code used to run the analysis and produce the figures can be followed [here](https://garner-code.github.io/context-clarity_produce-results/). 

To run the analysis script locally, you'll need the raw data, which can be found here, and should be preprocessed using [this R script](https://github.com/lydiabarnes01/doors/blob/main/src/run_wrangling.R) and associated functions from [this release](https://github.com/lydiabarnes01/doors/releases/tag/preprint) of [this repository](https://github.com/lydiabarnes01/doors/tree/main).

If you want to skip the preprocessing and get the data in the format required to run the analysis, you can visit the [OSF here]LINK COMING SOON. If you want to run [the analysis script from this repo](https://github.com/garner-code/context-clarity_produce-results/blob/main/context_learn-trans_task-switch_produce-results.qmd), then you will need the following folder structure:  

top_folder/  
│   ├── _quarto.yml  
│   ├── *.Rproj  
│   ├── this-qmd-doc.qmd  
│   ├── R/  
│   │   └── all-r-scripts.R  
│   ├── data-wrangled/  
│   │   └── exp_[exp_str]_evt.csv  
│   │   └── exp_[exp_str]_avg.csv  
│   │   └── exp_lt_maggi-k4.csv  
│   ├── figs/  
│   ├── res/    

If you have any issues or questions, please submit an issue to this repository.  
