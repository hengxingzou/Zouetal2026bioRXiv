**Data and code for: Large language models unlock the ecology of species interactions (https://doi.org/10.64898/2026.02.06.704115)**

1. Code

- `Process_eBird_Predictions.R` include all code for Case study 1: Bird-bird interactions.
- `Process_GBIF_Predictions.R` include all code for Case study 2: Plant-pollinator interactions.

2. Data

Under `Data`, the directory `eBird` contains the following files:

- `Training_Cleaned` contains all comments that have been manually categorized according to a protocol developed by Zarnetske et al. (https://github.com/AvianMetaNetwork/AvianMetaNetwork).
- `Original_Comments` contains original eBird data used in the case study with all fields.
- `Total_Labeled.csv` is a compiled version of all manually categorized comments.
- `Compare_Checked.csv` contains all comments with LLM predictions and manually checked results.
- `Output.csv` contains the original output from the LLM.

The directory `Plant-Pollinator` contains the following files:

- `Predictions_Sep15.csv` contains predictions from the LLM.
- `plant_poll_1000_output.csv` contains all interactions that have been manually corrected.
- `Vanessa_annabella.csv` contains all interactions of the butterfly _Vanessa annabella_ from the Global Biotic Interactions database (GloBi; https://www.globalbioticinteractions.org/).
- `Not_In_Globi.csv` contains all interactions identified by the LLM from the GBIF dataset but were not in the GloBi dataset.
- `Not_In_Globi_Matched.csv` takes `Not_In_Globi.csv` and manually searched if some of the absences were due to different species under the same genus.
