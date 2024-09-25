# MICROcosm: a protocol for a global microcosm experiment

<img src="http://img.shields.io/static/v1?label=STATUS&amp;message=EM%20DESENVOLVIMENTO&amp;color=GREEN&amp;style=for-the-badge"/> <img src="http://img.shields.io/static/v1?label=STATUS&amp;message=IN%20DEVELOPMENT&amp;color=GREEN&amp;style=for-the-badge"/>

Here we have the codes and data from the experiment coordinated by Dr. Gustavo Romero (UNICAMP, Brazil) and Dr. Martin Gossner (Swiss Federal Institute for Forest, Switzerland). We would like to inform you that the data and codes used in our project are currently being prepared for publication. **Until this occurs, they will not be available for external use**. We appreciate your understanding and patience as we work to make these resources available. Once they are ready, we will make an official announcement and share the details for access.

## Motivation

Climate change and land-use alterations are leading to biodiversity loss, with consequences for the functioning of forest ecosystems. The direction and magnitude of these impacts can be assessed through artificial microcosms installed in natural and managed forests distributed along a latitudinal gradient. These systems simulate natural microhabitats and host a diversity of aquatic organisms involved in the decomposition of organic matter. In this context, this project developed a global protocol for experimental microcosms to systematically generate data on anthropogenic impacts on biodiversity (taxonomic and functional) and the functioning (i.e., decomposition) of forest ecosystems.

## How to use

### How are the data organized?

The experiment was conducted on a global scale, and raw data were submitted by various researchers. Each dataset was processed using the `00_preprocessing_data.R` script, where a unique 'ID' key was assigned to each experiment, enabling comparisons between them. During the processing, we identified the need to reclassify traits for the morphospecies found, and were incorporated back into the dataset in `02_trait_revised_join.R` script. After this step, the data were organized into a nested structure, and the resulting `nested_df.RData` file contains all experiments cleaned and ready for analysis.

Each experiment, identified by its unique MD (Microcosm Data) key (**`ID`**), includes several important data columns. These columns collectively provide a comprehensive dataset that supports the analysis of biodiversity and ecosystem functioning within the experimental microcosms. Additionally, some experiments underwent specific treatments, recorded in the**`roof_treatment`** and **`height_treatment`** columns.

-   **Researcher (`researcher`)**: This column identifies the researcher responsible for conducting each experiment, ensuring accountability and facilitating collaboration among scientists.
-   **Locality (`locality`)**: This column specifies the geographic location of each experiment, allowing for spatial analysis of biodiversity patterns and environmental influences.
-   **Roof Treatment (`roof_treatment`)**: Indicates whether the experiment was conducted under a roof that prevents the entry of allocative material (e.g., leaves, debris).
    -   `1`: Roof present, allocative material blocked.
    -   `0`: No roof, allocative material allowed.
-   **Height Treatment (`height_treatment`)**: Indicates the height at which the microcosms were installed, with the following categories:
    -   `1`: 1.5 meters above ground.
    -   `2`: 15 meters above ground.
    -   `3`: Greater than 15 meters above ground.
-   **Abundance (`abundance`)**: This column contains a dataframe with abundance data for each experiment, indicating the number of organisms present within the microcosm.
-   **Traits (`traits_revised`)**: This column holds a dataframe listing the revised traits of the organisms observed in each experiment, providing insights into their functional characteristics.
-   **Measures (`measures`)**: This column includes a dataframe with measurements from cotton strips used in the experiments, along with both abiotic and biotic measurements taken during the study. These measurements are crucial for understanding the environmental conditions and interactions within the microcosms.

### What are the prerequisites or dependencies for manipulating the data?

To process and analyze the data, R version 4.4.1 is required. A virtual environment was set up using the `.renv` package to ensure that all necessary dependencies are available. To replicate the environment, simply run the following command in R: `renv::restore()`. Make sure the `renv.lock` file has been downloaded
