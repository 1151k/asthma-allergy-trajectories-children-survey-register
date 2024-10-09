##### R scripts used in 
##### Lisik, D., Wennergren, G., Kankaanranta, H., Basna, R., Shah, S. A., Alm, B., Celind, F. S., Goksör, E., & Nwaru, B. I. (2024). Asthma and allergy trajectories in children based on combined parental report and register data. *Pediatric allergy and immunology : official publication of the European Society of Pediatric Allergy and Immunology*, 35(10), e14254. https://doi.org/10.1111/pai.14254.

<hr>

#### Description of R scripts
- **ATC_CODES.r**: define Anatomical Therapeutic Chemical (ATC) codes used throughout the other R scripts
- **define-survey-data.r**: select variables of interest from the postal surveys and the National Medical Birth Register (NMBR), and create composite variables from these
- **define-medication-data-long.r**: select/create variables of interest from the National Prescribed Drug Register (NPDR), and output in long format for descriptive statistics
- **define-medication-data-wide.r**: convert the medication (NPDR) data from above in long format to a wide format for latent class analysis and characterization
- **combined-survey-and-medication-data-raw.r**: combine medication (NPDR) data with postal survey/NMBR data for pooling of/merging with imputed datasets
- **assess-survey-data.r**: descriptive statistics of postal survey/NMBR data
- **assess-medication-data.r**: descriptive statistics of medication (NPDR) data
- **assess-missingness.r**: assess and describe missingness in data
- **imputation.r**: assess correlation and perform and assess multiple imputation
- **pool-imputations.r**: pool imputations of background characteristics (i.e., not trajectory analysis results) and tabulate non-imputed/imputed data
- **trajectory-modelling.r**: perform trajectory analysis
- **assess-lca.r**: compile statistical metrics to assess models from statistical point of view
- **assess-trajectories.r**: plot trajectories to assess models from clinical point of view
- **pool-trajectories.r**: pool trajectory results (indicator probabilities, class labels...)

<br>

#### Data availability
Due to regulations and contractual agreement with study participants, the underlying data are not available.

<br>

#### Contact
For any inquiries, please contact [Daniil Lisik](https://www.gu.se/en/about/find-staff/daniillisik) ([daniil.lisik@gmail.com](mailto:daniil.lisik@gmail.com)).

<br>
