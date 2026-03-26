# Purpose

This dashboard uses data downloaded from ESSENCE to track daily incidence of selected syndromes (e.g., specific diseases, broad disease categories, symptoms, or other health-related queries) in the Kansas City metro region. SaTScan<sup>TM</sup> software is then used to detect clusters of any syndromes that may occur.

# App controls / user interface

This dashboard is interactive. The user can make certain selections to update the data that's displayed.

## Sidebar controls

Syndrome: This is the list of syndrome queries for which data is available on a given analysis date.

Analysis date: This list contains all dates on which analyses were run. Changing this selection may update the syndrome list, as the list of syndromes chosen for analysis may change over time.

Date range: This list allows the user to change the time range for data shown in the time series plots and data characteristics tables.

P-value checkbox: When checked, this control filters cluster data so that only clusters with a p-value below 0.05 are shown.

# ESSENCE data

- Records of emergency room visits
- Missouri hospitals report within three business days
- Out of state hospitals report slower
- Data can be downloaded from the ESSENCE system based on either the location of the patient or the location of the hospital in the record.
    + ER visits by patient location: This dataset consists of records in which the patient's residential ZIP code is at least partly within Cass, Clay, Jackson, or Platte County. It includes ER visits to hospitals anywhere in the country by residents of these local ZIP codes. However, most of the visits occur at local hospitals. This is the dataset that most closely represents the stable population over time. (Assignment of ZIP code if a residential ZIP code isn't available?)
    + ER visits by hospital location: This dataset consists of records in which the hospital where the patient was seen is located in Cass, Clay, Jackson, or Platte County. It includes ER visits by local residents and non-residents. This dataset should be responsive to changes in the non-resident population, e.g., due to an increase of visitors during the World Cup event series.
- Counts of ER visits do not represent diagnoses for any condition (do they?). They represent...
- Data may contain duplicates. An effort is made to deduplicate, but there is a potential duplicate error rate (table).

# SaTScan<sup>TM</sup> analysis

SaTScan<sup>TM</sup> is a trademark of Martin Kulldorff. The SaTScan<sup>TM</sup> software was developed under the joint auspices of (i) Martin Kulldorff, (ii) the National Cancer Institute, and (iii) Farzad Mostashari of the New York City Department of Health and Mental Hygiene.

- Describe software
- Describe analysis used
- Provide parameters, start/end dates, etc.

# References/resources

Levin-Rector A, Kulldorff M, Peterson E, Hostovich S, Greene S. Prospective Spatiotemporal Cluster Detection Using SaTScan: Tutorial for Designing and Fine-Tuning a System to Detect Reportable Communicable Disease Outbreaks. JMIR Public Health Surveill. 2024;10:e50653.
URL: https://publichealth.jmir.org/2024/1/e50653
DOI: [10.2196/50653](https://doi.org/10.2196/50653)

Greene SK, Peterson ER, Kapell D, Fine AD, Kulldorff M. Daily Reportable Disease Spatiotemporal Cluster Detection, New York City, New York, USA, 2014-2015. Emerg Infect Dis. 2016 Oct;22(10):1808-12. doi: [10.3201/eid2210.160097](https://doi.org/10.3201/eid2210.160097). PMID: [27648777](https://pubmed.ncbi.nlm.nih.gov/27648777/); PMCID: PMC5038417.

Kulldorff M, Heffernan R, Hartman J, Assunção RM, Mostashari F. A space-time permutation scan statistic for the early detection of disease outbreaks. PLoS Medicine, 2:216-224, 2005. [online](https://www.researchgate.net/publication/8014395_A_Space-Time_Permutation_Scan_Statistic_for_Disease_Outbreak_Detection)
