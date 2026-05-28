## Purpose

This dashboard uses data downloaded from ESSENCE to track emergency room visits for selected syndromes in the Kansas City metro region. Syndromes include specific diseases, broad disease categories, symptoms, and other health-related queries. Active syndrome clusters are detected using SaTScan.

This tool is intended for use by public health professionals. It may provide advanced warning of developing syndrome clusters. However, the results require interpretation and do not necessarily constitute cause for concern in isolation.

## Data updates

Records are downloaded from ESSENCE and analyzed daily. After a data update, the dashboard will automatically display the most recent data. If the dashboard was active during an update, reloading the page will refresh the data.

## App controls / user interface

The data displayed on the dashboard can be filtered using various controls.

**Analysis date**: Select the date of a data update. The date of the most recent data update is selected by default. If the syndrome list has changed between dates, it will be updated too.

**Syndrome**: Select the syndrome of interest. This list contains all syndromes available for a given analysis date.

**Date range**: Select the date range for time series data.

**Minimum recurrence interval (RI)**: Select a recurrence interval level to filter the clusters and data details displayed.

**Default map zoom level**: Increase or decrease the number to reset the default magnification for maps during the session.

## ESSENCE data

Records downloaded from ESSENCE represent emergency room visits and contain information about the reason for the visit, the location, and certain patient characteristics. Patient names and addresses are not included. Records are accessed via API using queries developed by public health experts. The list of queries used to download data for a given date are available on the Syndromes page.

Hospitals in Missouri are required to provide records to ESSENCE within three business days of a visit. Records from out of state hospitals may take longer to receive.

Data can be downloaded from the ESSENCE system based on either the location of the patient or the location of the hospital in the record. This dashboard displays both types of datasets.

**Data by patient location** consist of records for patients who live in a particular region. Datasets used for cluster analysis include records in which a patient's residential ZIP code is at least partly within Cass, Clay, Jackson, or Platte County. Datasets used for the time series plot include records in which at least 10% of a patient's residential ZIP code is within the Kansas City boundary. Records in these datasets can come from hospitals in any US state. However, most of the visits occur at local hospitals. This is the dataset that most closely reflects the long-term residential population.

(Assignment of ZIP code if a residential ZIP code isn't available?)

**Data by hospital location** consist of records from hospitals within a particular region. The datasets used for cluster analysis and the time series plot include records in which the hospital where the patient was seen is located in Cass, Clay, Jackson, or Platte County. Patients in these datasets may be both residents and non-residents. Therefore, these datasets should be more responsive to changes in the non-resident population, e.g., due to an increase of visitors during the World Cup events.
    
ESSENCE data may contain duplicates. An effort is made to deduplicate the data, but there is a potential duplicate error rate for each syndrome dataset.

## SaTScan analysis

### Methodology

The cluster analyses are performed by SaTScan software using a space-time permutation model for prospective surveillance.[^kul2005] For these analyses, emergency room visits for each syndrome are summarized by date and location.

<div class="md-casefile-table" style="margin-bottom:1rem;">

| Date       | Location   | Count |
|:-----------|:-----------|------:|
| 2026-01-01 | Hospital A | 25    |
| 2026-01-01 | Hospital B | 4     |
| 2026-01-02 | Hospital A | 28    |
| 2026-01-02 | Hospital B | 7     |

</div>

The study period for each analysis is one year, ending with the day prior to the data download. Only clusters that include the final date of the study period, i.e., active clusters, are reported.

A cluster is detected if a region has a higher proportion of visits than all other regions during a particular time period. This can be caused by a relative increase in the visit count in a region, such as if the visit count doubles but the region's population remains the same. It can also be caused when a region's relative visit count doesn't change but the population does increase (relative to all other regions), causing the absolute visit count to increase.[^userguide]

### Interpreting the results

The recurrence interval (RI) is an indicator of the strength of a cluster. From the SaTScan User Guide:

>For prospective analyses, the recurrence interval is shown as an alternative to the p-value. The measure reflects how often a cluster of the observed or larger likelihood will be observed by chance, assuming that analyses are repeated on a regular basis with a periodicity equal to the specified time interval length. For example, if the observed p-value is used as the cut-off for a signal and if the recurrence interval is once in 14 months, than the expected number of false signals in any 14 month period is one.[^userguide]

The dashboard uses the following guidance from the Bureau of Communicable Disease at the New York City Department of Health and Mental Hygiene to define RI levels.

>Consider an RI of 100 to < 365 days as a weak cluster, an RI of 365 days to < 5 years as a moderate cluster, an RI of 5 to < 100 years as a strong cluster, and an RI of ≥ 100 years as a very strong cluster.[^lev2024]

The results of cluster analyses are meant to be considered in combination with other data sources, e.g., confirmed case counts, to determine the risk to the public of a syndrome cluster.

>[RI levels] should be considered alongside other factors to establish whether a cluster is of public health importance. While we have suggested rules of thumb for RI interpretation, investigators should holistically interpret other cluster characteristics and apply epidemiological judgment considering the disease severity, relative risk, location, period, and patient characteristics in the line list [...].[^lev2024]

SaTScan<sup>TM</sup> is a trademark of Martin Kulldorff. The SaTScan<sup>TM</sup> software was developed under the joint auspices of (i) Martin Kulldorff, (ii) the National Cancer Institute, and (iii) Farzad Mostashari of the New York City Department of Health and Mental Hygiene.

## References/resources

[^lev2024]: Levin-Rector A, Kulldorff M, Peterson E, Hostovich S, Greene S. Prospective Spatiotemporal Cluster Detection Using SaTScan: Tutorial for Designing and Fine-Tuning a System to Detect Reportable Communicable Disease Outbreaks. JMIR Public Health Surveill. 2024;10:e50653.
URL: https://publichealth.jmir.org/2024/1/e50653
DOI: [10.2196/50653](https://doi.org/10.2196/50653)

[^userguide]: Kulldorff M. SaTScan<sup>TM</sup> user guide for version 10.1. SaTScan<sup>TM</sup>. 2022. URL: https://www.satscan.org/cgi-bin/satscan/register.pl/SaTScan_Users_Guide.pdf?todo=process_userguide_download

[^kul2005]: Kulldorff M, Heffernan R, Hartman J, Assunção RM, Mostashari F. A space-time permutation scan statistic for the early detection of disease outbreaks. PLoS Medicine, 2:216-224, 2005. [online](https://www.researchgate.net/publication/8014395_A_Space-Time_Permutation_Scan_Statistic_for_Disease_Outbreak_Detection)

