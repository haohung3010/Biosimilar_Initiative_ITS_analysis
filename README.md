# Biosimilar Initiative ITS analysis
## Introduction
Biologics, despite its effectiveness in treating chronic inflammation diseases, are the biggest expense to public drug plans in many healthcare systems worldwide. The arrival of biosimilars, highly similar versions of biologics but at a fraction of the cost, offered the potential for significant cost-savings without losing safety and effectiveness. In September 2019, British Columbia (BC), Canada, became the first public healthcare system in North America to announce a non-medical switching policy, known as the Biosimilars Initiative, to increase the uptake of several selected biosimilars. There has been concern that non-medical switching biosimilar policies could lead to increased utilization of other health care resources, such as physician visits, use of concomitant drugs or hospital and emergency room visits that could offset savings achieved from switching from originator biologics to biosimilars. This analysis examines whether usage of other health care resources and associated costs increased following a NMS in BC, Canada. 

## Analysis plan and Results
We utilized five BC administrative healthcare databases held and linked by Population Data BC, which captured healthcare services used by the BC population from January 1, 2013, to December 31, 2022. In the following section, we will present the analysis plans and results of inflammatory arthritis (IA) patients in BC. Patients with inflammatory bowel and skin diseases have the similar results. 

### Study population
People with IA were identified from 2013 to 2022 using a published identification algorithm. Briefly, to be classified as IA, an individual needed to have at least one hospital record or three physician visits (at least one specialist visit) with an ICD-9 or ICD-10-CA diagnosis code for an IA condition within a two-year period based on published algorithms. People with at least two prescriptions of adalimumab, infliximab or etanercept covering at least 84 non-overlapping days in a 12-month period before the first day of the policy switch period for a specific drug were included in the study cohort. Members of the cohort were observed from their index date of diagnosis, or January 1, 2015, whichever was earliest, until their death, last recorded date in administrative databases, or December 31, 2022, whichever was earliest. 

![Demographic Table](https://github.com/haohung3010/images_repos/blob/main/BI_ITS_analysis/Demographic%20Table.png)

### Healthcare resource utilization rate
The four major components of HCRU were visits to hospitals, physicians and emergency departments, and concomitant drug use. 
- Hospital and ED visits were categorized as ‘related‘ or ‘unrelated to switching’ based on the ICD-9 and ICD-10 codes associated with IA
- Physician visits were classified into two categories of “Specialist” (rheumatologist) and “Other” visits (e.g. internal medicine, general practitioner).
- Concomitant drug use was measured as the number of prescribed drug days for concomitant drugs (i.e. one drug-day is one prescribed day for one concomitant drug). Concomitant drugs included conventional synthetic disease-modifying antirheumatic drugs (csDMARDs), non-steroidal anti-inflammatory drugs (NSAIDs), and oral corticosteroids
Patient-level data was aggregated into the monthly data by splitting any prolonged hospitalization or prescriptions (> 1 day) evenly over the service use period.

![Monthly Rate](https://github.com/haohung3010/images_repos/blob/main/BI_ITS_analysis/Monthly%20Rate.png)

### Healthcare resource utilization cost 
The mean cost of each component of HCRU were costed from the public healthcare payer perspective and summed to represent the total care cost per person. 
- Inpatient hospitalizations were costed using a case-mix approach, which multiplies the cost of a standard hospital stay by the resource intensity weight corresponding to the case-mix group designated by CIHI.
- ED visits were costed by multiplying the number of unique visits by an emergency department facility cost.
- The reimbursement amount from the MSP database was used for fee-for-service physicians during a visit.
- Concomitant drug use was costed by combining the amount paid by PharmaCare and the out-of-pocket costs to individuals.
Mean care costs for individuals were calculated by dividing the total monthly cost of the healthcare resource for the cohort by the yearly cohort size. All costs were converted to 2022 Canadian Dollars.

![Cost Breakdown](https://github.com/haohung3010/images_repos/blob/main/BI_ITS_analysis/Cost%20Breakdown.png)

### Interrupted time series (ITS) analysis
The ITS analysis sought to determine whether there were any significant changes in the level and trend of the total cost of HCRU at the time of either of the policy start dates; the counterfactual assumption is that in the absence of policies, care cost would continue along the pre-switch trend. The 6-month transition periods for each policy were excluded from the ITS analysis, and linear segmented regression models fitted to the remaining data. The ITS model generated four coefficients (two for each policy) estimating the changes in the level and trend of total HCRU cost, excluding biologic/biosimilar acquisition cost, before and after the phase one (infliximab/etanercept) and adalimumab phase in the Biosimilar Initiative. The presence of non-stationery and autocorrelation between monthly data points was assessed by performing the Durbin-Watson test, autocorrelation function and partial autocorrelation function. The final model was fitted using a generalized least-squared approach. The likelihood-ratio test was used to assess moving average and autoregressive correlation terms to make the appropriate adjustments to the autocorrelation structures in our model. 
![BI HCRU ITS](https://github.com/haohung3010/images_repos/blob/main/BI_ITS_analysis/BI%20HCRU%20ITS.png)
![BI HCRU ITS Table](https://github.com/haohung3010/images_repos/blob/main/BI_ITS_analysis/BI%20HCRU%20ITS%20Table.png)

## Conclusion
Non-medical switching policies have increased the uptake of biosimilars offering payers the opportunity to make substantial savings in drug acquisition costs. Importantly, this analysis presents evidence that these savings will not be offset by increases in spending on other types of HCRU. This suggests that the magnitude of savings anticipated by payers, for example, the $732 million realized saving in pharmaceutical cost for the BC government in the first five years of the Biosimilar Initiative, are unlikely to be reduced through spending on other health services. These findings should provide confidence for other jurisdictions in North America and worldwide that non-medical switching policies can be effective for cost-containment and do not lead to consequences for people switched from originators to biosimilars, resulting in increased contacts with the healthcare system and additional costs to payers. 
