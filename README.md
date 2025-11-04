# brGDGT-NPP_Estimation_Tool
This interactive tool estimates terrestrial Net Primary Productivity (NPP) from branched GDGTs (brGDGTs) and/or 
climate variables. It implements a Random Forest model trained on global brGDGT–NPP calibrations and supports optional 
HANPP computation.
 
Features
 
•	brGDGT–derived NPP prediction (with uncertainty)
 
•	Climate-derived NPP (Miami model)
 
•	Human appropriation of NPP (HANPP)
 
•	Plot and summary statistics export
 
•	Excel output
 
# Website: 
https://nou42r-marco-vidal0cordasco.shinyapps.io/brgdgt-npp-tool/

# Instructions:

•	Step 1 — select context & method
 
a. Choose NPP context: Archaeological Site (uses Level as ID) or Natural Archive (uses ID).
 
b. If Natural Archive, choose a Sample Type from: Lacustrine Sediment, Lacustrine SPM, Low DO Lacustrine SPM, Peat, Riverine Sediment and SPM, Soil.
 
c. Choose Method: brGDGT, Climate-derived, or Both. If Both, optional checkbox: compute HANPP (human appropriation = climate − brGDGT).
 
d. Tick desired outputs: Plots and/or Summary Statistics. Click Next.
 
•	Step 2 — upload data & compute NPP
 
a. Upload brGDGT Excel (.xlsx) when using brGDGT method. File must contain the ID column (ID or Level) and brGDGT columns (the code expects columns 2:16 to hold the brGDGT percentages, an example is provided in Data folder. IMPORTANT NOTE: For penta- and hexa-methylated compounds, use an underscore rather than an apostrophe in the input file. For example, compounds commonly written as fIa' should be written as fIa_ ; for details, see examples provided in the Data folder).
 
b. Upload climate Excel (.xlsx) when using climate method. File must contain the ID column and MAT and MAP columns (example provided in Data folder).
 
c. Click Compute NPP.
 
•	View results
 
o	The results table shows: NPP_brGDGT, prediction SD, and 95% CI (for brGDGT) and/or NPP_climate.
 
o	If HANPP selected and IDs match, the app reports HANPP = NPP_climate − NPP_brGDGT.
 
o	Plots: time/level series of NPP and separate HANPP plot if requested.
 
o	Summary tab: mean, median, SD for reported variables.
 
5.	Export — click Download Results to save an Excel file of the active results.
