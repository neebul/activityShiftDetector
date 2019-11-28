# activityShiftDetector
Creation of a random forest classifier to detect transitions between physical behaviors (like lying, sitting, walking, etc.), thanks to accelererometer's data. In this project, features were generated using the raw data of a dual-accelerometer system (AX3 Axivity).

The beginning dataset needs two mani information: the raw data, and a column of 0/1 indicating if there's a transition (1) or not (0) at each moment. 
The features are created using a sliding window. After "1_fromRawData_toFeatures.R", each window corresponds to a line in the dataset, with features that summarize accelerometer's situated within the window. A column also indicates if there's a transition (0/1) in the window.
