# FlyingSnakeGaps
Code and datasets for manuscript on gap crossing in flying snakes

This respository contains:

* MOCAP files: the raw outputs from the Vicon motion capture system, containing 3D position data for each marker on the snake, for all 182 trials included in the study.
* R Files: includes all R scripts, and "Summary Datasets"
* Summary Datasets: .csv files that summarize reference material needed for data analysis (like snake body sizes and density data). Also includes the data on which statistics were run for each trial (bdata, tdata, vdata)
* Jupyter Notebooks (.ipynb files): all analyses run on the raw 3D .csvs including importing and smoothing, calculating velocities, torques, and postural metrics. The inlcude .py files are functions referenced in some of the notebooks.
  
The notebooks are numbered, as running Notebook 2 successfully (for example) relies on outputs from Notebook 1. 

All filenames referenced are relative; so if you download the entire repository to a folder on your computer things should generally work when the file structure is left as is. Let me know if they don't!
