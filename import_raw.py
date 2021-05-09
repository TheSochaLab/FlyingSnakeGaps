import numpy as np
import pandas as pd
import os

#OFFSETS FOR BRANCH MARKERS HAVE BEEN CHECKED IN IMAGEJ - DON'T CHANGE.

def get_raw_data(path):
    """ input parameters
        path = .csv file containing raw position data from motion capture, assuming 10 snake markers and up to 4 branch markers
        formatted as 'M1_X, M1_Y, M1_Z, M2_X..." as columns each row corresponding to a different frame.
        File name should be as follows: [trial#, 3 digits]_Snake[#, 2 digits]_gap[#, 2 digits]_[iteration#, 1 digit].csv
        
        Outputs: a dictionary of the following values
        frate: frame rate
        f_index: frame index (list of frame numbers corresponding to the rows of the section of interest of the raw file)
        tn : trial number
        snk_ID: snake number
        mass
        SVL
        raw snake position data
        position of origin branch
        position of target branch
        position of seconary target marker
        marker spacings
        drop point"""
    
    #GET TRIAL METADATA
    #get the frame rate from the csv
    with open(path,'r') as f:
        for i in range(2):
            out = f.readline()
    
    #frame rate and trial number     
    frate = np.int(out[0:3]) #sampling rate, 100 or 150
    trial = np.int(path[-23:-20]) #trial number
    
    #rest of metadata
    cwd = os.getcwd()
    meta_path = cwd + '/Summary Datasets/reference_material.csv'
    metadata = pd.read_csv(meta_path, delimiter=",")
    
    row = metadata[metadata.Trial==trial]
    
    snk_ID = row.ID.values[0]
    svl = row.SVL.values[0]
    mass = row.Mass.values[0]
    mark_s = row.Spacings.values[0]
    land_frame = row.LF.values[0]
    drop = row.DP.values[0]
    
    #change mark_s to a proper list of integers (from a string)
    mark_s = [np.float(x) for x in mark_s.split(',')]

    #GET COLUMNS CORRESPONDING TO BRANCH
    branch_csv = pd.read_csv(path, delimiter=",", skiprows = 4, usecols = [32,33,34,35,36,37,38,39,40,41,42,43])
    (D,E) = branch_csv.shape
    branch = np.reshape(branch_csv.values,(D,4,3))

    origin = np.nanmean(branch[:,0,:],0)
    target = np.nanmean(branch[:,1,:],0)

    if np.isnan(branch[:,3,:]).all():
        target_other = np.array([np.nan,np.nan,np.nan])
    else:
        target_other = np.nanmean(branch[:,3,:],0)
    origin_end = np.array([origin[0]-4, origin[1], origin[2]+6]) #adjust for marker offset from actual branch
    target_end = np.array([target[0]+5, target[1], target[2]+4]) 
    target_other = np.array([target_other[0],target_other[1], target_other[2]-5])

    
    #GET COLUMNS CORRESPONDING TO SNAKE
    col_list = [0]    
    for i in range(2,32):
        col_list.append(i)

    csv = pd.read_csv(path, delimiter=",", skiprows = 4, usecols = col_list, index_col = 0)
    #drops rows or columns when every value in that row or column is Nan, but doesn't drop missing markers 
    #(because heading is not empty). Basically just cleaning the fairly messy .csv files. 
    csv = csv.dropna(how = 'all') 
    f_index = csv.index.values

    #CHOP TO REGION OF INTEREST
    #convert to numpy array for easy slicing, chop to ROI (when snake is in gap, before landing.)
    head_x = np.array(csv.iloc[:,0])
    snake = csv.values
    
    #find start and end indices
    igood = np.where(~np.isnan(head_x))[0]
    start = np.argmax(head_x[igood] > origin_end[0]) #this shows the first time head_x is greater than origin_end[0] of frames where head is defined.
    start_i = igood[start] #switch back to index for all frames, not just non-nan frames.
    end_i = np.where(f_index == np.int(land_frame))[0][0]



    #crop snake and frame index to appropriate frames.
    snake1 = np.copy(snake[start_i:end_i+1,:])
    f_index = f_index[start_i:end_i+1]
    
    
    import_results = {
                'tn':trial,
                'ID':snk_ID,
                'ms':mark_s,
                'svl':svl,
                'mass':mass,
                'fr':frate,
                'oe':origin_end,
                'te':target_end,
                'to':target_other,
                'fn': f_index,
                'raw':snake1,
                'dp':drop,
                'lf':land_frame,
                 }
    
    return import_results
