import numpy as np
import pandas as pd


def reshape_and_interp(csv, mark_spaces, n, frate):
    """
    input parameters:
    snake = a n-time*30 array of position data, corresponding to 10 markers in 3 dimensions
    mark_s = list of marker spacings
    n = how big a gap to fill with interpolation (in seconds)
    frate = frame rate of the trial
    
    output: 
    snake1 = reshaped position data, with missing markers dropped, and small gaps interpolated
    mark_s1 = new marker spacings relfecting dropped markers
    m = list of which markers have been dropped
    """
    
    
    #linarly interpolates gaps that are less than n*fr frames, i.e. n seconds    
    csv = pd.DataFrame(csv)
    csv_int = csv.interpolate('linear',limit = np.int(n*frate)) 
    
    csv_int = np.asarray(csv_int)
    (A,B) = csv_int.shape
    snake1 = np.reshape(csv_int, (A,10,3)) #so snake's dimensions are [frame, marker, dimension]
    
    #update snake, marker spacings if any markers are totally missing, creating a list of which markers are dropped.
    dropped = []
    mark_s = mark_spaces.copy() #otherwise function updates the variable put in.
    for m in np.arange(9):
        if all(np.isnan(snake1[:,m,0])): #check the first 9 markers (vent marker treated separately below)
            #update marker spacings list
            mark_s[m+1] = mark_s[m+1]+mark_s[m] 
            dropped.append(m)
            
    if all(np.isnan(snake1[:,9,0])): 
    #if the vent marker is to be dropped - spacings don't need to add, just last value dropped.
        dropped.append(9)

    for ele in sorted(dropped,reverse = True):
        del mark_s[ele]
    
    #now delete the columns corresponding to the dropped data
    m_snake = np.delete(snake1, dropped, axis=1)
        
    
    return m_snake, dropped, mark_s
    
    