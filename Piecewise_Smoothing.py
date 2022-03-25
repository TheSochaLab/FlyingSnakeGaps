import numpy as np
from gcvspline import GCVSmoothedNSpline, SmoothedNSpline

def find_continuous_sections(A):
    good = np.concatenate(np.argwhere(~np.isnan(A)))
    for i in np.arange(len(A)):
        good_indices = []
        holding = []
        for t in range(1,len(good)):
            if good[t]-good[t-1] == 1:                                                  
                holding.append(good[t-1])          #add indices corresponding to continuous section to a holding list
            else:
                holding.append(good[t-1])
                good_indices.append(holding)
                holding = []
                #when there's a break: add the indices for the continuous section to good_indices, reset the holding list

        if good[-1]-good[-2] == 1:                 #for the last entry.
            holding.append(good[-1])
            good_indices.append(holding)
            
    return good_indices

def return_smoothed(A,pf):
    conts = find_continuous_sections(A)
    smoothed = []
    
    for i in np.arange(len(conts)): #go through each continuous section one at a time
        section = [A[m] for m in conts[i]] #get all the raw position data corresponding to the section
        section = np.asarray(section)
        t = conts[i]
        t = np.asarray(t) 
        GCV_auto = GCVSmoothedNSpline(t, section) #can change parameters, spline function as necessary.
        p = GCV_auto.p * pf
        GCV_manual = SmoothedNSpline(t, section, p=p)
        
        smoothed.append(GCV_manual)
        
    return conts, smoothed     


