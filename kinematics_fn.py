import numpy as np


def kins_fn(conts, smoothie, frate):
    """input parameters: (conts and smoothie are results of running "return_smoothed" function on raw position data)
    conts = list of indices at which to evaluate the kinematics [i.e. continuous values, no nans]
    smoothie = spline function of the position data smoothed, such that smoothie(conts(t)) = smoothed position value at t
    frate = frame rate, e.g. 150
    
    output: an array of data, ntimex4 with data[:,0] = times, data[:,1] = positions, data[:,2] = velocities, data[:,3] = 
    accelerations. (note: velocity and acceleration columns have nans as first and last values)."""

    data_arrays = []
    for t in np.arange(len(conts)):
        ts = [n for n in conts[t]]
        data = np.zeros((len(ts),4))

        res = np.asarray([smoothie[t](n) for n in ts])
        data[:,1] = res

        ts = np.asarray([n*1.0/frate for n in conts[t]])
        data[:,0] = np.asarray(ts)

        for i in range(1,len(ts)-1):
            v = (res[i+1]-res[i-1])/(2*1.0/frate)
            a = (res[i+1]-2*res[i]+res[i-1])/(1.0/frate)**2
            data[i,2] = v
            data[i,3] = a

        data[0,2:3] = np.nan
        data[-1,2:3] = np.nan

        data_arrays.append(data)

    if not len(data_arrays) == 1:
        smoothed_ks = np.vstack(data_arrays)
    else:
        smoothed_ks = data_arrays[0]
        
    return smoothed_ks