import numpy as np

def align(snake, origin, target):
    """align the coordinate system with the branches
    
    Parameters
    ----------
    snake: array of smoothed, gap-filled snake data from a given trial, nframes x mmarks x 3dimensions
    origin: location of the origin point defining the new coordinate system, 3x1 array
    target: location of the target point defininf the new coordinate system, 3x1 array
 
    
    Returns
    -------
    snake position data in the new coordinate system
    target end in the new coordinate system
    theta = angle of rotation between old and new
    """
    ps = snake.copy()
    
    if any(np.isnan(origin)) and any(np.isnan(target)):
        print('no branch markers')
        ps = ps
        theta = 0
        te_new = target
    
    elif any(np.isnan(origin)):
        print('no origin marker')
        ps = ps
        theta = 0
        te_new = target
        
    elif any(np.isnan(target)):
        print('no target marker')
        ps = ps-origin
        theta = 0
        te_new = target
        
    else:
        Xo, Yo, Zo = origin #use as the origin of the coordinate system
        Xt, Yt, Zt = target
        
        X2 = Yt-Yo
        X1 = Xt-Xo

        theta = np.arctan2(X2,X1)

        ps[:, :, 0] = (ps[:, :, 0] - Xo)*np.cos(theta)+(ps[:, :, 1] - Yo)*np.sin(theta) #x prime coordinate system (rotated)
        ps[:, :, 1] = -(ps[:, :, 0] - Xo)*np.sin(theta)+(ps[:, :, 1] - Yo)*np.cos(theta) #y prime coordinate system (rotated)
        ps[:, :, 2] = ps[:, :, 2] - Zo #z prime coordinate system (translated)
        
        Xt_new = (Xt-Xo)*np.cos(theta)+(Yt-Yo)*np.sin(theta)
        Yt_new = -(Xt-Xo)*np.sin(theta)+(Yt-Yo)*np.cos(theta)
        Zt_new = Zt-Zo
        
        te_new = np.array([Xt_new,Yt_new,Zt_new])
        

    return (ps, theta, te_new)