from __future__ import division

import numpy as np


def global_natural_spline(p, t, nspl):
    """Fit 3rd order global natural splines to the data.

    Parameters
    ----------
    p : array, size (nmark, 3)
        Measured data points
    t : array, size (nmark - 1)
        arc length distance between markers. Note that these don't have to
        be exact (and can be less than arc length between them),
        and can be modified to change the tangent angle at
        a given measurement point.
    nspl : int
        number of points to evaluate the spline at

    Returns
    -------
    r, dr, ddr, dddr : arrays, size (nspl, 3)
        x, y, z and the associated derivatives of the spline
    ts : array, size (nspl)
        cumulatve coordinate spine was **evaluated** at
    ss : array, size (nspl)
        cumulative **arc length** coordinate
    spl_ds : array, size (nspl)
        lengths of individual spline segments (in physical units)
    lengths_total : array, size (nmark - 1)
        the integrated length of the spline between the points
    idx_pts : array, size (nmark - 1)
        indices into the ts and ss for the measured points
    """

    # from scipy.integrate import cumtrapz

    # number of measured points
    n = p.shape[0]

    # construct C and D matries: Dm = Cp, solve for m (n x 3) matrix
    C = np.zeros((n, n))
    D = np.zeros((n, n))

    for j in np.arange(n - 2):
        D[j, j] = t[j + 1]
        D[j, j + 1] = 2 * (t[j + 1] + t[j])
        D[j, j + 2] = t[j]

        C[j, j] = 3 * (-t[j + 1] / t[j])
        C[j, j + 1] = 3 * (t[j + 1] / t[j] - t[j] / t[j + 1])
        C[j, j + 2] = 3 * (t[j] / t[j + 1])

    # end conditions for a global natural spline
    C[n - 2, 0] = -3 / t[0]  # x(0)
    C[n - 2, 1] = 3 / t[0]  # x(0)
    C[n - 1, n - 2] = -3 / t[-1]  # x_{n-1}(t_{n-1})
    C[n - 1, n - 1] = 3 / t[-1]  # x_{n-1}(t_{n-1})
    D[n - 2, 0] = 2  # x(0)
    D[n - 2, 1] = 1  # x(0)
    D[n - 1, n - 2] = 1  # x_{n-1}(t_{n-1})
    D[n - 1, n - 1] = 2  # x_{n-1}(t_{n-1})

    # solve for the tangent angles m
    m = np.linalg.solve(D, np.dot(C, p))

    # cubic spline coefficients
    a = p[:-1]
    b = m[:-1]
    c = (3 * (p[1:] - p[:-1]).T / t**2 - (2 * m[:-1] + m[1:]).T / t).T
    d = (2 * (p[:-1] - p[1:]).T / t**3 + (m[:-1] + m[1:]).T / t**2).T

    # number of spline points per segment, taking care so we have nspl total
    mm_per_spl_bit = t.sum() / nspl
    bits_per_seg_float = t / mm_per_spl_bit
    bits_per_seg = np.round(bits_per_seg_float).astype(np.int)

    nbits = bits_per_seg.sum()
    if nbits > nspl:
        bits_per_seg[-1] -= nbits - nspl
    elif nbits < nspl:
        bits_per_seg[-1] += nspl - nbits
    nspl_seg = bits_per_seg.copy()

    nspl_seg[:-1] += 1  # because of the inertior points we skip with
                        # jj > 0 below

    # indices in ts for the measured points
    # e.g. ts[idx_pts] == t.cumsum()
    idx_pts = (nspl_seg - 1).cumsum()

    # empty arrays to store the spline values
    r = np.zeros((nspl, 3))
    dr = np.zeros((nspl, 3))
    ddr = np.zeros((nspl, 3))
    dddr = np.zeros((nspl, 3))
    ts = np.zeros(nspl)

    # iterate through the (n - 1) segments and fit the spline
    cnt = 0
    for jj in np.arange(n - 1):
        ti = np.linspace(0, t[jj], nspl_seg[jj])
        if jj > 0:
            ti = ti[1:]

        for k in np.arange(len(ti)):
            ri = a[jj] + b[jj] * ti[k] + c[jj] * ti[k]**2 + d[jj] * ti[k]**3
            dri = b[jj] + 2 * c[jj] * ti[k] + 3 * d[jj] * ti[k]**2
            ddri = 2 * c[jj] + 6 * d[jj] * ti[k]
            dddri = 6 * d[jj]
            r[cnt] = ri
            dr[cnt] = dri
            ddr[cnt] = ddri
            dddr[cnt] = dddri
            ts[cnt] = t[:jj].sum() + ti[k]
            cnt += 1

    assert cnt == nspl

    # segment parameter lengths
    dts = np.gradient(ts, edge_order=2)

    # integrate arc length between the measured points
    ds = np.sqrt(np.sum(dr**2, axis=1))

    # length of each nspl segment
    seg_lens = ds * dts

    # arc length coordinate for the spline
    ss = seg_lens.cumsum()

    # integrate arc length between points
    int_seg = np.r_[0, bits_per_seg.cumsum()]  # indices to integrate between
    lengths_total = np.zeros(n - 1)  # total length of each segment
    for jj in np.arange(n - 1):
        i0, i1 = int_seg[jj], int_seg[jj + 1]
        assert i1 <= nspl
        lens = seg_lens[i0:i1].cumsum()
        lengths_total[jj] = lens[-1]

    return r, dr, ddr, dddr, ts, ss, seg_lens, lengths_total, idx_pts


def splinize_snake(pfe, te, nspl, times, mass, marker_df, density_df, chord_df):
    """Fit a spline to the recorded IR markers to model the backbone of the snake.
    Also overlay the mass and chord length distributions.

    Parameters
    ----------
    pfe : array, size (ntime, nmark + 1, 3)
        IR marker locations
    te : array, size (nmark + 1)
        Distance between markers
    nspl : int
        Number of points to evaluate spline at
    times : array, size (ntime)
        Measured time points (for making a 2D time point array)
    mass : float
        Measured mass of the snake
    marker_df : DataFrame
        Information about the markers for the snake
    density_df : DataFrame
        [s, rho] Normalized density (by average density) distribution
    chord_df : DataFrame
        [s, chord] Normalized chord length (by SVL) distribution

    Returns
    -------
    Dictionary with:
    out = dict(Ro_I=Ro_I, R_I=R_I, dRds_I=ddRds_I, ddRds_I=ddRds_I, dddRds_I=dddRds_I,
           spl_ds=spl_ds, mass_spl=mass_spl, chord_spl=chord_spl,
           vent_idx_spl=vent_idx_spl, times2D=times2D, t_coord=t_coord,
           s_coord=s_coord, spl_len_errors=spl_len_errors,
           idx_pts=idx_pts, SVL=SVL, VTL=VTL)
    """

    # marker information
    dist_btn_markers = marker_df['Dist to next, mm'].dropna().values
    vent_idx = np.where(marker_df['Marker type'] == 'vent')[0][0]
    SVL = marker_df['svl (mm)'].values[0]
    VTL = marker_df['tail (mm)'].values[0]

    # density distribution
    s_rho = density_df['s'].values
    body_rho = density_df['rho'].values

    # chord length distribution
    s_chord  = chord_df['s'].values
    body_chord = chord_df['chord'].values

    ntime, nmark_e, _ = pfe.shape  # number of markers on 'extended' neck snake
    nmark = nmark_e - 1  # number of markers on acutal snake

    Ro_I = np.zeros((ntime, 3))
    times2D = np.zeros((ntime, nspl))
    t_coord = np.zeros((ntime, nspl))
    s_coord = np.zeros((ntime, nspl))
    vent_idx_spls = np.zeros(ntime, dtype=np.int)
    R_I = np.zeros((ntime, nspl, 3))  # spl
    dRds_I = np.zeros((ntime, nspl, 3))  # dspl
    ddRds_I = np.zeros((ntime, nspl, 3))  # ddspl
    dddRds_I = np.zeros((ntime, nspl, 3))
    spl_ds = np.zeros((ntime, nspl))  # length of each segment in mm
    mass_spl = np.zeros((ntime, nspl))  # in g
    chord_spl = np.zeros((ntime, nspl))
    #spl_len_totals = np.zeros(ntime)
    spl_len_errors = np.zeros((ntime, nmark - 1))  # -1 because difference b/n

    for i in np.arange(ntime):

        # fit spline (fpe is the arc length coordinate of the markers)
        out = global_natural_spline(pfe[i], te, nspl)
        r, dr, ddr, dddr, ts, ss, seg_lens, lengths_total_e, idx_pts = out

        # exclude the virtual marker for error calculations
        lengths_total = np.zeros(nmark - 1)
        lengths_total[0] = lengths_total_e[0] + lengths_total_e[1]
        lengths_total[1:] = lengths_total_e[2:]

        # arc length coordinate differences (% along spline) of markers (no virtual marker)
         # %SVL of arc length coordinate
        spl_len_error = (dist_btn_markers - lengths_total) / SVL * 100

        # index into arc length coord where vent measurement is closest
        # based on segment parameters (maybe arc length would be better,
        # but it is making the tail too short)
        vent_idx_spl = idx_pts[vent_idx]

        # mass distribution
        mass_spl_i = np.interp(ts / SVL, s_rho, body_rho)
        mass_spl_i = mass * mass_spl_i / mass_spl_i.sum()

        # chord length distribution
        chord_spl[i] = SVL * np.interp(ts / SVL, s_chord, body_chord)

        # center of mass
        Ro_I[i] = np.sum((r.T * mass_spl_i).T, axis=0) / mass

        # store the spline and its derivative (for tangent angle calculations)
        R_I[i] = r
        dRds_I[i] = dr
        ddRds_I[i] = ddr
        dddRds_I[i] = dddr
        spl_ds[i] = seg_lens
        mass_spl[i] = mass_spl_i
        vent_idx_spls[i] = vent_idx_spl
        times2D[i] = times[i]
        t_coord[i] = ts
        s_coord[i] = ss
        spl_len_errors[i] = spl_len_error

    # the vent should be located at the same place for all splines
    assert(np.sum(vent_idx_spls == vent_idx_spls[0]) == ntime)

    # just use the one vent index into the spline
    # idx_pts should also be good then
    vent_idx_spl = vent_idx_spls[0]

    out = dict(Ro_I=Ro_I, R_I=R_I, dRds_I=dRds_I, ddRds_I=ddRds_I, dddRds_I=dddRds_I,
               spl_ds=spl_ds, mass_spl=mass_spl, chord_spl=chord_spl,
               vent_idx_spl=vent_idx_spl, times2D=times2D, t_coord=t_coord,
               s_coord=s_coord, spl_len_errors=spl_len_errors,
               idx_pts=idx_pts, SVL=SVL, VTL=VTL)

    return out

