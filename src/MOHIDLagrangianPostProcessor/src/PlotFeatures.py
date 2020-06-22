#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Mon Jun 22 15:28:44 2020

@author: gfnl143
"""

import numpy as np
from math import floor
from matplotlib import patheffects
import cartopy.crs as ccrs
import xarray as xr


def get_color_lims(dataArray: xr.DataArray, robust: bool = True,
                   min_quartile=0.01, max_quartile=0.99):
    """


    Args:
        dataArray (xr.DataArray): DESCRIPTION.
        robust (bool, optional): DESCRIPTION. Defaults to True.
        min_quartile (float, optional): DESCRIPTION. Defaults to 0.01.
        max_quartile (float, optional): DESCRIPTION. Defaults to 0.99.

    Returns:
        vmin (float): DESCRIPTION.
        vmax (float): DESCRIPTION.

    """
    if robust is True:
        vmin = dataArray.quantile(min_quartile).values
        vmax = dataArray.quantile(max_quartile).values
    else:
        vmin = dataArray.min().values
        vmax = dataArray.max().values
    return vmin, vmax


def get_extent(dataArray):
    extent = [dataArray.longitude.min(),
              dataArray.longitude.max(),
              dataArray.latitude.min(),
              dataArray.latitude.max()]
    return extent


def get_horizontal_scale(dataArray: xr.DataArray) -> np.int:
    """Get the horizontal scale to build the scale bar.

    It approaches 1º-100km to tenth part of horizontal extent

    Args:
        dataArray (xr.DataArray): DESCRIPTION.

    Returns:
        int: Scale in km to set the bar.

    """
    _max = dataArray.longitude.max().values
    _min = dataArray.longitude.min().values
    dlon = np.abs(_max - _min)

    extent_horizontal_fraction = 0.1
    degrees_to_km_approach = 100

    scale = np.round(dlon*extent_horizontal_fraction)*degrees_to_km_approach
    # If scale < 1 - 10 km.
    if scale < 1:
        scale = 10
    return scale


def get_title_methods(methods: list, variable: str,
                      debug_title: bool = True) -> str:
    """

    Args:
        methods (list): list with strings of methods used
        variable (str): str with the variable where the methods applied.
        debug_title (bool, optional): Debug flag with info. Defaults to True.

    Returns:
        str: Title.

    """
    source_name = get_source_from_variable(variable)
    measure_name = get_measure_from_variable(variable)

    methods_variable_list = methods[::-1] + [measure_name] + ['t']
    start_parethesis = '('.join(methods_variable_list)
    end_parenthesis = ''.join((len(methods_variable_list)-1)*[')'])


    title = r"$\bf{Method :}$" + start_parethesis + end_parenthesis + '\n'+\
    r"$\bf{Source :}$" + source_name

    return title



def get_source_from_variable(variable: str):
    """
    get the source name from variable

    Args:
        variable (str): variable name from postprocessor

    Returns:
        source_name (str): source name

    """

    post_measures = ['concentration_volume_',
                     'concentration_area_',
                     'n_counts_',
                     'residence_time_']

    for post_measure in post_measures:
        if post_measure in variable:
            source_name = variable.replace(post_measure, '')

    return source_name


def get_measure_from_variable(variable: str):
    """
    get the measure from variable

    Args:
        variable (str): DESCRIPTION.

    Returns:
        post_measure (TYPE): DESCRIPTION.

    """

    post_measures = ['concentration_volume_', 'concentration_area_',
                     'n_counts_', 'residence_time_']

    for post_measure in post_measures:
        if post_measure in variable:
            return post_measure


def utm_from_lon(lon):
    """
    utm_from_lon - UTM zone for a longitude

    Not right for some polar regions (Norway, Svalbard, Antartica)

    :param float lon: longitude
    :return: UTM zone number
    :rtype: int
    """
    return floor((lon + 180) / 6) + 1


def scale_bar(ax, proj, length, location=(0.5, 0.05), linewidth=3,
              units='km', m_per_unit=1000):
    """

    http://stackoverflow.com/a/35705477/1072212
    ax is the axes to draw the scalebar on.
    proj is the projection the axes are in
    location is center of the scalebar in axis coordinates ie.
    0.5 is the middle of the plot
    length is the length of the scalebar in km.
    linewidth is the thickness of the scalebar.
    units is the name of the unit
    m_per_unit is the number of meters in a unit
    """
    # find lat/lon center to find best UTM zone
    x0, x1, y0, y1 = ax.get_extent(proj.as_geodetic())
    # Projection in metres
    utm = ccrs.UTM(utm_from_lon((x0+x1)/2))
    # Get the extent of the plotted area in coordinates in metres
    x0, x1, y0, y1 = ax.get_extent(utm)
    # Turn the specified scalebar location into coordinates in metres
    sbcx, sbcy = x0 + (x1 - x0) * location[0], y0 + (y1 - y0) * location[1]
    # Generate the x coordinate for the ends of the scalebar
    bar_xs = [sbcx - length * m_per_unit/2, sbcx + length * m_per_unit/2]
    # buffer for scalebar
    buffer = [patheffects.withStroke(linewidth=5, foreground="w")]
    # Plot the scalebar with buffer
    ax.plot(bar_xs, [sbcy, sbcy], transform=utm, color='k',
            linewidth=linewidth, path_effects=buffer)
    # buffer for text
    buffer = [patheffects.withStroke(linewidth=3, foreground="w")]
    # Plot the scalebar label
    t0 = ax.text(sbcx, sbcy, str(length) + ' ' + units, transform=utm,
                 horizontalalignment='center', verticalalignment='bottom',
                 path_effects=buffer, zorder=2)

    left = x0+(x1-x0)*0.05
    # Plot the N arrow
    t1 = ax.text(left, sbcy, u'\u25B2\nN', transform=utm,
                 horizontalalignment='center', verticalalignment='bottom',
                 path_effects=buffer, zorder=2)
    # Plot the scalebar without buffer, in case covered by text buffer
    ax.plot(bar_xs, [sbcy, sbcy], transform=utm, color='k',
            linewidth=linewidth, zorder=3)