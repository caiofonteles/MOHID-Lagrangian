## MOHID Lagrangian - V0.8 - Work in progress!

MOHID Lagragian is a comprehensive high-performance Lagrangian tracer model, with sources, sinks, particle types and several options for forcing and I/O.

Available functionalities are

- Robust pre-processing, modelling and post-processing tools
- Support for netcdf-cf files with currents, winds and wave fields, as well as water quality (salinity, temperature)
- Ability to model passive, bouyant and degrading tracers
- Beaching and resuspension models and options
- Ability to model millions of tracers in a modest laptop machine
- Simple and fully documented simulation set-up files, ready to be abstracted by a UI
- Raw vtk time encoded output, directly compatible with Paraview and other standard post-processors and renderers
- Flexible python post processor, capable of computing volumetric averages and cumulative integrations, exporting the results to standard netcfd files (so you can explore the results using GIS software or publish to a thredds server) and producing high-quality maped plots using matplotlib and pandas, all scripted in cross-simulation reuseable post-processing recipes, ready to be automated
- Documentation on instalation, code structure, and useage. Fully self contained examples to get you started
- Pre-built windows executable
- Cross-platform compliant, tested and deployed
- Cmake based project, easy to set up for local compilation if required



![Vigo3D](https://github.com/mohid-water-modelling-system/MOHID-Lagrangian/blob/dev/docs/Vigo3DnoDiffusion.gif)

*3D passive tracers on a [MOHID](http://www.mohid.com) operational currents solution in Vigo region, Galiza, Spain.*

![Atlantic1](https://github.com/mohid-water-modelling-system/MOHID-Lagrangian/blob/dev/docs/Atlantic_2016_2017_density.gif)

*Floating passive tracers on a [CMEMS](http://marine.copernicus.eu/) Atlantic currents solution.*

![Arousa](https://github.com/Mohid-Water-Modelling-System/MOHID-Lagrangian/blob/master/docs/diff-mean-n_counts_PolygonTest.png)

*Hourly mean tracer concentration on the Arousa intertidal test case.*

![PCOMS](https://github.com/Mohid-Water-Modelling-System/MOHID-Lagrangian/blob/master/docs/mean-concentration_area_Box1.png)

*Mean tracer concentration on the PCOMS test case*

Check out our [code documentation page](https://mohid-water-modelling-system.github.io/MOHID-Lagrangian/)!

## Help, Bugs, Feedback
If you need help with MOHIDLagrangian or MOHID, want to keep up with progress, chat with developers or ask any other questions about MOHID, you can hang out by mail: <general@mohid.com> or consult our [MOHID wiki](http://wiki.mohid.com). You can also subscribe to our [MOHID forum](http://forum.mohid.com). To report bugs, please create a GitHub issue or contact any developers. More information consult <http://www.mohid.com>

## License
GNU General Public License. See the [GNU General Public License](http://www.gnu.org/copyleft/gpl.html) web page for more information.

<!--[![Build Status](https://travis-ci.org/RBCanelas/MOHID-Lagrangian.svg?branch=master)](https://travis-ci.org/RBCanelas/MOHID-Lagrangian)-->

[![License](https://img.shields.io/badge/license-GNU%20GeneraL%20Public%20License%20v3,%20GPLv3-blue.svg)]()
