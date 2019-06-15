# Chart-fltkhs
FLTKHS backend for the Chart Haskell library. 

Works somehow, but not fully correctly. Text (Labels) is now placed correctly, but the graphs are sometimes not displayed correctly, shifted or clipped. Probably the transformations are not fully correct.

Now it is split into a library and the examples, which resemble the examples from https://github.com/timbod7/haskell-chart/wiki. 

What will not work is charts with transparency, (e.g. Examples8 and 9 from the Chart library), as FLTK does not support transparency in it's primitive drawing functions.

