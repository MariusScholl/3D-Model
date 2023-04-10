# 3D-Model from any place in the World

Creating a 3d model of any place in the world with elevtr and rayshader

### Things u manually need to change are

-   N, O, S, W - Coordinates, u can use a vector file to create BBox as well, this is preference

-   z in get_elev_raster() function. 1-15 with 15 being the best resolution (more info about the resolution is being found here: <https://github.com/tilezen/joerd/blob/master/docs/data-sources.md#what-is-the-ground-resolution>)

-   zscale = depending on the range of the DEM

-   major_dim in image_size()-function. the bigger the dim, the better the resolution

-   The type of overlay you want in line 243

-   Parameters in plot_3d() function.

The "Make it a GIF" parameters should be adjusted (if needed) too.
