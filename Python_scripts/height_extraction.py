import geopandas as gpd
import rasterio
from rasterio.merge import merge
from rasterstats import zonal_stats
import glob
import os

# === 1. Load the building shapefile ===
shapefile_path = "/home/lmhlanga/repos/malaria_determinants/input/shapefiles/abidjan_grids.shp"
gdf = gpd.read_file(shapefile_path)

# === 2. Merge all raster tiles ===
tile_folder = "/home/lmhlanga/repos/malaria_determinants/input/Building_Height_rasters/"
raster_files = glob.glob(os.path.join(tile_folder, "*.tif"))


src_files_to_mosaic = [rasterio.open(fp) for fp in raster_files]
mosaic, out_transform = merge(src_files_to_mosaic)

#exit (0)
# Use metadata from the first file as a template
out_meta = src_files_to_mosaic[0].meta.copy()
out_meta.update({
    "driver": "GTiff",
    "height": mosaic.shape[1],
    "width": mosaic.shape[2],
    "transform": out_transform
})

# Save the merged raster
merged_raster_path = "/home/lmhlanga/repos/malaria_determinants/output/rasters/merged_building_heights.tif"
with rasterio.open(merged_raster_path, "w", **out_meta) as dest:
    dest.write(mosaic)

# Close raster files
for src in src_files_to_mosaic:
    src.close()

# === 3. Extract height values using zonal stats ===
# (You can change 'mean' to 'max', ' vcxz', etc.)
stats = zonal_stats(
    gdf,
    merged_raster_path,
    stats=["mean"],
    nodata=0,
    geojson_out=False
)

# Add the mean height to GeoDataFrame
gdf["mean_height"] = [s["mean"] for s in stats]

# === 4. Save to new shapefile or GeoPackage ===
gdf.to_file("/home/lmhlanga/repos/malaria_determinants/output/building_metrics/abidjan_height.shp")
# or as GeoPackage
# gdf.to_file("osm_buildings_with_height.gpkg", layer="buildings", driver="GPKG")
