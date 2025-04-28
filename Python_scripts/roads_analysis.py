import geopandas as gpd
import pandas as pd
from shapely.geometry import Point
from sklearn.neighbors import BallTree
import numpy as np


buildings = gpd.read_file("/home/lmhlanga/repos/malaria_determinants/input/shapefiles/foot_print/abidjan_grids_clustered.shp").to_crs(epsg=32630)
roads = gpd.read_file("/home/lmhlanga/repos/malaria_determinants/input/shapefiles/abidjan_roads/abidjan_osm_roads.shp").to_crs(buildings.crs)

# Road types to compute distances for
road_types = ["tertiary", "secondary", "footway", "path", "pedestrian"]

# Get building centroids
building_pts = buildings.geometry.centroid
building_coords = np.array([[pt.x, pt.y] for pt in building_pts])


distance_df = pd.DataFrame(index=buildings.index)

# Loop through road types
for rtype in road_types:
    r_subset = roads[roads["fclass"] == rtype]
    
    if r_subset.empty:
        distance_df[f"dist_{rtype}"] = np.nan
        continue

    # Use midpoints or centroids of road segments
    road_pts = r_subset.geometry.representative_point()
    road_coords = np.array([[pt.x, pt.y] for pt in road_pts])

    
    tree = BallTree(road_coords, leaf_size=40)
    dists, _ = tree.query(building_coords, k=1)
    distance_df[f"dist_{rtype}"] = dists[:, 0]


buildings = buildings.join(distance_df)

# Save output
buildings.to_file("/home/lmhlanga/repos/malaria_determinants/output/building_metrics/abidjan_buildings_with_road_distances.gpkg", driver="GPKG")
