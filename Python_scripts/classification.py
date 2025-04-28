import pandas as pd
import geopandas as gpd
import numpy as np
from sklearn.cluster import DBSCAN
from shapely.geometry.base import BaseGeometry
from shapely.ops import unary_union
from shapely.geometry import Polygon

# Load your building GeoDataFrame with a 'cluster' column
gdf = gpd.read_file("C:/Users/laure/Urban Malaria Proj Dropbox/urban_malaria/data/abidjan/Building Footprint/cluster_classifications/abidjan_grids_clustered.shp")  # or .geojson, .gpkg, etc.


# Step 1: Assign settlement type based on clusters
cluster_labels = {
    0: "industrial",
    1: "informal",
    2: "formal",
    3: "informal",
    4: "formal",
    5: "industrial",
    6: "informal"
}

print(gdf.columns)



gdf["settlement_type"] = gdf["clssfct"].map(cluster_labels)


# Step 2: DBSCAN on centroids per settlement type
polygons = []
for label in gdf["settlement_type"].unique():
    subset = gdf[gdf["settlement_type"] == label].copy()
    coords = np.array(list(zip(subset.geometry.centroid.x, subset.geometry.centroid.y)))
    db = DBSCAN(eps=60, min_samples=5).fit(coords)
    subset["db_cluster"] = db.labels_

    for cluster_id in set(db.labels_):
        if cluster_id == -1:
            continue
        cluster_points = subset[subset["db_cluster"] == cluster_id]
        if len(cluster_points) < 3:
            continue
        hull = cluster_points.unary_union.convex_hull
        if isinstance(hull, BaseGeometry):
            polygons.append({'settlement_type': label, 'geometry': hull})

# Step 3: Convert to GeoDataFrame
core_polygons = gpd.GeoDataFrame(polygons, crs=gdf.crs)

# Step 4: Buffer each core zone
buffered = core_polygons.copy()
buffered["geometry"] = buffered.geometry.buffer(50)

# Step 5: Separate by type and compute intersections
informal = buffered[buffered["settlement_type"] == "informal"]
formal = buffered[buffered["settlement_type"] == "formal"]
industrial = buffered[buffered["settlement_type"] == "industrial"]

def extract_polygons(intersection_geom, label):
    if intersection_geom.is_empty:
        return []
    elif intersection_geom.geom_type == "Polygon":
        return [{'transition_type': label, 'geometry': intersection_geom}]
    elif intersection_geom.geom_type == "MultiPolygon":
        return [{'transition_type': label, 'geometry': poly} for poly in intersection_geom.geoms]
    return []

transition_polygons = []
transition_polygons += extract_polygons(unary_union(informal.geometry).intersection(unary_union(formal.geometry)), "informal-formal")
transition_polygons += extract_polygons(unary_union(informal.geometry).intersection(unary_union(industrial.geometry)), "informal-industrial")
transition_polygons += extract_polygons(unary_union(formal.geometry).intersection(unary_union(industrial.geometry)), "formal-industrial")

# Step 6: Create transition zones GeoDataFrame
transition_zones = gpd.GeoDataFrame(transition_polygons, crs=gdf.crs)

# Step 7: Export if needed
# core_polygons.to_file("core_settlement_zones.shp")
# transition_zones.to_file("transition_zones.shp")
