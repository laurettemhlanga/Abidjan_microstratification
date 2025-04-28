import pandas as pd
import numpy as np
from sklearn.preprocessing import StandardScaler
from sklearn.mixture import GaussianMixture
from sklearn.feature_selection import SelectKBest, mutual_info_classif
from sklearn.decomposition import PCA
import matplotlib.pyplot as plt
from joblib import Parallel, delayed

# Load and scale data
abidjan_morphology = pd.read_csv("C:/Users/laure/Urban Malaria Proj Dropbox/urban_malaria/data/abidjan/Building Footprint/abidjan_building_metrics_ID.csv")

abidjan_morphology_updated = abidjan_morphology[['area_mean', 'perimeter_mean', 'compact_mean', 'angle_mean', 'shape_mean']]

morpho_scaled = StandardScaler().fit_transform(abidjan_morphology_updated) # exclude geometry if spatial

# print(abidjan_morphology_updated.columns)
# exit (0)

# Optional: PCA or variable selection
pca = PCA(n_components=0.95)  # keep 95% variance
morpho_reduced = pca.fit_transform(morpho_scaled)

# BIC computation for different GMM components (1–9)
def compute_bic(n_components, X):
    gmm = GaussianMixture(n_components=n_components, covariance_type='full', random_state=42)
    gmm.fit(X)
    return gmm.bic(X)

bic_scores = Parallel(n_jobs=-1)(delayed(compute_bic)(k, morpho_reduced) for k in range(1, 10))

# Plot BIC
plt.plot(range(1, 10), bic_scores, marker='o')
plt.xlabel('Number of Clusters')
plt.ylabel('BIC Score')
plt.title('Model Selection via BIC')
plt.grid(True)
plt.show()

# Fit final model using best number of components
best_k = np.argmin(bic_scores)
final_model = GaussianMixture(n_components=best_k, covariance_type='full', random_state=42)
final_model.fit(morpho_reduced)
labels = final_model.predict(morpho_reduced)

# Add to original dataframe
abidjan_morphology['classification'] = labels.astype(str)  # or keep as int

abidjan_morphology.to_csv("abidjan_morphology_clustered.csv", index=False)


