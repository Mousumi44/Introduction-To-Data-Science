#!/usr/bin/env python
# coding: utf-8

# In[1]:


import random 
import numpy as np 
import matplotlib.pyplot as plt 
from sklearn.cluster import KMeans 
from sklearn.datasets import make_blobs 
get_ipython().run_line_magic('matplotlib', 'inline')
import pandas as pd


# In[2]:


men_df = pd.read_csv("2020-Mens-Data/MEvents2019.csv")
men_df.head()


# In[3]:


df = men_df.drop(['EventID','Season','EventTeamID','EventPlayerID','EventType','EventSubType','X','Y','Area'], axis=1)
df.sample(5)


# In[4]:


from sklearn.preprocessing import StandardScaler
X = df.values[:,1:]
X = np.nan_to_num(X)
Clus_dataSet = StandardScaler().fit_transform(X)
Clus_dataSet


# In[5]:


clusterNum = 5
k_means = KMeans(init = "k-means++", n_clusters = clusterNum, n_init = 12)
k_means.fit(X)
labels = k_means.labels_
print(labels)


# In[6]:


df["Clus_km"] = labels
df.head(5)


# In[7]:


df.groupby('Clus_km').mean()


# In[ ]:


area = np.pi * ( X[:, 1])**2  
plt.scatter(X[:, 0], X[:, 3], s=area, c=labels.astype(np.float), alpha=0.5)
plt.xlabel('WCurrentScore', fontsize=18)
plt.ylabel('ElapsedSeconds', fontsize=16)

plt.show()


# In[ ]:



from mpl_toolkits.mplot3d import Axes3D 
fig = plt.figure(1, figsize=(8, 6))
plt.clf()
ax = Axes3D(fig, rect=[0, 0, .95, 1], elev=48, azim=134)

plt.cla()
# plt.ylabel('Age', fontsize=18)
# plt.xlabel('Income', fontsize=16)
# plt.zlabel('Education', fontsize=16)
ax.set_xlabel('WCurrentScore')
ax.set_ylabel('LCurrentScore')
ax.set_zlabel('ElapsedSeconds')

ax.scatter(X[:, 1], X[:, 0], X[:, 3], c= labels.astype(np.float))


# In[ ]:




