#!/usr/bin/env python
# coding: utf-8

# # Plotting Dynamic Time Warping

# Plan here is to plot the dynamic time warping graphs that show the identification of the different points. The input data is three csv's found in this directory.

# In[5]:


import numpy as np
import matplotlib.pyplot as plt
import pandas as pd


# In[2]:


path = '/Users/elliebloom/Desktop/Masters/Project/Analysis/Time_series_analysis/DTW_data_plots/'


# Get data:

# In[12]:


mobs = pd.read_csv(f'{path}/mob_series.csv')
prev = pd.read_csv(f'{path}/prev_series.csv')
dtwi = pd.read_csv(f'{path}/dtw_indices.csv')


# In[17]:


mob_ind = np.asarray(dtwi['mob_index'])
prev_ind = np.asarray(dtwi['prev_index'])


# In[59]:


mob_ind.shape


# In[60]:


inds = np.zeros((mob_ind.size, 2))
for i in range(inds.shape[0]):
    inds[i][0] = mob_ind[i]
    inds[i][1] = prev_ind[i]


# In[61]:


mob_series = np.asarray(mobs['series'])
prev_series = np.asarray(prev['series'])


# In[62]:


inds = inds.astype(int)


# In[84]:


plt.rcParams["figure.figsize"]


# In[85]:


plt.rcParams["figure.figsize"]=(12,8)


# In[86]:


plt.plot(mob_series, 'blue')
plt.plot(prev_series, 'green')
plt.legend(["Mobility", "Prevalence"])
c = 0
for indices in inds:
    x_data = [indices[0]-1, indices[1]-1]
    y_data = [mob_series[indices[0]-1], prev_series[indices[1]-1]]
    if c%5 == 0:
        plt.plot(x_data, y_data, 'gray', linestyle='--')
    c += 1
# Next 3 lines remove axis labels   
ax = plt.gca()
ax.axes.xaxis.set_visible(False)
ax.axes.yaxis.set_visible(False)
plt.show()


# In[87]:


plt.plot(mob_series, 'blue')
plt.plot(prev_series, 'green')
plt.legend(["Mobility", "Prevalence"])
c = 0
for index in range(mob_series.shape[0]):
    x_data = [index, index]
    y_data = [mob_series[index], prev_series[index]]
    if c%5 == 0:
        plt.plot(x_data, y_data, 'gray', linestyle='--')
    c += 1
ax = plt.gca()
ax.axes.xaxis.set_visible(False)
ax.axes.yaxis.set_visible(False)
plt.show()


# In[ ]:




