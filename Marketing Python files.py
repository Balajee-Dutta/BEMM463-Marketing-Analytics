import pandas as pd
import matplotlib.pyplot as plt
import seaborn as sns
import numpy as np
# Load dataset
df = pd.read_excel("SmartWatch Data File.xlsx")  # Ensure the file is in your working directory

# Compute correlation matrix for Wellness and Athlete
selected_columns = ["Wellness", "Athlete"]
correlation_matrix = df[selected_columns].corr(method="pearson")

# Display the correlation matrix
print(correlation_matrix)

# Set up the heatmap aesthetics
plt.figure(figsize=(6, 5))
sns.set_theme(style="whitegrid")  # Enhances readability

# Create heatmap with customization
sns.heatmap(
    correlation_matrix, 
    annot=True, 
    cmap="coolwarm",  # Aesthetic color scheme
    fmt=".2f",  # Format decimal places
    linewidths=1, 
    linecolor='black', 
    cbar=True, 
    square=True, 
    annot_kws={"size": 12}  # Adjust annotation size
)

# Add title
plt.title("Wellness vs. Athlete Feature Correlation Heatmap", fontsize=14, fontweight='bold')

# Display the heatmap
plt.show()

# ---------------------------------------------
# PURPOSE: This code generates a radar chart for smartwatch market segmentation.
# WHY: It helps visualize how different customer segments (e.g., Tech-Savvy, Fitness Enthusiasts)
#      prioritize key attributes like Innovation, Wellness, Value, and Communication.
# HOW: Each segment's average score for these attributes is plotted in a circular format,
#      making it easier to compare their preferences.
# ---------------------------------------------

## Define the labels for the radar chart (representing smartwatch attributes)
labels = ["          Innovation ", "Wellness", "Value", "Communication"]
num_vars = len(labels)  # Number of variables (4 attributes)

# Define the data for each customer segment based on their preferences
segments = {
    "Tech-Savvy Consumers (347)": [4.8, 4.5, 3.9, 4.6],  # Strong in innovation & communication
    "Business Professionals (246)": [4.2, 4.1, 3.8, 4.4],  # Balanced priorities
    "Fashion-Conscious (207)": [4.0, 3.9, 4.5, 4.1],      # Prioritizes style & value
    "Fitness Enthusiasts (200)": [4.1, 4.7, 3.7, 4.0]     # Strong focus on wellness
}

# Convert data into a format suitable for plotting on a radar chart
angles = np.linspace(0, 2 * np.pi, num_vars, endpoint=False).tolist()

# Initialize the figure and subplot with a polar (radar) projection
fig, ax = plt.subplots(figsize=(8, 5), subplot_kw=dict(polar=True))

# Plot each segment on the radar chart
for segment, values in segments.items():
    values += values[:1]  # Closing the circular chart by repeating the first value
    ax.plot(angles + [angles[0]], values, linewidth=2, linestyle='solid', label=segment)
    ax.fill(angles + [angles[0]], values, alpha=0.2)  # Add transparency for visual appeal

# Customize the radar chart
ax.set_yticklabels([])  # Hide radial labels for a cleaner look
ax.set_xticks(angles)   # Set position of category labels
ax.set_xticklabels(labels, fontsize=10, fontweight='bold')



ax.set_title("Smartwatch Market Segmentation - Radar Chart", fontsize=12, fontweight='bold')

# Adjust legend position to extreme right-center
plt.legend(loc='center right', bbox_to_anchor=(1.6, 0.5), frameon=False)

# Display the radar chart
plt.show()
