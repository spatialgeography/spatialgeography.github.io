import numpy as np
import matplotlib.pyplot as plt
from matplotlib.animation import FuncAnimation
import os

# Ensure img directory exists
os.makedirs('e:/spatialgeography/geography/img', exist_ok=True)

fig, ax = plt.subplots(figsize=(6, 4))
fig.patch.set_facecolor('#f8f9fa')
ax.set_facecolor('#ffffff')
ax.set_xlim(0, 10)
ax.set_ylim(0, 15)
ax.set_title("Analyzing Field Data Trends", fontsize=14, fontweight='bold', color='#333333', pad=15)
ax.set_xlabel("Observations (Time)", fontsize=11, color='#555555')
ax.set_ylabel("Measured Variable", fontsize=11, color='#555555')
ax.grid(True, linestyle='--', alpha=0.5, color='#cccccc')
ax.spines['top'].set_visible(False)
ax.spines['right'].set_visible(False)

x_data, y_data = [], []
scatter = ax.scatter([], [], c='#FF5722', s=70, alpha=0.8, edgecolor='white', label='Data Points', zorder=3)
line, = ax.plot([], [], c='#2196F3', lw=3, label='Regression Trend', zorder=2)
ax.legend(loc='upper left', frameon=True, shadow=True)

np.random.seed(42)
true_slope = 1.2
true_intercept = 2.0

def init():
    scatter.set_offsets(np.empty((0, 2)))
    line.set_data([], [])
    return scatter, line

def animate(i):
    if i < 20: # Generating 20 points
        x = i * 0.5 + 0.25
        noise = np.random.normal(0, 1.5)
        y = true_slope * x + true_intercept + noise
        x_data.append(x)
        y_data.append(y)
    
        # Update scatter plot
        scatter.set_offsets(np.c_[x_data, y_data])
        
        # Calculate dynamic regression line
        if len(x_data) > 1:
            slope, intercept = np.polyfit(x_data, y_data, 1)
            line_x = np.array([0, 10])
            line_y = slope * line_x + intercept
            line.set_data(line_x, line_y)
            
    # Leave 10 frames at the end to hold the final image
    return scatter, line

# Create animation
ani = FuncAnimation(fig, animate, init_func=init, frames=30, interval=250, blit=True)

# Save as gif using Pillow
save_path = 'e:/spatialgeography/geography/img/research_data.gif'
ani.save(save_path, writer='pillow', fps=4)
print(f"GIF generated successfully at {save_path}")
