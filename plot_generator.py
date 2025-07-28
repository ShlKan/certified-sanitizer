#!/usr/bin/env python3
import pandas as pd
import matplotlib.pyplot as plt
import numpy as np
from matplotlib.backends.backend_pgf import PdfPages

# Configure matplotlib for LaTeX output
plt.rcParams.update({
    "pgf.texsystem": "pdflatex",
    "font.family": "serif",
    "text.usetex": True,
    "pgf.rcfonts": False,
    "axes.labelsize": 18,
    "font.size": 18,
    "legend.fontsize": 16,
    "xtick.labelsize": 16,
    "ytick.labelsize": 16,
})

def load_data():
    """Load benchmark data from CSV file"""
    return pd.read_csv('benchmark_results.csv')

def create_linear_plot(data):
    """Create linear scale plot"""
    fig, ax = plt.subplots(figsize=(8, 6))
    
    ax.plot(data['size'], data['time'], 'bo-', markersize=4, linewidth=1.5, label='Execution Time')
    ax.set_xlabel('Input Size (characters)')
    ax.set_ylabel('Execution Time (seconds)')
    ax.grid(True, alpha=0.3)
    ax.legend()
    
    # Add trend line
    z = np.polyfit(data['size'], data['time'], 2)
    p = np.poly1d(z)
    ax.plot(data['size'], p(data['size']), "r--", alpha=0.8, label='Quadratic Trend')
    ax.legend()
    
    plt.tight_layout()
    return fig

def create_log_plot(data):
    """Create log-log scale plot"""
    fig, ax = plt.subplots(figsize=(8, 6))
    
    ax.loglog(data['size'], data['time'], 'bo-', markersize=4, linewidth=1.5, label='Execution Time')
    ax.set_xlabel('Input Size (characters)')
    ax.set_ylabel('Execution Time (seconds)')
    ax.grid(True, alpha=0.3)
    ax.legend()
    
    plt.tight_layout()
    return fig

def create_complexity_analysis(data):
    """Create complexity analysis plot with different theoretical curves"""
    fig, ax = plt.subplots(figsize=(10, 6))
    
    # Plot actual data
    ax.plot(data['size'], data['time'], 'bo-', markersize=4, linewidth=2, label='Actual Performance')
    
    # Generate theoretical complexity curves
    n = data['size'].values
    
    # Scale factors to fit the data range
    linear_scale = data['time'].iloc[-1] / n[-1]
    quadratic_scale = data['time'].iloc[-1] / (n[-1]**2)
    cubic_scale = data['time'].iloc[-1] / (n[-1]**3)
    
    ax.plot(n, linear_scale * n, 'g--', alpha=0.7, label=r'$O(n)$ Linear')
    ax.plot(n, quadratic_scale * n**2, 'r--', alpha=0.7, label=r'$O(n^2)$ Quadratic')
    ax.plot(n, cubic_scale * n**3, 'm--', alpha=0.7, label=r'$O(n^3)$ Cubic')
    
    ax.set_xlabel('Input Size (characters)')
    ax.set_ylabel('Execution Time (seconds)')
    ax.grid(True, alpha=0.3)
    ax.legend()
    ax.set_yscale('log')
    ax.set_xscale('log')
    
    plt.tight_layout()
    return fig

def generate_latex_table(data):
    """Generate LaTeX table code"""
    table_code = r"""
\begin{table}[htbp]
\centering
\caption{Certified Sanitizer Performance Benchmark Results}
\label{tab:benchmark_results}
\begin{tabular}{|r|r|}
\hline
\textbf{Input Size} & \textbf{Execution Time (s)} \\
\hline
"""
    
    # Select representative data points for the table
    selected_indices = [0, 4, 9, 14, 19, 24, 29]  # Every 5th point approximately
    
    for i in selected_indices:
        if i < len(data):
            size = int(data.iloc[i]['size'])
            time = data.iloc[i]['time']
            table_code += f"{size:,} & {time:.4f} \\\\\n"
    
    table_code += r"""\hline
\end{tabular}
\end{table}
"""
    return table_code

def main():
    # Load data
    data = load_data()
    print(f"Loaded {len(data)} data points")
    
    # Create plots
    print("Generating linear plot...")
    linear_fig = create_linear_plot(data)
    linear_fig.savefig('benchmark_linear.pdf', bbox_inches='tight')
    linear_fig.savefig('benchmark_linear.png', dpi=300, bbox_inches='tight')
    
    print("Generating log-log plot...")
    log_fig = create_log_plot(data)
    log_fig.savefig('benchmark_loglog.pdf', bbox_inches='tight')
    log_fig.savefig('benchmark_loglog.png', dpi=300, bbox_inches='tight')
    
    print("Generating complexity analysis plot...")
    complexity_fig = create_complexity_analysis(data)
    complexity_fig.savefig('benchmark_complexity.pdf', bbox_inches='tight')
    complexity_fig.savefig('benchmark_complexity.png', dpi=300, bbox_inches='tight')
    
    # Generate LaTeX table
    print("Generating LaTeX table...")
    latex_table = generate_latex_table(data)
    with open('benchmark_table.tex', 'w') as f:
        f.write(latex_table)
    
    # Generate statistics summary
    print("\nBenchmark Summary:")
    print(f"Smallest input: {data['size'].min()} characters")
    print(f"Largest input: {data['size'].max()} characters")
    print(f"Fastest execution: {data['time'].min():.6f} seconds")
    print(f"Slowest execution: {data['time'].max():.6f} seconds")
    print(f"Performance ratio (max/min): {data['time'].max()/data['time'].min():.1f}x")
    
    # Estimate complexity
    large_data = data[data['size'] > 10000]  # Use larger inputs for complexity estimation
    if len(large_data) > 5:
        # Fit polynomial to estimate complexity
        log_size = np.log(large_data['size'])
        log_time = np.log(large_data['time'])
        slope = np.polyfit(log_size, log_time, 1)[0]
        print(f"Estimated time complexity: O(n^{slope:.2f})")
    
    plt.close('all')
    print("\nGenerated files:")
    print("- benchmark_linear.pdf (Linear scale plot)")
    print("- benchmark_loglog.pdf (Log-log scale plot)")
    print("- benchmark_complexity.pdf (Complexity analysis)")
    print("- benchmark_table.tex (LaTeX table)")
    print("- PNG versions of all plots")

if __name__ == "__main__":
    main()