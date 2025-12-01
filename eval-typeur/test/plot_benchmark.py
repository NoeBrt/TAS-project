import csv
import matplotlib.pyplot as plt
import sys
import os

def read_data(filename):
    data = {
        'DeepAbs': {'size': [], 'classic': [], 'set': []},
        'WideApp': {'size': [], 'classic': [], 'set': []},
        'DeepLet': {'size': [], 'classic': [], 'set': []}
    }

    if not os.path.exists(filename):
        print(f"Error: File {filename} not found. Please run the benchmark executable first.")
        sys.exit(1)

    with open(filename, 'r') as f:
        reader = csv.DictReader(f)
        for row in reader:
            test_type = row['TestType']
            if test_type in data:
                data[test_type]['size'].append(int(row['Size']))
                data[test_type]['classic'].append(float(row['TimeClassic']))
                data[test_type]['set'].append(float(row['TimeSet']))

    return data

def plot_benchmark(data):
    # Create 3 subplots
    fig, axes = plt.subplots(1, 3, figsize=(18, 6))

    test_types = [
        ('DeepAbs', 'Deep Abstractions (Nested Lambdas)'),
        ('WideApp', 'Wide Applications (Balanced Tree)'),
        ('DeepLet', 'Deep Let Bindings')
    ]

    for i, (key, title) in enumerate(test_types):
        ax = axes[i]
        d = data[key]

        if not d['size']:
            ax.text(0.5, 0.5, 'No Data', ha='center', va='center')
            ax.set_title(title)
            continue

        # Sort by size just in case
        sorted_indices = sorted(range(len(d['size'])), key=lambda k: d['size'][k])
        sizes = [d['size'][i] for i in sorted_indices]
        classic = [d['classic'][i] for i in sorted_indices]
        set_based = [d['set'][i] for i in sorted_indices]

        ax.plot(sizes, classic, 'o-', label='Classic (Counter)', markersize=4)
        ax.plot(sizes, set_based, 'x-', label='Smart (Set)', markersize=4)

        ax.set_title(title)
        ax.set_xlabel('Expression Size (nodes)')
        ax.set_ylabel('Time (milliseconds)')
        ax.legend()
        ax.grid(True, linestyle='--', alpha=0.7)

    plt.tight_layout()
    output_file = 'benchmark_results.png'
    plt.savefig(output_file)
    print(f"Plots saved to {output_file}")

    # Try to show plot if environment supports it
    try:
        plt.show()
    except Exception:
        pass

if __name__ == "__main__":
    # Default filename
    filename = 'benchmark_results.csv'

    # Check if provided as argument
    if len(sys.argv) > 1:
        filename = sys.argv[1]

    print(f"Reading data from {filename}...")
    data = read_data(filename)
    plot_benchmark(data)
