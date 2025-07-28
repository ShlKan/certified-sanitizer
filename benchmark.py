#!/usr/bin/env python3
import subprocess
import time
import os

def generate_test_sizes():
    """Generate test sizes with logarithmic scaling for better coverage"""
    sizes = []
    
    # Small sizes (1-100): linear steps
    sizes.extend(range(1, 101, 10))
    
    # Medium sizes (100-10000): geometric progression
    current = 100
    while current <= 10000:
        sizes.append(current)
        current = int(current * 1.5)
    
    # Large sizes (10000-1000000): geometric progression with larger steps
    current = 10000
    while current <= 1000000:
        sizes.append(current)
        current = int(current * 2)
    
    # Remove duplicates and sort
    sizes = sorted(list(set(sizes)))
    return sizes

def run_benchmark(size):
    """Generate input file and measure execution time"""
    filename = f"test_{size}.txt"
    
    # Generate input file
    try:
        subprocess.run([
            "dune", "exec", "certified_sanitizer", "--", 
            "--input-gen", f"--size={size}", f"--file={filename}"
        ], check=True, capture_output=True)
    except subprocess.CalledProcessError as e:
        print(f"Error generating input for size {size}: {e}")
        return None
    
    # Measure execution time
    start_time = time.time()
    try:
        subprocess.run([
            "dune", "exec", "certified_sanitizer", "--", filename
        ], check=True, capture_output=True)
        end_time = time.time()
        execution_time = end_time - start_time
    except subprocess.CalledProcessError as e:
        print(f"Error running benchmark for size {size}: {e}")
        return None
    finally:
        # Clean up test file
        if os.path.exists(filename):
            os.remove(filename)
    
    return execution_time

def main():
    sizes = generate_test_sizes()
    results = []
    
    print(f"Running benchmarks for {len(sizes)} different input sizes...")
    print("Size\tTime (seconds)")
    print("-" * 30)
    
    for i, size in enumerate(sizes):
        execution_time = run_benchmark(size)
        if execution_time is not None:
            results.append((size, execution_time))
            print(f"{size}\t{execution_time:.6f}")
        
        # Progress indicator
        if (i + 1) % 5 == 0:
            print(f"Progress: {i + 1}/{len(sizes)} ({100 * (i + 1) / len(sizes):.1f}%)")
    
    # Save results to file
    with open("benchmark_results.csv", "w") as f:
        f.write("size,time\n")
        for size, time_val in results:
            f.write(f"{size},{time_val}\n")
    
    print(f"\nBenchmark complete! Results saved to benchmark_results.csv")
    print(f"Tested {len(results)} sizes from {min(sizes)} to {max(sizes)} characters")

if __name__ == "__main__":
    main()