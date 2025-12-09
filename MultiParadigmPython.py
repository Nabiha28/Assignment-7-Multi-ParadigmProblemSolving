from collections import Counter
from typing import List, Union, Dict, Optional
import math


class StatisticsCalculator:
    """A class for calculating basic statistics on a list of integers."""
    
    def __init__(self, data: Optional[List[int]] = None):
        """
        Initialize the StatisticsCalculator with optional data.
        
        Args:
            data: Optional list of integers to calculate statistics on.
        """
        self._data = data if data is not None else []
        self._sorted_data = None
        self._cache = {}  # Cache for calculated statistics
    
    @property
    def data(self) -> List[int]:
        """Get the current data list."""
        return self._data
    
    @data.setter
    def data(self, new_data: List[int]) -> None:
        """Set new data and clear cached results."""
        self._data = new_data
        self._sorted_data = None
        self._cache.clear()
    
    def add_value(self, value: int) -> None:
        """Add a single value to the data."""
        self._data.append(value)
        self._sorted_data = None
        self._cache.clear()
    
    def add_values(self, values: List[int]) -> None:
        """Add multiple values to the data."""
        self._data.extend(values)
        self._sorted_data = None
        self._cache.clear()
    
    def clear_data(self) -> None:
        """Clear all data and cached results."""
        self._data = []
        self._sorted_data = None
        self._cache.clear()
    
    def _get_sorted_data(self) -> List[int]:
        """Get sorted data (cached for performance)."""
        if self._sorted_data is None:
            self._sorted_data = sorted(self._data)
        return self._sorted_data
    
    def mean(self) -> float:
        """Calculate the mean (average) of the data."""
        if 'mean' not in self._cache:
            if not self._data:
                raise ValueError("Cannot calculate mean: data is empty")
            self._cache['mean'] = sum(self._data) / len(self._data)
        return self._cache['mean']
    
    def median(self) -> float:
        """Calculate the median of the data."""
        if 'median' not in self._cache:
            if not self._data:
                raise ValueError("Cannot calculate median: data is empty")
            
            sorted_data = self._get_sorted_data()
            n = len(sorted_data)
            mid = n // 2
            
            if n % 2 == 0:  # Even number of elements
                self._cache['median'] = (sorted_data[mid - 1] + sorted_data[mid]) / 2
            else:  # Odd number of elements
                self._cache['median'] = float(sorted_data[mid])
        
        return self._cache['median']
    
    def mode(self) -> List[int]:
        """Calculate the mode(s) of the data."""
        if 'mode' not in self._cache:
            if not self._data:
                raise ValueError("Cannot calculate mode: data is empty")
            
            # Count frequencies using Counter
            freq_counter = Counter(self._data)
            
            # Find the maximum frequency
            max_freq = max(freq_counter.values())
            
            # Find all values with the maximum frequency
            modes = [value for value, freq in freq_counter.items() if freq == max_freq]
            
            # Sort modes for consistent output
            modes.sort()
            self._cache['mode'] = modes
        
        return self._cache['mode']
    
    def standard_deviation(self, population: bool = False) -> float:
        """Calculate the standard deviation of the data.
        
        Args:
            population: If True, calculate population standard deviation.
                       If False, calculate sample standard deviation.
        """
        if not self._data:
            raise ValueError("Cannot calculate standard deviation: data is empty")
        
        n = len(self._data)
        if population and n == 0:
            return 0.0
        if not population and n < 2:
            raise ValueError("Cannot calculate sample standard deviation with less than 2 data points")
        
        mean_val = self.mean()
        variance = sum((x - mean_val) ** 2 for x in self._data) / (n if population else n - 1)
        return math.sqrt(variance)
    
    def range(self) -> int:
        """Calculate the range of the data (max - min)."""
        if not self._data:
            raise ValueError("Cannot calculate range: data is empty")
        
        sorted_data = self._get_sorted_data()
        return sorted_data[-1] - sorted_data[0]
    
    def summary(self) -> Dict[str, Union[float, List[int]]]:
        """Generate a summary of all statistics."""
        summary_dict = {}
        
        try:
            summary_dict['mean'] = self.mean()
        except ValueError:
            summary_dict['mean'] = None
        
        try:
            summary_dict['median'] = self.median()
        except ValueError:
            summary_dict['median'] = None
        
        try:
            summary_dict['mode'] = self.mode()
        except ValueError:
            summary_dict['mode'] = None
        
        try:
            summary_dict['std_dev_sample'] = self.standard_deviation(population=False)
        except ValueError:
            summary_dict['std_dev_sample'] = None
        
        try:
            summary_dict['std_dev_population'] = self.standard_deviation(population=True)
        except ValueError:
            summary_dict['std_dev_population'] = None
        
        try:
            summary_dict['range'] = self.range()
        except ValueError:
            summary_dict['range'] = None
        
        summary_dict['count'] = len(self._data)
        
        if self._data:
            sorted_data = self._get_sorted_data()
            summary_dict['min'] = sorted_data[0]
            summary_dict['max'] = sorted_data[-1]
        else:
            summary_dict['min'] = None
            summary_dict['max'] = None
        
        return summary_dict
    
    def __str__(self) -> str:
        """String representation of the calculator and its statistics."""
        if not self._data:
            return "StatisticsCalculator: No data available"
        
        try:
            summary = self.summary()
            
            output_lines = [
                "Statistics Calculator Summary:",
                f"Data Points: {summary['count']}",
                f"Min: {summary['min']}, Max: {summary['max']}, Range: {summary['range']}",
                f"Mean: {summary['mean']:.4f}",
                f"Median: {summary['median']}",
                f"Mode(s): {summary['mode']}",
                f"Sample Std Dev: {summary['std_dev_sample']:.4f}" if summary['std_dev_sample'] is not None else "Sample Std Dev: N/A",
                f"Population Std Dev: {summary['std_dev_population']:.4f}" if summary['std_dev_population'] is not None else "Population Std Dev: N/A",
            ]
            
            return "\n".join(output_lines)
        except Exception as e:
            return f"StatisticsCalculator: Error generating statistics - {e}"


# Example usage and demonstration
def demonstrate_statistics_calculator():
    """Demonstrate the usage of the StatisticsCalculator class."""
    
    print("=" * 60)
    print("Statistics Calculator Demonstration")
    print("=" * 60)
    
    # Example 1: Basic statistics
    print("\nExample 1: Basic Statistics")
    print("-" * 40)
    
    data1 = [1, 2, 2, 3, 4, 5, 5, 5, 6]
    calculator1 = StatisticsCalculator(data1)
    print(f"Data: {data1}")
    print(f"Mean: {calculator1.mean():.2f}")
    print(f"Median: {calculator1.median()}")
    print(f"Mode: {calculator1.mode()}")
    
    # Example 2: Complete summary
    print("\nExample 2: Complete Summary")
    print("-" * 40)
    
    data2 = [10, 20, 30, 40, 50, 60, 70, 80, 90, 100]
    calculator2 = StatisticsCalculator(data2)
    print(calculator2)
    
    # Example 3: Dynamic data manipulation
    print("\nExample 3: Dynamic Data Manipulation")
    print("-" * 40)
    
    calculator3 = StatisticsCalculator([1, 2, 3])
    print(f"Initial data: {calculator3.data}")
    print(f"Initial mean: {calculator3.mean():.2f}")
    
    calculator3.add_value(4)
    calculator3.add_value(5)
    print(f"After adding values: {calculator3.data}")
    print(f"New mean: {calculator3.mean():.2f}")
    
    calculator3.add_values([6, 7, 8])
    print(f"After adding multiple values: {calculator3.data}")
    print(f"Final statistics:")
    print(calculator3)
    
    # Example 4: Edge cases
    print("\nExample 4: Edge Cases")
    print("-" * 40)
    
    # Empty data
    calculator4 = StatisticsCalculator()
    print("Empty calculator summary:")
    print(calculator4.summary())
    
    # Single value
    calculator4.data = [42]
    print(f"\nSingle value {calculator4.data}:")
    print(f"Mean: {calculator4.mean()}")
    print(f"Median: {calculator4.median()}")
    print(f"Mode: {calculator4.mode()}")
    
    # Example 5: Multiple modes
    print("\nExample 5: Multiple Modes")
    print("-" * 40)
    
    data5 = [1, 1, 2, 2, 3, 3, 4]
    calculator5 = StatisticsCalculator(data5)
    print(f"Data: {data5}")
    print(f"Mode(s): {calculator5.mode()} (All values appear twice except 4)")
    
    # Example 6: Real-world scenario
    print("\nExample 6: Exam Scores Analysis")
    print("-" * 40)
    
    exam_scores = [85, 92, 78, 92, 85, 67, 85, 92, 74, 88, 90, 85]
    score_calculator = StatisticsCalculator(exam_scores)
    
    print(f"Exam Scores: {exam_scores}")
    print(f"\nAnalysis:")
    print(f"Number of students: {len(exam_scores)}")
    print(f"Average score: {score_calculator.mean():.1f}")
    print(f"Median score: {score_calculator.median()}")
    print(f"Most common score(s): {score_calculator.mode()}")
    print(f"Score range: {score_calculator.range()}")
    print(f"Standard deviation: {score_calculator.standard_deviation():.2f}")
    
    # Check for outliers (more than 2 standard deviations from mean)
    mean_score = score_calculator.mean()
    std_dev = score_calculator.standard_deviation()
    lower_bound = mean_score - 2 * std_dev
    upper_bound = mean_score + 2 * std_dev
    
    outliers = [score for score in exam_scores if score < lower_bound or score > upper_bound]
    if outliers:
        print(f"Potential outliers: {outliers}")
    else:
        print("No significant outliers found.")


if __name__ == "__main__":
    demonstrate_statistics_calculator()
