#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>

#define MAX_DATA_SIZE 1000
#define CACHE_SIZE 10

typedef struct {
    int data[MAX_DATA_SIZE];
    int count;
    int sorted_data[MAX_DATA_SIZE];
    int sorted_count;
    float cache_mean;
    float cache_median;
    int cache_mode[MAX_DATA_SIZE];
    int cache_mode_count;
    float cache_std_dev_sample;
    float cache_std_dev_population;
    int cache_range;
    int cache_flags;  // Bitwise flags for cached values
} StatisticsCalculator;

// Cache flags
#define CACHE_MEAN              0x01
#define CACHE_MEDIAN            0x02
#define CACHE_MODE              0x04
#define CACHE_STD_DEV_SAMPLE    0x08
#define CACHE_STD_DEV_POPULATION 0x10
#define CACHE_RANGE             0x20

// Function declarations
StatisticsCalculator* create_calculator(void);
void init_calculator(StatisticsCalculator* calc);
void add_value(StatisticsCalculator* calc, int value);
void add_values(StatisticsCalculator* calc, int values[], int count);
void clear_data(StatisticsCalculator* calc);
void sort_data(StatisticsCalculator* calc);
float calculate_mean(StatisticsCalculator* calc);
float calculate_median(StatisticsCalculator* calc);
void calculate_mode(StatisticsCalculator* calc, int modes[], int* mode_count);
float calculate_std_dev(StatisticsCalculator* calc, int population);
int calculate_range(StatisticsCalculator* calc);
void print_summary(StatisticsCalculator* calc);
void free_calculator(StatisticsCalculator* calc);

// Comparator for qsort
int compare_ints(const void* a, const void* b) {
    return *(int*)a - *(int*)b;
}

// Create and initialize a new calculator
StatisticsCalculator* create_calculator(void) {
    StatisticsCalculator* calc = (StatisticsCalculator*)malloc(sizeof(StatisticsCalculator));
    if (calc == NULL) {
        printf("Memory allocation failed\n");
        return NULL;
    }
    init_calculator(calc);
    return calc;
}

// Initialize calculator data
void init_calculator(StatisticsCalculator* calc) {
    calc->count = 0;
    calc->sorted_count = 0;
    calc->cache_flags = 0;
    calc->cache_mode_count = 0;
    memset(calc->data, 0, sizeof(calc->data));
    memset(calc->sorted_data, 0, sizeof(calc->sorted_data));
    memset(calc->cache_mode, 0, sizeof(calc->cache_mode));
}

// Add a single value
void add_value(StatisticsCalculator* calc, int value) {
    if (calc->count < MAX_DATA_SIZE) {
        calc->data[calc->count++] = value;
        calc->sorted_count = 0;  // Invalidate sorted data
        calc->cache_flags = 0;   // Clear all cache flags
    } else {
        printf("Error: Data size limit (%d) exceeded\n", MAX_DATA_SIZE);
    }
}

// Add multiple values
void add_values(StatisticsCalculator* calc, int values[], int count) {
    for (int i = 0; i < count; i++) {
        add_value(calc, values[i]);
    }
}

// Clear all data and cache
void clear_data(StatisticsCalculator* calc) {
    init_calculator(calc);
}

// Sort data for median and range calculations
void sort_data(StatisticsCalculator* calc) {
    if (calc->sorted_count == 0 && calc->count > 0) {
        memcpy(calc->sorted_data, calc->data, calc->count * sizeof(int));
        qsort(calc->sorted_data, calc->count, sizeof(int), compare_ints);
        calc->sorted_count = calc->count;
    }
}

// Calculate mean
float calculate_mean(StatisticsCalculator* calc) {
    if (calc->cache_flags & CACHE_MEAN) {
        return calc->cache_mean;
    }
    
    if (calc->count == 0) {
        printf("Error: Cannot calculate mean - data is empty\n");
        return 0.0f;
    }
    
    int sum = 0;
    for (int i = 0; i < calc->count; i++) {
        sum += calc->data[i];
    }
    
    calc->cache_mean = (float)sum / calc->count;
    calc->cache_flags |= CACHE_MEAN;
    return calc->cache_mean;
}

// Calculate median
float calculate_median(StatisticsCalculator* calc) {
    if (calc->cache_flags & CACHE_MEDIAN) {
        return calc->cache_median;
    }
    
    if (calc->count == 0) {
        printf("Error: Cannot calculate median - data is empty\n");
        return 0.0f;
    }
    
    sort_data(calc);
    int n = calc->count;
    float median;
    
    if (n % 2 == 0) {
        // Even number of elements
        median = (calc->sorted_data[n/2 - 1] + calc->sorted_data[n/2]) / 2.0f;
    } else {
        // Odd number of elements
        median = (float)calc->sorted_data[n/2];
    }
    
    calc->cache_median = median;
    calc->cache_flags |= CACHE_MEDIAN;
    return median;
}

// Calculate mode
void calculate_mode(StatisticsCalculator* calc, int modes[], int* mode_count) {
    if (calc->cache_flags & CACHE_MODE) {
        memcpy(modes, calc->cache_mode, calc->cache_mode_count * sizeof(int));
        *mode_count = calc->cache_mode_count;
        return;
    }
    
    if (calc->count == 0) {
        printf("Error: Cannot calculate mode - data is empty\n");
        *mode_count = 0;
        return;
    }
    
    sort_data(calc);
    
    int max_freq = 0;
    int freq = 1;
    *mode_count = 0;
    
    // First pass: find maximum frequency
    for (int i = 0; i < calc->count; i++) {
        if (i > 0 && calc->sorted_data[i] != calc->sorted_data[i-1]) {
            if (freq > max_freq) {
                max_freq = freq;
            }
            freq = 1;
        } else if (i > 0) {
            freq++;
        }
    }
    if (freq > max_freq) {
        max_freq = freq;
    }
    
    // Second pass: collect all values with maximum frequency
    freq = 1;
    for (int i = 0; i < calc->count; i++) {
        if (i > 0 && calc->sorted_data[i] != calc->sorted_data[i-1]) {
            if (freq == max_freq) {
                modes[*mode_count] = calc->sorted_data[i-1];
                (*mode_count)++;
            }
            freq = 1;
        } else if (i > 0) {
            freq++;
        }
    }
    if (freq == max_freq) {
        modes[*mode_count] = calc->sorted_data[calc->count-1];
        (*mode_count)++;
    }
    
    memcpy(calc->cache_mode, modes, *mode_count * sizeof(int));
    calc->cache_mode_count = *mode_count;
    calc->cache_flags |= CACHE_MODE;
}

// Calculate standard deviation
float calculate_std_dev(StatisticsCalculator* calc, int population) {
    if (calc->count == 0) {
        printf("Error: Cannot calculate standard deviation - data is empty\n");
        return 0.0f;
    }
    
    if (!population && calc->count < 2) {
        printf("Error: Need at least 2 data points for sample standard deviation\n");
        return 0.0f;
    }
    
    int cache_flag = population ? CACHE_STD_DEV_POPULATION : CACHE_STD_DEV_SAMPLE;
    if (calc->cache_flags & cache_flag) {
        return population ? calc->cache_std_dev_population : calc->cache_std_dev_sample;
    }
    
    float mean = calculate_mean(calc);
    float variance = 0.0f;
    
    for (int i = 0; i < calc->count; i++) {
        float diff = calc->data[i] - mean;
        variance += diff * diff;
    }
    
    int divisor = population ? calc->count : (calc->count - 1);
    variance /= divisor;
    float std_dev = sqrtf(variance);
    
    if (population) {
        calc->cache_std_dev_population = std_dev;
    } else {
        calc->cache_std_dev_sample = std_dev;
    }
    calc->cache_flags |= cache_flag;
    
    return std_dev;
}

// Calculate range
int calculate_range(StatisticsCalculator* calc) {
    if (calc->cache_flags & CACHE_RANGE) {
        return calc->cache_range;
    }
    
    if (calc->count == 0) {
        printf("Error: Cannot calculate range - data is empty\n");
        return 0;
    }
    
    sort_data(calc);
    int range = calc->sorted_data[calc->count - 1] - calc->sorted_data[0];
    
    calc->cache_range = range;
    calc->cache_flags |= CACHE_RANGE;
    return range;
}

// Print summary statistics
void print_summary(StatisticsCalculator* calc) {
    if (calc->count == 0) {
        printf("StatisticsCalculator: No data available\n");
        return;
    }
    
    printf("Statistics Calculator Summary:\n");
    printf("Data Points: %d\n", calc->count);
    
    sort_data(calc);
    printf("Min: %d, Max: %d, Range: %d\n", 
           calc->sorted_data[0], 
           calc->sorted_data[calc->count - 1], 
           calculate_range(calc));
    
    printf("Mean: %.4f\n", calculate_mean(calc));
    printf("Median: %.1f\n", calculate_median(calc));
    
    int modes[MAX_DATA_SIZE];
    int mode_count = 0;
    calculate_mode(calc, modes, &mode_count);
    printf("Mode(s): ");
    for (int i = 0; i < mode_count; i++) {
        if (i > 0) printf(", ");
        printf("%d", modes[i]);
    }
    printf("\n");
    
    if (calc->count >= 2) {
        printf("Sample Std Dev: %.4f\n", calculate_std_dev(calc, 0));
    } else {
        printf("Sample Std Dev: N/A\n");
    }
    printf("Population Std Dev: %.4f\n", calculate_std_dev(calc, 1));
}

// Free calculator
void free_calculator(StatisticsCalculator* calc) {
    if (calc != NULL) {
        free(calc);
    }
}

// Example 1: Basic statistics
void example_1(void) {
    printf("\n========== Example 1: Basic Statistics ==========\n");
    
    StatisticsCalculator* calc = create_calculator();
    int data[] = {1, 2, 2, 3, 4, 5, 5, 5, 6};
    int size = sizeof(data) / sizeof(data[0]);
    
    add_values(calc, data, size);
    
    printf("Data: ");
    for (int i = 0; i < size; i++) {
        printf("%d ", data[i]);
    }
    printf("\n");
    printf("Mean: %.2f\n", calculate_mean(calc));
    printf("Median: %.1f\n", calculate_median(calc));
    
    int modes[MAX_DATA_SIZE];
    int mode_count = 0;
    calculate_mode(calc, modes, &mode_count);
    printf("Mode: ");
    for (int i = 0; i < mode_count; i++) {
        if (i > 0) printf(", ");
        printf("%d", modes[i]);
    }
    printf("\n");
    
    free_calculator(calc);
}

// Example 2: Complete summary
void example_2(void) {
    printf("\n========== Example 2: Complete Summary ==========\n");
    
    StatisticsCalculator* calc = create_calculator();
    int data[] = {10, 20, 30, 40, 50, 60, 70, 80, 90, 100};
    int size = sizeof(data) / sizeof(data[0]);
    
    add_values(calc, data, size);
    print_summary(calc);
    
    free_calculator(calc);
}

// Example 3: Dynamic data manipulation
void example_3(void) {
    printf("\n========== Example 3: Dynamic Data Manipulation ==========\n");
    
    StatisticsCalculator* calc = create_calculator();
    
    printf("Initial data: [1, 2, 3]\n");
    add_value(calc, 1);
    add_value(calc, 2);
    add_value(calc, 3);
    printf("Initial mean: %.2f\n", calculate_mean(calc));
    
    printf("\nAfter adding values [4, 5]:\n");
    add_value(calc, 4);
    add_value(calc, 5);
    printf("Data: [1, 2, 3, 4, 5]\n");
    printf("New mean: %.2f\n", calculate_mean(calc));
    
    printf("\nAfter adding multiple values [6, 7, 8]:\n");
    int new_values[] = {6, 7, 8};
    add_values(calc, new_values, 3);
    printf("Final statistics:\n");
    print_summary(calc);
    
    free_calculator(calc);
}

// Example 4: Edge cases
void example_4(void) {
    printf("\n========== Example 4: Edge Cases ==========\n");
    
    // Empty data
    printf("Empty calculator:\n");
    StatisticsCalculator* calc = create_calculator();
    printf("Count: %d\n", calc->count);
    printf("Mean: %.4f\n", calculate_mean(calc));
    free_calculator(calc);
    
    // Single value
    printf("\nSingle value [42]:\n");
    calc = create_calculator();
    add_value(calc, 42);
    printf("Mean: %.2f\n", calculate_mean(calc));
    printf("Median: %.1f\n", calculate_median(calc));
    
    int modes[MAX_DATA_SIZE];
    int mode_count = 0;
    calculate_mode(calc, modes, &mode_count);
    printf("Mode: %d\n", modes[0]);
    
    free_calculator(calc);
}

// Example 5: Multiple modes
void example_5(void) {
    printf("\n========== Example 5: Multiple Modes ==========\n");
    
    StatisticsCalculator* calc = create_calculator();
    int data[] = {1, 1, 2, 2, 3, 3, 4};
    int size = sizeof(data) / sizeof(data[0]);
    
    add_values(calc, data, size);
    
    printf("Data: ");
    for (int i = 0; i < size; i++) {
        printf("%d ", data[i]);
    }
    printf("\n");
    
    int modes[MAX_DATA_SIZE];
    int mode_count = 0;
    calculate_mode(calc, modes, &mode_count);
    printf("Mode(s): ");
    for (int i = 0; i < mode_count; i++) {
        if (i > 0) printf(", ");
        printf("%d", modes[i]);
    }
    printf(" (All values appear twice except 4)\n");
    
    free_calculator(calc);
}

// Example 6: Real-world scenario - Exam scores
void example_6(void) {
    printf("\n========== Example 6: Exam Scores Analysis ==========\n");
    
    StatisticsCalculator* calc = create_calculator();
    int exam_scores[] = {85, 92, 78, 92, 85, 67, 85, 92, 74, 88, 90, 85};
    int size = sizeof(exam_scores) / sizeof(exam_scores[0]);
    
    add_values(calc, exam_scores, size);
    
    printf("Exam Scores: ");
    for (int i = 0; i < size; i++) {
        printf("%d ", exam_scores[i]);
    }
    printf("\n\nAnalysis:\n");
    printf("Number of students: %d\n", size);
    printf("Average score: %.1f\n", calculate_mean(calc));
    printf("Median score: %.1f\n", calculate_median(calc));
    
    int modes[MAX_DATA_SIZE];
    int mode_count = 0;
    calculate_mode(calc, modes, &mode_count);
    printf("Most common score(s): ");
    for (int i = 0; i < mode_count; i++) {
        if (i > 0) printf(", ");
        printf("%d", modes[i]);
    }
    printf("\n");
    
    printf("Score range: %d\n", calculate_range(calc));
    printf("Standard deviation: %.2f\n", calculate_std_dev(calc, 0));
    
    // Detect outliers (more than 2 standard deviations from mean)
    float mean = calculate_mean(calc);
    float std_dev = calculate_std_dev(calc, 0);
    float lower_bound = mean - 2 * std_dev;
    float upper_bound = mean + 2 * std_dev;
    
    printf("\nOutlier detection (±2 std dev):\n");
    printf("Lower bound: %.2f, Upper bound: %.2f\n", lower_bound, upper_bound);
    
    int outlier_count = 0;
    int outliers[MAX_DATA_SIZE];
    for (int i = 0; i < size; i++) {
        if (exam_scores[i] < lower_bound || exam_scores[i] > upper_bound) {
            outliers[outlier_count++] = exam_scores[i];
        }
    }
    
    if (outlier_count > 0) {
        printf("Potential outliers: ");
        for (int i = 0; i < outlier_count; i++) {
            printf("%d ", outliers[i]);
        }
        printf("\n");
    } else {
        printf("No significant outliers found.\n");
    }
    
    free_calculator(calc);
}

// Main function
int main(void) {
    printf("============================================================\n");
    printf("        Statistics Calculator Demonstration (C Version)\n");
    printf("============================================================\n");
    
    example_1();
    example_2();
    example_3();
    example_4();
    example_5();
    example_6();
    
    printf("\n============================================================\n");
    printf("              All examples completed successfully!\n");
    printf("============================================================\n");
    
    return 0;
}
