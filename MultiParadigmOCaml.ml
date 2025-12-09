(* Statistics Calculator in OCaml *)

(* Helper module for list operations *)
module ListUtils = struct
  let rec sum lst = match lst with
    | [] -> 0
    | x :: xs -> x + sum xs

  let length = List.length

  let rec take n lst = match n, lst with
    | 0, _ -> []
    | _, [] -> []
    | n, x :: xs -> x :: take (n - 1) xs

  let rec drop n lst = match n, lst with
    | 0, lst -> lst
    | _, [] -> []
    | n, _ :: xs -> drop (n - 1) xs

  let rec filter pred lst = match lst with
    | [] -> []
    | x :: xs -> if pred x then x :: filter pred xs else filter pred xs

  let rec map f lst = match lst with
    | [] -> []
    | x :: xs -> f x :: map f xs

  let rec count_occurrences v lst = match lst with
    | [] -> 0
    | x :: xs -> (if x = v then 1 else 0) + count_occurrences v xs
end

(* Statistics Calculator type and functions *)
type statistics_cache = {
  mutable mean_cached: float option;
  mutable median_cached: float option;
  mutable mode_cached: int list option;
  mutable std_dev_sample_cached: float option;
  mutable std_dev_population_cached: float option;
  mutable range_cached: int option;
}

type statistics_calculator = {
  mutable data: int list;
  mutable sorted_data: int list option;
  cache: statistics_cache;
}

let create_calculator () = {
  data = [];
  sorted_data = None;
  cache = {
    mean_cached = None;
    median_cached = None;
    mode_cached = None;
    std_dev_sample_cached = None;
    std_dev_population_cached = None;
    range_cached = None;
  }
}

let clear_cache calc =
  calc.cache.mean_cached <- None;
  calc.cache.median_cached <- None;
  calc.cache.mode_cached <- None;
  calc.cache.std_dev_sample_cached <- None;
  calc.cache.std_dev_population_cached <- None;
  calc.cache.range_cached <- None;
  calc.sorted_data <- None

let add_value calc value =
  calc.data <- calc.data @ [value];
  clear_cache calc

let add_values calc values =
  calc.data <- calc.data @ values;
  clear_cache calc

let clear_data calc =
  calc.data <- [];
  clear_cache calc

let get_sorted_data calc =
  match calc.sorted_data with
  | Some sorted -> sorted
  | None ->
    let sorted = List.sort compare calc.data in
    calc.sorted_data <- Some sorted;
    sorted

let calculate_mean calc =
  match calc.cache.mean_cached with
  | Some mean -> mean
  | None ->
    if List.length calc.data = 0 then
      (Printf.printf "Error: Cannot calculate mean - data is empty\n"; 0.0)
    else
      let sum = ListUtils.sum calc.data in
      let count = ListUtils.length calc.data in
      let mean = float_of_int sum /. float_of_int count in
      calc.cache.mean_cached <- Some mean;
      mean

let calculate_median calc =
  match calc.cache.median_cached with
  | Some median -> median
  | None ->
    if List.length calc.data = 0 then
      (Printf.printf "Error: Cannot calculate median - data is empty\n"; 0.0)
    else
      let sorted = get_sorted_data calc in
      let n = List.length sorted in
      let median = if n mod 2 = 0 then
        let mid = n / 2 in
        let left = List.nth sorted (mid - 1) in
        let right = List.nth sorted mid in
        (float_of_int left +. float_of_int right) /. 2.0
      else
        float_of_int (List.nth sorted (n / 2))
      in
      calc.cache.median_cached <- Some median;
      median

let calculate_mode calc =
  match calc.cache.mode_cached with
  | Some modes -> modes
  | None ->
    if List.length calc.data = 0 then (
      Printf.printf "Error: Cannot calculate mode - data is empty\n";
      []
    ) else
      let sorted = get_sorted_data calc in
      let unique_values = 
        let rec get_unique lst seen = match lst with
          | [] -> []
          | x :: xs -> if List.mem x seen then get_unique xs seen 
                       else x :: get_unique xs (x :: seen)
        in
        List.sort compare (get_unique sorted [])
      in
      let max_freq = 
        List.fold_left 
          (fun acc v -> max acc (ListUtils.count_occurrences v sorted))
          0 
          unique_values
      in
      let modes = 
        ListUtils.filter 
          (fun v -> ListUtils.count_occurrences v sorted = max_freq) 
          unique_values
      in
      calc.cache.mode_cached <- Some modes;
      modes

let calculate_range calc =
  match calc.cache.range_cached with
  | Some range -> range
  | None ->
    if List.length calc.data = 0 then (
      Printf.printf "Error: Cannot calculate range - data is empty\n";
      0
    ) else
      let sorted = get_sorted_data calc in
      let min_val = List.nth sorted 0 in
      let max_val = List.nth sorted (List.length sorted - 1) in
      let range = max_val - min_val in
      calc.cache.range_cached <- Some range;
      range

let calculate_std_dev calc population =
  let cache_ref = if population then 
    calc.cache.std_dev_population_cached 
  else 
    calc.cache.std_dev_sample_cached 
  in
  match cache_ref with
  | Some std_dev -> std_dev
  | None ->
    if List.length calc.data = 0 then (
      Printf.printf "Error: Cannot calculate standard deviation - data is empty\n";
      0.0
    ) else if not population && List.length calc.data < 2 then (
      Printf.printf "Error: Need at least 2 data points for sample standard deviation\n";
      0.0
    ) else
      let mean = calculate_mean calc in
      let count = float_of_int (List.length calc.data) in
      let sum_sq = 
        List.fold_left 
          (fun acc x -> 
            let diff = float_of_int x -. mean in
            acc +. diff *. diff)
          0.0 
          calc.data
      in
      let divisor = if population then count else count -. 1.0 in
      let variance = sum_sq /. divisor in
      let std_dev = sqrt variance in
      (if population then
        calc.cache.std_dev_population_cached <- Some std_dev
      else
        calc.cache.std_dev_sample_cached <- Some std_dev);
      std_dev

let print_summary calc =
  if List.length calc.data = 0 then
    Printf.printf "StatisticsCalculator: No data available\n"
  else (
    Printf.printf "Statistics Calculator Summary:\n";
    Printf.printf "Data Points: %d\n" (List.length calc.data);
    
    let sorted = get_sorted_data calc in
    let min_val = List.nth sorted 0 in
    let max_val = List.nth sorted (List.length sorted - 1) in
    Printf.printf "Min: %d, Max: %d, Range: %d\n" min_val max_val (calculate_range calc);
    
    Printf.printf "Mean: %.4f\n" (calculate_mean calc);
    Printf.printf "Median: %.1f\n" (calculate_median calc);
    
    let modes = calculate_mode calc in
    Printf.printf "Mode(s): ";
    (match modes with
    | [] -> Printf.printf "N/A"
    | _ -> Printf.printf "%s" (String.concat ", " (ListUtils.map string_of_int modes)));
    Printf.printf "\n";
    
    if List.length calc.data >= 2 then
      Printf.printf "Sample Std Dev: %.4f\n" (calculate_std_dev calc false)
    else
      Printf.printf "Sample Std Dev: N/A\n";
    
    Printf.printf "Population Std Dev: %.4f\n" (calculate_std_dev calc true)
  )

(* Examples *)

let example_1 () =
  Printf.printf "\n========== Example 1: Basic Statistics ==========\n";
  let calc = create_calculator () in
  let data = [1; 2; 2; 3; 4; 5; 5; 5; 6] in
  add_values calc data;
  
  Printf.printf "Data: ";
  List.iter (Printf.printf "%d ") data;
  Printf.printf "\n";
  Printf.printf "Mean: %.2f\n" (calculate_mean calc);
  Printf.printf "Median: %.1f\n" (calculate_median calc);
  Printf.printf "Mode: ";
  Printf.printf "%s\n" (String.concat ", " (ListUtils.map string_of_int (calculate_mode calc)))

let example_2 () =
  Printf.printf "\n========== Example 2: Complete Summary ==========\n";
  let calc = create_calculator () in
  let data = [10; 20; 30; 40; 50; 60; 70; 80; 90; 100] in
  add_values calc data;
  print_summary calc

let example_3 () =
  Printf.printf "\n========== Example 3: Dynamic Data Manipulation ==========\n";
  let calc = create_calculator () in
  
  Printf.printf "Initial data: [1, 2, 3]\n";
  add_values calc [1; 2; 3];
  Printf.printf "Initial mean: %.2f\n" (calculate_mean calc);
  
  Printf.printf "\nAfter adding values [4, 5]:\n";
  add_values calc [4; 5];
  Printf.printf "Data: [1, 2, 3, 4, 5]\n";
  Printf.printf "New mean: %.2f\n" (calculate_mean calc);
  
  Printf.printf "\nAfter adding multiple values [6, 7, 8]:\n";
  add_values calc [6; 7; 8];
  Printf.printf "Final statistics:\n";
  print_summary calc

let example_4 () =
  Printf.printf "\n========== Example 4: Edge Cases ==========\n";
  
  Printf.printf "Empty calculator:\n";
  let calc = create_calculator () in
  Printf.printf "Count: %d\n" (List.length calc.data);
  Printf.printf "Mean: %.4f\n" (calculate_mean calc);
  
  Printf.printf "\nSingle value [42]:\n";
  let calc = create_calculator () in
  add_value calc 42;
  Printf.printf "Mean: %.2f\n" (calculate_mean calc);
  Printf.printf "Median: %.1f\n" (calculate_median calc);
  Printf.printf "Mode: %d\n" (List.nth (calculate_mode calc) 0)

let example_5 () =
  Printf.printf "\n========== Example 5: Multiple Modes ==========\n";
  let calc = create_calculator () in
  let data = [1; 1; 2; 2; 3; 3; 4] in
  add_values calc data;
  
  Printf.printf "Data: ";
  List.iter (Printf.printf "%d ") data;
  Printf.printf "\n";
  Printf.printf "Mode(s): %s (All values appear twice except 4)\n" 
    (String.concat ", " (ListUtils.map string_of_int (calculate_mode calc)))

let example_6 () =
  Printf.printf "\n========== Example 6: Exam Scores Analysis ==========\n";
  let calc = create_calculator () in
  let exam_scores = [85; 92; 78; 92; 85; 67; 85; 92; 74; 88; 90; 85] in
  add_values calc exam_scores;
  
  Printf.printf "Exam Scores: ";
  List.iter (Printf.printf "%d ") exam_scores;
  Printf.printf "\n\nAnalysis:\n";
  Printf.printf "Number of students: %d\n" (List.length exam_scores);
  Printf.printf "Average score: %.1f\n" (calculate_mean calc);
  Printf.printf "Median score: %.1f\n" (calculate_median calc);
  
  Printf.printf "Most common score(s): ";
  Printf.printf "%s\n" (String.concat ", " (ListUtils.map string_of_int (calculate_mode calc)));
  
  Printf.printf "Score range: %d\n" (calculate_range calc);
  Printf.printf "Standard deviation: %.2f\n" (calculate_std_dev calc false);
  
  (* Outlier detection *)
  let mean = calculate_mean calc in
  let std_dev = calculate_std_dev calc false in
  let lower_bound = mean -. 2.0 *. std_dev in
  let upper_bound = mean +. 2.0 *. std_dev in
  
  Printf.printf "\nOutlier detection (±2 std dev):\n";
  Printf.printf "Lower bound: %.2f, Upper bound: %.2f\n" lower_bound upper_bound;
  
  let outliers = 
    ListUtils.filter 
      (fun x -> float_of_int x < lower_bound || float_of_int x > upper_bound) 
      exam_scores
  in
  
  match outliers with
  | [] -> Printf.printf "No significant outliers found.\n"
  | _ -> 
    Printf.printf "Potential outliers: ";
    List.iter (Printf.printf "%d ") outliers;
    Printf.printf "\n"

let () =
  Printf.printf "============================================================\n";
  Printf.printf "        Statistics Calculator Demonstration (OCaml Version)\n";
  Printf.printf "============================================================\n";
  
  example_1 ();
  example_2 ();
  example_3 ();
  example_4 ();
  example_5 ();
  example_6 ();
  
  Printf.printf "\n============================================================\n";
  Printf.printf "              All examples completed successfully!\n";
  Printf.printf "============================================================\n"
