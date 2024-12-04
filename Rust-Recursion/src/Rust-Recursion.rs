use std::fs::File;
use std::io::{BufRead, BufReader};
use std::str::FromStr;

fn read_config_file(filename: &str) -> (Vec<Vec<usize>>, usize) 
{
    let file = File::open(filename).expect("Failed to open config file");
    let reader = BufReader::new(file);

    let mut graph = vec![];
    let mut starting_city = 0;

    for line in reader.lines() 
    {
        let line = line.expect("Failed to read line");
        if line.trim().is_empty() || line.starts_with("#") 
        {
            continue;
        }

        let parts: Vec<&str> = line.split(',').collect();
        if parts.len() >= 2 
        {
            let city_id = usize::from_str(parts[0]).expect("Failed to parse city id");
            let distances: Vec<usize> = parts[1..]
                .iter()
                .map(|s| usize::from_str(s.trim()).expect("Failed to parse distance"))
                .collect();

            graph.push(distances);

            // Check if this line specifies the starting city
            if let Ok(key) = parts[0].trim().to_lowercase().as_str().parse::<String>() 
            {
                if key == "starting city" 
                {
                    starting_city = city_id;
                }
            }
        }
    }

    (graph, starting_city)
}

fn tsp_recursive(graph: &[Vec<usize>], visited: &mut Vec<bool>, current: usize, count: usize, cost: usize, min_cost: &mut usize, tour: &mut Vec<usize>, best_tour: &mut Vec<usize>) {
    if count == visited.len() 
    {
        let complete_cost = cost + graph[current][0];
        if complete_cost < *min_cost 
        {
            *min_cost = complete_cost;
            *best_tour = tour.clone();
        }
        return;
    }

    for v in 0..visited.len() 
    {
        if !visited[v] && graph[current][v] != 0 
        {
            visited[v] = true;
            tour.push(v);
            tsp_recursive(graph, visited, v, count + 1, cost + graph[current][v], min_cost, tour, best_tour);
            visited[v] = false;
            tour.pop();
        }
    }
}

fn main() 
{
    let (graph, starting_city) = read_config_file("config.txt");

    let mut visited = vec![false; graph.len()];
    visited[starting_city] = true;
    let mut min_cost = std::usize::MAX;
    let mut tour = vec![starting_city];
    let mut best_tour = vec![starting_city];

    tsp_recursive(&graph, &mut visited, starting_city, 1, 0, &mut min_cost, &mut tour, &mut best_tour);

    best_tour.push(starting_city);

    println!("Minimum Cost: {}", min_cost);
    println!("Tour: {:?}", best_tour);
}