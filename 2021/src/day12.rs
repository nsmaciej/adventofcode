use itertools::Itertools;
use std::collections::HashMap;

fn visit<'a, 'b: 'a>(
    graph: &'b HashMap<String, Vec<String>>,
    visited: &mut HashMap<&'a str, i64>,
    point: &str,
    max_visits: i64,
    visited_twice: bool,
) -> i64 {
    if point == "end" {
        return 1;
    }
    let mut sum = 0;
    for option in &graph[point] {
        let small = option.chars().next().unwrap().is_lowercase();
        let times = *visited.get(option.as_str()).unwrap_or(&0);
        if small && (times == 0 || times == 1 && !visited_twice) {
            *visited.entry(option.as_str()).or_default() += 1;
            sum += visit(
                graph,
                visited,
                option,
                max_visits,
                visited_twice || times == 1,
            );
            *visited.entry(option).or_default() -= 1;
        } else if !small {
            sum += visit(graph, visited, option, max_visits, visited_twice);
        }
    }
    sum
}

pub fn solve(input: Vec<String>) -> (i64, i64) {
    let mut graph = HashMap::<String, Vec<String>>::new();
    for line in input {
        let (a, b) = line
            .split('-')
            .map(|x| x.to_string())
            .collect_tuple()
            .unwrap();
        graph.entry(a.clone()).or_default().push(b.clone());
        graph.entry(b).or_default().push(a);
    }
    let mut visited = HashMap::new();
    visited.insert("start", 2);
    let part1 = visit(&graph, &mut visited, "start", 1, true);
    let part2 = visit(&graph, &mut visited, "start", 2, false);
    (part1, part2)
}
