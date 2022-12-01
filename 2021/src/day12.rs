use std::fs::File;
use std::io::BufRead;
use std::process;
//  use std::collections::HashMap;

struct Edge {
    source: String,
    target: String,
}

struct Graph {
    edges: Vec<Edge>,
}

fn is_uppercase(s: &str) -> bool {
    for c in s.chars() {
        if !c.is_uppercase() {
            return false;
        }
    }
    true
}

impl Graph {

    pub fn new(filename: &str) -> Result<Graph, std::io::Error> {
        let file = File::open(filename)?;
        let edges: Vec<Edge> = std::io::BufReader::new(file)
            .lines()
            .map(|line| {
                let li = line.unwrap();
                let mut s = li.split('-');
                let source = String::from(s.next().unwrap());
                let target = String::from(s.next().unwrap());
                [
                    Edge {source: target.clone(), target: source.clone()},
                    Edge {source, target},
                ]
            })
            .flatten()
            .collect();
        Ok(Graph { edges })
    }

    pub fn dfs<F>(&self, start: &str, end: &str, mut on_edge: F) -> ()
        where F: FnMut(Vec<&str>)->()
    {
        let mut queue = vec![
            (start, self.edges.iter()),
        ];

        while !queue.is_empty() {
            let last = queue.last_mut().unwrap();
            let current = last.0;
            let child = last.1.next();
            match child {
                Some(e) => {
                    if e.source == current {
                        if e.target == end {
                            let mut path: Vec<&str> =
                                queue.iter().map(|(n, _)| *n).collect();
                            path.push(end);
                            on_edge(path);
                        } else if is_uppercase(&e.target)
                            || queue.iter().all(|(n, _)| n != &e.target)
                        {
                            queue.push((&e.target, self.edges.iter()));
                        }
                    }
                },
                None => {
                    queue.pop();
                },
            }
        }
    }

    pub fn dfs_visit_twice<F>(
        &self, start: &str, end: &str, mut on_edge: F
    ) -> ()
        where F: FnMut(Vec<&str>)->()
    {
        let mut queue = vec![
            (start, self.edges.iter(), false),
        ];

        while !queue.is_empty() {
            let last = queue.last_mut().unwrap();
            let current = last.0;
            let child = last.1.next();
            match child {
                Some(e) => {
                    if e.source == current {
                        if e.target == end {
                            let mut path: Vec<&str> =
                                queue.iter()
                                   .map(|(n, _, _)| *n).collect();
                            path.push(end);
                            on_edge(path);
                        } else if is_uppercase(&e.target)
                            || queue.iter()
                                .all(|(n, _, _)| n != &e.target)
                        {
                            queue.push(
                                (&e.target, self.edges.iter(), false));
                        } else if e.target != start {
                            // it is lower-cased, and already visited.
                            // Only allowed if none other was also
                            // already visited
                            let has_visited = queue
                                .iter()
                                .any(|(_, _, visited)| *visited);
                            if !has_visited {
                                queue.push(
                                    (&e.target, self.edges.iter(), true));
                            }
                        }
                    }
                },
                None => {
                    queue.pop();
                },
            }
        }
    }
}

pub fn part1() {
    assert_eq!(is_uppercase("A"), true);
    assert_eq!(is_uppercase("a"), false);
    assert_eq!(is_uppercase("Aa"), false);

    let graph = Graph::new("input_day12").unwrap_or_else(|e| {
        println!("file not found input_day12: {}", e);
        process::exit(1);
    });
    let mut count = 0;
    graph.dfs("start", "end", |_e| {
        //  println!("{:?}", e);
        count += 1;
    });
    println!("Day12 Part1: count paths={}", count);
}

pub fn part2() {
    let graph = Graph::new("input_day12.small1").unwrap_or_else(|e| {
        println!("file not found input_day12: {}", e);
        process::exit(1);
    });
    let mut count = 0;
    graph.dfs_visit_twice("start", "end", |_e| {
        // println!("{:?}", e);
        count += 1;
    });
    println!("Day12 Part2: count paths={}", count);
}
