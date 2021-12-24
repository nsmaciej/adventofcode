use ahash::AHashMap;
use std::collections::BinaryHeap;

const FREE: i8 = -1;
const DOOR: i8 = -2;

// Clean hallway.
const HALLWAY: [i8; 11] = [
    FREE, FREE, DOOR, FREE, DOOR, FREE, DOOR, FREE, DOOR, FREE, FREE,
];
// Exit indices into the hallway.
const EXITS: [usize; 4] = [2, 4, 6, 8];
// Energy for a given amphipod.
const ENERGIES: [usize; 4] = [1, 10, 100, 1000];

#[derive(Debug, PartialEq, Eq, Clone, PartialOrd, Ord, Hash)]
struct State {
    rooms: [i8; 8],
    hallway: [i8; 11],
}

impl State {
    fn hallway_to_room(&self, e: i32, h: usize, t: usize, depth: usize) -> Option<(i32, State)> {
        // Check there is nothing in our way. Note we skip h itself.
        let range = if h < EXITS[t] {
            h + 1..=EXITS[t] - 1
        } else {
            EXITS[t] + 1..=h - 1
        };
        for i in range {
            if self.hallway[i] >= 0 {
                return None;
            }
        }
        let mut next = self.clone();
        next.rooms[t + depth * 4] = self.hallway[h];
        next.hallway[h] = FREE;
        // Save to use t for energy since we have to be moving into the final room.
        let cost = (1 + depth + EXITS[t].abs_diff(h)) * ENERGIES[t];
        Some((e - cost as i32, next))
    }

    fn room_to_hallway(&self, e: i32, t: usize, depth: usize, h: usize) -> (i32, State) {
        let mut next = self.clone();
        next.hallway[h] = self.rooms[t + depth * 4];
        next.rooms[t + depth * 4] = FREE;
        // Note t here is where we are, not who we are, check the energy accordingly.
        let cost =
            (1 + depth + EXITS[t].abs_diff(h)) * ENERGIES[self.rooms[t + depth * 4] as usize];
        (e - cost as i32, next)
    }

    fn try_hallways(&self, e: i32, t: usize, depth: usize) -> Vec<(i32, State)> {
        let mut r = Vec::new();
        // Try left.
        for i in (0..EXITS[t]).rev() {
            match self.hallway[i] {
                DOOR => continue,
                FREE => r.push(self.room_to_hallway(e, t, depth, i)),
                _ => break,
            };
        }
        // Try right.
        for i in EXITS[t] + 1..HALLWAY.len() {
            match self.hallway[i] {
                DOOR => continue,
                FREE => r.push(self.room_to_hallway(e, t, depth, i)),
                _ => break,
            };
        }
        r
    }
}

#[allow(dead_code)]
fn show(state: &State) {
    fn c(t: i8) -> char {
        match t {
            FREE => '.',
            DOOR => '.',
            _ => (t as u8 + b'A') as char,
        }
    }

    let rs = state.rooms;
    let hallway: String = state.hallway.iter().map(|x| c(*x)).collect();
    println!("#############");
    println!("#{hallway}#");
    println!("###{}#{}#{}#{}###", c(rs[0]), c(rs[1]), c(rs[2]), c(rs[3]));
    println!("  #{}#{}#{}#{}#  ", c(rs[4]), c(rs[5]), c(rs[6]), c(rs[7]));
    println!("  #########  ");
    println!();
}

fn find_soln(state: State) -> i32 {
    let mut heap = BinaryHeap::<(i32, State)>::new();
    let mut best = AHashMap::<State, i32>::new();
    heap.push((0, state));

    while let Some((e, state @ State { rooms, hallway, .. })) = heap.pop() {
        if rooms[0..4] == [0, 1, 2, 3] {
            return -e;
        }

        if *best.get(&state).unwrap_or(&i32::MAX) < -e {
            continue;
        }

        let mut futures = Vec::new();
        let mut solved_one = false;
        for h in 0..HALLWAY.len() {
            if hallway[h] < 0 {
                continue;
            }
            // Move into the bottom-most free room.
            let t = hallway[h] as usize;
            if rooms[4 + t] == FREE {
                futures.extend(state.hallway_to_room(e, h, t, 1));
                solved_one = true;
                break;
            } else if rooms[4 + t] == t as i8 && rooms[t] == FREE {
                futures.extend(state.hallway_to_room(e, h, t, 0));
                solved_one = true;
                break;
            }
        }

        if !solved_one {
            for t in 0..4 {
                // Move top ap if it's in the wrong room OR there is one under it that is.
                if rooms[t] >= 0 && (rooms[t] != t as i8 || rooms[4 + t] != t as i8) {
                    futures.extend_from_slice(&state.try_hallways(e, t, 0));
                } else if rooms[t] == FREE && rooms[4 + t] >= 0 && rooms[4 + t] != t as i8 {
                    futures.extend_from_slice(&state.try_hallways(e, t, 1));
                }
            }
        }

        for (next_e, next) in futures {
            if -next_e < *best.get(&next).unwrap_or(&i32::MAX) {
                best.insert(next.clone(), -next_e);
                heap.push((next_e, next));
            }
        }
    }
    panic!("no solution found");
}

pub fn solve(input: Vec<Vec<u8>>) -> (i32, i32) {
    let mut rooms = Vec::new();
    for y in [2, 3] {
        for x in [3, 5, 7, 9] {
            if input[y][x] == b'.' {
                // For testing.
                rooms.push(FREE);
            } else {
                rooms.push((input[y][x] - b'A') as i8);
            }
        }
    }
    // For testing.
    let mut hallway = HALLWAY;
    for x in 0..HALLWAY.len() {
        if input[1][1 + x] != b'.' {
            hallway[x] = (input[1][1 + x] - b'A') as i8;
        }
    }

    let part1 = find_soln(State {
        rooms: rooms.try_into().unwrap(),
        hallway: hallway,
    });
    (part1, 0)
}
