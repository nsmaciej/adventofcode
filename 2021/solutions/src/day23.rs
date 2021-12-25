use ahash::AHashMap;
use smallvec::{smallvec, SmallVec};
use std::collections::BinaryHeap;

const FREE: i8 = -1;
const DOOR: i8 = -2;

const HALLWAY_LEN: usize = 11;
const EXIT: [usize; 4] = [2, 4, 6, 8]; // Exit indices into the hallway.
const ENERGY: [usize; 4] = [1, 10, 100, 1000]; // Energy for a given amphipod.

// Implementation note: I tried making the hallway length use only 7 bytes, but
// it made the cost maths prohibitively complex. Rooms would also be cleaner as
// a multi-dimensional array, but that actually makes the program quite a bit
// slower (I checked).
#[derive(Debug, PartialEq, Eq, Clone, PartialOrd, Ord, Hash)]
struct State<const R: usize> {
    rooms: [i8; R],
    hallway: [i8; HALLWAY_LEN],
}

impl<const R: usize> State<R> {
    fn find_move_into_room(&self) -> Option<(i32, State<R>)> {
        let State { rooms, hallway } = self;
        for h in 0..HALLWAY_LEN {
            if hallway[h] < 0 {
                continue; // Not an amphipod.
            }

            let t = hallway[h] as usize;
            if h < EXIT[t] && self.hallway[h + 1..=EXIT[t] - 1].iter().any(|x| *x >= 0)
                || h >= EXIT[t] && self.hallway[EXIT[t] + 1..=h - 1].iter().any(|x| *x >= 0)
            {
                continue; // Hallway collision.
            }

            let Some(depth) = (0..R / 4).take_while(|d| rooms[4 * d + t] == FREE).last() else {
                continue; // No spare room.
            };
            if (depth + 1..R / 4).any(|d| rooms[4 * d + t] != t as i8) {
                continue; // Stragglers in the destination room.
            }

            let mut next = self.clone();
            next.rooms[4 * depth + t] = self.hallway[h];
            next.hallway[h] = FREE;
            let cost = (1 + depth + EXIT[t].abs_diff(h)) * ENERGY[t];
            // Simply moving into the destination room is always the best move.
            return Some((cost as i32, next));
        }
        None
    }

    fn move_into_hallway(&self, t: usize, depth: usize, h: usize) -> (i32, State<R>) {
        let mut next = self.clone();
        next.hallway[h] = self.rooms[t + depth * 4];
        next.rooms[t + depth * 4] = FREE;
        // Note t here is room we are in, not our identity, check the energy accordingly.
        let cost = (1 + depth + EXIT[t].abs_diff(h)) * ENERGY[next.hallway[h] as usize];
        (cost as i32, next)
    }

    fn find_moves(&self) -> SmallVec<[(i32, State<R>); 6]> {
        if let Some(next) = self.find_move_into_room() {
            return smallvec![next];
        }

        // Otherwise try moving into the hallway.
        let mut nexts = SmallVec::new(); // Covers 95% of the lengths.
        for t in 0..4 {
            let top_depth = (0..R / 4)
                .take_while(|dt| self.rooms[4 * dt + t] == FREE)
                .count();
            if (top_depth..R / 4).all(|dt| self.rooms[4 * dt + t] == t as i8) {
                continue; // Everyone in this room belongs there.
            }

            // Move left and right, skipping over doors (room entrances).
            for i in (0..EXIT[t]).rev() {
                match self.hallway[i] {
                    DOOR => continue,
                    FREE => nexts.push(self.move_into_hallway(t, top_depth, i)),
                    _ => break,
                };
            }
            for i in EXIT[t] + 1..HALLWAY_LEN {
                match self.hallway[i] {
                    DOOR => continue,
                    FREE => nexts.push(self.move_into_hallway(t, top_depth, i)),
                    _ => break,
                };
            }
        }
        nexts
    }
}

fn solve_rooms<const R: usize>(rooms: Vec<i8>) -> i32 {
    let mut best = AHashMap::<State<R>, i32>::new();
    let mut heap = BinaryHeap::<(i32, State<R>)>::new();

    let start = State {
        rooms: rooms.try_into().unwrap(),
        hallway: [
            FREE, FREE, DOOR, FREE, DOOR, FREE, DOOR, FREE, DOOR, FREE, FREE,
        ],
    };
    best.insert(start.clone(), 0);
    heap.push((0, start));

    while let Some((e, state)) = heap.pop() {
        if state.rooms[0..4] == [0, 1, 2, 3] {
            return -e;
        }
        for (cost, next) in state.find_moves() {
            if e - cost > *best.get(&next).unwrap_or(&i32::MIN) {
                best.insert(next.clone(), e - cost);
                heap.push((e - cost, next));
            }
        }
    }
    panic!("no solution found");
}

pub fn solve(input: Vec<Vec<u8>>) -> (i32, i32) {
    let mut rooms = Vec::new();
    for y in [2, 3] {
        for x in [3, 5, 7, 9] {
            if input[y][x] != b'.' {
                rooms.push((input[y][x] - b'A') as i8);
            }
        }
    }
    (
        solve_rooms::<8>(rooms.clone()),
        solve_rooms::<16>([&rooms[0..4], &[3, 2, 1, 0, 3, 1, 0, 2], &rooms[4..8]].concat()),
    )
}
