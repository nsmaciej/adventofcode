//! Packet Decoder

use bitvec::prelude::*;

#[derive(Debug)]
struct Reader {
    packet: BitVec<Msb0, u8>,
    pos: usize,
}

impl Reader {
    fn slice(&mut self, k: usize) -> &BitSlice<Msb0, u8> {
        let r = &self.packet[self.pos..self.pos + k];
        self.pos += k;
        r
    }
    fn bit(&mut self) -> bool {
        self.slice(1)[0]
    }
    fn load<T: bitvec::mem::BitMemory>(&mut self, k: usize) -> T {
        self.slice(k).load_be()
    }
}

fn parse_packet(reader: &mut Reader) -> (u64, u64) {
    let version: u8 = reader.load(3);
    let type_id: u8 = reader.load(3);

    if type_id == 4 {
        let mut value = BitVec::<Msb0, u8>::new();
        loop {
            let last = !reader.bit();
            value.extend(reader.slice(4));
            if last {
                break;
            }
        }
        return (version as u64, value.load_be());
    }

    let results = if reader.bit() {
        let ops_count: u64 = reader.load(11);
        (0..ops_count).map(|_| parse_packet(reader)).collect()
    } else {
        let ops_len = reader.load::<usize>(15);
        let target_pos = reader.pos + ops_len;
        let mut results = Vec::new();
        while reader.pos != target_pos {
            results.push(parse_packet(reader));
        }
        results
    };

    let mut values = results.iter().map(|x| x.1);
    let result = match type_id {
        0 => values.sum(),
        1 => values.product(),
        2 => values.min().unwrap(),
        3 => values.max().unwrap(),
        5 => (values.next().unwrap() > values.next().unwrap()) as u64,
        6 => (values.next().unwrap() < values.next().unwrap()) as u64,
        7 => (values.next().unwrap() == values.next().unwrap()) as u64,
        _ => panic!("invalid type id"),
    };

    let version_sum = results.iter().map(|x| x.0).sum();
    (version_sum, result)
}

pub fn solve(input: String) -> (u64, u64) {
    let bytes = (0..input.len())
        .step_by(2)
        .map(|i| u8::from_str_radix(&input[i..i + 2], 16).unwrap())
        .collect();
    parse_packet(&mut Reader {
        packet: BitVec::from_vec(bytes),
        pos: 0,
    })
}
