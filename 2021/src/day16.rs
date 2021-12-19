//! Packet Decoder

#[derive(Debug)]
struct Reader {
    packet: Vec<u8>,
    pos: usize,
}

impl Reader {
    fn bit(&mut self) -> bool {
        let r = self.packet[self.pos / 4] & (1 << 3 - self.pos % 4) > 0;
        self.pos += 1;
        r
    }

    fn load(&mut self, k: usize) -> u64 {
        (0..k).fold(0, |x, _| (x << 1) | self.bit() as u64)
    }
}

fn parse_packet(reader: &mut Reader, version_sum: &mut u64) -> u64 {
    *version_sum += reader.load(3);
    let type_id = reader.load(3);

    if type_id == 4 {
        let mut value = 0;
        let mut last = false;
        while !last {
            last = !reader.bit();
            value = (value << 4) | (reader.load(4));
        }
        return value;
    }

    let mut values = Vec::new();
    if reader.bit() {
        let ops_count: u64 = reader.load(11);
        for _ in 0..ops_count {
            values.push(parse_packet(reader, version_sum));
        }
    } else {
        let ops_len = reader.load(15) as usize;
        let target_pos = reader.pos + ops_len;
        while reader.pos != target_pos {
            values.push(parse_packet(reader, version_sum));
        }
    };

    match type_id {
        0 => values.iter().sum(),
        1 => values.iter().product(),
        2 => *values.iter().min().unwrap(),
        3 => *values.iter().max().unwrap(),
        5 => (values[0] > values[1]) as u64,
        6 => (values[0] < values[1]) as u64,
        7 => (values[0] == values[1]) as u64,
        _ => panic!("invalid type id"),
    }
}

pub fn solve(input: String) -> (u64, u64) {
    let packet = input
        .chars()
        .map(|x| x.to_digit(16).unwrap() as u8)
        .collect();
    let mut version_sum = 0;
    let result = parse_packet(&mut Reader { packet, pos: 0 }, &mut version_sum);
    (version_sum, result)
}
