use bitvec::prelude::*;

struct ParseResult {
    len: usize,
    version_sum: u64,
    result: u64,
}

fn parse_packet(packet: &BitSlice<Msb0, u8>) -> ParseResult {
    let version: u8 = packet[0..3].load_be();
    let type_id: u8 = packet[3..6].load_be();

    if type_id == 4 {
        let mut len = 6;
        let mut value = BitVec::<Msb0, u8>::new();
        loop {
            let last = !packet[len];
            value.extend(&packet[len + 1..len + 5]);
            len += 5;
            if last {
                break;
            }
        }
        return ParseResult {
            len,
            result: value.load_be(),
            version_sum: version as u64,
        };
    }

    let mut len;
    let mut version_sum = version as u64;

    let mut results = Vec::new();
    if packet[6] {
        let ops_count: u64 = packet[7..18].load_be();
        len = 18;
        for _ in 0..ops_count {
            let op = parse_packet(&packet[len..]);
            len += op.len;
            version_sum += op.version_sum;
            results.push(op.result);
        }
    } else {
        let ops_len: usize = packet[7..22].load_be();
        len = 22;
        while len != 22 + ops_len {
            let op = parse_packet(&packet[len..]);
            len += op.len;
            version_sum += op.version_sum;
            results.push(op.result);
        }
    }

    let result = match type_id {
        0 => results.iter().sum(),
        1 => results.iter().product(),
        2 => *results.iter().min().unwrap(),
        3 => *results.iter().max().unwrap(),
        5 => (results[0] > results[1]) as u64,
        6 => (results[0] < results[1]) as u64,
        7 => (results[0] == results[1]) as u64,
        _ => panic!("invalid type id"),
    };

    ParseResult {
        len,
        version_sum,
        result,
    }
}

pub fn solve(input: String) -> (u64, u64) {
    let mut bytes = Vec::new();
    for i in (0..input.len()).step_by(2) {
        bytes.push(u8::from_str_radix(&input[i..i + 2], 16).unwrap());
    }
    let packet = BitVec::<Msb0, _>::from_vec(bytes);
    let parsed = parse_packet(&packet);
    (parsed.version_sum, parsed.result)
}
