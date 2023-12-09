fn main() {
    let mut lines = std::io::stdin().lines();
    let directions = lines.next().unwrap().unwrap();
    let map: std::collections::HashMap<String, (String, String)> = lines.skip(1).map(
	|line| {
	    let line = line.unwrap();
	    let mut parts = line.splitn(2, " = (");
	    let src = parts.next().unwrap();
	    let mut sides = parts.next().unwrap().splitn(2, ", ");
	    let left = sides.next().unwrap();
	    let mut chars = sides.next().unwrap().chars();
	    chars.next_back();
	    let right = chars.as_str();
	    (src.to_string(), (left.to_string(), right.to_string()))
	}
    ).collect();
    let result_part1: u64 =
	path_length(&map, &mut directions.chars().cycle(), "AAA", |place| place == "ZZZ");
    let ghosts = map.keys().filter(|place| place.chars().last().unwrap() == 'A');
    // Not a general solution, but crafted for the kind of inputs AoC give...
    let result_part2: u64 =
	ghosts.map(
	    |place| path_length(
		&map, &mut directions.chars().cycle(), place,
		|place| place.chars().last().unwrap() == 'Z'
	    )
	).fold(1, lcm);
    println!("Part 1: {result_part1}");
    println!("Part 2: {result_part2}");
}

fn gcd(mut a: u64, mut b: u64) -> u64 {
    while a % b != 0 {
	(a, b) = (b, a % b)
    }
    b
}

fn lcm(a: u64, b: u64) -> u64 {
    a * b / gcd(a, b)
}

fn path_length(
    map: &std::collections::HashMap<String, (String, String)>,
    directions: &mut impl Iterator<Item = char>,
    start: &str,
    is_end: impl Fn(&str) -> bool
) -> u64 {
    let mut place = start;
    let mut count = 0;
    while !is_end(place) {
	let (left, right) = map.get(place).unwrap();
	place =
	    match directions.next().unwrap() {
		'L' => &left,
		'R' => &right,
		_ => panic!("unknown direction"),
	    };
	count += 1;
    }
    count
}
