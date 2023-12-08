fn number_ways(time: u64, distance: u64) -> u64 {
    /* #hold s.t. (time - hold) * hold > distance
       i.e. - hold ** 2 + time * hold - distance > 0
     */
    let time = time as f64;
    let distance = distance as f64;
    let delta = time.powf(2.) - 4.0 * distance;
    let sqrt_delta = delta.sqrt();
    let alpha = - (- time - sqrt_delta) / 2.;
    let beta = - (- time + sqrt_delta) / 2.;
    let max_holding_time =
	if alpha.floor() == alpha {
	    alpha - 1.0
	}
	else {
	    alpha.floor()
	};
    let min_holding_time =
	if beta.ceil() == beta {
	    beta + 1.0
	}
	else {
	    beta.ceil()
	};
    (max_holding_time - min_holding_time) as u64 + 1
}

fn part1(times_str: &str, distances_str: &str) -> u64 {
    let times = times_str.split(" ").filter_map(|s| s.parse().ok());
    let distances = distances_str.split(" ").filter_map(|s| s.parse().ok());
    times.zip(distances).map(|(time, distance)| number_ways(time, distance)).product()
}

fn part2(times_str: &str, distances_str: &str) -> u64 {
    let time = times_str.replace(" ", "").parse().unwrap();
    let distance = distances_str.replace(" ", "").parse().unwrap();
    number_ways(time, distance)
}

fn main() {
    let mut lines = std::io::stdin().lines();
    let times_line = lines.next().unwrap().unwrap();
    let times_str = times_line.splitn(2, ":").skip(1).next().unwrap();
    let distances_line = lines.next().unwrap().unwrap();
    let distances_str = distances_line.splitn(2, ":").skip(1).next().unwrap();
    let result_part1: u64 = part1(times_str, distances_str);
    let result_part2: u64 = part2(times_str, distances_str);
    println!("Part 1: {result_part1}");
    println!("Part 2: {result_part2}");
}
