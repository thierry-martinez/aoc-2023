struct CubeSet {
    red: u64,
    green: u64,
    blue: u64,
}

struct Game {
    id: u64,
    sets: Vec<CubeSet>,
}

fn main() {
    let lines = std::io::stdin().lines();
    let games: Vec<_> = lines.map(
	|line| {
	    let line = line.unwrap();
	    let mut line_items = line.splitn(2, ": ");
	    let game_items = line_items.next().unwrap().splitn(2, " ");
	    let id: u64 = game_items.skip(1).next().unwrap().parse().unwrap();
	    let set_items = line_items.next().unwrap().split("; ");
	    let sets: Vec<_> = set_items.map(
		|set_str| {
		    set_str.split(", ").fold(
			CubeSet { red: 0, green: 0, blue: 0 },
			|set, color_cube_str| {
			    let mut items = color_cube_str.splitn(2, " ");
			    let count : u64 = items.next().unwrap().parse().unwrap();
			    let color = items.next().unwrap();
			    match color {
				"red" => CubeSet { red: count, ..set },
				"green" => CubeSet { green: count, ..set },
				"blue" => CubeSet { blue: count, ..set },
				_ => panic!("Unknown color")
			    }
			}
		    )
		}
	    ).collect();
	    Game { id, sets }
	}
    ).collect();
    let valid_games = games.iter().filter(
	|game| game.sets.iter().all(|set| set.red <= 12 && set.green <= 13 && set.blue <= 14)
    );
    let result_part1: u64 = valid_games.map(|game| game.id).sum();
    let fewest_cubes = games.iter().map(
	|game|
	game.sets.iter().map(|set| set.red).max().unwrap() *
	game.sets.iter().map(|set| set.green).max().unwrap() *
	game.sets.iter().map(|set| set.blue).max().unwrap()
    );
    let result_part2: u64 = fewest_cubes.sum();
    println!("Part 1: {result_part1}");
    println!("Part 2: {result_part2}");
}
