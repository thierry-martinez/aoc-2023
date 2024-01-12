use advent_of_code::{
    Error, Result, Matrix2D, Coords2D, Zero, matrix_from_lines
};

#[derive(Clone, PartialEq, Eq, Hash)] 
struct Visit {
    from: Coords2D<usize>,
    direction: Coords2D<isize>,
    count_forward: u64,
}

struct VisitQueue {
    queue: Vec<Visit>,
    heat_loss: std::collections::HashMap<Visit, u64>,
}

impl VisitQueue {
    fn new() -> VisitQueue {
        VisitQueue {
            queue: Vec::new(),
            heat_loss: std::collections::HashMap::new(),
        }
    }

    fn push(
        &mut self, from: Coords2D<usize>, direction: Coords2D<isize>,
        count_forward: u64, heat_loss: u64
    ) {
        let visit = Visit { from, direction, count_forward };
        match self.heat_loss.entry(visit.clone()) {
            std::collections::hash_map::Entry::Vacant(vacant) => {
                self.queue.push(visit);
                vacant.insert(heat_loss);
            }
            std::collections::hash_map::Entry::Occupied(mut occupied) => {
                let value = occupied.get_mut();
                if heat_loss < *value {
                    *value = heat_loss;
                }
            }
        }
    }

    fn pop(&mut self) -> Option<(Visit, u64)> {
        let (visit, &heat_loss) = 
            self.heat_loss.iter().min_by_key(|(_visit, &heat_loss)| heat_loss)?;
        let visit = visit.clone();
        self.heat_loss.remove(&visit)?;
        let (index, _v) =
            self.queue.iter().enumerate().find(|(_index, v)| **v == visit)?;
        self.queue.remove(index);
        Some((visit, heat_loss))
    }
}

fn find_path(
    grid: &Matrix2D<u32>, min_turn: u64, max_forward: u64,
    from: Coords2D<usize>, to: Coords2D<usize>
) -> Result<u64> {
    let mut visit_queue = VisitQueue::new();
    visit_queue.push(from, Coords2D::RIGHT, 1, 0);
    visit_queue.push(from, Coords2D::DOWN, 1, 0);
    loop {
        let (visit, heat_loss) = visit_queue.pop().ok_or("Empty queue")?;
        let Some(pos) = visit.from.advance(grid.into(), visit.direction)
        else { continue };
        let heat_loss = heat_loss + *pos.get(grid) as u64;
        if visit.count_forward >= min_turn {
            if pos == to {
                return Ok(heat_loss)
            }
            visit_queue.push(
                pos, visit.direction.turn_clockwise(), 1, heat_loss
            );
            visit_queue.push(
                pos, visit.direction.turn_anticlockwise(), 1, heat_loss
            );
        }
        if visit.count_forward < max_forward {
            visit_queue.push(
                pos, visit.direction, visit.count_forward + 1, heat_loss
            );
        }
    }
}

fn main() -> Result<()> {
    let char_grid = matrix_from_lines(std::io::stdin().lines())?;
    let grid: Matrix2D<u32> = char_grid.iter().map(|line|
      line.iter().map(|c| c.to_digit(10)
          .ok_or(Error::from(format!("Invalid digit {c}"))))
      .collect::<Result<_>>()
    ).collect::<Result<_>>()?;
    let from: Coords2D<usize> = Coords2D::ZERO;
    let to: Coords2D<usize> =
        Coords2D::from(&grid).checked_add_signed(Coords2D::UP_LEFT)
        .ok_or("Empty grid")?;
    let result_part1 = find_path(&grid, 0, 3, from, to)?;
    println!("Part 1: {result_part1}");
    let result_part2 = find_path(&grid, 4, 10, from, to)?;
    println!("Part 2: {result_part2}");
    Ok(())
}
