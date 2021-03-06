/// Sort a vector of integers
/// using selection sort algorithm.
fn selection_sort(input: vec<int>) {
    if input.len() == 0 {
        return;
    }

    let mut min: int;
    for i in 0..(input.len() - 1) {
        min = i;

        for j in i..input.len() {
            if input[j] < input[min] {
                min = j;
            }
        }

        if min != i {
            let temp = input[i];
            input[i] = input[min];
            input[min] = temp;
        }
    }
}

fn main() {
    let vector: vec<int> = vec[10, 0, 45, 56, 12, -5, 19, 0];

    selection_sort(vector);

    dbg(vector);
}
