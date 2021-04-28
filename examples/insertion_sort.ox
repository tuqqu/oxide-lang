/// Sort a vector of integers
/// using insertion sort algorithm.
fn insertion_sort(input: vec<int>) {
    for let mut i = 1; i < input.len(); i += 1 {
        let cur = input[i];
        let mut j = i - 1;

        while input[j] > cur {
            let temp = input[j + 1];
            input[j + 1] = input[j];
            input[j] = temp;

            if j == 0 {
                break;
            }

            j -= 1;
        }
    }
}

fn main() {
    let input: vec<int> = vec[4, 13, 0, 3, -3, 4, 19, 1];

    insertion_sort(input);

    println(input);
}