/// Matrix multiplication
/// Compute product of two matrices.
fn multiply_matrices(a: vec<vec<int>>, b: vec<vec<int>>) -> vec<vec<int>> {
    let product = vec<vec<int>>[];

    for let mut i = 0; i < a.len(); i += 1 {
        product.push(vec<int>[]);
        for let mut j = 0; j < a[0].len(); j += 1 {
            product[i].push(0);
        }
    }

    for let mut i = 0; i < a.len(); i += 1 {
        for let mut j = 0; j < b[0].len(); j += 1 {
            product[i][j] = 0;
            for let mut k = 0; k < b.len(); k += 1 {
                product[i][j] += a[i][k] * b[k][j];
            }
        }
    }

    return product;
}

let matrix_a: vec<vec<int>> = vec[
    vec[1, 11, 3],
    vec[5, 0, -5],
    vec[6, 9, 19],
];

let matrix_b = vec[     // actually the type can be safely inferred as vec<vec<int>>
    vec[0, 0, 1],
    vec[2, -1, -3],
    vec[-4, 1, 3],
];

let matrix_c = multiply_matrices(matrix_a, matrix_b);

println(matrix_c);