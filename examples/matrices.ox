/// Simple matrix operations

struct Matrix {
    pub m: vec<vec<int>>,
}

impl Matrix {
    /// Add number to matrix
    fn add(number: int) {
        for let mut i = 0; i < self.m.len(); i += 1 {
            for let mut j = 0; j < self.m[0].len(); j += 1 {
                self.m[i][j] += number;
            }
        }
    }

    /// Matrix multiplication
    /// Compute product of two matrices.
    fn multiply(b: Matrix) -> Matrix {
        let product = vec<vec<int>>[];

        for let mut i = 0; i < self.m.len(); i += 1 {
            product.push(vec<int>[]);
            for let mut j = 0; j < self.m[0].len(); j += 1 {
                product[i].push(0);
            }
        }

        for let mut i = 0; i < self.m.len(); i += 1 {
            for let mut j = 0; j < b.m[0].len(); j += 1 {
                product[i][j] = 0;
                for let mut k = 0; k < b.m.len(); k += 1 {
                    product[i][j] += self.m[i][k] * b.m[k][j];
                }
            }
        }

        return Matrix { m: product };
    }
}

let matrix_a = Matrix {
    m: vec[
        vec[1, 11, 3],
        vec[5, 0, -5],
        vec[6, 9, 19],
    ],
};


let matrix_b = Matrix{
    m: vec[
        vec[0, 0, 1],
        vec[2, -1, -3],
        vec[-4, 1, 3],
    ],
};

let matrix_c = matrix_a.multiply(matrix_b);

matrix_a.add(10);

println(matrix_c);
println(matrix_a);