/// Simple matrix operations

struct Matrix {
    pub m: vec<vec<int>>,
}

impl Matrix {
    /// Add number to matrix
    pub fn add(self, number: int) {
        for let mut i = 0; i < self.m.len(); i += 1 {
            for let mut j = 0; j < self.m[0].len(); j += 1 {
                self.m[i][j] += number;
            }
        }
    }

    /// Matrix multiplication
    /// Compute product of two matrices.
    pub fn multiply(self, b: Matrix) -> Matrix {
        let product = Self::create_empty(self.m.len(), self.m[0].len());

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

    // Private static method: create empty matrix MxN
    fn create_empty(m: int, n: int) -> vec<vec<int>> {
        let matrix = vec<vec<int>>[];

        for let mut i = 0; i < m; i += 1 {
            matrix.push(vec<int>[]);
            for let mut j = 0; j < n; j += 1 {
                matrix[i].push(0);
            }
        }

        return matrix;
    }
}

fn main() {
    let matrix_a = Matrix {
        m: vec[
            vec[1, 11, 3],
            vec[5, 0, -5],
            vec[6, 9, 19],
        ],
    };


    let matrix_b = Matrix {
        m: vec[
            vec[0, 0, 1],
            vec[2, -1, -3],
            vec[-4, 1, 3],
        ],
    };

    let matrix_c = matrix_a.multiply(matrix_b);

    matrix_a.add(10);

    dbg(matrix_c);
    dbg(matrix_a);
}