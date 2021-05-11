/// Simple matrix operations

struct Matrix {
    pub m: vec<vec<int>>,
}

impl Matrix {
    /// Add number to matrix
    pub fn add(self, number: int) {
        for i in 0..self.m.len() {
            for j in 0..self.m[0].len() {
                self.m[i][j] += number;
            }
        }
    }

    /// Matrix multiplication
    /// Compute product of two matrices.
    pub fn multiply(self, b: Matrix) -> Matrix {
        let product = Self::create_empty(self.m.len(), self.m[0].len());

        for i in 0..self.m.len() {
            for j in 0..b.m[0].len() {
                product[i][j] = 0;
                for k in 0..b.m.len() {
                    product[i][j] += self.m[i][k] * b.m[k][j];
                }
            }
        }

        return Matrix { m: product };
    }

    // Private static method: create empty matrix MxN
    fn create_empty(m: int, n: int) -> vec<vec<int>> {
        let matrix = vec<vec<int>>[];

        for i in 0..m {
            matrix.push(0..n);
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