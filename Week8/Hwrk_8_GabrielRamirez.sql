-- Gabriel Ramirez
-- 2014-12-06
-- HWRK 8

USE
MatrixAlgebra
GO

/* 
	Create the two tables that result from the 
	“Sparse Matrices: Assignment” slide.
*/
BEGIN TRY 
DROP TABLE Table1
END TRY BEGIN CATCH END CATCH

-- A Table 1 will have as headers: A, B, C, & N. 
CREATE TABLE Table1 (A INT, B INT, C INT, N INT); 
INSERT INTO Table1 VALUES (2, 1, 3, 23)
INSERT INTO Table1 VALUES (1, 3, 3, 29)
INSERT INTO Table1 VALUES (2, 1, 1, 17)

/*
A	B	C	N
2	1	3	23
1	3	3	29
2	1	1	17
*/

-- B Table 2 will have as its headers: R, C, & M
CREATE TABLE Table2 (R INT, C VARCHAR(2), M FLOAT); 
INSERT INTO Table2 VALUES (1, 'A', 2)
INSERT INTO Table2 VALUES (1, 'B', 1)
INSERT INTO Table2 VALUES (1, 'C', 3)
INSERT INTO Table2 VALUES (1, 'N', 23)
INSERT INTO Table2 VALUES (2, 'A', 1)
INSERT INTO Table2 VALUES (2, 'B', 3)
INSERT INTO Table2 VALUES (2, 'C', 3)
INSERT INTO Table2 VALUES (2, 'N', 29)
INSERT INTO Table2 VALUES (3, 'A', 2)
INSERT INTO Table2 VALUES (3, 'B', 1)
INSERT INTO Table2 VALUES (3, 'C', 1)
INSERT INTO Table2 VALUES (3, 'N', 17)

/*
R	C	M
1	A	2
1	B	1
1	C	3
1	N	23
2	A	1
2	B	3
2	C	3
2	N	29
3	A	2
3	B	1
3	C	1
3	N	17
*/

/*
	Change the schema by changing Table 2. The new values
	will represent Cost per Square Foot.
*/

INSERT INTO Table2
SELECT AREA.R as R, 'CPSF' as C, (COST.M * 100000)/(AREA.M * 1000) as M   
FROM Table2 as AREA, Table2 as COST
WHERE AREA.C = 'A' and COST.C = 'C' and AREA.R = COST.R 

/*
R	C	M
1	A	2
1	B	1
1	C	3
1	N	23
2	A	1
2	B	3
2	C	3
2	N	29
3	A	2
3	B	1
3	C	1
3	N	17
1	CPSF	150
2	CPSF	300
3	CPSF	50
*/

-- 4
/* 
	SQL on Sparse Matrices. Given that sparse matrices 
	are encoded with the EAV schema do the following: 
*/

-- Matrix1 is a 4 by 3 matrix
TRUNCATE TABLE Matrix1
INSERT INTO Matrix1 VALUES ('1', '1', -2)
INSERT INTO Matrix1 VALUES ('2', '1', 2)
INSERT INTO Matrix1 VALUES ('3', '1', -1)
INSERT INTO Matrix1 VALUES ('4', '1', 1)
INSERT INTO Matrix1 VALUES ('1', '2', 2)
INSERT INTO Matrix1 VALUES ('2', '2', -2)
INSERT INTO Matrix1 VALUES ('3', '2', 1)
INSERT INTO Matrix1 VALUES ('4', '2', -1)
INSERT INTO Matrix1 VALUES ('1', '3', 0)
INSERT INTO Matrix1 VALUES ('2', '3', -1)
INSERT INTO Matrix1 VALUES ('3', '3', 0)
INSERT INTO Matrix1 VALUES ('4', '3', 1)
SELECT * FROM Matrix1

-- Matrix2 is 3 by 4 matrix
TRUNCATE TABLE Matrix2
INSERT INTO Matrix2 VALUES ('1', '1', 0)
INSERT INTO Matrix2 VALUES ('1', '2', 2)
INSERT INTO Matrix2 VALUES ('1', '3', 4)
INSERT INTO Matrix2 VALUES ('1', '4', 6)
INSERT INTO Matrix2 VALUES ('2', '1', 1)
INSERT INTO Matrix2 VALUES ('2', '2', 3)
INSERT INTO Matrix2 VALUES ('2', '3', 5)
INSERT INTO Matrix2 VALUES ('2', '4', 7)
INSERT INTO Matrix2 VALUES ('3', '1', -1)
INSERT INTO Matrix2 VALUES ('3', '2', 1)
INSERT INTO Matrix2 VALUES ('3', '3', 3)
INSERT INTO Matrix2 VALUES ('3', '4', 5)
SELECT * FROM Matrix2

-- A
-- Write SQL to multiply Matrix1 by 7
SELECT Matrix1.ROWID, Matrix1.COLUMNID, Matrix1.VALUE * 7  FROM Matrix1

-- B
-- Write SQL to transpose (Pivot) Matrix2
SELECT Matrix2.COLUMNID AS ROWID, Matrix2.ROWID AS COLUMNID, Matrix2.VALUE  FROM Matrix2

-- C
-- Add two Matrices
-- Matrix3 is a 4 by 3 matrix
TRUNCATE TABLE Matrix3
INSERT INTO Matrix3 VALUES ('1', '1', 10)
INSERT INTO Matrix3 VALUES ('2', '1', 20)
INSERT INTO Matrix3 VALUES ('3', '1', 30)
INSERT INTO Matrix3 VALUES ('4', '1', 40)
INSERT INTO Matrix3 VALUES ('1', '2', 50)
INSERT INTO Matrix3 VALUES ('2', '2', 60)
INSERT INTO Matrix3 VALUES ('3', '2', 70)
INSERT INTO Matrix3 VALUES ('4', '2', 80)
INSERT INTO Matrix3 VALUES ('1', '3', 90)
INSERT INTO Matrix3 VALUES ('2', '3', 100)
INSERT INTO Matrix3 VALUES ('3', '3', 110)
INSERT INTO Matrix3 VALUES ('4', '3', 120)
SELECT * FROM Matrix3

-- Add Matrix1 to Matrix3

SELECT Matrix1.RowID as ROWID, 
      Matrix1.ColumnID as COLUMNID, 
      Matrix1.Value + Matrix3.Value as value
FROM Matrix1, Matrix3
WHERE Matrix1.ColumnID = Matrix3.ColumnID and
      Matrix1.RowID = Matrix3.RowID