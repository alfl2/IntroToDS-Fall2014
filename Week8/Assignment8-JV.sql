-- Week8 Assignment
-- Jignesh Vyas
USE
MatrixAlgebra
GO

/* Drop the object if it already exists */

if object_id('Table1') is not null DROP TABLE Table1
if object_id('Matrix1') is not null DROP TABLE Matrix1
if object_id('Matrix2') is not null DROP TABLE Matrix2

/*
	1. Create the two tables that result from the “Sparse Matrices: Assignment” slide.
	a)Table 1 will have as headers: A, B, C, & N.
*/

CREATE TABLE Table1 (A float, B int, C float,N int); 
/*Load values into the tables */ 
INSERT INTO Table1 VALUES (1,3,3,29)
INSERT INTO Table1 VALUES (2,1,3,23)
INSERT INTO Table1 VALUES (2,1,1,17)


SELECT * FROM Table1

/*	b)Table 2 will have as its headers: R, C, & M. */


CREATE TABLE Matrix1 (RowID int, Col nchar(10), M float); 
INSERT INTO Matrix1 VALUES (1, 'A', 1)
INSERT INTO Matrix1 VALUES (1, 'B', 3)
INSERT INTO Matrix1 VALUES (1, 'C', 3)
INSERT INTO Matrix1 VALUES (1, 'N', 29)

INSERT INTO Matrix1 VALUES (2, 'A', 2)
INSERT INTO Matrix1 VALUES (2, 'B', 1)
INSERT INTO Matrix1 VALUES (2, 'C', 3)
INSERT INTO Matrix1 VALUES (2, 'N', 23)

INSERT INTO Matrix1 VALUES (3, 'A', 2)
INSERT INTO Matrix1 VALUES (3, 'B', 1)
INSERT INTO Matrix1 VALUES (3, 'C', 1)
INSERT INTO Matrix1 VALUES (3, 'N', 17)


SELECT * FROM Matrix1


/*	2.Change the schema by changing Table 2. The new values will represent Cost per Square Foot.
	
Note A : is in 1000 sq ft
     C : is in $100,000
*/
SELECT *,Round((C*100000)/(A*1000),3) as CostPerSqFeet FROM Table1
/*
A	B	C	N	CostPerSqFeet
1	3	3	29	300
2	1	3	23	150
2	1	1	17	50
*/

-- Represents this in Sparse Matrix
-- Note : V reprsents CostPerSqFeet

INSERT INTO Matrix1 VALUES (1, 'V', 300)
INSERT INTO Matrix1 VALUES (2, 'V', 150)
INSERT INTO Matrix1 VALUES (3, 'V', 50)

-- Check the output
SELECT * FROM Matrix1
SELECT * FROM Matrix1 Order by RowID,Col

/*
	4.SQL on Sparse Matrices. Given that sparse matrices are encoded with the EAV schema do the following:
	a)Write SQL for scalar multiplication of a Sparse Matrix (See Exercise 5 in MatrixAlgebra.sql
*/

SELECT RowID,Col,M*7 as M from Matrix1
Order by RowID,Col

/*
	b) Write SQL for transposition of a Sparse Matrix (See Exercise 6 in MatrixAlgebra.sql
*/
-- Matrix1 represents  3 (rows) X 5 (col) matrix
-- Transpose will reprsent 5 (rows) X 3 (col) matrix. This will transpose only rows and col, table structure of matrix1 remains same i.e. 3 columns, 15 rows.
-- Orignal Matrix
SELECT * from Matrix1 order by 2,1
-- Transpose Matrix
SELECT Col as RowID, RowID as Col , M from Matrix1 order by 2,1

-- This transposition using PIVOT function will be 5 X 3 Matrix structure
SELECT * 
FROM Matrix1 
PIVOT 
(
 sum(M)
 FOR ROWID in ([1],[2],[3])
)
as P
-- COL as columns in Transpose matrix
-- This transposition using PIVOT function will be  3 X 5 Matrix structure
SELECT * 
FROM Matrix1 
PIVOT 
(
 sum(M)
 FOR COL in ([A],[B],[C],[N],[V])
)
as P

/* c) Optional: Write SQL for addition of two matrices */
-- create Matrix2 - same as structure Matrix1 (3X5 matrix) with some different values
SELECT RowID,Col,m*7 AS M INTO Matrix2 FROM Matrix1

-- Adding 2 matrix
SELECT m1.ROWID,m1.COL,m1.M+m2.M as Value
FROM Matrix1 m1 join Matrix2 m2 on m1.RowID=m2.RowID and m1.Col=m2.Col

