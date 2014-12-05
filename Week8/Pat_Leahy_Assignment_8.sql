-- Pat_Leahy_Assignment_8.sql
-- 12/04/2014
-- Pat Leahy pat@patleahy.com
-- Introduction to Data Science - Lecture 8 Assignment

USE MatrixAlgebra;

-- 1.   Create the two tables that result from the “Sparse Matrices: Assignment” slide.
--   a.	Table 1 will have as headers:  A, B, C, & N.
--   b.	Table 2 will have as its headers:  R, C, & M.

BEGIN TRY 
DROP TABLE Table1;
END TRY BEGIN CATCH END CATCH

CREATE TABLE Table1 (A INT, B INT, C INT, N REAL);

--                         A  B  C  N
INSERT INTO Table1 VALUES (2, 1, 3, 23);
INSERT INTO Table1 VALUES (1, 3, 3, 29);
INSERT INTO Table1 VALUES (2, 1, 1, 17);

SELECT * FROM Table1;

-- Table1 Contents:

-- A	B	C	N
-- 2	1	3	23
-- 1	3	3	29
-- 2	1	1	17

BEGIN TRY 
DROP TABLE Table2;
END TRY BEGIN CATCH END CATCH

CREATE TABLE Table2 (R INT, C VARCHAR(20), M REAL);

                       --  R  C    M
INSERT INTO Table2 VALUES (1, 'A', 2);
INSERT INTO Table2 VALUES (1, 'B', 1);
INSERT INTO Table2 VALUES (1, 'C', 3);
INSERT INTO Table2 VALUES (1, 'N', 23);
INSERT INTO Table2 VALUES (2, 'A', 1);
INSERT INTO Table2 VALUES (2, 'B', 3);
INSERT INTO Table2 VALUES (2, 'C', 3);
INSERT INTO Table2 VALUES (2, 'N', 29);
INSERT INTO Table2 VALUES (3, 'A', 2);
INSERT INTO Table2 VALUES (3, 'B', 1);
INSERT INTO Table2 VALUES (3, 'C', 1);
INSERT INTO Table2 VALUES (3, 'N', 17);

SELECT * FROM Table2;

-- Table2 Contents:

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

-- 2.Change the schema by changing Table 2. The new values will represent Cost per Square Foot.

-- The inner sub-select will get the ID (R) for each entity by looking at the 
-- IDs of each A attribute.
--
-- The select statements in the outer select-clause will get value of the 
-- A and C attributes where the row ID of the attribute matches the row IDs 
-- returned from the inner sub-select.

INSERT INTO Table2
SELECT
	EachRow.R,
	'Cost per Square Foot',
	   (SELECT M * 100000 FROM Table2 WHERE R = EachRow.R AND C = 'C')
	 / (SELECT M * 1000   FROM Table2 WHERE R = EachRow.R AND C = 'A')
FROM
(
	SELECT R FROM Table2 WHERE C = 'A'
) AS EachRow;

SELECT * FROM Table2;

-- Table2 with Cost per Square Foot attribute:

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
1	Cost per Square Foot	150
2	Cost per Square Foot	300
3	Cost per Square Foot	50
*/

-- 4. SQL on Sparse Matrices. Given that sparse matrices are encoded with the EAV schema do the following:

BEGIN TRY 
DROP TABLE Matrix1;
END TRY BEGIN CATCH END CATCH

CREATE TABLE Matrix1 (RowID NCHAR(10), ColumnID NCHAR(10), Value REAL); 

-- Matrix1 is a 4 by 3 matrix
INSERT INTO Matrix1 VALUES ('1', '1', -2);
INSERT INTO Matrix1 VALUES ('2', '1', 2);
INSERT INTO Matrix1 VALUES ('3', '1', -1);
INSERT INTO Matrix1 VALUES ('4', '1', 1);
INSERT INTO Matrix1 VALUES ('1', '2', 2);
INSERT INTO Matrix1 VALUES ('2', '2', -2);
INSERT INTO Matrix1 VALUES ('3', '2', 1);
INSERT INTO Matrix1 VALUES ('4', '2', -1);
INSERT INTO Matrix1 VALUES ('1', '3', 0);
INSERT INTO Matrix1 VALUES ('2', '3', -1);
INSERT INTO Matrix1 VALUES ('3', '3', 0);
INSERT INTO Matrix1 VALUES ('4', '3', 1);
SELECT * FROM Matrix1;

--  a. Write SQL for scalar multiplication of a Sparse Matrix (See Exercise 5 in MatrixAlgebra.sql)

-- Expected:

-- 7 x | -2  2  0 |  =  | -14   14   0 |
--     |  2 -2 -1 |     |  14  -14  -7 |
--     | -1  1  0 |     | -7     7   0 |
--     |  1 -1  1 |     |  7    -7   7 |

SELECT RowID, ColumnID, Value * 7 AS Value FROM Matrix1;

-- Result:

/*
RowID	ColumnID	Value
1         	1         	-14
2         	1         	14
3         	1         	-7
4         	1         	7
1         	2         	14
2         	2         	-14
3         	2         	7
4         	2         	-7
1         	3         	0
2         	3         	-7
3         	3         	0
4         	3         	7
*/

--  b. Write SQL for transposition of a Sparse Matrix (See Exercise 6 in MatrixAlgebra.sql)

-- Expected:

-- | -2  2  0 |T = | -2  2 -1  1 |
-- |  2 -2 -1 |    |  2 -2  1 -1 | 
-- | -1  1  0 |    |  0 -1  0  1 |
-- |  1 -1  1 |     

SELECT ColumnID AS RowID, RowID as ColumnID, Value FROM Matrix1;

-- Result:

/*
RowID	ColumnID	Value
1         	1         	-2
1         	2         	2
1         	3         	-1
1         	4         	1
2         	1         	2
2         	2         	-2
2         	3         	1
2         	4         	-1
3         	1         	0
3         	2         	-1
3         	3         	0
3         	4         	1
*/

--  c. Optional: Write SQL for addition of two matrices

BEGIN TRY 
DROP TABLE Matrix3;
END TRY BEGIN CATCH END CATCH

CREATE TABLE Matrix3 (RowID NCHAR(10), ColumnID NCHAR(10), Value REAL); 

INSERT INTO Matrix3 VALUES ('1', '1', 10);
INSERT INTO Matrix3 VALUES ('2', '1', 20);
INSERT INTO Matrix3 VALUES ('3', '1', 30);
INSERT INTO Matrix3 VALUES ('4', '1', 40);
INSERT INTO Matrix3 VALUES ('1', '2', 50);
INSERT INTO Matrix3 VALUES ('2', '2', 60);
INSERT INTO Matrix3 VALUES ('3', '2', 70);
INSERT INTO Matrix3 VALUES ('4', '2', 80);
INSERT INTO Matrix3 VALUES ('1', '3', 90);
INSERT INTO Matrix3 VALUES ('2', '3', 100);
INSERT INTO Matrix3 VALUES ('3', '3', 110);
INSERT INTO Matrix3 VALUES ('4', '3', 120);
SELECT * FROM Matrix3;

-- Expected:

-- | -2  2  0 |  +  | 10  50   90 |  =  |  8  52   90 |
-- |  2 -2 -1 |     | 20  60  100 |     | 22  58   99 |
-- | -1  1  0 |     | 30  70  110 |     | 29  71  110 |
-- |  1 -1  1 |     | 40  80  120 |     | 41  79  121 |

SELECT Matrix1.RowID, Matrix1.ColumnID, Matrix1.Value + Matrix3.Value as Value
FROM Matrix1, Matrix3
WHERE Matrix1.RowID = Matrix3.RowID 
  AND Matrix1.ColumnID = Matrix3.ColumnID;

-- Result:

/*
RowID	ColumnID	Value
1         	1         	8
2         	1         	22
3         	1         	29
4         	1         	41
1         	2         	52
2         	2         	58
3         	2         	71
4         	2         	79
1         	3         	90
2         	3         	99
3         	3         	110
4         	3         	121
*/