# HomeWork08_WernerColangelo.sql.R
#
# DATASCI 250: Introduction to Data Science (4690)
# Autumn 14
# Instructor: Ernst Henle
#
# Homework 8
#
# Submitted by:
# Werner Colangelo
# wernercolangelo@gmail.com

# 1. Create the two tables that result from the “Sparse Matrices: Assignment” slide.
# a) Table 1 will have as headers: A, B, C, & N.

#  Table1
#  A  B  C  N
# == == == ==
#  2  1  3 23
#  1  3  3 29
#  2  1  1 17

CREATE TABLE "Table1" ("A" INTEGER, "B" INTEGER, "C" INTEGER, "N" REAL);

INSERT INTO Table1 VALUES (2, 1, 3, 23);
INSERT INTO Table1 VALUES (1, 3, 3, 29);
INSERT INTO Table1 VALUES (2, 1, 1, 17);


# b) Table 2 will have as its headers: R, C, & M.

#  Table2
#  R  C  M
# == == ==
#  1  A  2
#  1  B  1
#  1  C  3
#  1  N 23
#  2  A  1
#  2  B  3
#  2  C  3
#  2  N 29
#  3  A  2
#  3  B  1
#  3  C  1
#  3  N 17

CREATE TABLE "Table2" ("R" CHAR, "C" CHAR, "M" REAL);

INSERT INTO Table2 VALUES ('1', 'A', 2);
INSERT INTO Table2 VALUES ('1', 'B', 1);
INSERT INTO Table2 VALUES ('1', 'C', 3);
INSERT INTO Table2 VALUES ('1', 'N', 23);
INSERT INTO Table2 VALUES ('2', 'A', 1);
INSERT INTO Table2 VALUES ('2', 'B', 3);
INSERT INTO Table2 VALUES ('2', 'C', 3);
INSERT INTO Table2 VALUES ('2', 'N', 29);
INSERT INTO Table2 VALUES ('3', 'A', 2);
INSERT INTO Table2 VALUES ('3', 'B', 1);
INSERT INTO Table2 VALUES ('3', 'C', 1);
INSERT INTO Table2 VALUES ('3', 'N', 17);

# 2. Change the schema by changing Table 2. The new values will represent Cost per Square Foot.

# Let the new value, Cost per Square Foot, be represented by "S".

#  Table1
#  A  B  C  N  S
# == == == == ==
#  2  1  3 23 11
#  1  3  3 29 22
#  2  1  1 17 33

# This translates to Table2 as follows:

#  Table2
#  R  C  M
# == == ==
#  1  A  2
#  1  B  1
#  1  C  3
#  1  N 23
#  2  A  1
#  2  B  3
#  2  C  3
#  2  N 29
#  3  A  2
#  3  B  1
#  3  C  1
#  3  N 17
#  1  S 150 <-- Expected
#  2  S 300 <-- Expected
#  3  S  50 <-- Expected

# This translates into three inserts as follows:


INSERT INTO Table2 SELECT MatchRows.R, 'S',
	(SELECT M * 100 FROM Table2 WHERE R = MatchRows.R AND C = 'C') / (SELECT M FROM Table2 WHERE R = MatchRows.R AND C = 'A')
	FROM (SELECT R FROM Table2 WHERE C = 'A') AS MatchRows;


# 4. SQL on Sparse Matrices. Given that sparse matrices are encoded with the EAV schema do the following:
# a) Write SQL for scalar multiplication of a Sparse Matrix (See Exercise 5 in MatrixAlgebra.sql

/*Create sparse matrices*/ 
CREATE TABLE Matrix1 (RowID nchar(10), ColumnID nchar(10), Value real); 

INSERT INTO Matrix1 VALUES ('one', 'one', -2);
INSERT INTO Matrix1 VALUES ('one', '2', 2);
INSERT INTO Matrix1 VALUES ('one', '3', -1);
INSERT INTO Matrix1 VALUES ('one', '4', 1);

# Assumption is that we want to store the result back into Matrix1
UPDATE Matrix1 SET Value = Value * 7;

# b) Write SQL for transposition of a Sparse Matrix (See Exercise 6 in MatrixAlgebra.sql

CREATE TABLE Matrix2 (RowID nchar(10), ColumnID nchar(10), Value real); 

INSERT INTO Matrix2 VALUES ('one', 'one', 0);
INSERT INTO Matrix2 VALUES ('2', 'one', 2);
INSERT INTO Matrix2 VALUES ('3', 'one', 3);
INSERT INTO Matrix2 VALUES ('4', 'one', 5);

# Assumption is that we want to store the result back in Matrix2
UPDATE Matrix2 SET RowID = Columnd, ColumnID = RowID;


# c) Optional: Write SQL for addition of two matrices

CREATE TABLE Matrix1 (RowID nchar(10), ColumnID nchar(10), Value real); 

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

CREATE TABLE Matrix3 (RowID nchar(10), ColumnID nchar(10), Value real);

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

SELECT Matrix1.RowID, Matrix1.ColumnID, Matrix1.Value + Matrix3.Value as Value
FROM Matrix1, Matrix3 WHERE Matrix1.RowID = Matrix3.RowID AND Matrix1.ColumnID = Matrix3.ColumnID;
