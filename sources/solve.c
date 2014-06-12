/*
	Routines for solving systems of linear equations that have only
	numerical coefficients. The inhomogeneous part can be anything.
	This work can in principle be done externally but we need a way
	to solve relatively small systems of which we may have many.
	The main questions are:
	1: What are the ways the equations may occur (function arguments
	   vs. expressions)
	2: Sparse vs. dense algorithms.
	3: What to do with intermediate results?
	4: What to do with the results?

	1: f(eq1,...,eqn);
	   Solve,f,x1,...,xn;
	   --> f(v1,...,vn);
	      with x1 -> v1, xn -> vn
	2: F1,...,Fn;
	   #Solve,(F1,...,Fn),(x1,...,xm),(G1,...,Gm);
	   --> G1 = x1; etc.

        x00 x01 x02 a0
        x10 x11 x12 a1       X vec(x) = vec(a) -> vec(x) = X^-1 vec(a)
        x20 x21 x22 a2
*/

int CoSolve(UBYTE *s)
{
	
}

int DoSolve()
{
}

int SolveSmall(PHEAD WORD **eqns,WORD,nume,WORD *vars,WORD numv)
{
/*
	Solves small systems of linear equations in which the variables have only
	numerical coefficients and the inhomogeneous part can be anything.

	We use a Gauss elimination.
	Problem: space needed for the coefficients.
*/

}
