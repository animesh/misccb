/* $Id: square_matrices.pl,v 0.1alpha 2007/04/17 11:54:24 Zzz Exp $

    Part of BioProlog Logic Programming Resources for Bioinformatics

    Author:	Mauro Di Nuzzo
    E-Mail:	info@prologonlinereference.org
    WWW:	http://www.prologonlinereference.org/bioprolog.psp
    Copyright:	©2005-2007 Prolog Online Reference

    This program is free software; you can redistribute it and/or
    modify it under the terms of the GNU General Public License
    as published by the Free Software Foundation; either version 2
    of the License, or (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU Lesser General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

    As a special exception, if you link this library with other files,
    compiled with a Free Software compiler, to produce an executable, this
    library does not by itself cause the resulting executable to be covered
    by the GNU General Public License. This exception does not however
    invalidate any other reasons why the executable file might be covered by
    the GNU General Public License.
*/



:- module(bioprolog_square_matrices, [
		matrix/2,
		zeros/2,
		identity/2,
		element/4,
		row/3,
		column/3,
		diagonal/2,
		transpose/2,
		product/3
	]).



/* A square matrix of order N is represented by a list of list.
 * Example: identity square matrix of order 2.
 * 
 * |?- identity(I, 2).
 *
 *   I = [ [1, 0], [0, 1] ]
 */


/* matrix(-list Matrix, +integer Order)
 * Create an uninstantiated square matrix.
 */
matrix(Matrix, N) :-
	length(Matrix, N),
	length(Rows, N),
	fill_list(Matrix, Rows).
  
zeros(Zeros, N) :-
	matrix(Zeros, N),
	fill(Zeros, 0).

identity(Identity, N) :-
	identity(Identity, N, 1),
	fill(Identity, 0).
  
identity([Row|Rows], N, This) :-
	length(Row, N),
	nth1(This, Row, I),
	I = 1,
	!,
	Next is This+1,
	identity(Rows, N, Next).
identity([], _, _).
  
fill(Matrix, Value) :-
	term_variables(Matrix, Variables),
	fill_list(Variables, Value).
  
fill_list([Value|Variables], Value) :-
	!,
	copy_term(Value, Copy),
	fill_list(Variables, Copy).
fill_list([], _).
  
% The following works both for get and set.  
element(Matrix, R, C, Element) :-
	row(Matrix, R, Row),
	column([Row], C, [Element]). 

row(Matrix, N, Row) :-
	nth1(N, Matrix, Row).
  
column([Row|Rows], N, [C|Column]) :-
	nth1(N, Row, C), 
	!,
	column(Rows, N, Column).
column([], _, []).

diagonal(Matrix, Diagonal) :-
	diagonal(Matrix, Diagonal, 1).
  
diagonal(Matrix, [D|Diagonal], This) :-
	row(Matrix, This, Row),
	nth1(This, Row, D),
	!,
	Next is This+1,
	diagonal(Matrix, Diagonal, Next).
diagonal(_, [], _).

transpose(Matrix, Transpost) :-
	length(Matrix, N),
	matrix(Transpost, N),
	transpose(Matrix, Transpost, 1).
  
transpose([Row|Rows], Transpost, This) :-
	column(Transpost, This, Row),
	!,
	Next is This+1,
	transpose(Rows, Transpost, Next).
transpose([], _, _).

product(Matrix, Column, Product) :-
	length(Column, N),
	length(Product, N),
	product(Matrix, Column, Product, 1).
  
product([Row|Rows], Column, Product, This) :-
	multiply(Row, Column, Result, 0),
	nth1(This, Product, Result),
	!,
	Next is This+1,
	product(Rows, Column, Product, Next).
product([], _, _, _).

multiply([RowElem|Row], [ColElem|Column], Result, Current) :-
	New is Current+(RowElem*ColElem),
	!,
	multiply(Row, Column, Result, New).
multiply([], [], Result, Result).
  