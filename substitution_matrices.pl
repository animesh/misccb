/* $Id: substitution_matrices.pl,v 0.1alpha 2007/04/17 13:02:14 Zzz Exp $

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

:- module(bioprolog_substitution_matrices, [
		substitution_matrix/2,
		substitution_matrices/1,
		set_substitution_matrix/1,
		current_substitution_matrix/1
	]).

	
:- dynamic substitution_matrix/2.
:- multifile substitution_matrix/2.

:- absolute_file_name(bioprolog('substitution_matrices/*.pl'), Filter),
	expand_file_name(Filter, Files),
	load_files(Files, [silent(true)]). % SILENTLY IS BETTER!


substitution_matrices(Matrices) :-
	findall(
		Matrix,
		substitution_matrix(Matrix, _),
		List
	),
	list_to_set(List, Matrices).

:- dynamic current_substitution_matrix/1.

current_substitution_matrix(Matrix) :-
	substitution_matrices([Matrix|_]).
	
set_substitution_matrix(Matrix) :-
	(	var(Matrix)
	->	throw(error(instantiation_error, context(set_substitution_matrix/1, _)))
	;	substitution_matrices(Matrices),
		(	memberchk(Matrix, Matrices)
		->	retractall(current_substitution_matrix(_)),
			assert(current_substitution_matrix(Matrix))
		;	throw(error(existence_error(substitution_matrix, Matrix), context(set_substitution_matrix/1, _)))
		)
	).



