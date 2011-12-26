/* $Id: misc.pl,v 0.1alpha 2007/04/17 12:30:52 Zzz Exp $

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


:- module(bioprolog_misc, [
		convert_matrix_files/1,
		read_file_to_lines/2
	]).

:- use_module(library(lists)).
:- use_module(strings).

/* convert_matrix_files/1
 * +Filter
 * `Filter' is used to convert more than one file.
 * Example:
 *    convert_matrix_files('*.txt').
 */
convert_matrix_files(Filter) :-
	expand_file_name(Filter, List),
	list_txt2sm(List).

list_txt2sm([File|Files]) :-
	txt2sm(File),
	!,
	list_txt2sm(Files).
list_txt2sm([]).

/* txt2sm/1
 * +FileName
 * Converts txt well-formatted matrix file to bioprolog substitution matrix file.
 */
txt2sm(InputFile) :-
	file_name_extension(Base, txt, InputFile),
	file_name_extension(Base, pl, OutputFile),
	file_base_name(Base, Matrix),
	read_file_to_lines(InputFile, [Labels|Values]),
	atom_explode(Labels, ' ', Aminoacids),
	write(Aminoacids), nl,
	length(Aminoacids, Length),
	open(OutputFile, write, OutputStream),
	format(OutputStream, '\nsubstitution_matrix(~w, [\n', [Matrix]),
	(	between(1, Length, From),
		nth1(From, Values, Row),
		atom_explode(Row, ' ', Numeric),
		nth1(From, Aminoacids, FromLabel),
		(	length(Numeric, Length)
		->	true
		;	throw(matrix_elements_mismatch)
		),
		between(1, Length, To),
		nth1(To, Numeric, AtomNumber),
		atom_number(AtomNumber, Number),
		nth1(To, Aminoacids, ToLabel),
		(	From == Length,
			To == Length
		->	Comma = ''
		;	Comma = ','
		),
		format(OutputStream, '\t(\'~w\', \'~w\', ~q)~w\n', [FromLabel, ToLabel, Number, Comma]),
		fail
	;	true
	),	
	write(OutputStream, ']).\n'),
	close(OutputStream).
	
	
	
/* read_file_to_lines(+File, -Lines)
 * Unify `Lines' with thel list of `File' text lines.
 * This predicate remove also the extra spaces on a line (it is straightforward to remove
 * this feature and use the predicate for parsing purposes).
 */
read_file_to_lines(File, Lines) :-
	read_file_to_codes(File, Codes, []),
	parse_file_codes_to_lines(Codes, Lines).
	
parse_file_codes_to_lines(Codes, Lines) :-
	parse_file_codes_to_lines_(Codes, '', Lines).
	
parse_file_codes_to_lines_([], T, [Line]) :-
	!,
	atom_remove_extra_spaces(T, Line).
parse_file_codes_to_lines_([Code|Codes], T, [Line|Lines]) :-
	memberchk(Code, [10, 13]),
	!,
	atom_remove_extra_spaces(T, Line),
	parse_file_codes_to_lines_(Codes, '', Lines).
parse_file_codes_to_lines_([Code|Codes], T, Lines) :-
	between(32, 126, Code),
	!,
	atom_codes(Atom, [Code]),
	atom_concat(T, Atom, N),
	parse_file_codes_to_lines_(Codes, N, Lines).
parse_file_codes_to_lines_([_|Codes], T, Lines) :-
	parse_file_codes_to_lines_(Codes, T, Lines).
	
	
