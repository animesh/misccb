/* $Id: alphabets.pl,v 0.1alpha 2007/04/17 12:28:54 Zzz Exp $

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


:- module(bioprolog_alphabets, [
		alphabet/2
	]).
	
	
/* alphabet/2
 * +Alphabet, -Elements
 * Unify `Elements' with the list of elements of the alphabet `Alphabet' (both for protein or dna/rna).
 * Use of findall/3 instead of enumering elements explicitly is preferable to avoid redundancy.
 */
alphabet(Alphabet, _) :-
	var(Alphabet),
	!,
	throw(error(instantiation_error, context(alphabet/2, _))).
alphabet(Alphabet, Elements) :-
	findall(
		Element, 
		(	amino_acid(Element, _, Alphabets),
			memberchk(Alphabet, Alphabets)
		),
		List
	),
	list_to_set(List, Elements),
	Elements \= [],
	!.
alphabet(Alphabet, Elements) :-
	findall(
		Element, 
		(	nucleo_base(Element, _, Alphabets),
			memberchk(Alphabet, Alphabets)
		),
		List
	),
	list_to_set(List, Elements),
	Elements \= [],
	!.
alphabet(Alphabet, _) :-	
	throw(error(type_error(alphabet, Alphabet), context(alphabet/2, _))).
	
	
	

	
	
	
	
	