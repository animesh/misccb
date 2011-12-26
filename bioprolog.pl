/* $Id: bioprolog.pl,v 0.1alpha 2007/04/17 12:35:20 Zzz Exp $

    BioProlog Logic Programming Resources for Bioinformatics

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



user:file_search_path(bioprolog, Path) :-
	library_directory(Lib),
	absolute_file_name(bioprolog, Path, [relative_to(Lib)]),
	exists_directory(Path),
	!.
user:file_search_path(bioprolog, Path) :-
	working_directory(Work, _),
	absolute_file_name(bioprolog, Path, [relative_to(Work)]).


:- use_module(bioprolog('io.pl')).
:- use_module(bioprolog('amino_acids.pl')).
:- use_module(bioprolog('proteins.pl')).
:- use_module(bioprolog('nucleo_bases.pl')).
:- use_module(bioprolog('nucleic_acids.pl')).
:- use_module(bioprolog('alphabets.pl')).
:- use_module(bioprolog('sequences.pl')).
:- use_module(bioprolog('regions.pl')).
:- use_module(bioprolog('square_matrices.pl')).
:- use_module(bioprolog('substitution_matrices.pl')).
:- use_module(bioprolog('align.pl')).
:- use_module(bioprolog('strings.pl')).
:- use_module(bioprolog('misc.pl')).


:-  absolute_file_name(bioprolog('conf.pl'), ConfFile),
	(	exists_file(ConfFile)
	->	ensure_loaded(ConfFile)
	;	true
	).
	

