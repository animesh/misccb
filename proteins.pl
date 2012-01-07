/* $Id: proteins.pl,v 0.1alpha 2007/02/17 23:42:38 Zzz Exp $

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


:- module(bioprolog_proteins, [
		protein_type/2,
		protein_mass/2,
		protein_charge/2
	]).


protein_type(Protein, Type) :-
	(	nonvar(Protein)
	->	true
	;	throw(error(instantiation_error, context(protein_type/2, _)))
	),
	atom_codes(Protein, ProteinCodes),
	'$protein_type'(ProteinCodes, Type).
'$protein_type'(Protein, Type) :-
	'$protein_type'(Protein, '', Type).
'$protein_type'([AminoacidCode|Protein], Current, Type) :-
	atom_codes(Aminoacid, AminoacidCode),
	amino_acid(Aminoacid, Parts),
	amino_acids(Aminoacids),
	(	memberchk(Aminoacid, Aminoacids)
	->	memberchk(type(_, Code), Parts)
	;	throw(error(type_error(amino_acid, Aminoacid), context(protein_type/2, _)))
	),
	!,
	atom_codes(Char, [Code]),
	atom_concat(Current, Char, New),
	'$protein_type'(Protein, New, Type).
'$protein_type'([], Type, Type).
	


protein_mass(Protein, Mass) :-
	(	nonvar(Protein)
	->	true
	;	throw(error(instantiation_error, context(protein_mass/2, _)))
	),
	atom_codes(Protein, ProteinCodes),
	'$protein_mass'(ProteinCodes, Mass).
'$protein_mass'(Protein, Mass) :-
	'$protein_mass'(Protein, 0.0, Mass).
'$protein_mass'([AminoacidCode|Protein], Current, Mass) :-
	atom_codes(Aminoacid, AminoacidCode),
	amino_acid(Aminoacid, Parts),
	amino_acids(Aminoacids),
	(	memberchk(Aminoacid, Aminoacids)
	->	memberchk(mass(This), Parts)
	;	throw(error(type_error(amino_acid, Aminoacid), context(protein_mass/2, _)))
	),
	!,
	New is Current+This,
	'$protein_mass'(Protein, New, Mass).
'$protein_mass'([], Mass, Mass).
	
	

	
protein_charge(Protein, Charge) :-
	(	nonvar(Protein)
	->	true
	;	throw(error(instantiation_error, context(protein_charge/2, _)))
	),
	atom_codes(Protein, ProteinCodes),
	'$protein_charge'(ProteinCodes, Charge).
'$protein_charge'(Protein, Charge) :-
	'$protein_charge'(Protein, 0, Charge).
'$protein_charge'([AminoacidCode|Protein], Current, Charge) :-
	atom_codes(Aminoacid, AminoacidCode),
	amino_acid(Aminoacid, Parts),
	amino_acids(Aminoacids),
	(	memberchk(Aminoacid, Aminoacids)
	->	memberchk(charge(This), Parts)
	;	throw(error(type_error(amino_acid, Aminoacid), context(protein_charge/2, _)))
	),
	!,
	New is Current+This,
	'$protein_charge'(Protein, New, Charge).
'$protein_charge'([], Charge, Charge).	
	
	
	