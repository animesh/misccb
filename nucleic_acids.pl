/* $Id: nucleic_acids.pl,v 0.1alpha 2007/02/17 23:42:28 Zzz Exp $

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

:- module(bioprolog_nucleic_acids, [
		translate_dna/2,
		transcript_rna/2,
		dna_reverse_complement/2
	]).


translate_dna(Dna, Rna) :-
	atom_chars(Dna, DnaChars),
	'$translate_dna'(DnaChars, RnaChars),
	atom_chars(Rna, RnaChars).
'$translate_dna'([DnaBase|Dna], [RnaBase|Rna]) :-
	!,
	nucleo_bases(DnaBases),
	(	memberchk(DnaBase, DnaBases)
	->	true
	;	throw(error(type_error(nucleo_base, DnaBase), context(translate_dna/2, _)))
	),
	nucleo_base(DnaBase, Parts),
	memberchk(in(In), Parts),
	(	memberchk(dna, In)
	->	true
	;	throw(error(domain_error(dna_nucleo_base, DnaBase), context(translate_dna/2, _)))
	),
	memberchk(translate_to(RnaBase), Parts),
	'$translate_dna'(Dna, Rna).
'$translate_dna'([], []).	

transcript_rna(Rna, Protein) :-
	atom_chars(Rna, RnaChars),
	'$transcript_rna'(RnaChars, ProteinChars),
	atom_chars(Protein, ProteinChars).
'$transcript_rna'([Base1, Base2, Base3|Rna], [AminoacidChar|Protein]) :-
	!,
	nucleo_bases(RnaBases),
	(	memberchk(Base1, RnaBases)
	->	true
	;	throw(error(type_error(nucleo_base, Base1), context(transcript_rna/2, _)))
	),
	nucleo_base(Base1, Parts1),
	memberchk(in(In1), Parts1),
	(	memberchk(rna, In1)
	->	true
	;	throw(error(domain_error(rna_nucleo_base, Base1), context(transcript_rna/2, _)))
	),
	(	memberchk(Base2, RnaBases)
	->	true
	;	throw(error(type_error(nucleo_base, Base2), context(transcript_rna/2, _)))
	),
	nucleo_base(Base2, Parts2),
	memberchk(in(In2), Parts2),
	(	memberchk(rna, In2)
	->	true
	;	throw(error(domain_error(rna_nucleo_base, Base3), context(transcript_rna/2, _)))
	),
	(	memberchk(Base3, RnaBases)
	->	true
	;	throw(error(type_error(nucleo_base, Base3), context(transcript_rna/2, _)))
	),
	nucleo_base(Base3, Parts3),
	memberchk(in(In3), Parts3),
	(	memberchk(rna, In3)
	->	true
	;	throw(error(domain_error(rna_nucleo_base, Base3), context(transcript_rna/2, _)))
	),
	amino_acids(Aminoacids),
	amino_acid(Aminoacid, Parts),
	memberchk(Aminoacid, Aminoacids),
	memberchk(codons(Codons), Parts),
	atom_chars(Codon, [Base1, Base2, Base3]),
	memberchk(Codon, Codons),
	atom_chars(Aminoacid, [AminoacidChar]),
	'$transcript_rna'(Rna, Protein).
'$transcript_rna'(_, []).

dna_reverse_complement(Dna, RevComp) :-
	atom_chars(Dna, DnaChars),
	'$dna_reverse_complement'(DnaChars, CompChars),
	reverse(CompChars, RevCompChars),
	atom_chars(RevComp, RevCompChars).
'$dna_reverse_complement'([DnaBase|Dna], [CompBase|Comp]) :-
	!,
	nucleo_bases(DnaBases),
	(	memberchk(DnaBase, DnaBases)
	->	true
	;	throw(error(type_error(nucleo_base, DnaBase), context(dna_reverse_complement/2, _)))
	),
	nucleo_base(DnaBase, Parts),
	memberchk(in(In), Parts),
	(	memberchk(dna, In)
	->	true
	;	throw(error(domain_error(dna_nucleo_base, DnaBase), context(dna_reverse_complement/2, _)))
	),	
	memberchk(complement(CompBase), Parts),
	'$dna_reverse_complement'(Dna, Comp).
'$dna_reverse_complement'([], []).
