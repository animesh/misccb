/* $Id: nucleo_bases.pl,v 0.1alpha 2007/03/02 22:51:46 Zzz Exp $

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

:- module(bioprolog_nucleo_bases, [
		nucleo_bases/1,
		nucleo_base/3
	]).


nucleo_bases(['U', 'T', 'C', 'A', 'G']).


% masses are in g/mol

nucleo_base('U', [
		name('Uracil'),
		mass(112.09),
		shape(pyrimidine),
		in([rna]),
		complement('A')
	], [iupac_unambiguous_rna, iupac_ambiguous_rna, extended_rna, bioprolog_rna]).
  
nucleo_base('T', [
		name('Thymine'),
		mass(126.11),
		shape(pyrimidine),
		in([dna]),
		translate_to('U'),
		complement('A')
	], [iupac_unambiguous_dna, iupac_ambiguous_dna, extended_dna, bioprolog_dna, iupac_unambiguous_rna, iupac_ambiguous_rna, extended_rna, bioprolog_rna]).
  
nucleo_base('C', [
		name('Cytosine'),
		mass(111.10),
		shape(pyrimidine),
		in([dna, rna]),
		translate_to('C'),
		complement('G')
	], [iupac_unambiguous_dna, iupac_ambiguous_dna, extended_dna, bioprolog_dna, iupac_unambiguous_rna, iupac_ambiguous_rna, extended_rna, bioprolog_rna]).
  
nucleo_base('A', [
		name('Adenine'),
		mass(135.13),
		shape(purine),
		in([dna, rna]),
		translate_to('A'),
		complement('T')
	], [iupac_unambiguous_dna, iupac_ambiguous_dna, extended_dna, bioprolog_dna, iupac_unambiguous_rna, iupac_ambiguous_rna, extended_rna, bioprolog_rna]).
  
nucleo_base('G', [
		name('Guanine'),
		mass(151.13),
		shape(purine),
		in([dna, rna]),
		translate_to('G'),
		complement('C')
	], [iupac_unambiguous_dna, iupac_ambiguous_dna, extended_dna, bioprolog_dna, iupac_unambiguous_rna, iupac_ambiguous_rna, extended_rna, bioprolog_rna]).
  
nucleo_base('B', [
		name('5-Bromouridine')
	], [extended_dna, extended_rna]).
	
nucleo_base('D', [
		name('5,6-Dihydrouridine')
	], [extended_dna, extended_rna]).
	
nucleo_base('S', [
		name('Thiouridine')
	], [extended_dna, extended_rna]).

nucleo_base('W', [
		name('Wyosine')
	], [extended_dna, extended_rna]).
  
nucleo_base('Y', Parts, [iupac_ambiguous_dna, iupac_ambiguous_rna, bioprolog_dna, bioprolog_rna]) :-
	nucleo_base('C', Parts)
	;
	nucleo_base('T', Parts)
	;
	nucleo_base('U', Parts).
  
nucleo_base('R', Parts, [iupac_ambiguous_dna, iupac_ambiguous_rna, bioprolog_dna, bioprolog_rna]) :-
	nucleo_base('A', Parts)
	;
	nucleo_base('G', Parts).  
   
nucleo_base('W', Parts, [iupac_ambiguous_dna, iupac_ambiguous_rna, bioprolog_dna, bioprolog_rna]) :-
	nucleo_base('A', Parts)
	;
	nucleo_base('T', Parts)
	;
	nucleo_base('U', Parts).
  
nucleo_base('S', Parts, [iupac_ambiguous_dna, iupac_ambiguous_rna, bioprolog_dna, bioprolog_rna]) :-
	nucleo_base('C', Parts)
	;
	nucleo_base('G', Parts). 
  
nucleo_base('K', Parts, [iupac_ambiguous_dna, iupac_ambiguous_rna, bioprolog_dna, bioprolog_rna]) :-
	nucleo_base('T', Parts)
	;
	nucleo_base('U', Parts)
	;
	nucleo_base('G', Parts).
  
nucleo_base('M', Parts, [iupac_ambiguous_dna, iupac_ambiguous_rna, bioprolog_dna, bioprolog_rna]) :-
	nucleo_base('C', Parts)
	;
	nucleo_base('A', Parts).
  
nucleo_base('B', Parts, [iupac_ambiguous_dna, iupac_ambiguous_rna, bioprolog_dna, bioprolog_rna]) :-
	nucleo_base('C', Parts)
	;
	nucleo_base('G', Parts)
	;
	nucleo_base('T', Parts)
	;
	nucleo_base('U', Parts).
  
nucleo_base('D', Parts, [iupac_ambiguous_dna, iupac_ambiguous_rna, bioprolog_dna, bioprolog_rna]) :-
	nucleo_base('A', Parts)
	;
	nucleo_base('G', Parts)
	;
	nucleo_base('T', Parts)
	;
	nucleo_base('U', Parts).
  
nucleo_base('H', Parts, [iupac_ambiguous_dna, iupac_ambiguous_rna, bioprolog_dna, bioprolog_rna]) :-
	nucleo_base('A', Parts)
	;
	nucleo_base('C', Parts)
	;
	nucleo_base('T', Parts)
	;
	nucleo_base('U', Parts).
  
nucleo_base('V', Parts, [iupac_ambiguous_dna, iupac_ambiguous_rna, bioprolog_dna, bioprolog_rna]) :-
	nucleo_base('A', Parts)
	;
	nucleo_base('C', Parts)
	;
	nucleo_base('G', Parts).
  
nucleo_base('N', Parts, [iupac_ambiguous_dna, iupac_ambiguous_rna, bioprolog_dna, bioprolog_rna]) :-
	nucleo_bases(NucleoBases),
	member(NucleoBase, NucleoBases),
	nucleo_base(NucleoBase, Parts).
  
nucleo_base(Unknown, [
		name('Unknown'),
		in([_])
	], [bioprolog_dna, bioprolog_rna]) :-
	member(Unknown, ['X', '?']).
  
nucleo_base(Deletion, [
		name('Deletion')
	], [bioprolog_dna, bioprolog_rna]) :-
	member(Deletion, ['O', '-']).
  