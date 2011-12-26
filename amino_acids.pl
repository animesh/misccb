/* $Id: amino_acids.pl,v 0.1alpha 2007/04/17 13:09:40 Zzz Exp $

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


:- module(bioprolog_amino_acids, [
		amino_acids/1,
		amino_acid/3
	]).


amino_acids(['A', 'R', 'N', 'D', 'C', 'Q', 'E', 'G', 'H', 'I', 'L', 'K', 'M', 'F', 'P', 'S', 'T', 'W', 'Y', 'V']).

/* amino_acid/3
 * ?OneLetterCode, ?Parts, ?Alphabets
 * `Parts' is unified with a list containing the following:
 *		names(FullName, ThreeLettersCode)
 *		frequency(Frequency) % Creighton (1992)
 *		volume(Volume) % Creighton (1992)
 *		mass(Mass)
 *		charge(Charge)
 *		type(Type, CharCode) % where `Type' is one of "non_polar", "polar", "acidic", "basic".
 *		hydropathy_index(Index) % Kyte and Doolittle (1982)
 *		codons(Codons) % THIS SHOULD BE CHANGED IN ORDER TO IMPLEMENT DIFFERENT ALPHABETS
 */

amino_acid('G', [
		names('Glycine', 'Gly'), 
		frequency(7.2), 
		volume(48), 
		mass(57.05), 
		charge(0), 
		type(non_polar, 183),
		hydropathy_index(-0.4),
		codons(['GGU', 'GGC', 'GGA', 'GGG'])
	], [iupac_unambiguous_protein, iupac_ambiguous_protein, extended_protein, bioprolog_protein]).
	
amino_acid('A', [
		names('Alanine', 'Ala'), 
		frequency(8.3), 
		volume(67), 
		mass(71.09), 
		charge(0), 
		type(non_polar, 183),
		hydropathy_index(1.8),
		codons(['GCU', 'GCC', 'GCA', 'GCG'])
	], [iupac_unambiguous_protein, iupac_ambiguous_protein, extended_protein, bioprolog_protein]).
	
amino_acid('V', [
		names('Valine', 'Val'), 
		frequency(6.6), 
		volume(161), 
		mass(119.40), 
		charge(0), 
		type(non_polar, 183),
		hydropathy_index(4.2),
		codons(['GUU', 'GUC', 'GUA', 'GUG'])
	], [iupac_unambiguous_protein, iupac_ambiguous_protein, extended_protein, bioprolog_protein]).
	
amino_acid('L', [
		names('Leucine', 'Leu'), 
		frequency(9.0), 
		volume(124), 
		mass(113.16), 
		charge(0), 
		type(non_polar, 183),
		hydropathy_index(3.8),
		codons(['CUU', 'CUC', 'CUA', 'CUG', 'UUA', 'UUG'])
	], [iupac_unambiguous_protein, iupac_ambiguous_protein, extended_protein, bioprolog_protein]).
	
amino_acid('I', [
		names('Isoleucine', 'Ile'), 
		frequency(5.2), 
		volume(124), 
		mass(113.16), 
		charge(0), 
		type(non_polar, 183),
		hydropathy_index(4.5),
		codons(['AUU', 'AUC', 'AUA'])
	], [iupac_unambiguous_protein, iupac_ambiguous_protein, extended_protein, bioprolog_protein]).
	
amino_acid('P', [
		names('Proline', 'Pro'), 
		frequency(5.1), 
		volume(90), 
		mass(97.12), 
		charge(0), 
		type(non_polar, 183),
		hydropathy_index(-1.6),
		codons(['CCU', 'CCC', 'CCA', 'CCG'])
	], [iupac_unambiguous_protein, iupac_ambiguous_protein, extended_protein, bioprolog_protein]).
	
amino_acid('F', [
		names('Phenlalanine', 'Phe'), 
		frequency(3.9), 
		volume(135), 
		mass(128.17), 
		charge(0), 
		type(non_polar, 183),
		hydropathy_index(2.8),
		codons(['UUU', 'UUC'])
	], [iupac_unambiguous_protein, iupac_ambiguous_protein, extended_protein, bioprolog_protein]).
	
amino_acid('M', [
		names('Metionine', 'Met'), 
		frequency(2.4), 
		volume(124), 
		mass(131.19), 
		charge(0), 
		type(non_polar, 183),
		hydropathy_index(1.9),
		codons(['AUG'])
	], [iupac_unambiguous_protein, iupac_ambiguous_protein, extended_protein, bioprolog_protein]).
	
amino_acid('W', [
		names('Tryptophan', 'Trp'), 
		frequency(1.3), 
		volume(163), 
		mass(186.21), 
		charge(0), 
		type(non_polar, 183),
		hydropathy_index(-0.9),
		codons(['UGG'])
	], [iupac_unambiguous_protein, iupac_ambiguous_protein, extended_protein, bioprolog_protein]).
	
amino_acid('C', [
		names('Cysteine', 'Cys'), 
		frequency(1.7), 
		volume(86), 
		mass(103.15), 
		charge(0), 
		type(non_polar, 183),
		hydropathy_index(2.5),
		codons(['UGU', 'UGC'])
	], [iupac_unambiguous_protein, iupac_ambiguous_protein, extended_protein, bioprolog_protein]).
	
amino_acid('N', [
		names('Asparagine', 'Asn'), 
		frequency(4.4), 
		volume(96), 
		mass(114.11), 
		charge(0), 
		type(polar, 124),
		hydropathy_index(-3.5),
		codons(['AAU', 'AAC'])
	], [iupac_unambiguous_protein, iupac_ambiguous_protein, extended_protein, bioprolog_protein]).
	
amino_acid('Q', [
		names('Glutamine', 'Gln'), 
		frequency(4.0), 
		volume(114), 
		mass(128.14), 
		charge(0), 
		type(polar, 124),
		hydropathy_index(-3.5),
		codons(['CAA', 'CAG'])
	], [iupac_unambiguous_protein, iupac_ambiguous_protein, extended_protein, bioprolog_protein]).
	
amino_acid('S', [
		names('Serine', 'Ser'), 
		frequency(6.9), 
		volume(73), 
		mass(87.08), 
		charge(0), 
		type(polar, 124),
		hydropathy_index(-0.8),
		codons(['UCU', 'UCC', 'UCA', 'UCG', 'AGU', 'AGC'])
	], [iupac_unambiguous_protein, iupac_ambiguous_protein, extended_protein, bioprolog_protein]).
	
amino_acid('T', [
		names('Threonine', 'Thr'), 
		frequency(5.8), 
		volume(93), 
		mass(101.11), 
		charge(0), 
		type(polar, 124),
		hydropathy_index(-0.7),
		codons(['ACU', 'ACC', 'ACA', 'ACG'])
	], [iupac_unambiguous_protein, iupac_ambiguous_protein, extended_protein, bioprolog_protein]).
	
amino_acid('Y', [
		names('Tyrosine', 'Tyr'), 
		frequency(3.2), 
		volume(141), 
		mass(163.18), 
		charge(0), 
		type(polar, 124),
		hydropathy_index(-1.3),
		codons(['UAU', 'UAC'])
	], [iupac_unambiguous_protein, iupac_ambiguous_protein, extended_protein, bioprolog_protein]).
	
amino_acid('D', [
		names('Aspartic acid', 'Asp'), 
		frequency(5.3), 
		volume(91), 
		mass(115.09), 
		charge(-1), 
		type(acidic, 45),
		hydropathy_index(-3.5),
		codons(['GAU', 'GAC'])
	], [iupac_unambiguous_protein, iupac_ambiguous_protein, extended_protein, bioprolog_protein]).
	
amino_acid('E', [
		names('Glutamic acid', 'Glu'), 
		frequency(6.2), 
		volume(109), 
		mass(129.12), 
		charge(-1), 
		type(acidic, 45),
		hydropathy_index(-3.5),
		codons(['GAA', 'GAG'])
	], [iupac_unambiguous_protein, iupac_ambiguous_protein, extended_protein, bioprolog_protein]).
	
amino_acid('K', [
		names('Lysine', 'Lys'), 
		frequency(5.7), 
		volume(135), 
		mass(147.18), 
		charge(+1), 
		type(basic, 43),
		hydropathy_index(-3.9),
		codons(['AAA', 'AAG'])
	], [iupac_unambiguous_protein, iupac_ambiguous_protein, extended_protein, bioprolog_protein]).
	
amino_acid('R', [
		names('Arginine', 'Arg'), 
		frequency(5.7), 
		volume(148), 
		mass(156.19), 
		charge(+1), 
		type(basic, 43),
		hydropathy_index(-4.5),
		codons(['AGA', 'AGG', 'CGU', 'CGC', 'CGA', 'CGG'])
	], [iupac_unambiguous_protein, iupac_ambiguous_protein, extended_protein, bioprolog_protein]).
	
amino_acid('H', [
		names('Histidine', 'His'), 
		frequency(2.2), 
		volume(118), 
		mass(137.14), 
		charge(+1), 
		type(basic, 43),
		hydropathy_index(-3.2),
		codons(['CAU', 'CAC'])
	], [iupac_unambiguous_protein, iupac_ambiguous_protein, extended_protein, bioprolog_protein]).
	
amino_acid('X', Parts, [iupac_ambiguous_protein, bioprolog_protein]) :-
  amino_acids(Aminoacids),
  member(Aminoacid, Aminoacids),
  amino_acid(Aminoacid, Parts, _).
	
amino_acid('B', Parts, [iupac_ambiguous_protein, bioprolog_protein]) :-
  amino_acid('N', Parts, _)
  ;
  amino_acid('D', Parts, _).
  
amino_acid('Z', Parts, [iupac_ambiguous_protein, bioprolog_protein]) :-
  amino_acid('Q', Parts, _)
  ;
  amino_acid('E', Parts, _).
  
% IUPAC is moving to 'U' for selenocysteine instead of 'X'
amino_acid('U', [
		names('Selenocysteine', _)
	], [extended_protein]).
  
amino_acid('*', [
    names('Stop', 'stop'),
    codons(['UAA', 'UAG', 'UGA'])
  ], [bioprolog_protein]).
   
amino_acid('?', [
    names('Unknown', 'unknown'),
    codons([_])
  ], [bioprolog_protein]). 
  
amino_acid('-', [
    names('Deletion', 'deletion')
  ], [bioprolog_protein]).
  