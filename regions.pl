/* $Id: regions.pl,v 0.1alpha 2007/04/17 12:27:58 Zzz Exp $

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

:- module(bioprolog_regions, [
		determ_motif_rich_region/4,
		nondeterm_motif_rich_region/4,
		most_frequent_motif/2,
		low_complexity_region/3,
		low_entropy_region/3,
		low_entropy_region/4
		%,seg/2
	]).

/*
:- load_foreign_library(seg).

/ * seg/2
 * An example of embedding C source into bioprolog.
 * default: -x -a
 * ignored: -p -q
 * /
seg(File, Options) :-
	seg_option(chars, Options, Chars, 60),
	seg_option(window, Options, Window, 12),
	seg_option(locut, Options, LoCut, 2.2),
	seg_option(hicut, Options, HiCut, 2.5),
	seg_option(minimum_length, Options, MinLength, 0),
	seg_option(maximum_trimming, Options, MaxTrim, 100),
	seg(File, Chars, Window, LoCut, HiCut, MinLength, MaxTrim).
	
seg_option(Name, Options, Var, Default) :-
	Opt =.. [Name, Var],
	memberchk(Opt, Options),
	!.
seg_option(_, _, Var, Var).
*/	
	
low_complexity_region(Sequence, LowComplexitySequence, Parts) :-
	sub_sequence(Sequence, LowComplexitySequence, OldParts),
	/* % uncomment these lines to reduce only to aminoacids presents into sequences instead of all 20 aminoacids.
	 * atom_chars(Sequence, List),
	 * list_to_set(List, Set),
	 */
	amino_acids(Set),
	select(Element, Set, _),
	'$substitutions_to'(Element, Substitutions),
	'$do_substitutions'(LowComplexitySequence, Substitutions, GroundSequence, SubstitutionValue, 0, ISubstitutionValue, 0),
	sub_atom(GroundSequence, 0, 1, _, GroundElement),
	append(OldParts, [substitution_index(SubstitutionValue, ISubstitutionValue), homopolypeptide(GroundElement)], Parts).
    
'$substitutions_to'(To, Substitutions) :-  
	current_prolog_flag(bioprolog_substitution_matrix, Matrix),
	substitution_matrix(Matrix, SubstitutionList),
	!,
	'$substitutions_to_filter'(To, SubstitutionList, Substitutions).
  
'$substitutions_to_filter'(To, [Head|Tail], [Head|T]) :-
	Head = (_, To, _),
	!,
	'$substitutions_to_filter'(To, Tail, T).
'$substitutions_to_filter'(To, [_|Tail], T) :-
	!,
	'$substitutions_to_filter'(To, Tail, T).
'$substitutions_to_filter'(_, [], []).

'$do_substitutions'(Sequence, [(From, To, SubstitutionValue)|Substitutions], ResultSequence, ResultValue, CurrentValue, IdenticalValue, CurrentIdentical) :-
	'$substitute_single'(Sequence, From, To, Substituted, HowManySubstitutions),
	!,
	Add is SubstitutionValue*HowManySubstitutions,
	Value is CurrentValue+Add,
	(	From = To
	->	IValue is CurrentIdentical
	;	IValue is CurrentIdentical+Add
	),
	'$do_substitutions'(Substituted, Substitutions, ResultSequence, ResultValue, Value, IdenticalValue, IValue).
'$do_substitutions'(ResultSequence, [], ResultSequence, ResultValue, ResultValue, IdenticalValue, IdenticalValue).
    
'$substitute_single'(Sequence, From, To, Result, HowManySubstitutions) :-
	atom_codes(Sequence, Codes),
	char_code(From, FromCode),
	char_code(To, ToCode),
	'$substitute_single1'(Codes, FromCode, ToCode, ResultCodes, HowManySubstitutions, 0),
	atom_codes(Result, ResultCodes).  
  
'$substitute_single1'([What|Tail], What, Into, [Into|R], HowMany, Current) :-
	!,
	Another is Current+1,
	'$substitute_single1'(Tail, What, Into, R, HowMany, Another).
'$substitute_single1'([Head|Tail], What, Into, [Head|R], HowMany, Current) :-
	!,
	'$substitute_single1'(Tail, What, Into, R, HowMany, Current).
'$substitute_single1'([], _, _, [], HowMany, HowMany).





determ_motif_rich_region(Sequence, MotifRichRegion, Motif, Parts) :-
	findall((MRR, P), '$determ_motif_rich_region'(Sequence, MRR, Motif, P), List),
	'$determ_motif_rich_region_list'(List, Formatted),
	keysort(Formatted, RevSorted),
	reverse(RevSorted, Sorted),
	'$determ_motif_rich_region_superpositions'(Sorted, Accepted),
	retractall('$superposition'(_, _)),
	!,
	select(_-(MotifRichRegion, _, _, Parts), Accepted, _).

'$determ_motif_rich_region_list'([(MRR, Parts)|MRRs], [Length-(MRR, Start, End, Parts)|Fs]) :-
	!,
	member(length(Length), Parts),
	member(range(Start, End), Parts),
	assert('$superposition'(Start, End)),
	'$determ_motif_rich_region_list'(MRRs, Fs).
'$determ_motif_rich_region_list'([], []).

'$determ_motif_rich_region_superpositions'([MRR|MRRs], [MRR|As]) :-
	MRR = _-(_, Start, End, _),
	findall((IStart, IEnd), '$superposition'(IStart, IEnd), List),
	'$determ_motif_rich_region_accept'(List, Start, End),		
	!,
	assert('$superposition'(Start, End)),
	'$determ_motif_rich_region_superpositions'(MRRs, As).
'$determ_motif_rich_region_superpositions'([_|MRRs], As) :-
	!,
	'$determ_motif_rich_region_superpositions'(MRRs, As).
'$determ_motif_rich_region_superpositions'([], []).
	
'$determ_motif_rich_region_accept'([(IStart, IEnd)|R], Start, End) :-
	\+ Start > IStart,
	\+ End < IEnd,
	!,
	'$determ_motif_rich_region_accept'(R, Start, End).
'$determ_motif_rich_region_accept'([], _, _).
	
	

'$determ_motif_rich_region'(Sequence, MotifRichRegion, Motif, [
		range(Start, End), 
		length(Length), 
		motif(Motif), 
		lc_equivalent(LCEquivalent)
	]) :-
	(	nonvar(Motif)
	->	true
	;	throw(error(instantiation_error, context(determ_motif_rich_region/4, _)))
	),
	sub_atom(Sequence, Before, Length, _, MotifRichRegion),
	atom_concat(Motif, S0, MotifRichRegion),
	atom_concat(Inner, Motif, S0),
	sub_sequence_frequency(Sequence, Motif, Frequency),
	(	catch(sub_sequence_frequency(Inner, Motif, InnerFrequency), _, fail)
	->	true
	;	InnerFrequency = 0
	),
	InnerFrequency >= Frequency,
	Start is Before+1,
	'$lc_equivalent'(Motif, Length, LCEquivalent),
	End is Start+Length.


nondeterm_motif_rich_region(Sequence, MotifRichRegion, Motif, Parts) :-
	var(Motif),
	most_frequent_motif(Sequence, Motif),
	determ_motif_rich_region(Sequence, MotifRichRegion, Motif, Parts).
	
'$lc_equivalent'(Motif, Length, LCEquivalent) :-
	'$lc_equivalent'(Motif, Length, '', LCEquivalent).
'$lc_equivalent'(_, Length, LCEquivalent, LCEquivalent) :-
	atom_length(LCEquivalent, LCELength),
	LCELength >= Length,
	!.
'$lc_equivalent'(Motif, Length, T, LCEquivalent) :-
	atom_concat(T, Motif, Temp),
	'$lc_equivalent'(Motif, Length, Temp, LCEquivalent).	
	
	
	

/*
 * Unify `Motif' with the most frequent motif into `Sequence' 
 * (the longer, for equal frequency).
 */	
most_frequent_motif(Sequence, Motif) :-
	(	nonvar(Sequence)
	->	true
	;	throw(error(instantiation_error, context(most_frequent_motif/2, _)))
	),
	atom_length(Sequence, SequenceLength),
	Length is SequenceLength//2+1,
	sub_atom(Sequence, 0, Length, _, SubSequence),
	sub_sequence_frequency(Sequence, SubSequence, MinimalFrequency),
	nb_setval('$most_frequent_motif', ('', MinimalFrequency)),
	'$most_frequent_motif'(Sequence, Length),
	nb_getval('$most_frequent_motif', (Motif, _)),
	nb_delete('$most_frequent_motif'),
	(	Motif = ''
	->	fail
	;	true
	).
	
'$most_frequent_motif'(_, 0) :- !.
'$most_frequent_motif'(Sequence, Length) :-
	(	sub_atom(Sequence, _, Length, _, SubSequence),
		sub_sequence_frequency(Sequence, SubSequence, Frequency),
		nb_getval('$most_frequent_motif', (_, CurrentFrequency)),
		(	Frequency > CurrentFrequency
		->	nb_setval('$most_frequent_motif', (SubSequence, Frequency))
		;	true
		),
		fail
	;	NewLength is Length-1,
		'$most_frequent_motif'(Sequence, NewLength)
	).
	





/*


low_entropy_region(Sequence, LowEntropyRegion, Parts) :-
	'$low_entropy_region'(Sequence, LowEntropyRegion, 1, Parts, _, _).
	
'$low_entropy_region'(Sequence, LowEntropyRegion, Start, [
		range(Start, End),
		length(Length),
		entropy(Entropy)
	], NewSequence, NewStart) :-
	first_low_entropy_region(Sequence, LowEntropyRegion, Entropy, Limit),
	End is Start+Limit,
	Length is Limit-Start+1,
	!,
	sub_atom(Sequence, End, _, 0, NewSequence),
	NewStart is End+1.


first_low_entropy_region(Sequence, Region, Entropy, End) :-
	Sequence \= '',
	(	nonvar(Sequence)
	->	true
	;	throw(error(instantiation_error, context(low_entropy_region/2, _)))
	),
	atom_length(Sequence, SequenceLength),
	sequence_entropy(Sequence, MaximumEntropy),
	nb_setval('$first_low_entropy_region', ('', MaximumEntropy, SequenceLength)),
	'$first_low_entropy_region'(Sequence, 1),
	nb_getval('$first_low_entropy_region', (Region, Entropy, End)),
	nb_delete('$first_low_entropy_region'),
	(	region = ''
	->	fail
	;	true
	).
	
'$first_low_entropy_region'(Sequence, Length) :- 
	atom_length(Sequence, Length),
	!.
'$first_low_entropy_region'(Sequence, Length) :-
	(	sub_atom(Sequence, 0, Length, _, SubSequence),
		sub_sequence_entropy(Sequence, SubSequence, Entropy),
		nb_getval('$first_low_entropy_region', (_, CurrentEntropy, _)),
		(	Entropy < CurrentEntropy
		->	nb_setval('$first_low_entropy_region', (SubSequence, Entropy, Length)),
			fail
		;	true
		)
	;	NewLength is Length+1,
		'$first_low_entropy_region'(Sequence, NewLength)
	).


*/

	
low_entropy_region(Sequence, LowEntropyRegion, Parts) :-
	low_entropy_region(Sequence, LowEntropyRegion, Parts, []).
low_entropy_region(Sequence, LowEntropyRegion, Parts, Options) :-
	(	select(entropy_ratio_less_than(LessThan), Options, Options0)
	->	true
	;	LessThan = 0.8,
		Options0 = Options
	),
	sub_sequence(Sequence, LowEntropyRegion, TempParts, Options0),
	member(entropy(RegionEntropy, SequenceEntropy), TempParts),
	EntropyRatio is RegionEntropy/SequenceEntropy,
	EntropyRatio < LessThan,
	append(TempParts, [entropy_ratio(EntropyRatio)], Parts).
	


	
	