/* $Id: sequences.pl,v 0.1alpha 2007/04/16 23:40:42 Zzz Exp $

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

:- module(bioprolog_sequences, [
		sub_sequence/3,
		sub_sequence/4,
		nb_sub_sequence/4,
		sub_sequence_matches/3,
		sub_sequence_frequency/3,
		sequence_elements/2,
		sequence_entropy/2,
		sub_sequence_entropy/3
	]).


/* sub_sequence/[3,4]
 * (Sequence, SubSequence, Parts, Constraints)
 * Successively unify `SubSequence' with a sub sequence of `Sequence'.
 *
 * `Parts' is a list containing the following informations (relative to `SubSequence'):
 * 		range(StartPosition, EndPosition),
 *		length(Length),
 *		matches(Matches, Frequency),
 *		sequence(Sequence, Before, After),
 *		elements(Elements, Number),
 *		entropy(Absolute, Relative)
 *
 * `Constraints' is a list that prunes the results. It can contain the following:
 *      length op Length
 *		start op Start
 *		end op End
 *      matches op Matches
 *		frequency op Frequency
 *      elements op Elements
 *		absolute_entropy op AbsEntropy
 * 		relative_entropy op RelEntropy
 * where `op' is an operator (>, <, =, >=, =<, \=)
 *		nb_constraints
 *
 * For example:
 * ?- sub_sequence('ABCDE', Sub, P, [ length = 3 ]).
 * will return 'ABC', 'BCD', 'CDE'.
 *
 * NOTE: nb_constraints has no effect on "length", "start", "end" constraints.
 *
 *
 * For details, see the manual.
 */
 
sub_sequence(Sequence, SubSequence, Parts) :-
    sub_sequence(Sequence, SubSequence, Parts, []).
    
sub_sequence(Sequence, SubSequence, Parts, Constraints) :-
	(	nonvar(Sequence),
		var(SubSequence),
		var(Parts),
		nonvar(Constraints)
	->	true
	;	throw(error(instantiation_error, context(sub_sequence/4, _)))
	),
    atom_codes(Sequence, SequenceCodes), 
    '$sub_sequence'(SequenceCodes, SubSequenceCodes, Parts, Constraints),
    atom_codes(SubSequence, SubSequenceCodes).

'$sub_sequence'(Sequence, SubSequence, [
		range(Start, End), 
		length(Length), 
		matches(Matches, Frequency), 
		sequence(Original, Before, After),
		elements(Elements, Number),
		entropy(AbsoluteEntropy, RelativeEntropy)
	], Constraints) :-
	(	memberchk(nb_constraints, Constraints)
	->	StopOnFailure = true
	;	StopOnFailure = false
	),
    append(BeforeCodes, S, Sequence),
    append(SubSequence, AfterCodes, S),
    SubSequence \= [],
    length(BeforeCodes, Index),
    Start is Index+1,
    '$sub_sequence_constraints'(start, Start, Constraints),    
    length(SubSequence, Length),
	'$sub_sequence_constraints'(length, Length, Constraints),
    End is Start+Length-1,
	'$sub_sequence_constraints'(end, End, Constraints),
    '$sequence_elements'(SubSequence, Elements, Number),
    (	'$sub_sequence_constraints'(elements, Number, Constraints)
    ->	true
    ;	(	StopOnFailure = true
		->	!,
			fail
		;	fail
		)
	),
    '$sub_sequence_matches'(Sequence, SubSequence, Matches),
    (	'$sub_sequence_constraints'(matches, Matches, Constraints)
    ->	true
    ;	(	StopOnFailure = true
		->	!,
			fail
		;	fail
		)
	),
	length(Sequence, SequenceLength),
	Frequency is Matches/(SequenceLength-Length+1),
    (	'$sub_sequence_constraints'(frequency, Frequency, Constraints)
    ->	true
    ;	(	StopOnFailure = true
		->	!,
			fail
		;	fail
		)
	),
    '$sub_sequence_entropy'(Sequence, SubSequence, RelativeEntropy),
    (	'$sub_sequence_constraints'(relative_entropy, RelativeEntropy, Constraints)
    ->	true
    ;	(	StopOnFailure = true
		->	!,
			fail
		;	fail
		)
	),
    '$sequence_entropy'(SubSequence, AbsoluteEntropy),
    (	'$sub_sequence_constraints'(absolute_entropy, AbsoluteEntropy, Constraints)
    ->	true
    ;	(	StopOnFailure = true
		->	!,
			fail
		;	fail
		)
	), 
    atom_codes(Original, Sequence),
    atom_codes(Before, BeforeCodes),
    atom_codes(After, AfterCodes).

'$sub_sequence_constraints'(Property, Variable, Constraints) :-
	'$sub_sequence_constraints'(Property, Variable, Constraints, [=, \=, >, >=, <, =<]).

'$sub_sequence_constraints'(Property, Variable, Constraints, [Operator|Operators]) :-
	!,
	Constraint =.. [Operator, Property, Value],
	(	memberchk(Constraint, Constraints)
	->	Goal =.. [Operator, Variable, Value],
		once(Goal	)
	;	true
	),
	'$sub_sequence_constraints'(Property, Variable, Constraints, Operators).
'$sub_sequence_constraints'(_, _, _, []).	




/* nb_sub_sequence/4
 * Sequence, SubSequence, Parts, Constraints
 *
 * Unify `SubSequence' with the last solution of the corresponding sub_sequence.
 *
 * NOTE: You should not use `start' constraint within this predicate.
 */
nb_sub_sequence(Sequence, SubSequence, Parts, Constraints) :-
	'$nb_sub_sequences'(Sequence, 1, SubSequences, Constraints),
	select(SubSequence-Parts, SubSequences, _).

'$nb_sub_sequences'(Sequence, Length, [], _) :-
	atom_length(Sequence, Length),
	!.
'$nb_sub_sequences'(Sequence, Start, [SubSequence-Parts|SubSequences], Constraints) :-
	atom_length(Sequence, Length),
	'$nb_sub_sequence'(Sequence, SubSequence, Parts, [start = Start|Constraints]),
	!,
	memberchk(range(_, End), Parts),
	NewStart is End+1,
	(	NewStart =< Length
	->	'$nb_sub_sequences'(Sequence, NewStart, SubSequences, Constraints)
	;	SubSequences = []
	).
'$nb_sub_sequences'(Sequence, Start, SubSequences, Constraints) :-
	NewStart is Start+1,
	'$nb_sub_sequences'(Sequence, NewStart, SubSequences, Constraints).
 
'$nb_sub_sequence'(Sequence, SubSequence, Parts, Constraints) :-
	findall(
		SubSequenceSolution-PartsSolution, 
		sub_sequence(Sequence, SubSequenceSolution, PartsSolution, [nb_constraints|Constraints]),
		Solutions
	),
	last(Solutions, SubSequence-Parts).



/* sub_sequence_matches/3
 * +Sequence, +SubSequence, -Matches
 *
 * Unify `Matches' with the number of occurrences of `SubSequence' into `Sequence'.
 */
sub_sequence_matches(Sequence, SubSequence, Matches) :-
	(	nonvar(Sequence)
	->	true
	;	throw(error(instantiation_error, context(sub_sequence_matches/3, _)))
	),
	(	nonvar(SubSequence)
	->	true
	;	throw(error(instantiation_error, context(sub_sequence_matches/3, _)))
	),
	(	once(sub_atom(Sequence, _, _, _, SubSequence))
	->	true
	;	throw(error(domain_error(sub_sequence, SubSequence), context(sub_sequence_matches/3, _)))
	),
	atom_codes(Sequence, SequenceCodes),
	atom_codes(SubSequence, SubSequenceCodes),
	'$sub_sequence_matches'(SequenceCodes, SubSequenceCodes, Matches).

'$sub_sequence_matches'(Sequence, SubSequence, Matches) :-
    length(Sequence, SequenceLength),
    length(SubSequence, SubSequenceLength),
    '$sub_sequence_delete'(Sequence, SubSequence, Residue),
    length(Residue, ResidueLength),
    Matches is ((SequenceLength-ResidueLength)/SubSequenceLength).

'$sub_sequence_delete'(Sequence, SubSequence, Result) :-
	append(Pre, S, Sequence),
	append(SubSequence, Post, S),
	!,
	append(Pre, Post, New),
	'$sub_sequence_delete'(New, SubSequence, Result).
'$sub_sequence_delete'(Result, _, Result).




/* sequence_elements/2
 * +Sequence, -Elements
 *
 * Unify `ElementsNumber' with the set of the elements of `Sequence'.
 */
 
sequence_elements(Sequence, Elements) :-
	(	nonvar(Sequence)
	->	true
	;	throw(error(instantiation_error, context(sequence_elements/2, _)))
	),
	atom_codes(Sequence, SequenceCodes),
	'$sequence_elements'(SequenceCodes, Elements, _).

'$sequence_elements'(SubSequence, Elements, Number) :-
	!,
    list_to_set(SubSequence, Set),
    atom_codes(Atom, Set),
    atom_chars(Atom, Elements),
    length(Set, Number).
'$sequence_elements'([], [], 0).



/* sequence_entropy/2
 * +Sequence, -Entropy
 *
 * Unify `Entropy' with the normalised entropy of `Sequence'.
 */
sequence_entropy(Sequence, Entropy) :-
	(	nonvar(Sequence)
	->	true
	;	throw(error(instantiation_error, context(sequence_entropy/2, _)))
	),
	atom_codes(Sequence, SequenceCodes),
	'$sequence_entropy'(SequenceCodes, Entropy).

'$sequence_entropy'(Sequence, Entropy) :-
	list_to_set(Sequence, Set),
	%length(Set, N),
	'$set_negentropy'(Set, Sequence, 0.0, NegEntropy),
	Entropy is -NegEntropy/log(2).
	
'$set_negentropy'([Aminoacid|Aminoacids], Sequence, T, NegEntropy) :-
	'$aminoacid_frequency'(Aminoacid, Sequence, Frequency),
	!,
	N is T+Frequency*log(Frequency),
	'$set_negentropy'(Aminoacids, Sequence, N, NegEntropy).
'$set_negentropy'([], _, NegEntropy, NegEntropy).

'$aminoacid_frequency'(_, [], 0.0) :- !.
'$aminoacid_frequency'(Aminoacid, Sequence, Frequency) :-
	length(Sequence, Length),
	delete(Sequence, Aminoacid, Rest),
	length(Rest, Deletion),
	Frequency is (Length-Deletion)/Length.
	

/* sub_sequence_entropy/3
 * +Sequence, +SubSequence, -Entropy
 *
 * Unify `Entropy' with the normalised entropy of `SubSequence' relative to 'Sequence'.
 */
sub_sequence_entropy(Sequence, SubSequence, Entropy) :-
	(	nonvar(Sequence)
	->	true
	;	throw(error(instantiation_error, context(sub_sequence_entropy/3, _)))
	),
	(	nonvar(SubSequence)
	->	true
	;	throw(error(instantiation_error, context(sub_sequence_entropy/3, _)))
	),
	(	once(sub_atom(Sequence, _, _, _, SubSequence))
	->	true
	;	throw(error(domain_error(sub_sequence, SubSequence), context(sub_sequence_entropy/3, _)))
	),
	atom_codes(Sequence, SequenceCodes),
	atom_codes(SubSequence, SubSequenceCodes),
	'$sub_sequence_entropy'(SequenceCodes, SubSequenceCodes, Entropy).
	
'$sub_sequence_entropy'(Sequence, SubSequence, Entropy) :-
	list_to_set(SubSequence, Set),
	%length(Set, N),
	'$set_negentropy'(Set, Sequence, 0.0, NegEntropy),
	Entropy is -NegEntropy/log(2).


/* sub_sequence_frequency/3
 * +Sequence, +SubSequence, -Frequency
 *
 * Unify `Frequency' with the frequency of `SubSequence' into `Sequence'.
 */
sub_sequence_frequency(Sequence, SubSequence, Frequency) :-
	(	nonvar(Sequence)
	->	true
	;	throw(error(instantiation_error, context(sub_sequence_frequency/3, _)))
	),
	(	nonvar(SubSequence)
	->	true
	;	throw(error(instantiation_error, context(sub_sequence_frequency/3, _)))
	),
	(	once(sub_atom(Sequence, _, _, _, SubSequence))
	->	true
	;	throw(error(domain_error(sub_sequence, SubSequence), context(sub_sequence_frequency/3, _)))
	),
	atom_codes(Sequence, SequenceCodes),
	atom_codes(SubSequence, SubSequenceCodes),
	'$sub_sequence_frequency'(SequenceCodes, SubSequenceCodes, Frequency).

'$sub_sequence_frequency'(_, [], 0.0) :- !.
'$sub_sequence_frequency'(Sequence, SubSequence, Frequency) :-
	length(Sequence, Length),
	length(SubSequence, SubLength),
	'$sub_sequence_matches'(Sequence, SubSequence, Matches),
	Frequency is Matches/(Length-SubLength+1).





  
