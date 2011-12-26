/* $Id: strings.pl,v 0.1alpha 2007/04/17 11:55:26 Zzz Exp $

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



/* This is a general purpose module library. 
 * If oy want to use this module out of BioProlog, just rename it to `strings' instead of `bioprolog_strings'. 
 */

:- module(bioprolog_strings, [
		atom_trim/2,
		trim/2,
		atom_strip/2,
		strip/2,
		atom_replace/4,
		replace/4,
		atom_replace_once/4,
		replace_once/4,
		atom_word/4,
		word/4,
		is_sub_atom/2,
		is_sub_string/2,
		atom_count/3,
		count/3,
		atom_remove_extra_spaces/2,
		remove_extra_spaces/2,
		atom_balanced_parenthesis/1,
		balanced_parenthesis/1,
		atom_case/2,
		case/2,
		atom_explode/3,
		explode/3
	]).


atom_trim(Atom, AtomResult) :-
	atom_codes(Atom, String),
	trim(String, Result),
	atom_codes(AtomResult, Result).

trim(String, Result) :-
	append(" ", Trim, String),
	!,
	trim(Trim, Result).
trim(Result, Result).

atom_strip(Atom, AtomResult) :-
	atom_codes(Atom, String),
	strip(String, Result),
	atom_codes(AtomResult, Result).

strip(String, Result) :-
	append(Strip, " ", String),
	!,
	strip(Strip, Result).
strip(Result, Result).

atom_replace(AtomString, AtomFind, AtomReplace, AtomResult) :-
	atom_codes(AtomString, String),
	atom_codes(AtomFind, Find),
	atom_codes(AtomReplace, Replace),
	replace(String, Find, Replace, Result),
	atom_codes(AtomResult, Result).

replace(String, Find, Replace, Result) :-
	append(Left, S, String),
	append(Find, Right, S),
	!,
	append(Left, Replace, R),
	append(R, Right, String0),
	replace(String0, Find, Replace, Result).
replace(Result, _, _, Result).

atom_replace_once(AtomString, AtomFind, AtomReplace, AtomResult) :-
	atom_codes(AtomString, String),
	atom_codes(AtomFind, Find),
	atom_codes(AtomReplace, Replace),
	replace_once(String, Find, Replace, Result),
	atom_codes(AtomResult, Result).

replace_once(String, Find, Replace, Result) :-
	append(Left, S, String),
	append(Find, Right, S),
	!,
	append(Left, Replace, R),
	append(R, Right, Result).

atom_word(AtomString, AtomSeparator, AtomWord, AtomRest) :-
	atom_codes(AtomString, String),
	atom_codes(AtomSeparator, Separator),
	word(String, Separator, Word, Rest),
	atom_codes(AtomWord, Word),
	atom_codes(AtomRest, Rest).

word("", "", "") :- 
	!.
word(String, Separator, Word, Rest) :-
	append(Word, S, String),
	append(Separator, Rest, S),
	!.
word(Word, _, Word, "").

is_sub_atom(Atom, SubAtom) :-
	sub_atom(Atom, _, _, _, SubAtom).

is_sub_string(String, SubString) :-
	append(_, S, String),
	append(SubString, _, S),
	!.

atom_count(AtomString, AtomFind, Count) :-
	atom_codes(AtomString, String),
	atom_codes(AtomFind, Find),
	count(String, Find, Count).

count(String, Find, Count) :-
	count(String, Find, 0, Count).
count(String, Find, Current, Count) :-
	append(_, S, String),
	append(Find, T, S),
	!,
	Next is Current+1,
	count(T, Find, Next, Count).
count(_, _, Count, Count).

atom_remove_extra_spaces(AtomString, AtomResult) :-
	atom_codes(AtomString, String),
	remove_extra_spaces(String, Result),
	atom_codes(AtomResult, Result).

remove_extra_spaces(String, Result) :-
	trim(String, Trim),
	strip(Trim, Strip),
	replace(Strip, "  ", " ", Result).
	
atom_balanced_parenthesis(AtomString) :-
	atom_codes(AtomString, String),
	balanced_parenthesis(String).

balanced_parenthesis(String) :-
	count(String, "(", Count),
	count(String, ")", Count).
	
atom_case(AtomDown, AtomUp) :-
	nonvar(AtomDown),
	!,
	upcase_atom(AtomDown, AtomUp).
atom_case(AtomDown, AtomUp) :-
	nonvar(AtomUp),
	!,
	downcase_atom(AtomUp, AtomDown).
	
case([Code|Codes], [UCode|UCodes]) :-
	code_type(Code, to_lower(UCode)),
	!,
	case(Codes, UCodes).
case([], []).

atom_explode(AtomString, AtomSeparator, AtomWords) :-
	atom_codes(AtomString, String),
	atom_codes(AtomSeparator, Separator),
	explode(String, Separator, Words),
	maplist(atom_codes, AtomWords, Words).

explode("", _, []) :-
	!.
explode(String, Separator, [Word|Words]) :-
	!,
	word(String, Separator, Word, Rest),
	explode(Rest, Separator, Words).



	



